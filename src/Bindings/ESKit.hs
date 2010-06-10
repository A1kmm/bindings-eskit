module Bindings.ESKit (OptimisationProblem(OptimisationProblem),
                       DistributionHandler(CMAHandler, SepCMAHandler, CSAHandler),
                       n, mu, lambda, fitnessFunction, distributionHandlerSettings,
                       optimize)
where

import qualified Bindings.ESKitLowlevel as LL
import Control.Monad
import Data.Maybe
import Foreign
import Foreign.C.Types

data OptimisationProblem = OptimisationProblem {
      n :: Int,
      mu :: Maybe Int,
      lambda :: Maybe Int,
      fitnessFunction :: [Double] -> Double,
      distributionHandlerSettings :: DistributionHandler
    };

data DistributionHandler =
    CMAHandler (Double, Double) |
    SepCMAHandler (Double, Double) |
    CSAHandler (Double, Double)

setupDistribHandler :: CSize -> Ptr LL.C'ekOptimizer -> DistributionHandler -> IO (Ptr CChar)
setupDistribHandler np eo (CMAHandler (sigmalow, sigmahigh)) = do
  p <- LL.c'mkekCMA np
  LL.c'ekCMA_setSigma p (realToFrac sigmahigh) (realToFrac sigmalow)
  LL.c'ekCMA_setOptimizer p eo
  return $ castPtr p

setupDistribHandler np eo (SepCMAHandler (sigmalow, sigmahigh)) = do
  p <- LL.c'mkekSepCMA np
  LL.c'ekSepCMA_setSigma p (realToFrac sigmahigh) (realToFrac sigmalow)
  LL.c'ekSepCMA_setOptimizer p eo
  return $ castPtr p

setupDistribHandler np eo (CSAHandler (sigmalow, sigmahigh)) = do
  p <- LL.c'mkekCSA np
  LL.c'ekCSA_setSigma p (realToFrac sigmahigh) (realToFrac sigmalow)
  LL.c'ekCSA_setOptimizer p eo
  return $ castPtr p

destroyDistribHandler :: Ptr CChar -> DistributionHandler -> IO ()
destroyDistribHandler dh (CMAHandler _) =
  LL.c'delekCMA (castPtr dh)
destroyDistribHandler dh (SepCMAHandler _) =
  LL.c'delekSepCMA (castPtr dh)
destroyDistribHandler dh (CSAHandler _) =
  LL.c'delekCSA (castPtr dh)

optimize :: OptimisationProblem -> Either String [Double]
optimize (OptimisationProblem { n = np, mu = mmu, lambda = mlambda,
                                fitnessFunction = fitness,
                                distributionHandlerSettings = dhsettings }) =
          unsafePerformIO $ do
            eo <- LL.c'mkekOptimizer (fromIntegral np)
            dh <- setupDistribHandler (fromIntegral np) eo dhsettings
            eod <- peek eo
            let lambdav = (flip fromMaybe (fmap fromIntegral mlambda) (LL.c'ekOptimizer'lambda eod))
            (LL.c'ekOptimizer_setMuLambda eo)
              (flip fromMaybe (fmap fromIntegral mmu) (LL.c'ekOptimizer'mu eod))
              lambdav
            LL.c'ekOptimizer_start eo
            let continueOptimization =
                  do
                      LL.c'ekOptimizer_sampleCloud eo
                      allPts <- liftM LL.c'ekOptimizer'points (peek eo)
                      forM_ [0..((fromIntegral lambdav)-1)] $ \i ->
                                do
                                  ptp <- peekElemOff allPts i
                                  pt <- peek ptp
                                  let xcoordp = LL.c'ekPoint'x pt
                                  xcoord <- mapM (liftM (realToFrac :: CDouble -> Double) . peekElemOff xcoordp) [0..(np-1)]
                                  poke ptp (pt {LL.c'ekPoint'fitness = realToFrac $ fitness xcoord })
                      LL.c'ekOptimizer_update eo
                      stop <- LL.c'ekOptimizer_stop eo
                      if stop == 0
                        then
                          continueOptimization
                        else
                          return stop
            whyStop <- continueOptimization
            destroyDistribHandler dh dhsettings
            ret <-
                    do
                      bestPoint <- liftM LL.c'ekOptimizer'bestPoint (peek eo)
                      let xcoordp = LL.c'ekPoint'x bestPoint
                      xcoord <- mapM (liftM (realToFrac :: CDouble -> Double) . peekElemOff xcoordp) [0..(np-1)]
                      return $ Right xcoord
              {-
              case ()
              of
                () | whyStop == LL.c'ekStopCriterionId_LowSigma ->
                () | whyStop == LL.c'ekStopCriterionId_DistributionNotSet ->
	          return $ Left "No point distribution handler has been associated with the opimiser"
                () | whyStop == LL.c'ekStopCriterionId_NoEffectAxis ->
	          return $ Left "Axes of the Gaussian distribution are beyond what numerical precision can handle"
                () | whyStop == LL.c'ekStopCriterionId_NoEffectCoord ->
	          return $ Left "Eigen vector basis of the Gaussian distribution are beyond what numerical precision can handle"
                () | whyStop == LL.c'ekStopCriterionId_ConditionCov ->
	          return $ Left "Covariance matrix conditionning is beyond what numerical precision can handle."
                () ->
	          return $ Left "The eigenvalue solver failed."
              -}
            LL.c'delekOptimizer eo
            return ret

{-
To do: tests:
  optimize (OptimisationProblem { n = 2, mu = Nothing, lambda = Nothing, fitnessFunction = \(x:y:[]) -> (1-x)^2 + 100 * (y - x^2)^2, distributionHandlerSettings = CMAHandler (0.0001,1000)})
  => Should be close to [1,1]

  optimize (OptimisationProblem { n = 1, mu = Nothing, lambda = Nothing, fitnessFunction = \(x:[]) -> abs((x-100) * (x - 200) * (x - 300) * (x + 400)), distributionHandlerSettings = CMAHandler (0.0001,1000)})
  => Several solutions (obviously).
-}