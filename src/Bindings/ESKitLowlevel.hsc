#include <bindings.dsl.h>
#include <eskit.h>

module Bindings.ESKitLowlevel where
#strict_import

#opaque_t ekCMA
#opaque_t ekSepCMA
#opaque_t ekCSA

#starttype ekPoint
#field x , Ptr CDouble
#field fitness, CDouble
#stoptype

#starttype ekOptimizer
#field N, CSize
#field mu, CSize
#field lambda, CSize
#field nbUpdates, CSize
#field xMean, Ptr CDouble
#field points, Ptr (Ptr <ekPoint>)
#field bestPoint, <ekPoint>
#stoptype

#define hsc_sizeof(type) \
  bc_varid(#type) printf("Size = %u\n", sizeof(type)); \
  bc_varid(#type) printf("Size :: (Num t) => t\n");

#sizeof ekPoint
#ccall mkekOptimizer, CSize -> IO (Ptr <ekOptimizer>)
#ccall delekOptimizer, Ptr <ekOptimizer> -> IO ()
#ccall mkekCMA, CSize -> IO (Ptr <ekCMA>)
#ccall delekCMA, Ptr <ekOptimizer> -> IO ()
#ccall mkekSepCMA, CSize -> IO (Ptr <ekSepCMA>)
#ccall delekSepCMA, Ptr <ekOptimizer> -> IO ()
#ccall mkekCSA, CSize -> IO (Ptr <ekCSA>)
#ccall delekCSA, Ptr <ekOptimizer> -> IO ()

#ccall ekOptimizer_setMuLambda, Ptr <ekOptimizer> -> CSize -> CSize -> IO ()
#ccall ekOptimizer_start, Ptr <ekOptimizer> -> IO ()
#ccall ekOptimizer_sampleCloud, Ptr <ekOptimizer> -> IO ()
#ccall ekOptimizer_samplePoint, Ptr <ekOptimizer> -> CSize -> IO ()
#ccall ekOptimizer_update, Ptr <ekOptimizer> -> IO ()
#integral_t enum ekStopCriterionId
#num ekStopCriterionId_DistributionNotSet
#num ekStopCriterionId_LowSigma
#num ekStopCriterionId_NoEffectAxis
#num ekStopCriterionId_NoEffectCoord
#num ekStopCriterionId_ConditionCov
#num ekStopCriterionId_EigenSolverFailure
#ccall ekOptimizer_stop, Ptr <ekOptimizer> -> IO <ekStopCriterionId>
#callback ekEvalFunction, Ptr CDouble -> CSize -> IO CDouble
#ccall ekOptimizer_evaluateFunction, Ptr <ekOptimizer> -> <ekEvalFunction> -> IO ()
#ccall ekCMA_setSigma, Ptr <ekCMA> -> CDouble -> CDouble -> IO ()
#ccall ekCMA_setOptimizer, Ptr <ekCMA> -> Ptr <ekOptimizer> -> IO ()
#ccall ekCMA_sigma, Ptr <ekCMA> -> CDouble
#ccall ekSepCMA_setSigma, Ptr <ekSepCMA> -> CDouble -> CDouble -> IO ()
#ccall ekSepCMA_setOptimizer, Ptr <ekSepCMA> -> Ptr <ekOptimizer> -> IO ()
#ccall ekSepCMA_sigma, Ptr <ekSepCMA> -> CDouble
#ccall ekCSA_setSigma, Ptr <ekCSA> -> CDouble -> CDouble -> IO ()
#ccall ekCSA_setOptimizer, Ptr <ekCSA> -> Ptr <ekOptimizer> -> IO ()
#ccall ekCSA_sigma, Ptr <ekCSA> -> CDouble
