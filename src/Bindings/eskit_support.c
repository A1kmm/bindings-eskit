#include <eskit.h>
#include <stdlib.h>

#define MKDEL_DATA(type, paramTypes, params) \
  type*\
  mk##type(paramTypes)\
  {\
    type* eo = malloc(sizeof(type)); \
    type##_init(eo, params); \
    return eo; \
  } \
  void \
  del##type(type* eo) \
  { \
    type##_destroy(eo); \
    free(eo); \
  }

MKDEL_DATA(ekOptimizer, size_t N, N)
MKDEL_DATA(ekCMA, size_t N, N)
MKDEL_DATA(ekSepCMA, size_t N, N)
MKDEL_DATA(ekCSA, size_t N, N)
