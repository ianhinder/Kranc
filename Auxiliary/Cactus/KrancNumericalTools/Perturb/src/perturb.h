/* $Header$ */

#ifndef PERTURB_H
#define PERTURB_H

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "cctk.h"


/* constants */

#define MAXDIM  3


/* macros */

#define RAND_VAL ((random()*(1.0/RAND_MAX)-0.5)*amplitude)

#define BOUNDARY_PERTURB(doBC,                                            \
                       iend, jend, kend,                                \
                       ii, jj, kk)                                      \
{                                                                       \
  if (doBC)                                                             \
  {                                                                     \
    CCTK_REAL* v= CCTK_VarDataPtrI(GH, timelvl, var);                   \
    for (k = 0; k < kend; k++)                                          \
    {                                                                   \
      for (j = 0; j < jend; j++)                                        \
      {                                                                 \
        for (i = 0; i < iend; i++)                                      \
        {                                                               \
          const int _index = CCTK_GFINDEX3D(GH, (ii), (jj), (kk));      \
          v[_index] += RAND_VAL;                                        \
        }                                                               \
      }                                                                 \
    }                                                                   \
  }                                                                     \
}



#endif /* !define(PERTURB_H) */
