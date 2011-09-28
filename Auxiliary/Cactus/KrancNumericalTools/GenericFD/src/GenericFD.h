/*@@                                         
   @file      GenericFD/src/GenericFD.h
   @date      June 16 2002
   @author    S. Husa                           
   @desc

   $Id$                                  
   
   @enddesc                                     
 @@*/                                           

/*  Copyright 2004 Sascha Husa, Ian Hinder, Christiane Lechner

    This file is part of Kranc.

    Kranc is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Kranc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Kranc; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include "cctk.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef __cplusplus
#  ifdef CCTK_CXX_RESTRICT
#    define restrict CCTK_CXX_RESTRICT
#  endif
#endif

#include "MathematicaCompat.h"

#ifdef KRANC_C

  /* Grid function access */
  /* var is a pointer to a grid point (an lvalue), i,j,k are offsets
     with respect to that point.
     For example: KRANC_GFINDEX3D_OFFSET(&u[ind3d],-1,-1,0) */
#ifndef VECTORISE
  /* standard, no vectorisation */
  /* simple implementation */
  /* #  define KRANC_GFOFFSET3D(var,i,j,k) ((var)[di*(i)+dj*(j)+dk*(k)]) */
  /* more efficient implementation for some compilers */
#  define KRANC_GFOFFSET3D(var,i,j,k)                                   \
  (*(CCTK_REAL const*)&((char const*)(var))[cdi*(i)+cdj*(j)+cdk*(k)])
#else
  /* vectorised version */
#  define KRANC_GFOFFSET3D(var,i,j,k)                                   \
  vec_loadu_maybe3((i),(j),(k),                                         \
                   *(CCTK_REAL const*)&                                 \
                   ((char const*)(var))[cdi*(i)+cdj*(j)+cdk*(k)])
#endif

int sgn(CCTK_REAL x);

int GenericFD_GetBoundaryWidth(cGH const * restrict const cctkGH);

void GenericFD_GetBoundaryInfo(cGH const * restrict cctkGH,
                               int const * restrict cctk_lsh,
                               int const * restrict cctk_lssh,
                               int const * restrict cctk_bbox,
			       int const * restrict cctk_nghostzones,
                               int * restrict imin, 
			       int * restrict imax,
                               int * restrict is_symbnd, 
			       int * restrict is_physbnd,
                               int * restrict is_ipbnd);

void GenericFD_AssertGroupStorage(cGH const * restrict const cctkGH, const char *calc,
                                  int ngroups, const char *group_names[]);

/* Summation by parts */

static inline CCTK_REAL sbp_deriv_x(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
  CCTK_ATTRIBUTE_PURE;
static inline CCTK_REAL sbp_deriv_x(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
{
  CCTK_REAL dvarl = 0;
  int ni = cctkGH->cctk_lsh[0];
  for (int ii=min[i]-1; ii<=max[i]-1; ++ii) {
    dvarl += q[ii+ni*i]*var[CCTK_GFINDEX3D (cctkGH, ii, j, k)];
  }
  dvarl /= d;
  return dvarl;
}

static inline CCTK_REAL sbp_deriv_y(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
  CCTK_ATTRIBUTE_PURE;
static inline CCTK_REAL sbp_deriv_y(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
{
  CCTK_REAL dvarl = 0;
  int nj = cctkGH->cctk_lsh[1];
  for (int jj=min[j]-1; jj<=max[j]-1; ++jj) {
    dvarl += q[jj+nj*j]*var[CCTK_GFINDEX3D (cctkGH, i, jj, k)];
  }
  dvarl /= d;
  return dvarl;
}

static inline CCTK_REAL sbp_deriv_z(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
  CCTK_ATTRIBUTE_PURE;
static inline CCTK_REAL sbp_deriv_z(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
{
  CCTK_REAL dvarl = 0;
  int nk = cctkGH->cctk_lsh[2];
  for (int kk=min[k]-1; kk<=max[k]-1; ++kk) {
    dvarl += q[kk+nk*k]*var[CCTK_GFINDEX3D (cctkGH, i, j, kk)];
  }
  dvarl /= d;
  return dvarl;
}

/* New calculation format */

typedef void(*Kranc_Calculation)(cGH const * restrict cctkGH,
                                 int eir,
                                 int face,
                                 CCTK_REAL const normal[3],
                                 CCTK_REAL const tangentA[3],
                                 CCTK_REAL const tangentB[3],
                                 int const min[3],
                                 int const max[3], 
                                 int n_subblock_gfs, 
                                 CCTK_REAL * restrict const subblock_gfs[]);

void GenericFD_LoopOverEverything(cGH const * restrict cctkGH, Kranc_Calculation calc);
void GenericFD_LoopOverBoundary(cGH const * restrict cctkGH, Kranc_Calculation calc);
void GenericFD_LoopOverBoundaryWithGhosts(cGH const * restrict cctkGH, Kranc_Calculation calc);
void GenericFD_LoopOverInterior(cGH const * restrict cctkGH, Kranc_Calculation calc);

void GenericFD_GroupDataPointers(cGH const * restrict const cctkGH, const char *group_name,
                                 int nvars, CCTK_REAL const *restrict *ptrs);
void GenericFD_EnsureStencilFits(cGH const * restrict const cctkGH, const char *calc, int ni, int nj, int nk);


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
