/*  File produced by Kranc */

#define KRANC_C

#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"
#include "GenericFD.h"
#include "Differencing.h"
#include "cctk_Loop.h"
#include "loopcontrol.h"

/* Define macros used in calculations */
#define INITVALUE (42)
#define QAD(x) (SQR(SQR(x)))
#define INV(x) ((1.0) / (x))
#define SQR(x) ((x) * (x))
#define CUB(x) ((x) * (x) * (x))

static void eulerauto_cons_calc_intercell_conserved_2_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* Declare finite differencing variables */
  
  /* Include user-supplied include files */
  
  /* Initialise finite differencing variables */
  ptrdiff_t const di = 1;
  ptrdiff_t const dj = CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  ptrdiff_t const dk = CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  ptrdiff_t const cdi = sizeof(CCTK_REAL) * di;
  ptrdiff_t const cdj = sizeof(CCTK_REAL) * dj;
  ptrdiff_t const cdk = sizeof(CCTK_REAL) * dk;
  CCTK_REAL const dx = ToReal(CCTK_DELTA_SPACE(0));
  CCTK_REAL const dy = ToReal(CCTK_DELTA_SPACE(1));
  CCTK_REAL const dz = ToReal(CCTK_DELTA_SPACE(2));
  CCTK_REAL const dt = ToReal(CCTK_DELTA_TIME);
  CCTK_REAL const t = ToReal(cctk_time);
  CCTK_REAL const dxi = INV(dx);
  CCTK_REAL const dyi = INV(dy);
  CCTK_REAL const dzi = INV(dz);
  CCTK_REAL const khalf = 0.5;
  CCTK_REAL const kthird = 1/3.0;
  CCTK_REAL const ktwothird = 2.0/3.0;
  CCTK_REAL const kfourthird = 4.0/3.0;
  CCTK_REAL const keightthird = 8.0/3.0;
  CCTK_REAL const hdxi = 0.5 * dxi;
  CCTK_REAL const hdyi = 0.5 * dyi;
  CCTK_REAL const hdzi = 0.5 * dzi;
  
  /* Initialize predefined quantities */
  CCTK_REAL const p1o1 = 1;
  CCTK_REAL const p1odx = INV(dx);
  CCTK_REAL const p1ody = INV(dy);
  CCTK_REAL const p1odz = INV(dz);
  
  /* Assign local copies of arrays functions */
  
  
  
  /* Calculate temporaries and arrays functions */
  
  /* Copy local copies back to grid functions */
  
  /* Loop over the grid points */
  #pragma omp parallel
  CCTK_LOOP3 (eulerauto_cons_calc_intercell_conserved_2,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_lsh[0],cctk_lsh[1],cctk_lsh[2])
  {
    ptrdiff_t const index = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL pLeftL = pLeft[index];
    CCTK_REAL pRightL = pRight[index];
    CCTK_REAL rhoLeftL = rhoLeft[index];
    CCTK_REAL rhoRightL = rhoRight[index];
    CCTK_REAL v1LeftL = v1Left[index];
    CCTK_REAL v1RightL = v1Right[index];
    CCTK_REAL v2LeftL = v2Left[index];
    CCTK_REAL v2RightL = v2Right[index];
    CCTK_REAL v3LeftL = v3Left[index];
    CCTK_REAL v3RightL = v3Right[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL DenLeftL = rhoLeftL;
    
    CCTK_REAL S1LeftL = rhoLeftL*v1LeftL;
    
    CCTK_REAL S2LeftL = rhoLeftL*v2LeftL;
    
    CCTK_REAL S3LeftL = rhoLeftL*v3LeftL;
    
    CCTK_REAL EnLeftL = 0.5*INV(-1 + ToReal(gamma))*(2*pLeftL + 
      rhoLeftL*(SQR(v1LeftL) + SQR(v2LeftL) + SQR(v3LeftL))*(-1 + 
      ToReal(gamma)));
    
    CCTK_REAL DenRightL = rhoRightL;
    
    CCTK_REAL S1RightL = rhoRightL*v1RightL;
    
    CCTK_REAL S2RightL = rhoRightL*v2RightL;
    
    CCTK_REAL S3RightL = rhoRightL*v3RightL;
    
    CCTK_REAL EnRightL = 0.5*INV(-1 + ToReal(gamma))*(2*pRightL + 
      rhoRightL*(SQR(v1RightL) + SQR(v2RightL) + SQR(v3RightL))*(-1 + 
      ToReal(gamma)));
    
    /* Copy local copies back to grid functions */
    DenLeft[index] = DenLeftL;
    DenRight[index] = DenRightL;
    EnLeft[index] = EnLeftL;
    EnRight[index] = EnRightL;
    S1Left[index] = S1LeftL;
    S1Right[index] = S1RightL;
    S2Left[index] = S2LeftL;
    S2Right[index] = S2RightL;
    S3Left[index] = S3LeftL;
    S3Right[index] = S3RightL;
  }
  CCTK_ENDLOOP3 (eulerauto_cons_calc_intercell_conserved_2);
}

extern "C" void eulerauto_cons_calc_intercell_conserved_2(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulerauto_cons_calc_intercell_conserved_2_Body");
  }
  
  if (cctk_iteration % eulerauto_cons_calc_intercell_conserved_2_calc_every != eulerauto_cons_calc_intercell_conserved_2_calc_offset)
  {
    return;
  }
  
  const char *groups[] = {"EulerAuto::Den_lr_group","EulerAuto::En_lr_group","EulerAuto::p_lr_group","EulerAuto::rho_lr_group","EulerAuto::S1_lr_group","EulerAuto::S2_lr_group","EulerAuto::S3_lr_group","EulerAuto::v1_lr_group","EulerAuto::v2_lr_group","EulerAuto::v3_lr_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulerauto_cons_calc_intercell_conserved_2", 10, groups);
  
  
  GenericFD_LoopOverEverything(cctkGH, &eulerauto_cons_calc_intercell_conserved_2_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulerauto_cons_calc_intercell_conserved_2_Body");
  }
}
