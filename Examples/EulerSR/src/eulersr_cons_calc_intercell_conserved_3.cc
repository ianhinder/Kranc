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

static void eulersr_cons_calc_intercell_conserved_3_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
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
  CCTK_LOOP3 (eulersr_cons_calc_intercell_conserved_3,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_lsh[0],cctk_lsh[1],cctk_lsh[2])
  {
    ptrdiff_t const index = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenLeftL = DenLeft[index];
    CCTK_REAL DenRightL = DenRight[index];
    CCTK_REAL epsiLeftL = epsiLeft[index];
    CCTK_REAL epsiRightL = epsiRight[index];
    CCTK_REAL hL = h[index];
    CCTK_REAL pL = p[index];
    CCTK_REAL rhoLeftL = rhoLeft[index];
    CCTK_REAL rhoRightL = rhoRight[index];
    CCTK_REAL v1LeftL = v1Left[index];
    CCTK_REAL v1RightL = v1Right[index];
    CCTK_REAL v2LeftL = v2Left[index];
    CCTK_REAL v2RightL = v2Right[index];
    CCTK_REAL v3LeftL = v3Left[index];
    CCTK_REAL v3RightL = v3Right[index];
    CCTK_REAL WL = W[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL Wx = 1 - SQR(v1LeftL) - SQR(v2LeftL) - SQR(v3LeftL);
    
    WL = INV(sqrt(Wx));
    
    pL = epsiLeftL*rhoLeftL*(-1 + ToReal(gamma));
    
    hL = 1 + epsiLeftL + pL*INV(rhoLeftL);
    
    DenLeftL = rhoLeftL*WL;
    
    CCTK_REAL S1LeftL = hL*rhoLeftL*v1LeftL*SQR(WL);
    
    CCTK_REAL S2LeftL = hL*rhoLeftL*v2LeftL*SQR(WL);
    
    CCTK_REAL S3LeftL = hL*rhoLeftL*v3LeftL*SQR(WL);
    
    CCTK_REAL tauLeftL = -DenLeftL - pL + hL*rhoLeftL*SQR(WL);
    
    Wx = 1 - SQR(v1RightL) - SQR(v2RightL) - SQR(v3RightL);
    
    WL = INV(sqrt(Wx));
    
    pL = epsiRightL*rhoRightL*(-1 + ToReal(gamma));
    
    hL = 1 + epsiRightL + pL*INV(rhoRightL);
    
    DenRightL = rhoRightL*WL;
    
    CCTK_REAL S1RightL = hL*rhoRightL*v1RightL*SQR(WL);
    
    CCTK_REAL S2RightL = hL*rhoRightL*v2RightL*SQR(WL);
    
    CCTK_REAL S3RightL = hL*rhoRightL*v3RightL*SQR(WL);
    
    CCTK_REAL tauRightL = -DenRightL - pL + 
      hL*rhoRightL*SQR(WL);
    
    /* Copy local copies back to grid functions */
    DenLeft[index] = DenLeftL;
    DenRight[index] = DenRightL;
    h[index] = hL;
    p[index] = pL;
    S1Left[index] = S1LeftL;
    S1Right[index] = S1RightL;
    S2Left[index] = S2LeftL;
    S2Right[index] = S2RightL;
    S3Left[index] = S3LeftL;
    S3Right[index] = S3RightL;
    tauLeft[index] = tauLeftL;
    tauRight[index] = tauRightL;
    W[index] = WL;
  }
  CCTK_ENDLOOP3 (eulersr_cons_calc_intercell_conserved_3);
}

extern "C" void eulersr_cons_calc_intercell_conserved_3(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_intercell_conserved_3_Body");
  }
  
  if (cctk_iteration % eulersr_cons_calc_intercell_conserved_3_calc_every != eulersr_cons_calc_intercell_conserved_3_calc_offset)
  {
    return;
  }
  
  const char *groups[] = {"EulerSR::Den_lr_group","EulerSR::epsi_lr_group","EulerSR::h_group","EulerSR::p_group","EulerSR::rho_lr_group","EulerSR::S1_lr_group","EulerSR::S2_lr_group","EulerSR::S3_lr_group","EulerSR::tau_lr_group","EulerSR::v1_lr_group","EulerSR::v2_lr_group","EulerSR::v3_lr_group","EulerSR::W_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulersr_cons_calc_intercell_conserved_3", 13, groups);
  
  
  GenericFD_LoopOverEverything(cctkGH, &eulersr_cons_calc_intercell_conserved_3_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_intercell_conserved_3_Body");
  }
}
