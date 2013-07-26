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
#define INV(x) ((CCTK_REAL)1.0 / (x))
#define SQR(x) ((x) * (x))
#define CUB(x) ((x) * SQR(x))
#define QAD(x) (SQR(SQR(x)))

static void eulersr_cons_calc_intercell_conserved_2_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* Include user-supplied include files */
  
  /* Initialise finite differencing variables */
  ptrdiff_t /*const*/ di CCTK_ATTRIBUTE_UNUSED  = 1;
  ptrdiff_t /*const*/ dj CCTK_ATTRIBUTE_UNUSED  = CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  ptrdiff_t /*const*/ dk CCTK_ATTRIBUTE_UNUSED  = CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  ptrdiff_t /*const*/ cdi CCTK_ATTRIBUTE_UNUSED  = sizeof(CCTK_REAL) * di;
  ptrdiff_t /*const*/ cdj CCTK_ATTRIBUTE_UNUSED  = sizeof(CCTK_REAL) * dj;
  ptrdiff_t /*const*/ cdk CCTK_ATTRIBUTE_UNUSED  = sizeof(CCTK_REAL) * dk;
  CCTK_REAL /*const*/ dx CCTK_ATTRIBUTE_UNUSED  = ToReal(CCTK_DELTA_SPACE(0));
  CCTK_REAL /*const*/ dy CCTK_ATTRIBUTE_UNUSED  = ToReal(CCTK_DELTA_SPACE(1));
  CCTK_REAL /*const*/ dz CCTK_ATTRIBUTE_UNUSED  = ToReal(CCTK_DELTA_SPACE(2));
  CCTK_REAL /*const*/ dt CCTK_ATTRIBUTE_UNUSED  = ToReal(CCTK_DELTA_TIME);
  CCTK_REAL /*const*/ t CCTK_ATTRIBUTE_UNUSED  = ToReal(cctk_time);
  CCTK_REAL /*const*/ dxi CCTK_ATTRIBUTE_UNUSED  = INV(dx);
  CCTK_REAL /*const*/ dyi CCTK_ATTRIBUTE_UNUSED  = INV(dy);
  CCTK_REAL /*const*/ dzi CCTK_ATTRIBUTE_UNUSED  = INV(dz);
  CCTK_REAL /*const*/ khalf CCTK_ATTRIBUTE_UNUSED  = 0.5;
  CCTK_REAL /*const*/ kthird CCTK_ATTRIBUTE_UNUSED  = 1/3.0;
  CCTK_REAL /*const*/ ktwothird CCTK_ATTRIBUTE_UNUSED  = 2.0/3.0;
  CCTK_REAL /*const*/ kfourthird CCTK_ATTRIBUTE_UNUSED  = 4.0/3.0;
  CCTK_REAL /*const*/ keightthird CCTK_ATTRIBUTE_UNUSED  = 8.0/3.0;
  CCTK_REAL /*const*/ hdxi CCTK_ATTRIBUTE_UNUSED  = 0.5 * dxi;
  CCTK_REAL /*const*/ hdyi CCTK_ATTRIBUTE_UNUSED  = 0.5 * dyi;
  CCTK_REAL /*const*/ hdzi CCTK_ATTRIBUTE_UNUSED  = 0.5 * dzi;
  
  /* Initialize predefined quantities */
  CCTK_REAL /*const*/ p1o1 CCTK_ATTRIBUTE_UNUSED  = 1.;
  CCTK_REAL /*const*/ p1odx CCTK_ATTRIBUTE_UNUSED  = INV(dx);
  CCTK_REAL /*const*/ p1ody CCTK_ATTRIBUTE_UNUSED  = INV(dy);
  CCTK_REAL /*const*/ p1odz CCTK_ATTRIBUTE_UNUSED  = INV(dz);
  
  /* Assign local copies of arrays functions */
  
  
  
  /* Calculate temporaries and arrays functions */
  
  /* Copy local copies back to grid functions */
  
  /* Loop over the grid points */
  #pragma omp parallel
  CCTK_LOOP3(eulersr_cons_calc_intercell_conserved_2,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    ptrdiff_t /*const*/ index CCTK_ATTRIBUTE_UNUSED  = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenLeftL CCTK_ATTRIBUTE_UNUSED = DenLeft[index];
    CCTK_REAL DenRightL CCTK_ATTRIBUTE_UNUSED = DenRight[index];
    CCTK_REAL epsiLeftL CCTK_ATTRIBUTE_UNUSED = epsiLeft[index];
    CCTK_REAL epsiRightL CCTK_ATTRIBUTE_UNUSED = epsiRight[index];
    CCTK_REAL hL CCTK_ATTRIBUTE_UNUSED = h[index];
    CCTK_REAL pL CCTK_ATTRIBUTE_UNUSED = p[index];
    CCTK_REAL rhoLeftL CCTK_ATTRIBUTE_UNUSED = rhoLeft[index];
    CCTK_REAL rhoRightL CCTK_ATTRIBUTE_UNUSED = rhoRight[index];
    CCTK_REAL v1LeftL CCTK_ATTRIBUTE_UNUSED = v1Left[index];
    CCTK_REAL v1RightL CCTK_ATTRIBUTE_UNUSED = v1Right[index];
    CCTK_REAL v2LeftL CCTK_ATTRIBUTE_UNUSED = v2Left[index];
    CCTK_REAL v2RightL CCTK_ATTRIBUTE_UNUSED = v2Right[index];
    CCTK_REAL v3LeftL CCTK_ATTRIBUTE_UNUSED = v3Left[index];
    CCTK_REAL v3RightL CCTK_ATTRIBUTE_UNUSED = v3Right[index];
    CCTK_REAL WL CCTK_ATTRIBUTE_UNUSED = W[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED Wx = 1. - 1.*(SQR(v1LeftL) + 
      SQR(v2LeftL) + SQR(v3LeftL));
    
    WL = INV(sqrt(Wx));
    
    pL = epsiLeftL*rhoLeftL*(-1. + ToReal(gamma));
    
    hL = 1. + epsiLeftL + pL*INV(rhoLeftL);
    
    DenLeftL = rhoLeftL*WL;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S1LeftL = 
      hL*rhoLeftL*v1LeftL*SQR(WL);
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S2LeftL = 
      hL*rhoLeftL*v2LeftL*SQR(WL);
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S3LeftL = 
      hL*rhoLeftL*v3LeftL*SQR(WL);
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED tauLeftL = -1.*(DenLeftL + pL) + 
      hL*rhoLeftL*SQR(WL);
    
    Wx = 1. - 1.*(SQR(v1RightL) + SQR(v2RightL) + SQR(v3RightL));
    
    WL = INV(sqrt(Wx));
    
    pL = epsiRightL*rhoRightL*(-1. + ToReal(gamma));
    
    hL = 1. + epsiRightL + pL*INV(rhoRightL);
    
    DenRightL = rhoRightL*WL;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S1RightL = 
      hL*rhoRightL*v1RightL*SQR(WL);
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S2RightL = 
      hL*rhoRightL*v2RightL*SQR(WL);
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S3RightL = 
      hL*rhoRightL*v3RightL*SQR(WL);
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED tauRightL = -1.*(DenRightL + pL) + 
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
  CCTK_ENDLOOP3(eulersr_cons_calc_intercell_conserved_2);
}

extern "C" void eulersr_cons_calc_intercell_conserved_2(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_intercell_conserved_2_Body");
  }
  
  if (cctk_iteration % eulersr_cons_calc_intercell_conserved_2_calc_every != eulersr_cons_calc_intercell_conserved_2_calc_offset)
  {
    return;
  }
  
  const char *const groups[] = {
    "EulerSR::Den_lr_group",
    "EulerSR::epsi_lr_group",
    "EulerSR::h_group",
    "EulerSR::p_group",
    "EulerSR::rho_lr_group",
    "EulerSR::S1_lr_group",
    "EulerSR::S2_lr_group",
    "EulerSR::S3_lr_group",
    "EulerSR::tau_lr_group",
    "EulerSR::v1_lr_group",
    "EulerSR::v2_lr_group",
    "EulerSR::v3_lr_group",
    "EulerSR::W_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulersr_cons_calc_intercell_conserved_2", 13, groups);
  
  
  GenericFD_LoopOverEverything(cctkGH, eulersr_cons_calc_intercell_conserved_2_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_intercell_conserved_2_Body");
  }
}
