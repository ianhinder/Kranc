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

static void eulersr_cons_calc_primitives_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
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
  CCTK_LOOP3(eulersr_cons_calc_primitives,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    ptrdiff_t /*const*/ index CCTK_ATTRIBUTE_UNUSED  = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenL CCTK_ATTRIBUTE_UNUSED = Den[index];
    CCTK_REAL epsiL CCTK_ATTRIBUTE_UNUSED = epsi[index];
    CCTK_REAL hL CCTK_ATTRIBUTE_UNUSED = h[index];
    CCTK_REAL pL CCTK_ATTRIBUTE_UNUSED = p[index];
    CCTK_REAL rhoL CCTK_ATTRIBUTE_UNUSED = rho[index];
    CCTK_REAL S1L CCTK_ATTRIBUTE_UNUSED = S1[index];
    CCTK_REAL S2L CCTK_ATTRIBUTE_UNUSED = S2[index];
    CCTK_REAL S3L CCTK_ATTRIBUTE_UNUSED = S3[index];
    CCTK_REAL tauL CCTK_ATTRIBUTE_UNUSED = tau[index];
    CCTK_REAL WL CCTK_ATTRIBUTE_UNUSED = W[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED pBar = pL;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED f = 10.;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED Z = DenL + tauL + pBar;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED Ssq = SQR(S1L) + SQR(S2L) + 
      SQR(S3L);
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED vsq = Ssq*INV(SQR(Z));
    
    WL = INV(sqrt(1. - 1.*vsq));
    
    rhoL = DenL*INV(WL);
    
    hL = Z*INV(rhoL*SQR(WL));
    
    epsiL = hL - 1.*(rhoL + pBar)*INV(rhoL);
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED pEOS = epsiL*rhoL*(-1. + 
      ToReal(gamma));
    
    f = -1.*pBar + pEOS;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED cs = sqrt(epsiL*INV(hL)*(-1. + 
      ToReal(gamma))*ToReal(gamma));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED df = -1. + vsq*SQR(cs);
    
    pBar = pBar - 1.*f*INV(df);
    
    Z = DenL + tauL + pBar;
    
    Ssq = SQR(S1L) + SQR(S2L) + SQR(S3L);
    
    vsq = Ssq*INV(SQR(Z));
    
    WL = INV(sqrt(1. - 1.*vsq));
    
    rhoL = DenL*INV(WL);
    
    hL = Z*INV(rhoL*SQR(WL));
    
    epsiL = hL - 1.*(rhoL + pBar)*INV(rhoL);
    
    pEOS = epsiL*rhoL*(-1. + ToReal(gamma));
    
    f = -1.*pBar + pEOS;
    
    cs = sqrt(epsiL*INV(hL)*(-1. + ToReal(gamma))*ToReal(gamma));
    
    df = -1. + vsq*SQR(cs);
    
    pBar = pBar - 1.*f*INV(df);
    
    Z = DenL + tauL + pBar;
    
    Ssq = SQR(S1L) + SQR(S2L) + SQR(S3L);
    
    vsq = Ssq*INV(SQR(Z));
    
    WL = INV(sqrt(1. - 1.*vsq));
    
    rhoL = DenL*INV(WL);
    
    hL = Z*INV(rhoL*SQR(WL));
    
    epsiL = hL - 1.*(rhoL + pBar)*INV(rhoL);
    
    pEOS = epsiL*rhoL*(-1. + ToReal(gamma));
    
    f = -1.*pBar + pEOS;
    
    cs = sqrt(epsiL*INV(hL)*(-1. + ToReal(gamma))*ToReal(gamma));
    
    df = -1. + vsq*SQR(cs);
    
    pBar = pBar - 1.*f*INV(df);
    
    Z = DenL + tauL + pBar;
    
    Ssq = SQR(S1L) + SQR(S2L) + SQR(S3L);
    
    vsq = Ssq*INV(SQR(Z));
    
    WL = INV(sqrt(1. - 1.*vsq));
    
    rhoL = DenL*INV(WL);
    
    hL = Z*INV(rhoL*SQR(WL));
    
    epsiL = hL - 1.*(rhoL + pBar)*INV(rhoL);
    
    pEOS = epsiL*rhoL*(-1. + ToReal(gamma));
    
    f = -1.*pBar + pEOS;
    
    cs = sqrt(epsiL*INV(hL)*(-1. + ToReal(gamma))*ToReal(gamma));
    
    df = -1. + vsq*SQR(cs);
    
    pBar = pBar - 1.*f*INV(df);
    
    Z = DenL + tauL + pBar;
    
    Ssq = SQR(S1L) + SQR(S2L) + SQR(S3L);
    
    vsq = Ssq*INV(SQR(Z));
    
    WL = INV(sqrt(1. - 1.*vsq));
    
    rhoL = DenL*INV(WL);
    
    hL = Z*INV(rhoL*SQR(WL));
    
    epsiL = hL - 1.*(rhoL + pBar)*INV(rhoL);
    
    pEOS = epsiL*rhoL*(-1. + ToReal(gamma));
    
    f = -1.*pBar + pEOS;
    
    cs = sqrt(epsiL*INV(hL)*(-1. + ToReal(gamma))*ToReal(gamma));
    
    df = -1. + vsq*SQR(cs);
    
    pBar = pBar - 1.*f*INV(df);
    
    pL = pBar;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED v1L = 
      S1L*INV(hL*rhoL*SQR(WL));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED v2L = 
      S2L*INV(hL*rhoL*SQR(WL));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED v3L = 
      S3L*INV(hL*rhoL*SQR(WL));
    
    /* Copy local copies back to grid functions */
    epsi[index] = epsiL;
    h[index] = hL;
    p[index] = pL;
    rho[index] = rhoL;
    v1[index] = v1L;
    v2[index] = v2L;
    v3[index] = v3L;
    W[index] = WL;
  }
  CCTK_ENDLOOP3(eulersr_cons_calc_primitives);
}

extern "C" void eulersr_cons_calc_primitives(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_primitives_Body");
  }
  
  if (cctk_iteration % eulersr_cons_calc_primitives_calc_every != eulersr_cons_calc_primitives_calc_offset)
  {
    return;
  }
  
  const char *const groups[] = {
    "EulerSR::Den_group",
    "EulerSR::epsi_group",
    "EulerSR::h_group",
    "EulerSR::p_group",
    "EulerSR::rho_group",
    "EulerSR::S_group",
    "EulerSR::tau_group",
    "EulerSR::v_group",
    "EulerSR::W_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulersr_cons_calc_primitives", 9, groups);
  
  
  GenericFD_LoopOverEverything(cctkGH, eulersr_cons_calc_primitives_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_primitives_Body");
  }
}
