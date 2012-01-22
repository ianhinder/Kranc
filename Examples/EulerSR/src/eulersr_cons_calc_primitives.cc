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

/* Define macros used in calculations */
#define INITVALUE (42)
#define QAD(x) (SQR(SQR(x)))
#define INV(x) ((1.0) / (x))
#define SQR(x) ((x) * (x))
#define CUB(x) ((x) * (x) * (x))

static void eulersr_cons_calc_primitives_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const min[3], int const max[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* Declare the variables used for looping over grid points */
  CCTK_INT i, j, k;
  // CCTK_INT index = INITVALUE;
  
  /* Declare finite differencing variables */
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_primitives_Body");
  }
  
  if (cctk_iteration % eulersr_cons_calc_primitives_calc_every != eulersr_cons_calc_primitives_calc_offset)
  {
    return;
  }
  
  const char *groups[] = {"EulerSR::Den_group","EulerSR::epsi_group","EulerSR::h_group","EulerSR::p_group","EulerSR::rho_group","EulerSR::S_group","EulerSR::tau_group","EulerSR::v_group","EulerSR::W_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulersr_cons_calc_primitives", 9, groups);
  
  
  /* Include user-supplied include files */
  
  /* Initialise finite differencing variables */
  ptrdiff_t const di = 1;
  ptrdiff_t const dj = CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  ptrdiff_t const dk = CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  CCTK_REAL const dx = ToReal(CCTK_DELTA_SPACE(0));
  CCTK_REAL const dy = ToReal(CCTK_DELTA_SPACE(1));
  CCTK_REAL const dz = ToReal(CCTK_DELTA_SPACE(2));
  CCTK_REAL const dt = ToReal(CCTK_DELTA_TIME);
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
  
  /* Loop over the grid points */
  for (k = min[2]; k < max[2]; k++)
  {
    for (j = min[1]; j < max[1]; j++)
    {
      for (i = min[0]; i < max[0]; i++)
      {
         int  const  index  =  CCTK_GFINDEX3D(cctkGH,i,j,k) ;
        
        /* Assign local copies of grid functions */
        
        CCTK_REAL DenL = Den[index];
        CCTK_REAL epsiL = epsi[index];
        CCTK_REAL hL = h[index];
        CCTK_REAL pL = p[index];
        CCTK_REAL rhoL = rho[index];
        CCTK_REAL S1L = S1[index];
        CCTK_REAL S2L = S2[index];
        CCTK_REAL S3L = S3[index];
        CCTK_REAL tauL = tau[index];
        CCTK_REAL WL = W[index];
        
        
        /* Include user supplied include files */
        
        /* Precompute derivatives */
        
        /* Calculate temporaries and grid functions */
        CCTK_REAL pBar = pL;
        
        CCTK_REAL f = 10;
        
        CCTK_REAL Z = DenL + pBar + tauL;
        
        CCTK_REAL Ssq = SQR(S1L) + SQR(S2L) + SQR(S3L);
        
        CCTK_REAL vsq = Ssq*INV(SQR(Z));
        
        WL = INV(sqrt(1 - vsq));
        
        rhoL = DenL*INV(WL);
        
        hL = Z*INV(rhoL)*INV(SQR(WL));
        
        epsiL = hL - (pBar + rhoL)*INV(rhoL);
        
        CCTK_REAL pEOS = epsiL*rhoL*(-1 + ToReal(gamma));
        
        f = -pBar + pEOS;
        
        CCTK_REAL cs = sqrt(epsiL*INV(hL)*(-1 + ToReal(gamma))*ToReal(gamma));
        
        CCTK_REAL df = -1 + vsq*SQR(cs);
        
        pBar = pBar - f*INV(df);
        
        Z = DenL + pBar + tauL;
        
        Ssq = SQR(S1L) + SQR(S2L) + SQR(S3L);
        
        vsq = Ssq*INV(SQR(Z));
        
        WL = INV(sqrt(1 - vsq));
        
        rhoL = DenL*INV(WL);
        
        hL = Z*INV(rhoL)*INV(SQR(WL));
        
        epsiL = hL - (pBar + rhoL)*INV(rhoL);
        
        pEOS = epsiL*rhoL*(-1 + ToReal(gamma));
        
        f = -pBar + pEOS;
        
        cs = sqrt(epsiL*INV(hL)*(-1 + ToReal(gamma))*ToReal(gamma));
        
        df = -1 + vsq*SQR(cs);
        
        pBar = pBar - f*INV(df);
        
        Z = DenL + pBar + tauL;
        
        Ssq = SQR(S1L) + SQR(S2L) + SQR(S3L);
        
        vsq = Ssq*INV(SQR(Z));
        
        WL = INV(sqrt(1 - vsq));
        
        rhoL = DenL*INV(WL);
        
        hL = Z*INV(rhoL)*INV(SQR(WL));
        
        epsiL = hL - (pBar + rhoL)*INV(rhoL);
        
        pEOS = epsiL*rhoL*(-1 + ToReal(gamma));
        
        f = -pBar + pEOS;
        
        cs = sqrt(epsiL*INV(hL)*(-1 + ToReal(gamma))*ToReal(gamma));
        
        df = -1 + vsq*SQR(cs);
        
        pBar = pBar - f*INV(df);
        
        Z = DenL + pBar + tauL;
        
        Ssq = SQR(S1L) + SQR(S2L) + SQR(S3L);
        
        vsq = Ssq*INV(SQR(Z));
        
        WL = INV(sqrt(1 - vsq));
        
        rhoL = DenL*INV(WL);
        
        hL = Z*INV(rhoL)*INV(SQR(WL));
        
        epsiL = hL - (pBar + rhoL)*INV(rhoL);
        
        pEOS = epsiL*rhoL*(-1 + ToReal(gamma));
        
        f = -pBar + pEOS;
        
        cs = sqrt(epsiL*INV(hL)*(-1 + ToReal(gamma))*ToReal(gamma));
        
        df = -1 + vsq*SQR(cs);
        
        pBar = pBar - f*INV(df);
        
        Z = DenL + pBar + tauL;
        
        Ssq = SQR(S1L) + SQR(S2L) + SQR(S3L);
        
        vsq = Ssq*INV(SQR(Z));
        
        WL = INV(sqrt(1 - vsq));
        
        rhoL = DenL*INV(WL);
        
        hL = Z*INV(rhoL)*INV(SQR(WL));
        
        epsiL = hL - (pBar + rhoL)*INV(rhoL);
        
        pEOS = epsiL*rhoL*(-1 + ToReal(gamma));
        
        f = -pBar + pEOS;
        
        cs = sqrt(epsiL*INV(hL)*(-1 + ToReal(gamma))*ToReal(gamma));
        
        df = -1 + vsq*SQR(cs);
        
        pBar = pBar - f*INV(df);
        
        pL = pBar;
        
        CCTK_REAL v1L = S1L*INV(hL)*INV(rhoL)*INV(SQR(WL));
        
        CCTK_REAL v2L = S2L*INV(hL)*INV(rhoL)*INV(SQR(WL));
        
        CCTK_REAL v3L = S3L*INV(hL)*INV(rhoL)*INV(SQR(WL));
        
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
    }
  }
}

extern "C" void eulersr_cons_calc_primitives(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  GenericFD_LoopOverEverything(cctkGH, &eulersr_cons_calc_primitives_Body);
}