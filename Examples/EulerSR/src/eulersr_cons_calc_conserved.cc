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

static void eulersr_cons_calc_conserved_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const min[3], int const max[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* Declare the variables used for looping over grid points */
  CCTK_INT i, j, k;
  // CCTK_INT index = INITVALUE;
  
  /* Declare finite differencing variables */
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_conserved_Body");
  }
  
  if (cctk_iteration % eulersr_cons_calc_conserved_calc_every != eulersr_cons_calc_conserved_calc_offset)
  {
    return;
  }
  
  const char *groups[] = {"EulerSR::Den_group","EulerSR::epsi_group","EulerSR::h_group","EulerSR::p_group","EulerSR::rho_group","EulerSR::S_group","EulerSR::tau_group","EulerSR::v_group","EulerSR::W_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulersr_cons_calc_conserved", 9, groups);
  
  
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
        CCTK_REAL v1L = v1[index];
        CCTK_REAL v2L = v2[index];
        CCTK_REAL v3L = v3[index];
        CCTK_REAL WL = W[index];
        
        
        /* Include user supplied include files */
        
        /* Precompute derivatives */
        
        /* Calculate temporaries and grid functions */
        CCTK_REAL Wx = 1 - SQR(v1L) - SQR(v2L) - SQR(v3L);
        
        WL = INV(sqrt(Wx));
        
        pL = epsiL*rhoL*(-1 + ToReal(gamma));
        
        hL = 1 + epsiL + pL*INV(rhoL);
        
        DenL = rhoL*WL;
        
        CCTK_REAL S1L = hL*rhoL*v1L*SQR(WL);
        
        CCTK_REAL S2L = hL*rhoL*v2L*SQR(WL);
        
        CCTK_REAL S3L = hL*rhoL*v3L*SQR(WL);
        
        CCTK_REAL tauL = -DenL - pL + hL*rhoL*SQR(WL);
        
        /* Copy local copies back to grid functions */
        Den[index] = DenL;
        h[index] = hL;
        p[index] = pL;
        S1[index] = S1L;
        S2[index] = S2L;
        S3[index] = S3L;
        tau[index] = tauL;
        W[index] = WL;
      }
    }
  }
}

extern "C" void eulersr_cons_calc_conserved(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  GenericFD_LoopOverEverything(cctkGH, &eulersr_cons_calc_conserved_Body);
}
