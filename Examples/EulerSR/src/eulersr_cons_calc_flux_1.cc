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

extern "C" void eulersr_cons_calc_flux_1_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::Den_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::Den_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::S1_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::S1_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::S2_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::S2_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::S3_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::S3_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::tau_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::tau_flux_group.");
  return;
}

static void eulersr_cons_calc_flux_1_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const min[3], int const max[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* Declare the variables used for looping over grid points */
  CCTK_INT i, j, k;
  // CCTK_INT index = INITVALUE;
  
  /* Declare finite differencing variables */
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_flux_1_Body");
  }
  
  if (cctk_iteration % eulersr_cons_calc_flux_1_calc_every != eulersr_cons_calc_flux_1_calc_offset)
  {
    return;
  }
  
  const char *groups[] = {"EulerSR::Den_flux_group","EulerSR::Den_lr_group","EulerSR::epsi_lr_group","EulerSR::rho_lr_group","EulerSR::S1_flux_group","EulerSR::S1_lr_group","EulerSR::S2_flux_group","EulerSR::S2_lr_group","EulerSR::S3_flux_group","EulerSR::S3_lr_group","EulerSR::tau_flux_group","EulerSR::tau_lr_group","EulerSR::v1_lr_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulersr_cons_calc_flux_1", 13, groups);
  
  GenericFD_EnsureStencilFits(cctkGH, "eulersr_cons_calc_flux_1", 1, 1, 1);
  
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
        
        CCTK_REAL DenLeftL = DenLeft[index];
        CCTK_REAL DenRightL = DenRight[index];
        CCTK_REAL epsiLeftL = epsiLeft[index];
        CCTK_REAL epsiRightL = epsiRight[index];
        CCTK_REAL rhoLeftL = rhoLeft[index];
        CCTK_REAL rhoRightL = rhoRight[index];
        CCTK_REAL S1LeftL = S1Left[index];
        CCTK_REAL S1RightL = S1Right[index];
        CCTK_REAL S2LeftL = S2Left[index];
        CCTK_REAL S2RightL = S2Right[index];
        CCTK_REAL S3LeftL = S3Left[index];
        CCTK_REAL S3RightL = S3Right[index];
        CCTK_REAL tauLeftL = tauLeft[index];
        CCTK_REAL tauRightL = tauRight[index];
        CCTK_REAL v1LeftL = v1Left[index];
        CCTK_REAL v1RightL = v1Right[index];
        
        
        /* Include user supplied include files */
        
        /* Precompute derivatives */
        CCTK_REAL const ShiftMinus1DenRight = ShiftMinus1(&DenRight[index]);
        CCTK_REAL const ShiftMinus1epsiRight = ShiftMinus1(&epsiRight[index]);
        CCTK_REAL const ShiftMinus1rhoRight = ShiftMinus1(&rhoRight[index]);
        CCTK_REAL const ShiftMinus1S1Right = ShiftMinus1(&S1Right[index]);
        CCTK_REAL const ShiftMinus1S2Right = ShiftMinus1(&S2Right[index]);
        CCTK_REAL const ShiftMinus1S3Right = ShiftMinus1(&S3Right[index]);
        CCTK_REAL const ShiftMinus1tauRight = ShiftMinus1(&tauRight[index]);
        CCTK_REAL const ShiftMinus1v1Right = ShiftMinus1(&v1Right[index]);
        
        /* Calculate temporaries and grid functions */
        CCTK_REAL DenFluxLeft = DenLeftL*v1LeftL;
        
        CCTK_REAL DenFluxRight = ShiftMinus1DenRight*ShiftMinus1v1Right;
        
        CCTK_REAL DenFluxL = 0.5*(DenFluxLeft + DenFluxRight + (-DenLeftL + 
          ShiftMinus1DenRight)*ToReal(hlleAlpha));
        
        CCTK_REAL S1FluxLeft = S1LeftL*v1LeftL + epsiLeftL*rhoLeftL*(-1 + 
          ToReal(gamma));
        
        CCTK_REAL S1FluxRight = ShiftMinus1S1Right*ShiftMinus1v1Right + 
          ShiftMinus1epsiRight*ShiftMinus1rhoRight*(-1 + ToReal(gamma));
        
        CCTK_REAL S1FluxL = 0.5*(S1FluxLeft + S1FluxRight + (-S1LeftL + 
          ShiftMinus1S1Right)*ToReal(hlleAlpha));
        
        CCTK_REAL S2FluxLeft = S2LeftL*v1LeftL;
        
        CCTK_REAL S2FluxRight = ShiftMinus1S2Right*ShiftMinus1v1Right;
        
        CCTK_REAL S2FluxL = 0.5*(S2FluxLeft + S2FluxRight + (-S2LeftL + 
          ShiftMinus1S2Right)*ToReal(hlleAlpha));
        
        CCTK_REAL S3FluxLeft = S3LeftL*v1LeftL;
        
        CCTK_REAL S3FluxRight = ShiftMinus1S3Right*ShiftMinus1v1Right;
        
        CCTK_REAL S3FluxL = 0.5*(S3FluxLeft + S3FluxRight + (-S3LeftL + 
          ShiftMinus1S3Right)*ToReal(hlleAlpha));
        
        CCTK_REAL tauFluxLeft = v1LeftL*(tauLeftL + epsiLeftL*rhoLeftL*(-1 + 
          ToReal(gamma)));
        
        CCTK_REAL tauFluxRight = ShiftMinus1v1Right*(ShiftMinus1tauRight + 
          ShiftMinus1epsiRight*ShiftMinus1rhoRight*(-1 + ToReal(gamma)));
        
        CCTK_REAL tauFluxL = 0.5*(tauFluxLeft + tauFluxRight + 
          (ShiftMinus1tauRight - tauLeftL)*ToReal(hlleAlpha));
        
        /* Copy local copies back to grid functions */
        DenFlux[index] = DenFluxL;
        S1Flux[index] = S1FluxL;
        S2Flux[index] = S2FluxL;
        S3Flux[index] = S3FluxL;
        tauFlux[index] = tauFluxL;
      }
    }
  }
}

extern "C" void eulersr_cons_calc_flux_1(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  GenericFD_LoopOverInterior(cctkGH, &eulersr_cons_calc_flux_1_Body);
}
