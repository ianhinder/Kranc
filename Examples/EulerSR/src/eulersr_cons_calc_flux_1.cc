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

extern "C" void eulersr_cons_calc_flux_1_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED  = 0;
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

static void eulersr_cons_calc_flux_1_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
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
  CCTK_LOOP3(eulersr_cons_calc_flux_1,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    ptrdiff_t /*const*/ index CCTK_ATTRIBUTE_UNUSED  = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenLeftL CCTK_ATTRIBUTE_UNUSED = DenLeft[index];
    CCTK_REAL DenRightL CCTK_ATTRIBUTE_UNUSED = DenRight[index];
    CCTK_REAL epsiLeftL CCTK_ATTRIBUTE_UNUSED = epsiLeft[index];
    CCTK_REAL epsiRightL CCTK_ATTRIBUTE_UNUSED = epsiRight[index];
    CCTK_REAL rhoLeftL CCTK_ATTRIBUTE_UNUSED = rhoLeft[index];
    CCTK_REAL rhoRightL CCTK_ATTRIBUTE_UNUSED = rhoRight[index];
    CCTK_REAL S1LeftL CCTK_ATTRIBUTE_UNUSED = S1Left[index];
    CCTK_REAL S1RightL CCTK_ATTRIBUTE_UNUSED = S1Right[index];
    CCTK_REAL S2LeftL CCTK_ATTRIBUTE_UNUSED = S2Left[index];
    CCTK_REAL S2RightL CCTK_ATTRIBUTE_UNUSED = S2Right[index];
    CCTK_REAL S3LeftL CCTK_ATTRIBUTE_UNUSED = S3Left[index];
    CCTK_REAL S3RightL CCTK_ATTRIBUTE_UNUSED = S3Right[index];
    CCTK_REAL tauLeftL CCTK_ATTRIBUTE_UNUSED = tauLeft[index];
    CCTK_REAL tauRightL CCTK_ATTRIBUTE_UNUSED = tauRight[index];
    CCTK_REAL v1LeftL CCTK_ATTRIBUTE_UNUSED = v1Left[index];
    CCTK_REAL v1RightL CCTK_ATTRIBUTE_UNUSED = v1Right[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    CCTK_REAL /*const*/ ShiftMinus1DenRight CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&DenRight[index]);
    CCTK_REAL /*const*/ ShiftMinus1epsiRight CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&epsiRight[index]);
    CCTK_REAL /*const*/ ShiftMinus1rhoRight CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&rhoRight[index]);
    CCTK_REAL /*const*/ ShiftMinus1S1Right CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&S1Right[index]);
    CCTK_REAL /*const*/ ShiftMinus1S2Right CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&S2Right[index]);
    CCTK_REAL /*const*/ ShiftMinus1S3Right CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&S3Right[index]);
    CCTK_REAL /*const*/ ShiftMinus1tauRight CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&tauRight[index]);
    CCTK_REAL /*const*/ ShiftMinus1v1Right CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&v1Right[index]);
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED DenFluxLeft = DenLeftL*v1LeftL;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED DenFluxRight = 
      ShiftMinus1DenRight*ShiftMinus1v1Right;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED DenFluxL = 0.5*(DenFluxLeft + 
      DenFluxRight + (-1.*DenLeftL + 
      ShiftMinus1DenRight)*ToReal(hlleAlpha));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S1FluxLeft = S1LeftL*v1LeftL + 
      epsiLeftL*rhoLeftL*(-1. + ToReal(gamma));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S1FluxRight = 
      ShiftMinus1S1Right*ShiftMinus1v1Right + 
      ShiftMinus1epsiRight*ShiftMinus1rhoRight*(-1. + ToReal(gamma));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S1FluxL = 0.5*(S1FluxLeft + 
      S1FluxRight + (-1.*S1LeftL + ShiftMinus1S1Right)*ToReal(hlleAlpha));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S2FluxLeft = S2LeftL*v1LeftL;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S2FluxRight = 
      ShiftMinus1S2Right*ShiftMinus1v1Right;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S2FluxL = 0.5*(S2FluxLeft + 
      S2FluxRight + (-1.*S2LeftL + ShiftMinus1S2Right)*ToReal(hlleAlpha));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S3FluxLeft = S3LeftL*v1LeftL;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S3FluxRight = 
      ShiftMinus1S3Right*ShiftMinus1v1Right;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED S3FluxL = 0.5*(S3FluxLeft + 
      S3FluxRight + (-1.*S3LeftL + ShiftMinus1S3Right)*ToReal(hlleAlpha));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED tauFluxLeft = v1LeftL*(tauLeftL + 
      epsiLeftL*rhoLeftL*(-1. + ToReal(gamma)));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED tauFluxRight = 
      ShiftMinus1v1Right*(ShiftMinus1tauRight + 
      ShiftMinus1epsiRight*ShiftMinus1rhoRight*(-1. + ToReal(gamma)));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED tauFluxL = 0.5*(tauFluxLeft + 
      tauFluxRight + (-1.*tauLeftL + 
      ShiftMinus1tauRight)*ToReal(hlleAlpha));
    
    /* Copy local copies back to grid functions */
    DenFlux[index] = DenFluxL;
    S1Flux[index] = S1FluxL;
    S2Flux[index] = S2FluxL;
    S3Flux[index] = S3FluxL;
    tauFlux[index] = tauFluxL;
  }
  CCTK_ENDLOOP3(eulersr_cons_calc_flux_1);
}

extern "C" void eulersr_cons_calc_flux_1(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_flux_1_Body");
  }
  
  if (cctk_iteration % eulersr_cons_calc_flux_1_calc_every != eulersr_cons_calc_flux_1_calc_offset)
  {
    return;
  }
  
  const char *const groups[] = {
    "EulerSR::Den_flux_group",
    "EulerSR::Den_lr_group",
    "EulerSR::epsi_lr_group",
    "EulerSR::rho_lr_group",
    "EulerSR::S1_flux_group",
    "EulerSR::S1_lr_group",
    "EulerSR::S2_flux_group",
    "EulerSR::S2_lr_group",
    "EulerSR::S3_flux_group",
    "EulerSR::S3_lr_group",
    "EulerSR::tau_flux_group",
    "EulerSR::tau_lr_group",
    "EulerSR::v1_lr_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulersr_cons_calc_flux_1", 13, groups);
  
  GenericFD_EnsureStencilFits(cctkGH, "eulersr_cons_calc_flux_1", 1, 1, 1);
  
  GenericFD_LoopOverInterior(cctkGH, eulersr_cons_calc_flux_1_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_flux_1_Body");
  }
}
