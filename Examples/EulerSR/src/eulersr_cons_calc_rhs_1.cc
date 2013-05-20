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

extern "C" void eulersr_cons_calc_rhs_1_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED  = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::Den_grouprhs","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::Den_grouprhs.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::S_grouprhs","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::S_grouprhs.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::tau_grouprhs","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::tau_grouprhs.");
  return;
}

static void eulersr_cons_calc_rhs_1_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
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
  CCTK_LOOP3(eulersr_cons_calc_rhs_1,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    ptrdiff_t /*const*/ index CCTK_ATTRIBUTE_UNUSED  = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenFluxL CCTK_ATTRIBUTE_UNUSED = DenFlux[index];
    CCTK_REAL DenrhsL CCTK_ATTRIBUTE_UNUSED = Denrhs[index];
    CCTK_REAL S1FluxL CCTK_ATTRIBUTE_UNUSED = S1Flux[index];
    CCTK_REAL S1rhsL CCTK_ATTRIBUTE_UNUSED = S1rhs[index];
    CCTK_REAL S2FluxL CCTK_ATTRIBUTE_UNUSED = S2Flux[index];
    CCTK_REAL S2rhsL CCTK_ATTRIBUTE_UNUSED = S2rhs[index];
    CCTK_REAL S3FluxL CCTK_ATTRIBUTE_UNUSED = S3Flux[index];
    CCTK_REAL S3rhsL CCTK_ATTRIBUTE_UNUSED = S3rhs[index];
    CCTK_REAL tauFluxL CCTK_ATTRIBUTE_UNUSED = tauFlux[index];
    CCTK_REAL taurhsL CCTK_ATTRIBUTE_UNUSED = taurhs[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    CCTK_REAL /*const*/ PDplus1DenFlux CCTK_ATTRIBUTE_UNUSED  = PDplus1(&DenFlux[index]);
    CCTK_REAL /*const*/ PDplus1S1Flux CCTK_ATTRIBUTE_UNUSED  = PDplus1(&S1Flux[index]);
    CCTK_REAL /*const*/ PDplus1S2Flux CCTK_ATTRIBUTE_UNUSED  = PDplus1(&S2Flux[index]);
    CCTK_REAL /*const*/ PDplus1S3Flux CCTK_ATTRIBUTE_UNUSED  = PDplus1(&S3Flux[index]);
    CCTK_REAL /*const*/ PDplus1tauFlux CCTK_ATTRIBUTE_UNUSED  = PDplus1(&tauFlux[index]);
    
    /* Calculate temporaries and grid functions */
    DenrhsL = DenrhsL - 1.*PDplus1DenFlux;
    
    S1rhsL = S1rhsL - 1.*PDplus1S1Flux;
    
    S2rhsL = S2rhsL - 1.*PDplus1S2Flux;
    
    S3rhsL = S3rhsL - 1.*PDplus1S3Flux;
    
    taurhsL = taurhsL - 1.*PDplus1tauFlux;
    
    /* Copy local copies back to grid functions */
    Denrhs[index] = DenrhsL;
    S1rhs[index] = S1rhsL;
    S2rhs[index] = S2rhsL;
    S3rhs[index] = S3rhsL;
    taurhs[index] = taurhsL;
  }
  CCTK_ENDLOOP3(eulersr_cons_calc_rhs_1);
}

extern "C" void eulersr_cons_calc_rhs_1(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_rhs_1_Body");
  }
  
  if (cctk_iteration % eulersr_cons_calc_rhs_1_calc_every != eulersr_cons_calc_rhs_1_calc_offset)
  {
    return;
  }
  
  const char *const groups[] = {
    "EulerSR::Den_flux_group",
    "EulerSR::Den_grouprhs",
    "EulerSR::S1_flux_group",
    "EulerSR::S2_flux_group",
    "EulerSR::S3_flux_group",
    "EulerSR::S_grouprhs",
    "EulerSR::tau_flux_group",
    "EulerSR::tau_grouprhs"};
  GenericFD_AssertGroupStorage(cctkGH, "eulersr_cons_calc_rhs_1", 8, groups);
  
  GenericFD_EnsureStencilFits(cctkGH, "eulersr_cons_calc_rhs_1", 1, 1, 1);
  
  GenericFD_LoopOverInterior(cctkGH, eulersr_cons_calc_rhs_1_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_rhs_1_Body");
  }
}
