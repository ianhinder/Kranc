/*  File produced by Kranc */

#define KRANC_C

#include <algorithm>
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"
#include "Kranc.hh"
#include "Differencing.h"
#include "loopcontrol.h"

namespace EulerSR {

extern "C" void eulersr_cons_calc_rhs_2_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (cctk_iteration % eulersr_cons_calc_rhs_2_calc_every != eulersr_cons_calc_rhs_2_calc_offset)
    return;
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::Den_grouprhs","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::Den_grouprhs.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::S_grouprhs","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::S_grouprhs.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::tau_grouprhs","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::tau_grouprhs.");
  return;
}

static void eulersr_cons_calc_rhs_2_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  /* Include user-supplied include files */
  /* Initialise finite differencing variables */
  const ptrdiff_t di CCTK_ATTRIBUTE_UNUSED = 1;
  const ptrdiff_t dj CCTK_ATTRIBUTE_UNUSED = 
    CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  const ptrdiff_t dk CCTK_ATTRIBUTE_UNUSED = 
    CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL) * di;
  const ptrdiff_t cdj CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL) * dj;
  const ptrdiff_t cdk CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL) * dk;
  const ptrdiff_t cctkLbnd1 CCTK_ATTRIBUTE_UNUSED = cctk_lbnd[0];
  const ptrdiff_t cctkLbnd2 CCTK_ATTRIBUTE_UNUSED = cctk_lbnd[1];
  const ptrdiff_t cctkLbnd3 CCTK_ATTRIBUTE_UNUSED = cctk_lbnd[2];
  const CCTK_REAL t CCTK_ATTRIBUTE_UNUSED = cctk_time;
  const CCTK_REAL cctkOriginSpace1 CCTK_ATTRIBUTE_UNUSED = 
    CCTK_ORIGIN_SPACE(0);
  const CCTK_REAL cctkOriginSpace2 CCTK_ATTRIBUTE_UNUSED = 
    CCTK_ORIGIN_SPACE(1);
  const CCTK_REAL cctkOriginSpace3 CCTK_ATTRIBUTE_UNUSED = 
    CCTK_ORIGIN_SPACE(2);
  const CCTK_REAL dt CCTK_ATTRIBUTE_UNUSED = CCTK_DELTA_TIME;
  const CCTK_REAL dx CCTK_ATTRIBUTE_UNUSED = CCTK_DELTA_SPACE(0);
  const CCTK_REAL dy CCTK_ATTRIBUTE_UNUSED = CCTK_DELTA_SPACE(1);
  const CCTK_REAL dz CCTK_ATTRIBUTE_UNUSED = CCTK_DELTA_SPACE(2);
  const CCTK_REAL dxi CCTK_ATTRIBUTE_UNUSED = pow(dx,-1);
  const CCTK_REAL dyi CCTK_ATTRIBUTE_UNUSED = pow(dy,-1);
  const CCTK_REAL dzi CCTK_ATTRIBUTE_UNUSED = pow(dz,-1);
  const CCTK_REAL khalf CCTK_ATTRIBUTE_UNUSED = 0.5;
  const CCTK_REAL kthird CCTK_ATTRIBUTE_UNUSED = 
    0.333333333333333333333333333333;
  const CCTK_REAL ktwothird CCTK_ATTRIBUTE_UNUSED = 
    0.666666666666666666666666666667;
  const CCTK_REAL kfourthird CCTK_ATTRIBUTE_UNUSED = 
    1.33333333333333333333333333333;
  const CCTK_REAL hdxi CCTK_ATTRIBUTE_UNUSED = 0.5*dxi;
  const CCTK_REAL hdyi CCTK_ATTRIBUTE_UNUSED = 0.5*dyi;
  const CCTK_REAL hdzi CCTK_ATTRIBUTE_UNUSED = 0.5*dzi;
  /* Initialize predefined quantities */
  const CCTK_REAL p1o1 CCTK_ATTRIBUTE_UNUSED = 1;
  const CCTK_REAL p1odx CCTK_ATTRIBUTE_UNUSED = pow(dx,-1);
  const CCTK_REAL p1ody CCTK_ATTRIBUTE_UNUSED = pow(dy,-1);
  const CCTK_REAL p1odz CCTK_ATTRIBUTE_UNUSED = pow(dz,-1);
  /* Assign local copies of arrays functions */
  
  
  /* Calculate temporaries and arrays functions */
  /* Copy local copies back to grid functions */
  /* Loop over the grid points */
  const int imin0=imin[0];
  const int imin1=imin[1];
  const int imin2=imin[2];
  const int imax0=imax[0];
  const int imax1=imax[1];
  const int imax2=imax[2];
  #pragma omp parallel
  CCTK_LOOP3(eulersr_cons_calc_rhs_2,
    i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    const ptrdiff_t index CCTK_ATTRIBUTE_UNUSED = di*i + dj*j + dk*k;
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
    const CCTK_REAL PDplus2DenFlux CCTK_ATTRIBUTE_UNUSED = PDplus2(&DenFlux[index]);
    const CCTK_REAL PDplus2S1Flux CCTK_ATTRIBUTE_UNUSED = PDplus2(&S1Flux[index]);
    const CCTK_REAL PDplus2S2Flux CCTK_ATTRIBUTE_UNUSED = PDplus2(&S2Flux[index]);
    const CCTK_REAL PDplus2S3Flux CCTK_ATTRIBUTE_UNUSED = PDplus2(&S3Flux[index]);
    const CCTK_REAL PDplus2tauFlux CCTK_ATTRIBUTE_UNUSED = PDplus2(&tauFlux[index]);
    /* Calculate temporaries and grid functions */
    DenrhsL = DenrhsL - PDplus2DenFlux;
    
    S1rhsL = S1rhsL - PDplus2S1Flux;
    
    S2rhsL = S2rhsL - PDplus2S2Flux;
    
    S3rhsL = S3rhsL - PDplus2S3Flux;
    
    taurhsL = taurhsL - PDplus2tauFlux;
    /* Copy local copies back to grid functions */
    Denrhs[index] = DenrhsL;
    S1rhs[index] = S1rhsL;
    S2rhs[index] = S2rhsL;
    S3rhs[index] = S3rhsL;
    taurhs[index] = taurhsL;
  }
  CCTK_ENDLOOP3(eulersr_cons_calc_rhs_2);
}
extern "C" void eulersr_cons_calc_rhs_2(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_rhs_2_Body");
  }
  if (cctk_iteration % eulersr_cons_calc_rhs_2_calc_every != eulersr_cons_calc_rhs_2_calc_offset)
  {
    return;
  }
  
  const char* const groups[] = {
    "EulerSR::Den_flux_group",
    "EulerSR::Den_grouprhs",
    "EulerSR::S1_flux_group",
    "EulerSR::S2_flux_group",
    "EulerSR::S3_flux_group",
    "EulerSR::S_grouprhs",
    "EulerSR::tau_flux_group",
    "EulerSR::tau_grouprhs"};
  AssertGroupStorage(cctkGH, "eulersr_cons_calc_rhs_2", 8, groups);
  
  EnsureStencilFits(cctkGH, "eulersr_cons_calc_rhs_2", 1, 1, 1);
  
  LoopOverInterior(cctkGH, eulersr_cons_calc_rhs_2_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_rhs_2_Body");
  }
}

} // namespace EulerSR
