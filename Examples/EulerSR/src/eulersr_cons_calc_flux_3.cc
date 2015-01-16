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

extern "C" void eulersr_cons_calc_flux_3_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (cctk_iteration % eulersr_cons_calc_flux_3_calc_every != eulersr_cons_calc_flux_3_calc_offset)
    return;
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::Den_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::Den_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::S1_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::S1_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::S2_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::S2_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::S3_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::S3_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::tau_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::tau_flux_group.");
  return;
}

static void eulersr_cons_calc_flux_3_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
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
  CCTK_LOOP3(eulersr_cons_calc_flux_3,
    i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    const ptrdiff_t index CCTK_ATTRIBUTE_UNUSED = di*i + dj*j + dk*k;
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
    CCTK_REAL v3LeftL CCTK_ATTRIBUTE_UNUSED = v3Left[index];
    CCTK_REAL v3RightL CCTK_ATTRIBUTE_UNUSED = v3Right[index];
    
    /* Include user supplied include files */
    /* Precompute derivatives */
    const CCTK_REAL ShiftMinus3DenRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus3(&DenRight[index]);
    const CCTK_REAL ShiftMinus3epsiRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus3(&epsiRight[index]);
    const CCTK_REAL ShiftMinus3rhoRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus3(&rhoRight[index]);
    const CCTK_REAL ShiftMinus3S1Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus3(&S1Right[index]);
    const CCTK_REAL ShiftMinus3S2Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus3(&S2Right[index]);
    const CCTK_REAL ShiftMinus3S3Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus3(&S3Right[index]);
    const CCTK_REAL ShiftMinus3tauRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus3(&tauRight[index]);
    const CCTK_REAL ShiftMinus3v3Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus3(&v3Right[index]);
    /* Calculate temporaries and grid functions */
    CCTK_REAL DenFluxLeft CCTK_ATTRIBUTE_UNUSED = DenLeftL*v3LeftL;
    
    CCTK_REAL DenFluxRight CCTK_ATTRIBUTE_UNUSED = 
      ShiftMinus3DenRight*ShiftMinus3v3Right;
    
    CCTK_REAL DenFluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(DenFluxLeft + 
      DenFluxRight + hlleAlpha*(-DenLeftL + ShiftMinus3DenRight));
    
    CCTK_REAL S1FluxLeft CCTK_ATTRIBUTE_UNUSED = S1LeftL*v3LeftL;
    
    CCTK_REAL S1FluxRight CCTK_ATTRIBUTE_UNUSED = 
      ShiftMinus3S1Right*ShiftMinus3v3Right;
    
    CCTK_REAL S1FluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(S1FluxLeft + 
      S1FluxRight + hlleAlpha*(-S1LeftL + ShiftMinus3S1Right));
    
    CCTK_REAL S2FluxLeft CCTK_ATTRIBUTE_UNUSED = S2LeftL*v3LeftL;
    
    CCTK_REAL S2FluxRight CCTK_ATTRIBUTE_UNUSED = 
      ShiftMinus3S2Right*ShiftMinus3v3Right;
    
    CCTK_REAL S2FluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(S2FluxLeft + 
      S2FluxRight + hlleAlpha*(-S2LeftL + ShiftMinus3S2Right));
    
    CCTK_REAL S3FluxLeft CCTK_ATTRIBUTE_UNUSED = S3LeftL*v3LeftL + 
      epsiLeftL*rhoLeftL*(-1 + gamma);
    
    CCTK_REAL S3FluxRight CCTK_ATTRIBUTE_UNUSED = (-1 + 
      gamma)*ShiftMinus3epsiRight*ShiftMinus3rhoRight + 
      ShiftMinus3S3Right*ShiftMinus3v3Right;
    
    CCTK_REAL S3FluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(S3FluxLeft + 
      S3FluxRight + hlleAlpha*(-S3LeftL + ShiftMinus3S3Right));
    
    CCTK_REAL tauFluxLeft CCTK_ATTRIBUTE_UNUSED = v3LeftL*(tauLeftL + 
      epsiLeftL*rhoLeftL*(-1 + gamma));
    
    CCTK_REAL tauFluxRight CCTK_ATTRIBUTE_UNUSED = ((-1 + 
      gamma)*ShiftMinus3epsiRight*ShiftMinus3rhoRight + 
      ShiftMinus3tauRight)*ShiftMinus3v3Right;
    
    CCTK_REAL tauFluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(hlleAlpha*(-tauLeftL + 
      ShiftMinus3tauRight) + tauFluxLeft + tauFluxRight);
    /* Copy local copies back to grid functions */
    DenFlux[index] = DenFluxL;
    S1Flux[index] = S1FluxL;
    S2Flux[index] = S2FluxL;
    S3Flux[index] = S3FluxL;
    tauFlux[index] = tauFluxL;
  }
  CCTK_ENDLOOP3(eulersr_cons_calc_flux_3);
}
extern "C" void eulersr_cons_calc_flux_3(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_flux_3_Body");
  }
  if (cctk_iteration % eulersr_cons_calc_flux_3_calc_every != eulersr_cons_calc_flux_3_calc_offset)
  {
    return;
  }
  
  const char* const groups[] = {
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
    "EulerSR::v3_lr_group"};
  AssertGroupStorage(cctkGH, "eulersr_cons_calc_flux_3", 13, groups);
  
  EnsureStencilFits(cctkGH, "eulersr_cons_calc_flux_3", 1, 1, 1);
  
  LoopOverInterior(cctkGH, eulersr_cons_calc_flux_3_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_flux_3_Body");
  }
}

} // namespace EulerSR
