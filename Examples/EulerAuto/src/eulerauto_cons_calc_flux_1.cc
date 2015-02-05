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

namespace EulerAuto {

extern "C" void eulerauto_cons_calc_flux_1_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (cctk_iteration % eulerauto_cons_calc_flux_1_calc_every != eulerauto_cons_calc_flux_1_calc_offset)
    return;
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::Den_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::Den_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::En_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::En_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::S1_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::S1_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::S2_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::S2_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::S3_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::S3_flux_group.");
  return;
}

static void eulerauto_cons_calc_flux_1_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
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
  CCTK_LOOP3(eulerauto_cons_calc_flux_1,
    i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    const ptrdiff_t index CCTK_ATTRIBUTE_UNUSED = di*i + dj*j + dk*k;
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenLeftL CCTK_ATTRIBUTE_UNUSED = DenLeft[index];
    CCTK_REAL DenRightL CCTK_ATTRIBUTE_UNUSED = DenRight[index];
    CCTK_REAL EnLeftL CCTK_ATTRIBUTE_UNUSED = EnLeft[index];
    CCTK_REAL EnRightL CCTK_ATTRIBUTE_UNUSED = EnRight[index];
    CCTK_REAL pLeftL CCTK_ATTRIBUTE_UNUSED = pLeft[index];
    CCTK_REAL pRightL CCTK_ATTRIBUTE_UNUSED = pRight[index];
    CCTK_REAL rhoLeftL CCTK_ATTRIBUTE_UNUSED = rhoLeft[index];
    CCTK_REAL rhoRightL CCTK_ATTRIBUTE_UNUSED = rhoRight[index];
    CCTK_REAL S1LeftL CCTK_ATTRIBUTE_UNUSED = S1Left[index];
    CCTK_REAL S1RightL CCTK_ATTRIBUTE_UNUSED = S1Right[index];
    CCTK_REAL S2LeftL CCTK_ATTRIBUTE_UNUSED = S2Left[index];
    CCTK_REAL S2RightL CCTK_ATTRIBUTE_UNUSED = S2Right[index];
    CCTK_REAL S3LeftL CCTK_ATTRIBUTE_UNUSED = S3Left[index];
    CCTK_REAL S3RightL CCTK_ATTRIBUTE_UNUSED = S3Right[index];
    CCTK_REAL v1LeftL CCTK_ATTRIBUTE_UNUSED = v1Left[index];
    CCTK_REAL v1RightL CCTK_ATTRIBUTE_UNUSED = v1Right[index];
    CCTK_REAL v2LeftL CCTK_ATTRIBUTE_UNUSED = v2Left[index];
    CCTK_REAL v2RightL CCTK_ATTRIBUTE_UNUSED = v2Right[index];
    CCTK_REAL v3LeftL CCTK_ATTRIBUTE_UNUSED = v3Left[index];
    CCTK_REAL v3RightL CCTK_ATTRIBUTE_UNUSED = v3Right[index];
    
    /* Include user supplied include files */
    /* Precompute derivatives */
    const CCTK_REAL ShiftMinus1DenRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&DenRight[index]);
    const CCTK_REAL ShiftMinus1EnRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&EnRight[index]);
    const CCTK_REAL ShiftMinus1pRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&pRight[index]);
    const CCTK_REAL ShiftMinus1rhoRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&rhoRight[index]);
    const CCTK_REAL ShiftMinus1S1Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&S1Right[index]);
    const CCTK_REAL ShiftMinus1S2Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&S2Right[index]);
    const CCTK_REAL ShiftMinus1S3Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&S3Right[index]);
    const CCTK_REAL ShiftMinus1v1Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&v1Right[index]);
    const CCTK_REAL ShiftMinus1v2Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&v2Right[index]);
    const CCTK_REAL ShiftMinus1v3Right CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&v3Right[index]);
    /* Calculate temporaries and grid functions */
    CCTK_REAL DenFluxLeft CCTK_ATTRIBUTE_UNUSED = rhoLeftL*v1LeftL;
    
    CCTK_REAL DenFluxRight CCTK_ATTRIBUTE_UNUSED = 
      ShiftMinus1rhoRight*ShiftMinus1v1Right;
    
    CCTK_REAL DenFluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(DenFluxLeft + 
      DenFluxRight + hlleAlpha*(-DenLeftL + ShiftMinus1DenRight));
    
    CCTK_REAL S1FluxLeft CCTK_ATTRIBUTE_UNUSED = pLeftL + 
      rhoLeftL*pow(v1LeftL,2);
    
    CCTK_REAL S1FluxRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus1pRight + 
      ShiftMinus1rhoRight*pow(ShiftMinus1v1Right,2);
    
    CCTK_REAL S1FluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(S1FluxLeft + 
      S1FluxRight + hlleAlpha*(-S1LeftL + ShiftMinus1S1Right));
    
    CCTK_REAL S2FluxLeft CCTK_ATTRIBUTE_UNUSED = rhoLeftL*v1LeftL*v2LeftL;
    
    CCTK_REAL S2FluxRight CCTK_ATTRIBUTE_UNUSED = 
      ShiftMinus1rhoRight*ShiftMinus1v1Right*ShiftMinus1v2Right;
    
    CCTK_REAL S2FluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(S2FluxLeft + 
      S2FluxRight + hlleAlpha*(-S2LeftL + ShiftMinus1S2Right));
    
    CCTK_REAL S3FluxLeft CCTK_ATTRIBUTE_UNUSED = rhoLeftL*v1LeftL*v3LeftL;
    
    CCTK_REAL S3FluxRight CCTK_ATTRIBUTE_UNUSED = 
      ShiftMinus1rhoRight*ShiftMinus1v1Right*ShiftMinus1v3Right;
    
    CCTK_REAL S3FluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(S3FluxLeft + 
      S3FluxRight + hlleAlpha*(-S3LeftL + ShiftMinus1S3Right));
    
    CCTK_REAL EnFluxLeft CCTK_ATTRIBUTE_UNUSED = (EnLeftL + 
      pLeftL)*v1LeftL;
    
    CCTK_REAL EnFluxRight CCTK_ATTRIBUTE_UNUSED = (ShiftMinus1EnRight + 
      ShiftMinus1pRight)*ShiftMinus1v1Right;
    
    CCTK_REAL EnFluxL CCTK_ATTRIBUTE_UNUSED = 0.5*(EnFluxLeft + 
      EnFluxRight + hlleAlpha*(-EnLeftL + ShiftMinus1EnRight));
    /* Copy local copies back to grid functions */
    DenFlux[index] = DenFluxL;
    EnFlux[index] = EnFluxL;
    S1Flux[index] = S1FluxL;
    S2Flux[index] = S2FluxL;
    S3Flux[index] = S3FluxL;
  }
  CCTK_ENDLOOP3(eulerauto_cons_calc_flux_1);
}
extern "C" void eulerauto_cons_calc_flux_1(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulerauto_cons_calc_flux_1_Body");
  }
  if (cctk_iteration % eulerauto_cons_calc_flux_1_calc_every != eulerauto_cons_calc_flux_1_calc_offset)
  {
    return;
  }
  
  const char* const groups[] = {
    "EulerAuto::Den_flux_group",
    "EulerAuto::Den_lr_group",
    "EulerAuto::En_flux_group",
    "EulerAuto::En_lr_group",
    "EulerAuto::p_lr_group",
    "EulerAuto::rho_lr_group",
    "EulerAuto::S1_flux_group",
    "EulerAuto::S1_lr_group",
    "EulerAuto::S2_flux_group",
    "EulerAuto::S2_lr_group",
    "EulerAuto::S3_flux_group",
    "EulerAuto::S3_lr_group",
    "EulerAuto::v1_lr_group",
    "EulerAuto::v2_lr_group",
    "EulerAuto::v3_lr_group"};
  AssertGroupStorage(cctkGH, "eulerauto_cons_calc_flux_1", 15, groups);
  
  EnsureStencilFits(cctkGH, "eulerauto_cons_calc_flux_1", 1, 1, 1);
  
  LoopOverInterior(cctkGH, eulerauto_cons_calc_flux_1_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulerauto_cons_calc_flux_1_Body");
  }
}

} // namespace EulerAuto
