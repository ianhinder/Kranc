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

extern "C" void eulerauto_cons_calc_flux_2_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::Den_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::Den_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::En_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::En_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::S1_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::S1_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::S2_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::S2_flux_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::S3_flux_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::S3_flux_group.");
  return;
}

static void eulerauto_cons_calc_flux_2_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* Include user-supplied include files */
  
  /* Initialise finite differencing variables */
  ptrdiff_t const di = 1;
  ptrdiff_t const dj = CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  ptrdiff_t const dk = CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  ptrdiff_t const cdi = sizeof(CCTK_REAL) * di;
  ptrdiff_t const cdj = sizeof(CCTK_REAL) * dj;
  ptrdiff_t const cdk = sizeof(CCTK_REAL) * dk;
  CCTK_REAL const dx = ToReal(CCTK_DELTA_SPACE(0));
  CCTK_REAL const dy = ToReal(CCTK_DELTA_SPACE(1));
  CCTK_REAL const dz = ToReal(CCTK_DELTA_SPACE(2));
  CCTK_REAL const dt = ToReal(CCTK_DELTA_TIME);
  CCTK_REAL const t = ToReal(cctk_time);
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
  
  /* Assign local copies of arrays functions */
  
  
  
  /* Calculate temporaries and arrays functions */
  
  /* Copy local copies back to grid functions */
  
  /* Loop over the grid points */
  #pragma omp parallel
  CCTK_LOOP3(eulerauto_cons_calc_flux_2,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    ptrdiff_t const index = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenLeftL = DenLeft[index];
    CCTK_REAL DenRightL = DenRight[index];
    CCTK_REAL EnLeftL = EnLeft[index];
    CCTK_REAL EnRightL = EnRight[index];
    CCTK_REAL pLeftL = pLeft[index];
    CCTK_REAL pRightL = pRight[index];
    CCTK_REAL rhoLeftL = rhoLeft[index];
    CCTK_REAL rhoRightL = rhoRight[index];
    CCTK_REAL S1LeftL = S1Left[index];
    CCTK_REAL S1RightL = S1Right[index];
    CCTK_REAL S2LeftL = S2Left[index];
    CCTK_REAL S2RightL = S2Right[index];
    CCTK_REAL S3LeftL = S3Left[index];
    CCTK_REAL S3RightL = S3Right[index];
    CCTK_REAL v1LeftL = v1Left[index];
    CCTK_REAL v1RightL = v1Right[index];
    CCTK_REAL v2LeftL = v2Left[index];
    CCTK_REAL v2RightL = v2Right[index];
    CCTK_REAL v3LeftL = v3Left[index];
    CCTK_REAL v3RightL = v3Right[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    CCTK_REAL const ShiftMinus2DenRight = ShiftMinus2(&DenRight[index]);
    CCTK_REAL const ShiftMinus2EnRight = ShiftMinus2(&EnRight[index]);
    CCTK_REAL const ShiftMinus2pRight = ShiftMinus2(&pRight[index]);
    CCTK_REAL const ShiftMinus2rhoRight = ShiftMinus2(&rhoRight[index]);
    CCTK_REAL const ShiftMinus2S1Right = ShiftMinus2(&S1Right[index]);
    CCTK_REAL const ShiftMinus2S2Right = ShiftMinus2(&S2Right[index]);
    CCTK_REAL const ShiftMinus2S3Right = ShiftMinus2(&S3Right[index]);
    CCTK_REAL const ShiftMinus2v1Right = ShiftMinus2(&v1Right[index]);
    CCTK_REAL const ShiftMinus2v2Right = ShiftMinus2(&v2Right[index]);
    CCTK_REAL const ShiftMinus2v3Right = ShiftMinus2(&v3Right[index]);
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL DenFluxLeft = rhoLeftL*v2LeftL;
    
    CCTK_REAL DenFluxRight = ShiftMinus2rhoRight*ShiftMinus2v2Right;
    
    CCTK_REAL DenFluxL = 0.5*(DenFluxLeft + DenFluxRight + (-DenLeftL + 
      ShiftMinus2DenRight)*ToReal(hlleAlpha));
    
    CCTK_REAL S1FluxLeft = rhoLeftL*v1LeftL*v2LeftL;
    
    CCTK_REAL S1FluxRight = 
      ShiftMinus2rhoRight*ShiftMinus2v1Right*ShiftMinus2v2Right;
    
    CCTK_REAL S1FluxL = 0.5*(S1FluxLeft + S1FluxRight + (-S1LeftL + 
      ShiftMinus2S1Right)*ToReal(hlleAlpha));
    
    CCTK_REAL S2FluxLeft = pLeftL + rhoLeftL*SQR(v2LeftL);
    
    CCTK_REAL S2FluxRight = ShiftMinus2pRight + 
      ShiftMinus2rhoRight*SQR(ShiftMinus2v2Right);
    
    CCTK_REAL S2FluxL = 0.5*(S2FluxLeft + S2FluxRight + (-S2LeftL + 
      ShiftMinus2S2Right)*ToReal(hlleAlpha));
    
    CCTK_REAL S3FluxLeft = rhoLeftL*v2LeftL*v3LeftL;
    
    CCTK_REAL S3FluxRight = 
      ShiftMinus2rhoRight*ShiftMinus2v2Right*ShiftMinus2v3Right;
    
    CCTK_REAL S3FluxL = 0.5*(S3FluxLeft + S3FluxRight + (-S3LeftL + 
      ShiftMinus2S3Right)*ToReal(hlleAlpha));
    
    CCTK_REAL EnFluxLeft = (EnLeftL + pLeftL)*v2LeftL;
    
    CCTK_REAL EnFluxRight = (ShiftMinus2EnRight + 
      ShiftMinus2pRight)*ShiftMinus2v2Right;
    
    CCTK_REAL EnFluxL = 0.5*(EnFluxLeft + EnFluxRight + (-EnLeftL + 
      ShiftMinus2EnRight)*ToReal(hlleAlpha));
    
    /* Copy local copies back to grid functions */
    DenFlux[index] = DenFluxL;
    EnFlux[index] = EnFluxL;
    S1Flux[index] = S1FluxL;
    S2Flux[index] = S2FluxL;
    S3Flux[index] = S3FluxL;
  }
  CCTK_ENDLOOP3(eulerauto_cons_calc_flux_2);
}

extern "C" void eulerauto_cons_calc_flux_2(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulerauto_cons_calc_flux_2_Body");
  }
  
  if (cctk_iteration % eulerauto_cons_calc_flux_2_calc_every != eulerauto_cons_calc_flux_2_calc_offset)
  {
    return;
  }
  
  const char *const groups[] = {
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
  GenericFD_AssertGroupStorage(cctkGH, "eulerauto_cons_calc_flux_2", 15, groups);
  
  GenericFD_EnsureStencilFits(cctkGH, "eulerauto_cons_calc_flux_2", 1, 1, 1);
  
  GenericFD_LoopOverInterior(cctkGH, eulerauto_cons_calc_flux_2_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulerauto_cons_calc_flux_2_Body");
  }
}
