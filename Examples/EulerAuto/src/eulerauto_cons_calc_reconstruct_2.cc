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

extern "C" void eulerauto_cons_calc_reconstruct_2_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::p_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::p_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::rho_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::rho_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::v1_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::v1_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::v2_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::v2_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerAuto::v3_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerAuto::v3_lr_group.");
  return;
}

static void eulerauto_cons_calc_reconstruct_2_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
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
  CCTK_LOOP3(eulerauto_cons_calc_reconstruct_2,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    ptrdiff_t const index = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL pL = p[index];
    CCTK_REAL rhoL = rho[index];
    CCTK_REAL v1L = v1[index];
    CCTK_REAL v2L = v2[index];
    CCTK_REAL v3L = v3[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    CCTK_REAL const DiffPlus2p = DiffPlus2(&p[index]);
    CCTK_REAL const DiffMinus2p = DiffMinus2(&p[index]);
    CCTK_REAL const DiffPlus2rho = DiffPlus2(&rho[index]);
    CCTK_REAL const DiffMinus2rho = DiffMinus2(&rho[index]);
    CCTK_REAL const DiffPlus2v1 = DiffPlus2(&v1[index]);
    CCTK_REAL const DiffMinus2v1 = DiffMinus2(&v1[index]);
    CCTK_REAL const DiffPlus2v2 = DiffPlus2(&v2[index]);
    CCTK_REAL const DiffMinus2v2 = DiffMinus2(&v2[index]);
    CCTK_REAL const DiffPlus2v3 = DiffPlus2(&v3[index]);
    CCTK_REAL const DiffMinus2v3 = DiffMinus2(&v3[index]);
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL slopeL = DiffMinus2rho;
    
    CCTK_REAL slopeR = DiffPlus2rho;
    
    CCTK_REAL slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL rhoLeftL = rhoL - 0.5*slope;
    
    CCTK_REAL rhoRightL = rhoL + 0.5*slope;
    
    slopeL = DiffMinus2v1;
    
    slopeR = DiffPlus2v1;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL v1LeftL = v1L - 0.5*slope;
    
    CCTK_REAL v1RightL = v1L + 0.5*slope;
    
    slopeL = DiffMinus2v2;
    
    slopeR = DiffPlus2v2;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL v2LeftL = v2L - 0.5*slope;
    
    CCTK_REAL v2RightL = v2L + 0.5*slope;
    
    slopeL = DiffMinus2v3;
    
    slopeR = DiffPlus2v3;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL v3LeftL = v3L - 0.5*slope;
    
    CCTK_REAL v3RightL = v3L + 0.5*slope;
    
    slopeL = DiffMinus2p;
    
    slopeR = DiffPlus2p;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL pLeftL = pL - 0.5*slope;
    
    CCTK_REAL pRightL = pL + 0.5*slope;
    
    /* Copy local copies back to grid functions */
    pLeft[index] = pLeftL;
    pRight[index] = pRightL;
    rhoLeft[index] = rhoLeftL;
    rhoRight[index] = rhoRightL;
    v1Left[index] = v1LeftL;
    v1Right[index] = v1RightL;
    v2Left[index] = v2LeftL;
    v2Right[index] = v2RightL;
    v3Left[index] = v3LeftL;
    v3Right[index] = v3RightL;
  }
  CCTK_ENDLOOP3(eulerauto_cons_calc_reconstruct_2);
}

extern "C" void eulerauto_cons_calc_reconstruct_2(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulerauto_cons_calc_reconstruct_2_Body");
  }
  
  if (cctk_iteration % eulerauto_cons_calc_reconstruct_2_calc_every != eulerauto_cons_calc_reconstruct_2_calc_offset)
  {
    return;
  }
  
  const char *const groups[] = {
    "EulerAuto::p_group",
    "EulerAuto::p_lr_group",
    "EulerAuto::rho_group",
    "EulerAuto::rho_lr_group",
    "EulerAuto::v1_lr_group",
    "EulerAuto::v2_lr_group",
    "EulerAuto::v3_lr_group",
    "EulerAuto::v_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulerauto_cons_calc_reconstruct_2", 8, groups);
  
  GenericFD_EnsureStencilFits(cctkGH, "eulerauto_cons_calc_reconstruct_2", 1, 1, 1);
  
  GenericFD_LoopOverInterior(cctkGH, eulerauto_cons_calc_reconstruct_2_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulerauto_cons_calc_reconstruct_2_Body");
  }
}
