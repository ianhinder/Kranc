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

extern "C" void euler_reconstruct_1_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::pLeft_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::pLeft_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::pRight_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::pRight_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::rhoLeft_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::rhoLeft_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::rhoRight_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::rhoRight_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::vLeft_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::vLeft_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::vRight_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::vRight_group.");
  return;
}

static void euler_reconstruct_1_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
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
  CCTK_REAL const p1o12dx = 0.0833333333333333333333333333333*INV(dx);
  CCTK_REAL const p1o12dy = 0.0833333333333333333333333333333*INV(dy);
  CCTK_REAL const p1o12dz = 0.0833333333333333333333333333333*INV(dz);
  CCTK_REAL const p1o144dxdy = 0.00694444444444444444444444444444*INV(dx*dy);
  CCTK_REAL const p1o144dxdz = 0.00694444444444444444444444444444*INV(dx*dz);
  CCTK_REAL const p1o144dydz = 0.00694444444444444444444444444444*INV(dy*dz);
  CCTK_REAL const p1o2dx = 0.5*INV(dx);
  CCTK_REAL const p1o2dy = 0.5*INV(dy);
  CCTK_REAL const p1o2dz = 0.5*INV(dz);
  CCTK_REAL const p1o4dxdy = 0.25*INV(dx*dy);
  CCTK_REAL const p1o4dxdz = 0.25*INV(dx*dz);
  CCTK_REAL const p1o4dydz = 0.25*INV(dy*dz);
  CCTK_REAL const p1odx = INV(dx);
  CCTK_REAL const p1odx2 = INV(SQR(dx));
  CCTK_REAL const p1ody = INV(dy);
  CCTK_REAL const p1ody2 = INV(SQR(dy));
  CCTK_REAL const p1odz = INV(dz);
  CCTK_REAL const p1odz2 = INV(SQR(dz));
  CCTK_REAL const pm1o12dx2 = -0.0833333333333333333333333333333*INV(SQR(dx));
  CCTK_REAL const pm1o12dy2 = -0.0833333333333333333333333333333*INV(SQR(dy));
  CCTK_REAL const pm1o12dz2 = -0.0833333333333333333333333333333*INV(SQR(dz));
  CCTK_REAL const pm1o2dx = -0.5*INV(dx);
  CCTK_REAL const pm1o2dy = -0.5*INV(dy);
  CCTK_REAL const pm1o2dz = -0.5*INV(dz);
  
  /* Assign local copies of arrays functions */
  
  
  
  /* Calculate temporaries and arrays functions */
  
  /* Copy local copies back to grid functions */
  
  /* Loop over the grid points */
  #pragma omp parallel
  CCTK_LOOP3(euler_reconstruct_1,
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
    CCTK_REAL const DiffPlus1p = DiffPlus1(&p[index]);
    CCTK_REAL const DiffMinus1p = DiffMinus1(&p[index]);
    CCTK_REAL const DiffPlus1rho = DiffPlus1(&rho[index]);
    CCTK_REAL const DiffMinus1rho = DiffMinus1(&rho[index]);
    CCTK_REAL const DiffPlus1v1 = DiffPlus1(&v1[index]);
    CCTK_REAL const DiffMinus1v1 = DiffMinus1(&v1[index]);
    CCTK_REAL const DiffPlus1v2 = DiffPlus1(&v2[index]);
    CCTK_REAL const DiffMinus1v2 = DiffMinus1(&v2[index]);
    CCTK_REAL const DiffPlus1v3 = DiffPlus1(&v3[index]);
    CCTK_REAL const DiffMinus1v3 = DiffMinus1(&v3[index]);
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL slopeL = DiffMinus1rho;
    
    CCTK_REAL slopeR = DiffPlus1rho;
    
    CCTK_REAL slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL rhoLeftL = rhoL - 0.5*slope;
    
    CCTK_REAL rhoRightL = rhoL + 0.5*slope;
    
    slopeL = DiffMinus1v1;
    
    slopeR = DiffPlus1v1;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL vLeft1L = v1L - 0.5*slope;
    
    CCTK_REAL vRight1L = v1L + 0.5*slope;
    
    slopeL = DiffMinus1v2;
    
    slopeR = DiffPlus1v2;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL vLeft2L = v2L - 0.5*slope;
    
    CCTK_REAL vRight2L = v2L + 0.5*slope;
    
    slopeL = DiffMinus1v3;
    
    slopeR = DiffPlus1v3;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL vLeft3L = v3L - 0.5*slope;
    
    CCTK_REAL vRight3L = v3L + 0.5*slope;
    
    slopeL = DiffMinus1p;
    
    slopeR = DiffPlus1p;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL pLeftL = pL - 0.5*slope;
    
    CCTK_REAL pRightL = pL + 0.5*slope;
    
    /* Copy local copies back to grid functions */
    pLeft[index] = pLeftL;
    pRight[index] = pRightL;
    rhoLeft[index] = rhoLeftL;
    rhoRight[index] = rhoRightL;
    vLeft1[index] = vLeft1L;
    vLeft2[index] = vLeft2L;
    vLeft3[index] = vLeft3L;
    vRight1[index] = vRight1L;
    vRight2[index] = vRight2L;
    vRight3[index] = vRight3L;
  }
  CCTK_ENDLOOP3(euler_reconstruct_1);
}

extern "C" void euler_reconstruct_1(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering euler_reconstruct_1_Body");
  }
  
  if (cctk_iteration % euler_reconstruct_1_calc_every != euler_reconstruct_1_calc_offset)
  {
    return;
  }
  
  const char *const groups[] = {
    "Euler::p_group",
    "Euler::pLeft_group",
    "Euler::pRight_group",
    "Euler::rho_group",
    "Euler::rhoLeft_group",
    "Euler::rhoRight_group",
    "Euler::v_group",
    "Euler::vLeft_group",
    "Euler::vRight_group"};
  GenericFD_AssertGroupStorage(cctkGH, "euler_reconstruct_1", 9, groups);
  
  GenericFD_EnsureStencilFits(cctkGH, "euler_reconstruct_1", 1, 1, 1);
  
  GenericFD_LoopOverInterior(cctkGH, euler_reconstruct_1_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving euler_reconstruct_1_Body");
  }
}
