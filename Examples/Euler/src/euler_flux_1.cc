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
#define QAD(x) (SQR(SQR(x)))
#define INV(x) ((1.0) / (x))
#define SQR(x) ((x) * (x))
#define CUB(x) ((x) * (x) * (x))

extern "C" void euler_flux_1_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::DenF_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::DenF_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::EnF_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::EnF_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::SF_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::SF_group.");
  return;
}

static void euler_flux_1_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* Declare finite differencing variables */
  
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
  CCTK_REAL const p1o144dxdy = 0.00694444444444444444444444444444*INV(dx)*INV(dy);
  CCTK_REAL const p1o144dxdz = 0.00694444444444444444444444444444*INV(dx)*INV(dz);
  CCTK_REAL const p1o144dydz = 0.00694444444444444444444444444444*INV(dy)*INV(dz);
  CCTK_REAL const p1o2dx = 0.5*INV(dx);
  CCTK_REAL const p1o2dy = 0.5*INV(dy);
  CCTK_REAL const p1o2dz = 0.5*INV(dz);
  CCTK_REAL const p1o4dxdy = 0.25*INV(dx)*INV(dy);
  CCTK_REAL const p1o4dxdz = 0.25*INV(dx)*INV(dz);
  CCTK_REAL const p1o4dydz = 0.25*INV(dy)*INV(dz);
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
  CCTK_LOOP3 (euler_flux_1,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_lsh[0],cctk_lsh[1],cctk_lsh[2])
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
    CCTK_REAL SLeft1L = SLeft1[index];
    CCTK_REAL SLeft2L = SLeft2[index];
    CCTK_REAL SLeft3L = SLeft3[index];
    CCTK_REAL SRight1L = SRight1[index];
    CCTK_REAL SRight2L = SRight2[index];
    CCTK_REAL SRight3L = SRight3[index];
    CCTK_REAL vLeft1L = vLeft1[index];
    CCTK_REAL vLeft2L = vLeft2[index];
    CCTK_REAL vLeft3L = vLeft3[index];
    CCTK_REAL vRight1L = vRight1[index];
    CCTK_REAL vRight2L = vRight2[index];
    CCTK_REAL vRight3L = vRight3[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    CCTK_REAL const ShiftMinus1DenRight = ShiftMinus1(&DenRight[index]);
    CCTK_REAL const ShiftMinus1EnRight = ShiftMinus1(&EnRight[index]);
    CCTK_REAL const ShiftMinus1pRight = ShiftMinus1(&pRight[index]);
    CCTK_REAL const ShiftMinus1rhoRight = ShiftMinus1(&rhoRight[index]);
    CCTK_REAL const ShiftMinus1SRight1 = ShiftMinus1(&SRight1[index]);
    CCTK_REAL const ShiftMinus1SRight2 = ShiftMinus1(&SRight2[index]);
    CCTK_REAL const ShiftMinus1SRight3 = ShiftMinus1(&SRight3[index]);
    CCTK_REAL const ShiftMinus1vRight1 = ShiftMinus1(&vRight1[index]);
    CCTK_REAL const ShiftMinus1vRight2 = ShiftMinus1(&vRight2[index]);
    CCTK_REAL const ShiftMinus1vRight3 = ShiftMinus1(&vRight3[index]);
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL vRightTemp1 = ShiftMinus1vRight1;
    
    CCTK_REAL vRightTemp2 = ShiftMinus1vRight2;
    
    CCTK_REAL vRightTemp3 = ShiftMinus1vRight3;
    
    CCTK_REAL DenFL = 0.5*(rhoLeftL*vLeft1L + 
      ShiftMinus1rhoRight*vRightTemp1 + (-DenLeftL + 
      ShiftMinus1DenRight)*ToReal(alpha));
    
    CCTK_REAL SF1L = 0.5*(pLeftL + ShiftMinus1pRight + 
      rhoLeftL*SQR(vLeft1L) + ShiftMinus1rhoRight*SQR(vRightTemp1) + 
      (-SLeft1L + ShiftMinus1SRight1)*ToReal(alpha));
    
    CCTK_REAL SF2L = 0.5*(rhoLeftL*vLeft1L*vLeft2L + 
      ShiftMinus1rhoRight*vRightTemp1*vRightTemp2 + (-SLeft2L + 
      ShiftMinus1SRight2)*ToReal(alpha));
    
    CCTK_REAL SF3L = 0.5*(rhoLeftL*vLeft1L*vLeft3L + 
      ShiftMinus1rhoRight*vRightTemp1*vRightTemp3 + (-SLeft3L + 
      ShiftMinus1SRight3)*ToReal(alpha));
    
    CCTK_REAL EnFL = 0.5*((EnLeftL + pLeftL)*vLeft1L + 
      ShiftMinus1pRight*vRightTemp1 - EnLeftL*ToReal(alpha) + 
      ShiftMinus1EnRight*(vRightTemp1 + ToReal(alpha)));
    
    /* Copy local copies back to grid functions */
    DenF[index] = DenFL;
    EnF[index] = EnFL;
    SF1[index] = SF1L;
    SF2[index] = SF2L;
    SF3[index] = SF3L;
  }
  CCTK_ENDLOOP3 (euler_flux_1);
}

extern "C" void euler_flux_1(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering euler_flux_1_Body");
  }
  
  if (cctk_iteration % euler_flux_1_calc_every != euler_flux_1_calc_offset)
  {
    return;
  }
  
  const char *groups[] = {"Euler::DenF_group","Euler::DenLeft_group","Euler::DenRight_group","Euler::EnF_group","Euler::EnLeft_group","Euler::EnRight_group","Euler::pLeft_group","Euler::pRight_group","Euler::rhoLeft_group","Euler::rhoRight_group","Euler::SF_group","Euler::SLeft_group","Euler::SRight_group","Euler::vLeft_group","Euler::vRight_group"};
  GenericFD_AssertGroupStorage(cctkGH, "euler_flux_1", 15, groups);
  
  GenericFD_EnsureStencilFits(cctkGH, "euler_flux_1", 1, 1, 1);
  
  GenericFD_LoopOverInterior(cctkGH, &euler_flux_1_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving euler_flux_1_Body");
  }
}
