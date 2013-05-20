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

extern "C" void euler_flux_1_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED  = 0;
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
  CCTK_REAL /*const*/ p1o12dx CCTK_ATTRIBUTE_UNUSED  = 0.0833333333333333333333333333333*INV(dx);
  CCTK_REAL /*const*/ p1o12dy CCTK_ATTRIBUTE_UNUSED  = 0.0833333333333333333333333333333*INV(dy);
  CCTK_REAL /*const*/ p1o12dz CCTK_ATTRIBUTE_UNUSED  = 0.0833333333333333333333333333333*INV(dz);
  CCTK_REAL /*const*/ p1o144dxdy CCTK_ATTRIBUTE_UNUSED  = 0.00694444444444444444444444444444*INV(dx*dy);
  CCTK_REAL /*const*/ p1o144dxdz CCTK_ATTRIBUTE_UNUSED  = 0.00694444444444444444444444444444*INV(dx*dz);
  CCTK_REAL /*const*/ p1o144dydz CCTK_ATTRIBUTE_UNUSED  = 0.00694444444444444444444444444444*INV(dy*dz);
  CCTK_REAL /*const*/ p1o2dx CCTK_ATTRIBUTE_UNUSED  = 0.5*INV(dx);
  CCTK_REAL /*const*/ p1o2dy CCTK_ATTRIBUTE_UNUSED  = 0.5*INV(dy);
  CCTK_REAL /*const*/ p1o2dz CCTK_ATTRIBUTE_UNUSED  = 0.5*INV(dz);
  CCTK_REAL /*const*/ p1o4dxdy CCTK_ATTRIBUTE_UNUSED  = 0.25*INV(dx*dy);
  CCTK_REAL /*const*/ p1o4dxdz CCTK_ATTRIBUTE_UNUSED  = 0.25*INV(dx*dz);
  CCTK_REAL /*const*/ p1o4dydz CCTK_ATTRIBUTE_UNUSED  = 0.25*INV(dy*dz);
  CCTK_REAL /*const*/ p1odx CCTK_ATTRIBUTE_UNUSED  = INV(dx);
  CCTK_REAL /*const*/ p1odx2 CCTK_ATTRIBUTE_UNUSED  = INV(SQR(dx));
  CCTK_REAL /*const*/ p1ody CCTK_ATTRIBUTE_UNUSED  = INV(dy);
  CCTK_REAL /*const*/ p1ody2 CCTK_ATTRIBUTE_UNUSED  = INV(SQR(dy));
  CCTK_REAL /*const*/ p1odz CCTK_ATTRIBUTE_UNUSED  = INV(dz);
  CCTK_REAL /*const*/ p1odz2 CCTK_ATTRIBUTE_UNUSED  = INV(SQR(dz));
  CCTK_REAL /*const*/ pm1o12dx2 CCTK_ATTRIBUTE_UNUSED  = -0.0833333333333333333333333333333*INV(SQR(dx));
  CCTK_REAL /*const*/ pm1o12dy2 CCTK_ATTRIBUTE_UNUSED  = -0.0833333333333333333333333333333*INV(SQR(dy));
  CCTK_REAL /*const*/ pm1o12dz2 CCTK_ATTRIBUTE_UNUSED  = -0.0833333333333333333333333333333*INV(SQR(dz));
  CCTK_REAL /*const*/ pm1o2dx CCTK_ATTRIBUTE_UNUSED  = -0.5*INV(dx);
  CCTK_REAL /*const*/ pm1o2dy CCTK_ATTRIBUTE_UNUSED  = -0.5*INV(dy);
  CCTK_REAL /*const*/ pm1o2dz CCTK_ATTRIBUTE_UNUSED  = -0.5*INV(dz);
  
  /* Assign local copies of arrays functions */
  
  
  
  /* Calculate temporaries and arrays functions */
  
  /* Copy local copies back to grid functions */
  
  /* Loop over the grid points */
  #pragma omp parallel
  CCTK_LOOP3(euler_flux_1,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    ptrdiff_t /*const*/ index CCTK_ATTRIBUTE_UNUSED  = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenLeftL CCTK_ATTRIBUTE_UNUSED = DenLeft[index];
    CCTK_REAL DenRightL CCTK_ATTRIBUTE_UNUSED = DenRight[index];
    CCTK_REAL EnLeftL CCTK_ATTRIBUTE_UNUSED = EnLeft[index];
    CCTK_REAL EnRightL CCTK_ATTRIBUTE_UNUSED = EnRight[index];
    CCTK_REAL pLeftL CCTK_ATTRIBUTE_UNUSED = pLeft[index];
    CCTK_REAL pRightL CCTK_ATTRIBUTE_UNUSED = pRight[index];
    CCTK_REAL rhoLeftL CCTK_ATTRIBUTE_UNUSED = rhoLeft[index];
    CCTK_REAL rhoRightL CCTK_ATTRIBUTE_UNUSED = rhoRight[index];
    CCTK_REAL SLeft1L CCTK_ATTRIBUTE_UNUSED = SLeft1[index];
    CCTK_REAL SLeft2L CCTK_ATTRIBUTE_UNUSED = SLeft2[index];
    CCTK_REAL SLeft3L CCTK_ATTRIBUTE_UNUSED = SLeft3[index];
    CCTK_REAL SRight1L CCTK_ATTRIBUTE_UNUSED = SRight1[index];
    CCTK_REAL SRight2L CCTK_ATTRIBUTE_UNUSED = SRight2[index];
    CCTK_REAL SRight3L CCTK_ATTRIBUTE_UNUSED = SRight3[index];
    CCTK_REAL vLeft1L CCTK_ATTRIBUTE_UNUSED = vLeft1[index];
    CCTK_REAL vLeft2L CCTK_ATTRIBUTE_UNUSED = vLeft2[index];
    CCTK_REAL vLeft3L CCTK_ATTRIBUTE_UNUSED = vLeft3[index];
    CCTK_REAL vRight1L CCTK_ATTRIBUTE_UNUSED = vRight1[index];
    CCTK_REAL vRight2L CCTK_ATTRIBUTE_UNUSED = vRight2[index];
    CCTK_REAL vRight3L CCTK_ATTRIBUTE_UNUSED = vRight3[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    CCTK_REAL /*const*/ ShiftMinus1DenRight CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&DenRight[index]);
    CCTK_REAL /*const*/ ShiftMinus1EnRight CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&EnRight[index]);
    CCTK_REAL /*const*/ ShiftMinus1pRight CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&pRight[index]);
    CCTK_REAL /*const*/ ShiftMinus1rhoRight CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&rhoRight[index]);
    CCTK_REAL /*const*/ ShiftMinus1SRight1 CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&SRight1[index]);
    CCTK_REAL /*const*/ ShiftMinus1SRight2 CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&SRight2[index]);
    CCTK_REAL /*const*/ ShiftMinus1SRight3 CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&SRight3[index]);
    CCTK_REAL /*const*/ ShiftMinus1vRight1 CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&vRight1[index]);
    CCTK_REAL /*const*/ ShiftMinus1vRight2 CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&vRight2[index]);
    CCTK_REAL /*const*/ ShiftMinus1vRight3 CCTK_ATTRIBUTE_UNUSED  = ShiftMinus1(&vRight3[index]);
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED vRightTemp1 = ShiftMinus1vRight1;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED vRightTemp2 = ShiftMinus1vRight2;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED vRightTemp3 = ShiftMinus1vRight3;
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED DenFL = 0.5*(rhoLeftL*vLeft1L + 
      ShiftMinus1rhoRight*vRightTemp1 + (-1.*DenLeftL + 
      ShiftMinus1DenRight)*ToReal(alpha));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED SF1L = 0.5*(pLeftL + 
      ShiftMinus1pRight + rhoLeftL*SQR(vLeft1L) + 
      ShiftMinus1rhoRight*SQR(vRightTemp1) + (-1.*SLeft1L + 
      ShiftMinus1SRight1)*ToReal(alpha));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED SF2L = 
      0.5*(rhoLeftL*vLeft1L*vLeft2L + 
      ShiftMinus1rhoRight*vRightTemp1*vRightTemp2 + (-1.*SLeft2L + 
      ShiftMinus1SRight2)*ToReal(alpha));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED SF3L = 
      0.5*(rhoLeftL*vLeft1L*vLeft3L + 
      ShiftMinus1rhoRight*vRightTemp1*vRightTemp3 + (-1.*SLeft3L + 
      ShiftMinus1SRight3)*ToReal(alpha));
    
    CCTK_REAL CCTK_ATTRIBUTE_UNUSED EnFL = 0.5*((EnLeftL + 
      pLeftL)*vLeft1L + ShiftMinus1pRight*vRightTemp1 - 
      1.*EnLeftL*ToReal(alpha) + ShiftMinus1EnRight*(vRightTemp1 + 
      ToReal(alpha)));
    
    /* Copy local copies back to grid functions */
    DenF[index] = DenFL;
    EnF[index] = EnFL;
    SF1[index] = SF1L;
    SF2[index] = SF2L;
    SF3[index] = SF3L;
  }
  CCTK_ENDLOOP3(euler_flux_1);
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
  
  const char *const groups[] = {
    "Euler::DenF_group",
    "Euler::DenLeft_group",
    "Euler::DenRight_group",
    "Euler::EnF_group",
    "Euler::EnLeft_group",
    "Euler::EnRight_group",
    "Euler::pLeft_group",
    "Euler::pRight_group",
    "Euler::rhoLeft_group",
    "Euler::rhoRight_group",
    "Euler::SF_group",
    "Euler::SLeft_group",
    "Euler::SRight_group",
    "Euler::vLeft_group",
    "Euler::vRight_group"};
  GenericFD_AssertGroupStorage(cctkGH, "euler_flux_1", 15, groups);
  
  GenericFD_EnsureStencilFits(cctkGH, "euler_flux_1", 1, 1, 1);
  
  GenericFD_LoopOverInterior(cctkGH, euler_flux_1_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving euler_flux_1_Body");
  }
}
