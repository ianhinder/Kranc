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

namespace Euler {

extern "C" void euler_flux_1_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (cctk_iteration % euler_flux_1_calc_every != euler_flux_1_calc_offset)
    return;
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::DenF_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::DenF_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::EnF_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::EnF_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "Euler::SF_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for Euler::SF_group.");
  return;
}

static void euler_flux_1_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
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
  const CCTK_REAL p1o12dx CCTK_ATTRIBUTE_UNUSED = 0.0833333333333333333333333333333*pow(dx,-1);
  const CCTK_REAL p1o12dy CCTK_ATTRIBUTE_UNUSED = 0.0833333333333333333333333333333*pow(dy,-1);
  const CCTK_REAL p1o12dz CCTK_ATTRIBUTE_UNUSED = 0.0833333333333333333333333333333*pow(dz,-1);
  const CCTK_REAL p1o144dxdy CCTK_ATTRIBUTE_UNUSED = 0.00694444444444444444444444444444*pow(dx,-1)*pow(dy,-1);
  const CCTK_REAL p1o144dxdz CCTK_ATTRIBUTE_UNUSED = 0.00694444444444444444444444444444*pow(dx,-1)*pow(dz,-1);
  const CCTK_REAL p1o144dydz CCTK_ATTRIBUTE_UNUSED = 0.00694444444444444444444444444444*pow(dy,-1)*pow(dz,-1);
  const CCTK_REAL p1o2dx CCTK_ATTRIBUTE_UNUSED = 0.5*pow(dx,-1);
  const CCTK_REAL p1o2dy CCTK_ATTRIBUTE_UNUSED = 0.5*pow(dy,-1);
  const CCTK_REAL p1o2dz CCTK_ATTRIBUTE_UNUSED = 0.5*pow(dz,-1);
  const CCTK_REAL p1o4dxdy CCTK_ATTRIBUTE_UNUSED = 0.25*pow(dx,-1)*pow(dy,-1);
  const CCTK_REAL p1o4dxdz CCTK_ATTRIBUTE_UNUSED = 0.25*pow(dx,-1)*pow(dz,-1);
  const CCTK_REAL p1o4dydz CCTK_ATTRIBUTE_UNUSED = 0.25*pow(dy,-1)*pow(dz,-1);
  const CCTK_REAL p1odx CCTK_ATTRIBUTE_UNUSED = pow(dx,-1);
  const CCTK_REAL p1odx2 CCTK_ATTRIBUTE_UNUSED = pow(dx,-2);
  const CCTK_REAL p1ody CCTK_ATTRIBUTE_UNUSED = pow(dy,-1);
  const CCTK_REAL p1ody2 CCTK_ATTRIBUTE_UNUSED = pow(dy,-2);
  const CCTK_REAL p1odz CCTK_ATTRIBUTE_UNUSED = pow(dz,-1);
  const CCTK_REAL p1odz2 CCTK_ATTRIBUTE_UNUSED = pow(dz,-2);
  const CCTK_REAL pm1o12dx2 CCTK_ATTRIBUTE_UNUSED = -0.0833333333333333333333333333333*pow(dx,-2);
  const CCTK_REAL pm1o12dy2 CCTK_ATTRIBUTE_UNUSED = -0.0833333333333333333333333333333*pow(dy,-2);
  const CCTK_REAL pm1o12dz2 CCTK_ATTRIBUTE_UNUSED = -0.0833333333333333333333333333333*pow(dz,-2);
  const CCTK_REAL pm1o2dx CCTK_ATTRIBUTE_UNUSED = -0.5*pow(dx,-1);
  const CCTK_REAL pm1o2dy CCTK_ATTRIBUTE_UNUSED = -0.5*pow(dy,-1);
  const CCTK_REAL pm1o2dz CCTK_ATTRIBUTE_UNUSED = -0.5*pow(dz,-1);
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
  CCTK_LOOP3(euler_flux_1,
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
    const CCTK_REAL ShiftMinus1DenRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&DenRight[index]);
    const CCTK_REAL ShiftMinus1EnRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&EnRight[index]);
    const CCTK_REAL ShiftMinus1pRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&pRight[index]);
    const CCTK_REAL ShiftMinus1rhoRight CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&rhoRight[index]);
    const CCTK_REAL ShiftMinus1SRight1 CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&SRight1[index]);
    const CCTK_REAL ShiftMinus1SRight2 CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&SRight2[index]);
    const CCTK_REAL ShiftMinus1SRight3 CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&SRight3[index]);
    const CCTK_REAL ShiftMinus1vRight1 CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&vRight1[index]);
    const CCTK_REAL ShiftMinus1vRight2 CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&vRight2[index]);
    const CCTK_REAL ShiftMinus1vRight3 CCTK_ATTRIBUTE_UNUSED = ShiftMinus1(&vRight3[index]);
    /* Calculate temporaries and grid functions */
    CCTK_REAL vRightTemp1 CCTK_ATTRIBUTE_UNUSED = ShiftMinus1vRight1;
    
    CCTK_REAL vRightTemp2 CCTK_ATTRIBUTE_UNUSED = ShiftMinus1vRight2;
    
    CCTK_REAL vRightTemp3 CCTK_ATTRIBUTE_UNUSED = ShiftMinus1vRight3;
    
    CCTK_REAL DenFL CCTK_ATTRIBUTE_UNUSED = 0.5*(rhoLeftL*vLeft1L + 
      alpha*(-DenLeftL + ShiftMinus1DenRight) + 
      ShiftMinus1rhoRight*vRightTemp1);
    
    CCTK_REAL SF1L CCTK_ATTRIBUTE_UNUSED = 0.5*(pLeftL + ShiftMinus1pRight 
      + alpha*(-SLeft1L + ShiftMinus1SRight1) + rhoLeftL*pow(vLeft1L,2) + 
      ShiftMinus1rhoRight*pow(vRightTemp1,2));
    
    CCTK_REAL SF2L CCTK_ATTRIBUTE_UNUSED = 0.5*(rhoLeftL*vLeft1L*vLeft2L + 
      alpha*(-SLeft2L + ShiftMinus1SRight2) + 
      ShiftMinus1rhoRight*vRightTemp1*vRightTemp2);
    
    CCTK_REAL SF3L CCTK_ATTRIBUTE_UNUSED = 0.5*(rhoLeftL*vLeft1L*vLeft3L + 
      alpha*(-SLeft3L + ShiftMinus1SRight3) + 
      ShiftMinus1rhoRight*vRightTemp1*vRightTemp3);
    
    CCTK_REAL EnFL CCTK_ATTRIBUTE_UNUSED = 0.5*((EnLeftL + pLeftL)*vLeft1L 
      - EnLeftL*alpha + ShiftMinus1pRight*vRightTemp1 + 
      ShiftMinus1EnRight*(alpha + vRightTemp1));
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
  
  const char* const groups[] = {
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
  AssertGroupStorage(cctkGH, "euler_flux_1", 15, groups);
  
  EnsureStencilFits(cctkGH, "euler_flux_1", 1, 1, 1);
  
  LoopOverInterior(cctkGH, euler_flux_1_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving euler_flux_1_Body");
  }
}

} // namespace Euler
