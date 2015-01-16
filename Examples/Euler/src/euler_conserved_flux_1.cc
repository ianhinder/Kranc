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


static void euler_conserved_flux_1_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
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
  CCTK_LOOP3(euler_conserved_flux_1,
    i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    const ptrdiff_t index CCTK_ATTRIBUTE_UNUSED = di*i + dj*j + dk*k;
    /* Assign local copies of grid functions */
    
    CCTK_REAL pLeftL CCTK_ATTRIBUTE_UNUSED = pLeft[index];
    CCTK_REAL pRightL CCTK_ATTRIBUTE_UNUSED = pRight[index];
    CCTK_REAL rhoLeftL CCTK_ATTRIBUTE_UNUSED = rhoLeft[index];
    CCTK_REAL rhoRightL CCTK_ATTRIBUTE_UNUSED = rhoRight[index];
    CCTK_REAL vLeft1L CCTK_ATTRIBUTE_UNUSED = vLeft1[index];
    CCTK_REAL vLeft2L CCTK_ATTRIBUTE_UNUSED = vLeft2[index];
    CCTK_REAL vLeft3L CCTK_ATTRIBUTE_UNUSED = vLeft3[index];
    CCTK_REAL vRight1L CCTK_ATTRIBUTE_UNUSED = vRight1[index];
    CCTK_REAL vRight2L CCTK_ATTRIBUTE_UNUSED = vRight2[index];
    CCTK_REAL vRight3L CCTK_ATTRIBUTE_UNUSED = vRight3[index];
    
    /* Include user supplied include files */
    /* Precompute derivatives */
    /* Calculate temporaries and grid functions */
    CCTK_REAL DenLeftL CCTK_ATTRIBUTE_UNUSED = rhoLeftL;
    
    CCTK_REAL DenRightL CCTK_ATTRIBUTE_UNUSED = rhoRightL;
    
    CCTK_REAL SLeft1L CCTK_ATTRIBUTE_UNUSED = rhoLeftL*vLeft1L;
    
    CCTK_REAL SLeft2L CCTK_ATTRIBUTE_UNUSED = rhoLeftL*vLeft2L;
    
    CCTK_REAL SLeft3L CCTK_ATTRIBUTE_UNUSED = rhoLeftL*vLeft3L;
    
    CCTK_REAL SRight1L CCTK_ATTRIBUTE_UNUSED = rhoRightL*vRight1L;
    
    CCTK_REAL SRight2L CCTK_ATTRIBUTE_UNUSED = rhoRightL*vRight2L;
    
    CCTK_REAL SRight3L CCTK_ATTRIBUTE_UNUSED = rhoRightL*vRight3L;
    
    CCTK_REAL EnLeftL CCTK_ATTRIBUTE_UNUSED = 0.5*rhoLeftL*(pow(vLeft1L,2) 
      + pow(vLeft2L,2) + pow(vLeft3L,2)) + pLeftL*pow(-1 + gamma,-1);
    
    CCTK_REAL EnRightL CCTK_ATTRIBUTE_UNUSED = 
      0.5*rhoRightL*(pow(vRight1L,2) + pow(vRight2L,2) + pow(vRight3L,2)) + 
      pRightL*pow(-1 + gamma,-1);
    /* Copy local copies back to grid functions */
    DenLeft[index] = DenLeftL;
    DenRight[index] = DenRightL;
    EnLeft[index] = EnLeftL;
    EnRight[index] = EnRightL;
    SLeft1[index] = SLeft1L;
    SLeft2[index] = SLeft2L;
    SLeft3[index] = SLeft3L;
    SRight1[index] = SRight1L;
    SRight2[index] = SRight2L;
    SRight3[index] = SRight3L;
  }
  CCTK_ENDLOOP3(euler_conserved_flux_1);
}
extern "C" void euler_conserved_flux_1(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering euler_conserved_flux_1_Body");
  }
  if (cctk_iteration % euler_conserved_flux_1_calc_every != euler_conserved_flux_1_calc_offset)
  {
    return;
  }
  
  const char* const groups[] = {
    "Euler::DenLeft_group",
    "Euler::DenRight_group",
    "Euler::EnLeft_group",
    "Euler::EnRight_group",
    "Euler::pLeft_group",
    "Euler::pRight_group",
    "Euler::rhoLeft_group",
    "Euler::rhoRight_group",
    "Euler::SLeft_group",
    "Euler::SRight_group",
    "Euler::vLeft_group",
    "Euler::vRight_group"};
  AssertGroupStorage(cctkGH, "euler_conserved_flux_1", 12, groups);
  
  
  LoopOverEverything(cctkGH, euler_conserved_flux_1_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving euler_conserved_flux_1_Body");
  }
}

} // namespace Euler
