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


static void eulersr_cons_calc_intercell_conserved_2_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
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
  CCTK_LOOP3(eulersr_cons_calc_intercell_conserved_2,
    i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    const ptrdiff_t index CCTK_ATTRIBUTE_UNUSED = di*i + dj*j + dk*k;
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenLeftL CCTK_ATTRIBUTE_UNUSED = DenLeft[index];
    CCTK_REAL DenRightL CCTK_ATTRIBUTE_UNUSED = DenRight[index];
    CCTK_REAL epsiLeftL CCTK_ATTRIBUTE_UNUSED = epsiLeft[index];
    CCTK_REAL epsiRightL CCTK_ATTRIBUTE_UNUSED = epsiRight[index];
    CCTK_REAL hL CCTK_ATTRIBUTE_UNUSED = h[index];
    CCTK_REAL pL CCTK_ATTRIBUTE_UNUSED = p[index];
    CCTK_REAL rhoLeftL CCTK_ATTRIBUTE_UNUSED = rhoLeft[index];
    CCTK_REAL rhoRightL CCTK_ATTRIBUTE_UNUSED = rhoRight[index];
    CCTK_REAL v1LeftL CCTK_ATTRIBUTE_UNUSED = v1Left[index];
    CCTK_REAL v1RightL CCTK_ATTRIBUTE_UNUSED = v1Right[index];
    CCTK_REAL v2LeftL CCTK_ATTRIBUTE_UNUSED = v2Left[index];
    CCTK_REAL v2RightL CCTK_ATTRIBUTE_UNUSED = v2Right[index];
    CCTK_REAL v3LeftL CCTK_ATTRIBUTE_UNUSED = v3Left[index];
    CCTK_REAL v3RightL CCTK_ATTRIBUTE_UNUSED = v3Right[index];
    CCTK_REAL WL CCTK_ATTRIBUTE_UNUSED = W[index];
    
    /* Include user supplied include files */
    /* Precompute derivatives */
    /* Calculate temporaries and grid functions */
    CCTK_REAL Wx CCTK_ATTRIBUTE_UNUSED = 1 - pow(v1LeftL,2) - 
      pow(v2LeftL,2) - pow(v3LeftL,2);
    
    WL = pow(Wx,-0.5);
    
    pL = epsiLeftL*rhoLeftL*(-1 + gamma);
    
    hL = 1 + epsiLeftL + pL*pow(rhoLeftL,-1);
    
    DenLeftL = rhoLeftL*WL;
    
    CCTK_REAL S1LeftL CCTK_ATTRIBUTE_UNUSED = 
      hL*rhoLeftL*v1LeftL*pow(WL,2);
    
    CCTK_REAL S2LeftL CCTK_ATTRIBUTE_UNUSED = 
      hL*rhoLeftL*v2LeftL*pow(WL,2);
    
    CCTK_REAL S3LeftL CCTK_ATTRIBUTE_UNUSED = 
      hL*rhoLeftL*v3LeftL*pow(WL,2);
    
    CCTK_REAL tauLeftL CCTK_ATTRIBUTE_UNUSED = -DenLeftL - pL + 
      hL*rhoLeftL*pow(WL,2);
    
    Wx = 1 - pow(v1RightL,2) - pow(v2RightL,2) - pow(v3RightL,2);
    
    WL = pow(Wx,-0.5);
    
    pL = epsiRightL*rhoRightL*(-1 + gamma);
    
    hL = 1 + epsiRightL + pL*pow(rhoRightL,-1);
    
    DenRightL = rhoRightL*WL;
    
    CCTK_REAL S1RightL CCTK_ATTRIBUTE_UNUSED = 
      hL*rhoRightL*v1RightL*pow(WL,2);
    
    CCTK_REAL S2RightL CCTK_ATTRIBUTE_UNUSED = 
      hL*rhoRightL*v2RightL*pow(WL,2);
    
    CCTK_REAL S3RightL CCTK_ATTRIBUTE_UNUSED = 
      hL*rhoRightL*v3RightL*pow(WL,2);
    
    CCTK_REAL tauRightL CCTK_ATTRIBUTE_UNUSED = -DenRightL - pL + 
      hL*rhoRightL*pow(WL,2);
    /* Copy local copies back to grid functions */
    DenLeft[index] = DenLeftL;
    DenRight[index] = DenRightL;
    h[index] = hL;
    p[index] = pL;
    S1Left[index] = S1LeftL;
    S1Right[index] = S1RightL;
    S2Left[index] = S2LeftL;
    S2Right[index] = S2RightL;
    S3Left[index] = S3LeftL;
    S3Right[index] = S3RightL;
    tauLeft[index] = tauLeftL;
    tauRight[index] = tauRightL;
    W[index] = WL;
  }
  CCTK_ENDLOOP3(eulersr_cons_calc_intercell_conserved_2);
}
extern "C" void eulersr_cons_calc_intercell_conserved_2(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_intercell_conserved_2_Body");
  }
  if (cctk_iteration % eulersr_cons_calc_intercell_conserved_2_calc_every != eulersr_cons_calc_intercell_conserved_2_calc_offset)
  {
    return;
  }
  
  const char* const groups[] = {
    "EulerSR::Den_lr_group",
    "EulerSR::epsi_lr_group",
    "EulerSR::h_group",
    "EulerSR::p_group",
    "EulerSR::rho_lr_group",
    "EulerSR::S1_lr_group",
    "EulerSR::S2_lr_group",
    "EulerSR::S3_lr_group",
    "EulerSR::tau_lr_group",
    "EulerSR::v1_lr_group",
    "EulerSR::v2_lr_group",
    "EulerSR::v3_lr_group",
    "EulerSR::W_group"};
  AssertGroupStorage(cctkGH, "eulersr_cons_calc_intercell_conserved_2", 13, groups);
  
  
  LoopOverEverything(cctkGH, eulersr_cons_calc_intercell_conserved_2_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_intercell_conserved_2_Body");
  }
}

} // namespace EulerSR
