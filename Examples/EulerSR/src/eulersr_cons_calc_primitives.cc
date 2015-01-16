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


static void eulersr_cons_calc_primitives_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
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
  CCTK_LOOP3(eulersr_cons_calc_primitives,
    i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    const ptrdiff_t index CCTK_ATTRIBUTE_UNUSED = di*i + dj*j + dk*k;
    /* Assign local copies of grid functions */
    
    CCTK_REAL DenL CCTK_ATTRIBUTE_UNUSED = Den[index];
    CCTK_REAL epsiL CCTK_ATTRIBUTE_UNUSED = epsi[index];
    CCTK_REAL hL CCTK_ATTRIBUTE_UNUSED = h[index];
    CCTK_REAL pL CCTK_ATTRIBUTE_UNUSED = p[index];
    CCTK_REAL rhoL CCTK_ATTRIBUTE_UNUSED = rho[index];
    CCTK_REAL S1L CCTK_ATTRIBUTE_UNUSED = S1[index];
    CCTK_REAL S2L CCTK_ATTRIBUTE_UNUSED = S2[index];
    CCTK_REAL S3L CCTK_ATTRIBUTE_UNUSED = S3[index];
    CCTK_REAL tauL CCTK_ATTRIBUTE_UNUSED = tau[index];
    CCTK_REAL WL CCTK_ATTRIBUTE_UNUSED = W[index];
    
    /* Include user supplied include files */
    /* Precompute derivatives */
    /* Calculate temporaries and grid functions */
    CCTK_REAL pBar CCTK_ATTRIBUTE_UNUSED = pL;
    
    CCTK_REAL f CCTK_ATTRIBUTE_UNUSED = 10;
    
    CCTK_REAL Z CCTK_ATTRIBUTE_UNUSED = DenL + tauL + pBar;
    
    CCTK_REAL Ssq CCTK_ATTRIBUTE_UNUSED = pow(S1L,2) + pow(S2L,2) + 
      pow(S3L,2);
    
    CCTK_REAL vsq CCTK_ATTRIBUTE_UNUSED = Ssq*pow(Z,-2);
    
    WL = pow(1 - vsq,-0.5);
    
    rhoL = DenL*pow(WL,-1);
    
    hL = Z*pow(rhoL,-1)*pow(WL,-2);
    
    epsiL = hL - (rhoL + pBar)*pow(rhoL,-1);
    
    CCTK_REAL pEOS CCTK_ATTRIBUTE_UNUSED = epsiL*rhoL*(-1 + gamma);
    
    f = -pBar + pEOS;
    
    CCTK_REAL cs CCTK_ATTRIBUTE_UNUSED = pow(epsiL*(-1 + 
      gamma)*gamma*pow(hL,-1),0.5);
    
    CCTK_REAL df CCTK_ATTRIBUTE_UNUSED = -1 + vsq*pow(cs,2);
    
    pBar = pBar - f*pow(df,-1);
    
    Z = DenL + tauL + pBar;
    
    Ssq = pow(S1L,2) + pow(S2L,2) + pow(S3L,2);
    
    vsq = Ssq*pow(Z,-2);
    
    WL = pow(1 - vsq,-0.5);
    
    rhoL = DenL*pow(WL,-1);
    
    hL = Z*pow(rhoL,-1)*pow(WL,-2);
    
    epsiL = hL - (rhoL + pBar)*pow(rhoL,-1);
    
    pEOS = epsiL*rhoL*(-1 + gamma);
    
    f = -pBar + pEOS;
    
    cs = pow(epsiL*(-1 + gamma)*gamma*pow(hL,-1),0.5);
    
    df = -1 + vsq*pow(cs,2);
    
    pBar = pBar - f*pow(df,-1);
    
    Z = DenL + tauL + pBar;
    
    Ssq = pow(S1L,2) + pow(S2L,2) + pow(S3L,2);
    
    vsq = Ssq*pow(Z,-2);
    
    WL = pow(1 - vsq,-0.5);
    
    rhoL = DenL*pow(WL,-1);
    
    hL = Z*pow(rhoL,-1)*pow(WL,-2);
    
    epsiL = hL - (rhoL + pBar)*pow(rhoL,-1);
    
    pEOS = epsiL*rhoL*(-1 + gamma);
    
    f = -pBar + pEOS;
    
    cs = pow(epsiL*(-1 + gamma)*gamma*pow(hL,-1),0.5);
    
    df = -1 + vsq*pow(cs,2);
    
    pBar = pBar - f*pow(df,-1);
    
    Z = DenL + tauL + pBar;
    
    Ssq = pow(S1L,2) + pow(S2L,2) + pow(S3L,2);
    
    vsq = Ssq*pow(Z,-2);
    
    WL = pow(1 - vsq,-0.5);
    
    rhoL = DenL*pow(WL,-1);
    
    hL = Z*pow(rhoL,-1)*pow(WL,-2);
    
    epsiL = hL - (rhoL + pBar)*pow(rhoL,-1);
    
    pEOS = epsiL*rhoL*(-1 + gamma);
    
    f = -pBar + pEOS;
    
    cs = pow(epsiL*(-1 + gamma)*gamma*pow(hL,-1),0.5);
    
    df = -1 + vsq*pow(cs,2);
    
    pBar = pBar - f*pow(df,-1);
    
    Z = DenL + tauL + pBar;
    
    Ssq = pow(S1L,2) + pow(S2L,2) + pow(S3L,2);
    
    vsq = Ssq*pow(Z,-2);
    
    WL = pow(1 - vsq,-0.5);
    
    rhoL = DenL*pow(WL,-1);
    
    hL = Z*pow(rhoL,-1)*pow(WL,-2);
    
    epsiL = hL - (rhoL + pBar)*pow(rhoL,-1);
    
    pEOS = epsiL*rhoL*(-1 + gamma);
    
    f = -pBar + pEOS;
    
    cs = pow(epsiL*(-1 + gamma)*gamma*pow(hL,-1),0.5);
    
    df = -1 + vsq*pow(cs,2);
    
    pBar = pBar - f*pow(df,-1);
    
    pL = pBar;
    
    CCTK_REAL v1L CCTK_ATTRIBUTE_UNUSED = 
      S1L*pow(hL,-1)*pow(rhoL,-1)*pow(WL,-2);
    
    CCTK_REAL v2L CCTK_ATTRIBUTE_UNUSED = 
      S2L*pow(hL,-1)*pow(rhoL,-1)*pow(WL,-2);
    
    CCTK_REAL v3L CCTK_ATTRIBUTE_UNUSED = 
      S3L*pow(hL,-1)*pow(rhoL,-1)*pow(WL,-2);
    /* Copy local copies back to grid functions */
    epsi[index] = epsiL;
    h[index] = hL;
    p[index] = pL;
    rho[index] = rhoL;
    v1[index] = v1L;
    v2[index] = v2L;
    v3[index] = v3L;
    W[index] = WL;
  }
  CCTK_ENDLOOP3(eulersr_cons_calc_primitives);
}
extern "C" void eulersr_cons_calc_primitives(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_primitives_Body");
  }
  if (cctk_iteration % eulersr_cons_calc_primitives_calc_every != eulersr_cons_calc_primitives_calc_offset)
  {
    return;
  }
  
  const char* const groups[] = {
    "EulerSR::Den_group",
    "EulerSR::epsi_group",
    "EulerSR::h_group",
    "EulerSR::p_group",
    "EulerSR::rho_group",
    "EulerSR::S_group",
    "EulerSR::tau_group",
    "EulerSR::v_group",
    "EulerSR::W_group"};
  AssertGroupStorage(cctkGH, "eulersr_cons_calc_primitives", 9, groups);
  
  
  LoopOverEverything(cctkGH, eulersr_cons_calc_primitives_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_primitives_Body");
  }
}

} // namespace EulerSR
