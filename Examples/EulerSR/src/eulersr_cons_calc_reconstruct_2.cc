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

extern "C" void eulersr_cons_calc_reconstruct_2_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (cctk_iteration % eulersr_cons_calc_reconstruct_2_calc_every != eulersr_cons_calc_reconstruct_2_calc_offset)
    return;
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::epsi_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::epsi_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::rho_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::rho_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::v1_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::v1_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::v2_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::v2_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::v3_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::v3_lr_group.");
  return;
}

static void eulersr_cons_calc_reconstruct_2_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
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
  CCTK_LOOP3(eulersr_cons_calc_reconstruct_2,
    i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,
    cctk_ash[0],cctk_ash[1],cctk_ash[2])
  {
    const ptrdiff_t index CCTK_ATTRIBUTE_UNUSED = di*i + dj*j + dk*k;
    /* Assign local copies of grid functions */
    
    CCTK_REAL epsiL CCTK_ATTRIBUTE_UNUSED = epsi[index];
    CCTK_REAL rhoL CCTK_ATTRIBUTE_UNUSED = rho[index];
    CCTK_REAL v1L CCTK_ATTRIBUTE_UNUSED = v1[index];
    CCTK_REAL v2L CCTK_ATTRIBUTE_UNUSED = v2[index];
    CCTK_REAL v3L CCTK_ATTRIBUTE_UNUSED = v3[index];
    
    /* Include user supplied include files */
    /* Precompute derivatives */
    const CCTK_REAL DiffPlus2epsi CCTK_ATTRIBUTE_UNUSED = DiffPlus2(&epsi[index]);
    const CCTK_REAL DiffMinus2epsi CCTK_ATTRIBUTE_UNUSED = DiffMinus2(&epsi[index]);
    const CCTK_REAL DiffPlus2rho CCTK_ATTRIBUTE_UNUSED = DiffPlus2(&rho[index]);
    const CCTK_REAL DiffMinus2rho CCTK_ATTRIBUTE_UNUSED = DiffMinus2(&rho[index]);
    const CCTK_REAL DiffPlus2v1 CCTK_ATTRIBUTE_UNUSED = DiffPlus2(&v1[index]);
    const CCTK_REAL DiffMinus2v1 CCTK_ATTRIBUTE_UNUSED = DiffMinus2(&v1[index]);
    const CCTK_REAL DiffPlus2v2 CCTK_ATTRIBUTE_UNUSED = DiffPlus2(&v2[index]);
    const CCTK_REAL DiffMinus2v2 CCTK_ATTRIBUTE_UNUSED = DiffMinus2(&v2[index]);
    const CCTK_REAL DiffPlus2v3 CCTK_ATTRIBUTE_UNUSED = DiffPlus2(&v3[index]);
    const CCTK_REAL DiffMinus2v3 CCTK_ATTRIBUTE_UNUSED = DiffMinus2(&v3[index]);
    /* Calculate temporaries and grid functions */
    CCTK_REAL slopeL CCTK_ATTRIBUTE_UNUSED = DiffMinus2rho;
    
    CCTK_REAL slopeR CCTK_ATTRIBUTE_UNUSED = DiffPlus2rho;
    
    CCTK_REAL slope CCTK_ATTRIBUTE_UNUSED = VanLeer(slopeL,slopeR);
    
    CCTK_REAL rhoLeftL CCTK_ATTRIBUTE_UNUSED = rhoL - 0.5*slope;
    
    CCTK_REAL rhoRightL CCTK_ATTRIBUTE_UNUSED = rhoL + 0.5*slope;
    
    slopeL = DiffMinus2v1;
    
    slopeR = DiffPlus2v1;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL v1LeftL CCTK_ATTRIBUTE_UNUSED = v1L - 0.5*slope;
    
    CCTK_REAL v1RightL CCTK_ATTRIBUTE_UNUSED = v1L + 0.5*slope;
    
    slopeL = DiffMinus2v2;
    
    slopeR = DiffPlus2v2;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL v2LeftL CCTK_ATTRIBUTE_UNUSED = v2L - 0.5*slope;
    
    CCTK_REAL v2RightL CCTK_ATTRIBUTE_UNUSED = v2L + 0.5*slope;
    
    slopeL = DiffMinus2v3;
    
    slopeR = DiffPlus2v3;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL v3LeftL CCTK_ATTRIBUTE_UNUSED = v3L - 0.5*slope;
    
    CCTK_REAL v3RightL CCTK_ATTRIBUTE_UNUSED = v3L + 0.5*slope;
    
    slopeL = DiffMinus2epsi;
    
    slopeR = DiffPlus2epsi;
    
    slope = VanLeer(slopeL,slopeR);
    
    CCTK_REAL epsiLeftL CCTK_ATTRIBUTE_UNUSED = epsiL - 0.5*slope;
    
    CCTK_REAL epsiRightL CCTK_ATTRIBUTE_UNUSED = epsiL + 0.5*slope;
    /* Copy local copies back to grid functions */
    epsiLeft[index] = epsiLeftL;
    epsiRight[index] = epsiRightL;
    rhoLeft[index] = rhoLeftL;
    rhoRight[index] = rhoRightL;
    v1Left[index] = v1LeftL;
    v1Right[index] = v1RightL;
    v2Left[index] = v2LeftL;
    v2Right[index] = v2RightL;
    v3Left[index] = v3LeftL;
    v3Right[index] = v3RightL;
  }
  CCTK_ENDLOOP3(eulersr_cons_calc_reconstruct_2);
}
extern "C" void eulersr_cons_calc_reconstruct_2(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_reconstruct_2_Body");
  }
  if (cctk_iteration % eulersr_cons_calc_reconstruct_2_calc_every != eulersr_cons_calc_reconstruct_2_calc_offset)
  {
    return;
  }
  
  const char* const groups[] = {
    "EulerSR::epsi_group",
    "EulerSR::epsi_lr_group",
    "EulerSR::rho_group",
    "EulerSR::rho_lr_group",
    "EulerSR::v1_lr_group",
    "EulerSR::v2_lr_group",
    "EulerSR::v3_lr_group",
    "EulerSR::v_group"};
  AssertGroupStorage(cctkGH, "eulersr_cons_calc_reconstruct_2", 8, groups);
  
  EnsureStencilFits(cctkGH, "eulersr_cons_calc_reconstruct_2", 1, 1, 1);
  
  LoopOverInterior(cctkGH, eulersr_cons_calc_reconstruct_2_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving eulersr_cons_calc_reconstruct_2_Body");
  }
}

} // namespace EulerSR
