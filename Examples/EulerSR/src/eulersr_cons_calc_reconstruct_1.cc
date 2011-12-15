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

/* Define macros used in calculations */
#define INITVALUE (42)
#define QAD(x) (SQR(SQR(x)))
#define INV(x) ((1.0) / (x))
#define SQR(x) ((x) * (x))
#define CUB(x) ((x) * (x) * (x))

extern "C" void eulersr_cons_calc_reconstruct_1_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::epsi_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::epsi_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::rho_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::rho_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::v1_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::v1_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::v2_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::v2_lr_group.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EulerSR::v3_lr_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EulerSR::v3_lr_group.");
  return;
}

static void eulersr_cons_calc_reconstruct_1_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const min[3], int const max[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* Declare the variables used for looping over grid points */
  CCTK_INT i, j, k;
  // CCTK_INT index = INITVALUE;
  
  /* Declare finite differencing variables */
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering eulersr_cons_calc_reconstruct_1_Body");
  }
  
  if (cctk_iteration % eulersr_cons_calc_reconstruct_1_calc_every != eulersr_cons_calc_reconstruct_1_calc_offset)
  {
    return;
  }
  
  const char *groups[] = {"EulerSR::epsi_group","EulerSR::epsi_lr_group","EulerSR::rho_group","EulerSR::rho_lr_group","EulerSR::v1_lr_group","EulerSR::v2_lr_group","EulerSR::v3_lr_group","EulerSR::v_group"};
  GenericFD_AssertGroupStorage(cctkGH, "eulersr_cons_calc_reconstruct_1", 8, groups);
  
  GenericFD_EnsureStencilFits(cctkGH, "eulersr_cons_calc_reconstruct_1", 1, 1, 1);
  
  /* Include user-supplied include files */
  
  /* Initialise finite differencing variables */
  ptrdiff_t const di = 1;
  ptrdiff_t const dj = CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  ptrdiff_t const dk = CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0);
  CCTK_REAL const dx = ToReal(CCTK_DELTA_SPACE(0));
  CCTK_REAL const dy = ToReal(CCTK_DELTA_SPACE(1));
  CCTK_REAL const dz = ToReal(CCTK_DELTA_SPACE(2));
  CCTK_REAL const dt = ToReal(CCTK_DELTA_TIME);
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
  
  /* Loop over the grid points */
  for (k = min[2]; k < max[2]; k++)
  {
    for (j = min[1]; j < max[1]; j++)
    {
      for (i = min[0]; i < max[0]; i++)
      {
         int  const  index  =  CCTK_GFINDEX3D(cctkGH,i,j,k) ;
        
        /* Assign local copies of grid functions */
        
        CCTK_REAL epsiL = epsi[index];
        CCTK_REAL rhoL = rho[index];
        CCTK_REAL v1L = v1[index];
        CCTK_REAL v2L = v2[index];
        CCTK_REAL v3L = v3[index];
        
        
        /* Include user supplied include files */
        
        /* Precompute derivatives */
        CCTK_REAL const DiffPlus1epsi = DiffPlus1(&epsi[index]);
        CCTK_REAL const DiffMinus1epsi = DiffMinus1(&epsi[index]);
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
        
        CCTK_REAL v1LeftL = -0.5*slope + v1L;
        
        CCTK_REAL v1RightL = 0.5*slope + v1L;
        
        slopeL = DiffMinus1v2;
        
        slopeR = DiffPlus1v2;
        
        slope = VanLeer(slopeL,slopeR);
        
        CCTK_REAL v2LeftL = -0.5*slope + v2L;
        
        CCTK_REAL v2RightL = 0.5*slope + v2L;
        
        slopeL = DiffMinus1v3;
        
        slopeR = DiffPlus1v3;
        
        slope = VanLeer(slopeL,slopeR);
        
        CCTK_REAL v3LeftL = -0.5*slope + v3L;
        
        CCTK_REAL v3RightL = 0.5*slope + v3L;
        
        slopeL = DiffMinus1epsi;
        
        slopeR = DiffPlus1epsi;
        
        slope = VanLeer(slopeL,slopeR);
        
        CCTK_REAL epsiLeftL = epsiL - 0.5*slope;
        
        CCTK_REAL epsiRightL = epsiL + 0.5*slope;
        
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
    }
  }
}

extern "C" void eulersr_cons_calc_reconstruct_1(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  GenericFD_LoopOverInterior(cctkGH, &eulersr_cons_calc_reconstruct_1_Body);
}
