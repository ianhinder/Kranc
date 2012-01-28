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

extern "C" void EM_evol_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EMScript::B_grouprhs","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EMScript::B_grouprhs.");
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "EMScript::El_grouprhs","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for EMScript::El_grouprhs.");
  return;
}

static void EM_evol_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
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
  CCTK_REAL const p1odx2 = INV(SQR(dx));
  CCTK_REAL const p1ody2 = INV(SQR(dy));
  CCTK_REAL const p1odz2 = INV(SQR(dz));
  CCTK_REAL const pm1o12dx2 = -0.0833333333333333333333333333333*INV(SQR(dx));
  CCTK_REAL const pm1o12dy2 = -0.0833333333333333333333333333333*INV(SQR(dy));
  CCTK_REAL const pm1o12dz2 = -0.0833333333333333333333333333333*INV(SQR(dz));
  
  /* Assign local copies of arrays functions */
  
  
  
  /* Calculate temporaries and arrays functions */
  
  /* Copy local copies back to grid functions */
  
  /* Loop over the grid points */
  #pragma omp parallel
  CCTK_LOOP3 (EM_evol,
    i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],
    cctk_lsh[0],cctk_lsh[1],cctk_lsh[2])
  {
    ptrdiff_t const index = di*i + dj*j + dk*k;
    
    /* Assign local copies of grid functions */
    
    CCTK_REAL B1L = B1[index];
    CCTK_REAL B2L = B2[index];
    CCTK_REAL B3L = B3[index];
    CCTK_REAL El1L = El1[index];
    CCTK_REAL El2L = El2[index];
    CCTK_REAL El3L = El3[index];
    
    
    /* Include user supplied include files */
    
    /* Precompute derivatives */
    CCTK_REAL PDstandard2B1;
    CCTK_REAL PDstandard3B1;
    CCTK_REAL PDstandard1B2;
    CCTK_REAL PDstandard3B2;
    CCTK_REAL PDstandard1B3;
    CCTK_REAL PDstandard2B3;
    CCTK_REAL PDstandard2El1;
    CCTK_REAL PDstandard3El1;
    CCTK_REAL PDstandard1El2;
    CCTK_REAL PDstandard3El2;
    CCTK_REAL PDstandard1El3;
    CCTK_REAL PDstandard2El3;
    
    switch(fdOrder)
    {
      case 2:
        PDstandard2B1 = PDstandardfdOrder22(&B1[index]);
        PDstandard3B1 = PDstandardfdOrder23(&B1[index]);
        PDstandard1B2 = PDstandardfdOrder21(&B2[index]);
        PDstandard3B2 = PDstandardfdOrder23(&B2[index]);
        PDstandard1B3 = PDstandardfdOrder21(&B3[index]);
        PDstandard2B3 = PDstandardfdOrder22(&B3[index]);
        PDstandard2El1 = PDstandardfdOrder22(&El1[index]);
        PDstandard3El1 = PDstandardfdOrder23(&El1[index]);
        PDstandard1El2 = PDstandardfdOrder21(&El2[index]);
        PDstandard3El2 = PDstandardfdOrder23(&El2[index]);
        PDstandard1El3 = PDstandardfdOrder21(&El3[index]);
        PDstandard2El3 = PDstandardfdOrder22(&El3[index]);
        break;
      
      case 4:
        PDstandard2B1 = PDstandardfdOrder42(&B1[index]);
        PDstandard3B1 = PDstandardfdOrder43(&B1[index]);
        PDstandard1B2 = PDstandardfdOrder41(&B2[index]);
        PDstandard3B2 = PDstandardfdOrder43(&B2[index]);
        PDstandard1B3 = PDstandardfdOrder41(&B3[index]);
        PDstandard2B3 = PDstandardfdOrder42(&B3[index]);
        PDstandard2El1 = PDstandardfdOrder42(&El1[index]);
        PDstandard3El1 = PDstandardfdOrder43(&El1[index]);
        PDstandard1El2 = PDstandardfdOrder41(&El2[index]);
        PDstandard3El2 = PDstandardfdOrder43(&El2[index]);
        PDstandard1El3 = PDstandardfdOrder41(&El3[index]);
        PDstandard2El3 = PDstandardfdOrder42(&El3[index]);
        break;
    }
    
    /* Calculate temporaries and grid functions */
    CCTK_REAL El1rhsL = PDstandard2B3 - PDstandard3B2;
    
    CCTK_REAL El2rhsL = -PDstandard1B3 + PDstandard3B1;
    
    CCTK_REAL El3rhsL = PDstandard1B2 - PDstandard2B1;
    
    CCTK_REAL B1rhsL = -PDstandard2El3 + PDstandard3El2;
    
    CCTK_REAL B2rhsL = PDstandard1El3 - PDstandard3El1;
    
    CCTK_REAL B3rhsL = -PDstandard1El2 + PDstandard2El1;
    
    /* Copy local copies back to grid functions */
    B1rhs[index] = B1rhsL;
    B2rhs[index] = B2rhsL;
    B3rhs[index] = B3rhsL;
    El1rhs[index] = El1rhsL;
    El2rhs[index] = El2rhsL;
    El3rhs[index] = El3rhsL;
  }
  CCTK_ENDLOOP3 (EM_evol);
}

extern "C" void EM_evol(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering EM_evol_Body");
  }
  
  if (cctk_iteration % EM_evol_calc_every != EM_evol_calc_offset)
  {
    return;
  }
  
  const char *groups[] = {"EMScript::B_group","EMScript::B_grouprhs","EMScript::El_group","EMScript::El_grouprhs"};
  GenericFD_AssertGroupStorage(cctkGH, "EM_evol", 4, groups);
  
  switch(fdOrder)
  {
    case 2:
      GenericFD_EnsureStencilFits(cctkGH, "EM_evol", 1, 1, 1);
      break;
    
    case 4:
      GenericFD_EnsureStencilFits(cctkGH, "EM_evol", 2, 2, 2);
      break;
  }
  
  GenericFD_LoopOverInterior(cctkGH, &EM_evol_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving EM_evol_Body");
  }
}
