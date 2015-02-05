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
#include "OpenCLRunTime.h"

namespace SimpleWaveOpenCL {

extern "C" void calc_rhs_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (cctk_iteration % calc_rhs_calc_every != calc_rhs_calc_offset)
    return;
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, "SimpleWaveOpenCL::evolved_grouprhs","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for SimpleWaveOpenCL::evolved_grouprhs.");
  return;
}

static void calc_rhs_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  const char* const source =
  "/* Include user-supplied include files */\n"
  "/* Initialise finite differencing variables */\n"
  "const ptrdiff_t di CCTK_ATTRIBUTE_UNUSED = 1;\n"
  "const ptrdiff_t dj CCTK_ATTRIBUTE_UNUSED = \n"
  "  CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0);\n"
  "const ptrdiff_t dk CCTK_ATTRIBUTE_UNUSED = \n"
  "  CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0);\n"
  "const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL) * di;\n"
  "const ptrdiff_t cdj CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL) * dj;\n"
  "const ptrdiff_t cdk CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL) * dk;\n"
  "const ptrdiff_t cctkLbnd1 CCTK_ATTRIBUTE_UNUSED = cctk_lbnd[0];\n"
  "const ptrdiff_t cctkLbnd2 CCTK_ATTRIBUTE_UNUSED = cctk_lbnd[1];\n"
  "const ptrdiff_t cctkLbnd3 CCTK_ATTRIBUTE_UNUSED = cctk_lbnd[2];\n"
  "const CCTK_REAL t CCTK_ATTRIBUTE_UNUSED = cctk_time;\n"
  "const CCTK_REAL cctkOriginSpace1 CCTK_ATTRIBUTE_UNUSED = \n"
  "  CCTK_ORIGIN_SPACE(0);\n"
  "const CCTK_REAL cctkOriginSpace2 CCTK_ATTRIBUTE_UNUSED = \n"
  "  CCTK_ORIGIN_SPACE(1);\n"
  "const CCTK_REAL cctkOriginSpace3 CCTK_ATTRIBUTE_UNUSED = \n"
  "  CCTK_ORIGIN_SPACE(2);\n"
  "const CCTK_REAL dt CCTK_ATTRIBUTE_UNUSED = CCTK_DELTA_TIME;\n"
  "const CCTK_REAL dx CCTK_ATTRIBUTE_UNUSED = CCTK_DELTA_SPACE(0);\n"
  "const CCTK_REAL dy CCTK_ATTRIBUTE_UNUSED = CCTK_DELTA_SPACE(1);\n"
  "const CCTK_REAL dz CCTK_ATTRIBUTE_UNUSED = CCTK_DELTA_SPACE(2);\n"
  "const CCTK_REAL dxi CCTK_ATTRIBUTE_UNUSED = pow(dx,-1);\n"
  "const CCTK_REAL dyi CCTK_ATTRIBUTE_UNUSED = pow(dy,-1);\n"
  "const CCTK_REAL dzi CCTK_ATTRIBUTE_UNUSED = pow(dz,-1);\n"
  "const CCTK_REAL khalf CCTK_ATTRIBUTE_UNUSED = 0.5;\n"
  "const CCTK_REAL kthird CCTK_ATTRIBUTE_UNUSED = \n"
  "  0.333333333333333333333333333333;\n"
  "const CCTK_REAL ktwothird CCTK_ATTRIBUTE_UNUSED = \n"
  "  0.666666666666666666666666666667;\n"
  "const CCTK_REAL kfourthird CCTK_ATTRIBUTE_UNUSED = \n"
  "  1.33333333333333333333333333333;\n"
  "const CCTK_REAL hdxi CCTK_ATTRIBUTE_UNUSED = 0.5*dxi;\n"
  "const CCTK_REAL hdyi CCTK_ATTRIBUTE_UNUSED = 0.5*dyi;\n"
  "const CCTK_REAL hdzi CCTK_ATTRIBUTE_UNUSED = 0.5*dzi;\n"
  "/* Initialize predefined quantities */\n"
  "const CCTK_REAL p1o2dx CCTK_ATTRIBUTE_UNUSED = 0.5*pow(dx,-1);\n"
  "const CCTK_REAL p1o2dy CCTK_ATTRIBUTE_UNUSED = 0.5*pow(dy,-1);\n"
  "const CCTK_REAL p1o2dz CCTK_ATTRIBUTE_UNUSED = 0.5*pow(dz,-1);\n"
  "const CCTK_REAL p1odx2 CCTK_ATTRIBUTE_UNUSED = pow(dx,-2);\n"
  "const CCTK_REAL p1ody2 CCTK_ATTRIBUTE_UNUSED = pow(dy,-2);\n"
  "const CCTK_REAL p1odz2 CCTK_ATTRIBUTE_UNUSED = pow(dz,-2);\n"
  "/* Assign local copies of arrays functions */\n"
  "\n"
  "\n"
  "/* Calculate temporaries and arrays functions */\n"
  "/* Copy local copies back to grid functions */\n"
  "/* Loop over the grid points */\n"
  "const int imin0=imin[0];\n"
  "const int imin1=imin[1];\n"
  "const int imin2=imin[2];\n"
  "const int imax0=imax[0];\n"
  "const int imax1=imax[1];\n"
  "const int imax2=imax[2];\n"
  "#pragma omp parallel\n"
  "CCTK_LOOP3(calc_rhs,\n"
  "  i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,\n"
  "  cctk_ash[0],cctk_ash[1],cctk_ash[2])\n"
  "{\n"
  "  const ptrdiff_t index CCTK_ATTRIBUTE_UNUSED = di*i + dj*j + dk*k;\n"
  "  /* Assign local copies of grid functions */\n"
  "  \n"
  "  CCTK_REAL phiL CCTK_ATTRIBUTE_UNUSED = phi[index];\n"
  "  CCTK_REAL piL CCTK_ATTRIBUTE_UNUSED = pi[index];\n"
  "  \n"
  "  /* Include user supplied include files */\n"
  "  /* Precompute derivatives */\n"
  "  const CCTK_REAL PDstandard2nd11phi CCTK_ATTRIBUTE_UNUSED = PDstandard2nd11(&phi[index]);\n"
  "  const CCTK_REAL PDstandard2nd22phi CCTK_ATTRIBUTE_UNUSED = PDstandard2nd22(&phi[index]);\n"
  "  const CCTK_REAL PDstandard2nd33phi CCTK_ATTRIBUTE_UNUSED = PDstandard2nd33(&phi[index]);\n"
  "  /* Calculate temporaries and grid functions */\n"
  "  CCTK_REAL phirhsL CCTK_ATTRIBUTE_UNUSED = piL;\n"
  "  \n"
  "  CCTK_REAL pirhsL CCTK_ATTRIBUTE_UNUSED = PDstandard2nd11phi + \n"
  "    PDstandard2nd22phi + PDstandard2nd33phi;\n"
  "  /* Copy local copies back to grid functions */\n"
  "  vec_store_partial_prepare(i,lc_imin,lc_imax);\n"
  "  vec_store_nta_partial(phirhs[index],phirhsL);\n"
  "  vec_store_nta_partial(pirhs[index],pirhsL);\n"
  "}\n"
  "CCTK_ENDLOOP3(calc_rhs);\n"
  ""
  ;
  
  const char* const groups[] = {
    "SimpleWaveOpenCL::evolved_group",
    "SimpleWaveOpenCL::evolved_grouprhs",
    NULL};
  
  static struct OpenCLKernel *kernel = NULL;
  const char* const sources[] = {differencing, source, NULL};
  OpenCLRunTime_CallKernel(cctkGH, CCTK_THORNSTRING, "calc_rhs",
                           sources, groups, NULL, NULL, NULL, -1,
                           imin, imax, &kernel);
  
}
extern "C" void calc_rhs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering calc_rhs_Body");
  }
  if (cctk_iteration % calc_rhs_calc_every != calc_rhs_calc_offset)
  {
    return;
  }
  
  const char* const groups[] = {
    "SimpleWaveOpenCL::evolved_group",
    "SimpleWaveOpenCL::evolved_grouprhs"};
  AssertGroupStorage(cctkGH, "calc_rhs", 2, groups);
  
  EnsureStencilFits(cctkGH, "calc_rhs", 1, 1, 1);
  
  LoopOverInterior(cctkGH, calc_rhs_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving calc_rhs_Body");
  }
}

} // namespace SimpleWaveOpenCL
