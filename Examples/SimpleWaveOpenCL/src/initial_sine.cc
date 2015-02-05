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


static void initial_sine_Body(const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[])
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
  "CCTK_LOOP3(initial_sine,\n"
  "  i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,\n"
  "  cctk_ash[0],cctk_ash[1],cctk_ash[2])\n"
  "{\n"
  "  const ptrdiff_t index CCTK_ATTRIBUTE_UNUSED = di*i + dj*j + dk*k;\n"
  "  /* Assign local copies of grid functions */\n"
  "  \n"
  "  CCTK_REAL xL CCTK_ATTRIBUTE_UNUSED = x[index];\n"
  "  \n"
  "  /* Include user supplied include files */\n"
  "  /* Precompute derivatives */\n"
  "  /* Calculate temporaries and grid functions */\n"
  "  CCTK_REAL phiL CCTK_ATTRIBUTE_UNUSED = sin(2*Pi*(xL - t));\n"
  "  \n"
  "  CCTK_REAL piL CCTK_ATTRIBUTE_UNUSED = -2*Pi*cos(2*Pi*(xL - t));\n"
  "  /* Copy local copies back to grid functions */\n"
  "  vec_store_partial_prepare(i,lc_imin,lc_imax);\n"
  "  vec_store_nta_partial(phi[index],phiL);\n"
  "  vec_store_nta_partial(pi[index],piL);\n"
  "}\n"
  "CCTK_ENDLOOP3(initial_sine);\n"
  ""
  ;
  
  const char* const groups[] = {
    "SimpleWaveOpenCL::evolved_group",
    "grid::coordinates",
    NULL};
  
  static struct OpenCLKernel *kernel = NULL;
  const char* const sources[] = {differencing, source, NULL};
  OpenCLRunTime_CallKernel(cctkGH, CCTK_THORNSTRING, "initial_sine",
                           sources, groups, NULL, NULL, NULL, -1,
                           imin, imax, &kernel);
  
}
extern "C" void initial_sine(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering initial_sine_Body");
  }
  if (cctk_iteration % initial_sine_calc_every != initial_sine_calc_offset)
  {
    return;
  }
  
  const char* const groups[] = {
    "SimpleWaveOpenCL::evolved_group",
    "grid::coordinates"};
  AssertGroupStorage(cctkGH, "initial_sine", 2, groups);
  
  
  LoopOverEverything(cctkGH, initial_sine_Body);
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving initial_sine_Body");
  }
}

} // namespace SimpleWaveOpenCL
