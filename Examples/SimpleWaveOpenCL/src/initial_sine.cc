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
#include "OpenCLRunTime.h"

/* Define macros used in calculations */
#define INITVALUE (42)
#define INV(x) ((CCTK_REAL)1.0 / (x))
#define SQR(x) ((x) * (x))
#define CUB(x) ((x) * SQR(x))
#define QAD(x) (SQR(SQR(x)))

static void initial_sine_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  char const *const source =
  "\n"
  "/* Include user-supplied include files */\n"
  "\n"
  "/* Initialise finite differencing variables */\n"
  "ptrdiff_t /*const*/ di CCTK_ATTRIBUTE_UNUSED  = 1;\n"
  "ptrdiff_t /*const*/ dj CCTK_ATTRIBUTE_UNUSED  = CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0);\n"
  "ptrdiff_t /*const*/ dk CCTK_ATTRIBUTE_UNUSED  = CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0);\n"
  "ptrdiff_t /*const*/ cdi CCTK_ATTRIBUTE_UNUSED  = sizeof(CCTK_REAL) * di;\n"
  "ptrdiff_t /*const*/ cdj CCTK_ATTRIBUTE_UNUSED  = sizeof(CCTK_REAL) * dj;\n"
  "ptrdiff_t /*const*/ cdk CCTK_ATTRIBUTE_UNUSED  = sizeof(CCTK_REAL) * dk;\n"
  "CCTK_REAL /*const*/ dx CCTK_ATTRIBUTE_UNUSED  = ToReal(CCTK_DELTA_SPACE(0));\n"
  "CCTK_REAL /*const*/ dy CCTK_ATTRIBUTE_UNUSED  = ToReal(CCTK_DELTA_SPACE(1));\n"
  "CCTK_REAL /*const*/ dz CCTK_ATTRIBUTE_UNUSED  = ToReal(CCTK_DELTA_SPACE(2));\n"
  "CCTK_REAL /*const*/ dt CCTK_ATTRIBUTE_UNUSED  = ToReal(CCTK_DELTA_TIME);\n"
  "CCTK_REAL /*const*/ t CCTK_ATTRIBUTE_UNUSED  = ToReal(cctk_time);\n"
  "CCTK_REAL /*const*/ dxi CCTK_ATTRIBUTE_UNUSED  = INV(dx);\n"
  "CCTK_REAL /*const*/ dyi CCTK_ATTRIBUTE_UNUSED  = INV(dy);\n"
  "CCTK_REAL /*const*/ dzi CCTK_ATTRIBUTE_UNUSED  = INV(dz);\n"
  "CCTK_REAL /*const*/ khalf CCTK_ATTRIBUTE_UNUSED  = 0.5;\n"
  "CCTK_REAL /*const*/ kthird CCTK_ATTRIBUTE_UNUSED  = 1/3.0;\n"
  "CCTK_REAL /*const*/ ktwothird CCTK_ATTRIBUTE_UNUSED  = 2.0/3.0;\n"
  "CCTK_REAL /*const*/ kfourthird CCTK_ATTRIBUTE_UNUSED  = 4.0/3.0;\n"
  "CCTK_REAL /*const*/ keightthird CCTK_ATTRIBUTE_UNUSED  = 8.0/3.0;\n"
  "CCTK_REAL /*const*/ hdxi CCTK_ATTRIBUTE_UNUSED  = 0.5 * dxi;\n"
  "CCTK_REAL /*const*/ hdyi CCTK_ATTRIBUTE_UNUSED  = 0.5 * dyi;\n"
  "CCTK_REAL /*const*/ hdzi CCTK_ATTRIBUTE_UNUSED  = 0.5 * dzi;\n"
  "\n"
  "/* Initialize predefined quantities */\n"
  "CCTK_REAL /*const*/ p1o2dx CCTK_ATTRIBUTE_UNUSED  = 0.5*INV(dx);\n"
  "CCTK_REAL /*const*/ p1o2dy CCTK_ATTRIBUTE_UNUSED  = 0.5*INV(dy);\n"
  "CCTK_REAL /*const*/ p1o2dz CCTK_ATTRIBUTE_UNUSED  = 0.5*INV(dz);\n"
  "CCTK_REAL /*const*/ p1odx2 CCTK_ATTRIBUTE_UNUSED  = INV(SQR(dx));\n"
  "CCTK_REAL /*const*/ p1ody2 CCTK_ATTRIBUTE_UNUSED  = INV(SQR(dy));\n"
  "CCTK_REAL /*const*/ p1odz2 CCTK_ATTRIBUTE_UNUSED  = INV(SQR(dz));\n"
  "\n"
  "/* Assign local copies of arrays functions */\n"
  "\n"
  "\n"
  "\n"
  "/* Calculate temporaries and arrays functions */\n"
  "\n"
  "/* Copy local copies back to grid functions */\n"
  "\n"
  "/* Loop over the grid points */\n"
  "#pragma omp parallel\n"
  "CCTK_LOOP3(initial_sine,\n"
  "  i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],\n"
  "  cctk_ash[0],cctk_ash[1],cctk_ash[2])\n"
  "{\n"
  "  ptrdiff_t /*const*/ index CCTK_ATTRIBUTE_UNUSED  = di*i + dj*j + dk*k;\n"
  "  \n"
  "  /* Assign local copies of grid functions */\n"
  "  \n"
  "  CCTK_REAL xL CCTK_ATTRIBUTE_UNUSED = x[index];\n"
  "  \n"
  "  \n"
  "  /* Include user supplied include files */\n"
  "  \n"
  "  /* Precompute derivatives */\n"
  "  \n"
  "  /* Calculate temporaries and grid functions */\n"
  "  CCTK_REAL CCTK_ATTRIBUTE_UNUSED phiL = sin(6.283185307179586*(xL - \n"
  "    1.*t));\n"
  "  \n"
  "  CCTK_REAL CCTK_ATTRIBUTE_UNUSED piL = \n"
  "    -6.283185307179586*cos(6.283185307179586*(xL - 1.*t));\n"
  "  \n"
  "  /* Copy local copies back to grid functions */\n"
  "  vec_store_partial_prepare(i,lc_imin,lc_imax);\n"
  "  vec_store_nta_partial(phi[index],phiL);\n"
  "  vec_store_nta_partial(pi[index],piL);\n"
  "}\n"
  "CCTK_ENDLOOP3(initial_sine);\n"
  ""
  ;
  
  char const *const groups[] = {
    "SimpleWaveOpenCL::evolved_group",
    "grid::coordinates",
    NULL};
  
  static struct OpenCLKernel *kernel = NULL;
  char const *const sources[] = {differencing, source, NULL};
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
  
  const char *const groups[] = {
    "SimpleWaveOpenCL::evolved_group",
    "grid::coordinates"};
  GenericFD_AssertGroupStorage(cctkGH, "initial_sine", 2, groups);
  
  
  GenericFD_LoopOverEverything(cctkGH, initial_sine_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving initial_sine_Body");
  }
}
