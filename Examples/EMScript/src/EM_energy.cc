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
#include "vectors.h"

/* Define macros used in calculations */
#define INITVALUE (42)
#define QAD(x) (SQR(SQR(x)))
#define INV(x) (kdiv(ToReal(1.0),x))
#define SQR(x) (kmul(x,x))
#define CUB(x) (kmul(x,SQR(x)))

extern "C" void EM_energy_SelectBCs(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GenericFD_GetBoundaryWidth(cctkGH), -1 /* no table */, "My_New_Implementation::rho_group","flat");
  if (ierr < 0)
    CCTK_WARN(1, "Failed to register flat BC for My_New_Implementation::rho_group.");
  return;
}

static void EM_energy_Body(cGH const * restrict const cctkGH, int const dir, int const face, CCTK_REAL const normal[3], CCTK_REAL const tangentA[3], CCTK_REAL const tangentB[3], int const imin[3], int const imax[3], int const n_subblock_gfs, CCTK_REAL * restrict const subblock_gfs[])
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  char const * const source =
  "\n"
  "/* Declare finite differencing variables */\n"
  "\n"
  "/* Include user-supplied include files */\n"
  "\n"
  "/* Initialise finite differencing variables */\n"
  "ptrdiff_t const di = 1;\n"
  "ptrdiff_t const dj = CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0);\n"
  "ptrdiff_t const dk = CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0);\n"
  "ptrdiff_t const cdi = sizeof(CCTK_REAL) * di;\n"
  "ptrdiff_t const cdj = sizeof(CCTK_REAL) * dj;\n"
  "ptrdiff_t const cdk = sizeof(CCTK_REAL) * dk;\n"
  "CCTK_REAL_VEC const dx = ToReal(CCTK_DELTA_SPACE(0));\n"
  "CCTK_REAL_VEC const dy = ToReal(CCTK_DELTA_SPACE(1));\n"
  "CCTK_REAL_VEC const dz = ToReal(CCTK_DELTA_SPACE(2));\n"
  "CCTK_REAL_VEC const dt = ToReal(CCTK_DELTA_TIME);\n"
  "CCTK_REAL_VEC const t = ToReal(cctk_time);\n"
  "CCTK_REAL_VEC const dxi = INV(dx);\n"
  "CCTK_REAL_VEC const dyi = INV(dy);\n"
  "CCTK_REAL_VEC const dzi = INV(dz);\n"
  "CCTK_REAL_VEC const khalf = ToReal(0.5);\n"
  "CCTK_REAL_VEC const kthird = ToReal(1.0/3.0);\n"
  "CCTK_REAL_VEC const ktwothird = ToReal(2.0/3.0);\n"
  "CCTK_REAL_VEC const kfourthird = ToReal(4.0/3.0);\n"
  "CCTK_REAL_VEC const keightthird = ToReal(8.0/3.0);\n"
  "CCTK_REAL_VEC const hdxi = kmul(ToReal(0.5), dxi);\n"
  "CCTK_REAL_VEC const hdyi = kmul(ToReal(0.5), dyi);\n"
  "CCTK_REAL_VEC const hdzi = kmul(ToReal(0.5), dzi);\n"
  "\n"
  "/* Initialize predefined quantities */\n"
  "CCTK_REAL_VEC const p1o12dx = kmul(INV(dx),ToReal(0.0833333333333333333333333333333));\n"
  "CCTK_REAL_VEC const p1o12dy = kmul(INV(dy),ToReal(0.0833333333333333333333333333333));\n"
  "CCTK_REAL_VEC const p1o12dz = kmul(INV(dz),ToReal(0.0833333333333333333333333333333));\n"
  "CCTK_REAL_VEC const p1o144dxdy = kmul(INV(dx),kmul(INV(dy),ToReal(0.00694444444444444444444444444444)));\n"
  "CCTK_REAL_VEC const p1o144dxdz = kmul(INV(dx),kmul(INV(dz),ToReal(0.00694444444444444444444444444444)));\n"
  "CCTK_REAL_VEC const p1o144dydz = kmul(INV(dy),kmul(INV(dz),ToReal(0.00694444444444444444444444444444)));\n"
  "CCTK_REAL_VEC const p1o2dx = kmul(INV(dx),ToReal(0.5));\n"
  "CCTK_REAL_VEC const p1o2dy = kmul(INV(dy),ToReal(0.5));\n"
  "CCTK_REAL_VEC const p1o2dz = kmul(INV(dz),ToReal(0.5));\n"
  "CCTK_REAL_VEC const p1o4dxdy = kmul(INV(dx),kmul(INV(dy),ToReal(0.25)));\n"
  "CCTK_REAL_VEC const p1o4dxdz = kmul(INV(dx),kmul(INV(dz),ToReal(0.25)));\n"
  "CCTK_REAL_VEC const p1o4dydz = kmul(INV(dy),kmul(INV(dz),ToReal(0.25)));\n"
  "CCTK_REAL_VEC const p1odx2 = INV(SQR(dx));\n"
  "CCTK_REAL_VEC const p1ody2 = INV(SQR(dy));\n"
  "CCTK_REAL_VEC const p1odz2 = INV(SQR(dz));\n"
  "CCTK_REAL_VEC const pm1o12dx2 = kmul(INV(SQR(dx)),ToReal(-0.0833333333333333333333333333333));\n"
  "CCTK_REAL_VEC const pm1o12dy2 = kmul(INV(SQR(dy)),ToReal(-0.0833333333333333333333333333333));\n"
  "CCTK_REAL_VEC const pm1o12dz2 = kmul(INV(SQR(dz)),ToReal(-0.0833333333333333333333333333333));\n"
  "\n"
  "/* Jacobian variable pointers */\n"
  "bool const use_jacobian = (!CCTK_IsFunctionAliased(\"MultiPatch_GetMap\") || MultiPatch_GetMap(cctkGH) != jacobian_identity_map)\n"
  "                     && strlen(jacobian_group) > 0;\n"
  "if (use_jacobian && strlen(jacobian_derivative_group) == 0)\n"
  "{\n"
  "  CCTK_WARN (1, \"GenericFD::jacobian_group and GenericFD::jacobian_derivative_group must both be set to valid group names\");\n"
  "}\n"
  "\n"
  "CCTK_REAL const *restrict jacobian_ptrs[9];\n"
  "if (use_jacobian) GenericFD_GroupDataPointers(cctkGH, jacobian_group,\n"
  "                                              9, jacobian_ptrs);\n"
  "\n"
  "CCTK_REAL const *restrict const J11 = use_jacobian ? jacobian_ptrs[0] : 0;\n"
  "CCTK_REAL const *restrict const J12 = use_jacobian ? jacobian_ptrs[1] : 0;\n"
  "CCTK_REAL const *restrict const J13 = use_jacobian ? jacobian_ptrs[2] : 0;\n"
  "CCTK_REAL const *restrict const J21 = use_jacobian ? jacobian_ptrs[3] : 0;\n"
  "CCTK_REAL const *restrict const J22 = use_jacobian ? jacobian_ptrs[4] : 0;\n"
  "CCTK_REAL const *restrict const J23 = use_jacobian ? jacobian_ptrs[5] : 0;\n"
  "CCTK_REAL const *restrict const J31 = use_jacobian ? jacobian_ptrs[6] : 0;\n"
  "CCTK_REAL const *restrict const J32 = use_jacobian ? jacobian_ptrs[7] : 0;\n"
  "CCTK_REAL const *restrict const J33 = use_jacobian ? jacobian_ptrs[8] : 0;\n"
  "\n"
  "CCTK_REAL const *restrict jacobian_derivative_ptrs[18];\n"
  "if (use_jacobian) GenericFD_GroupDataPointers(cctkGH, jacobian_derivative_group,\n"
  "                                              18, jacobian_derivative_ptrs);\n"
  "\n"
  "CCTK_REAL const *restrict const dJ111 = use_jacobian ? jacobian_derivative_ptrs[0] : 0;\n"
  "CCTK_REAL const *restrict const dJ112 = use_jacobian ? jacobian_derivative_ptrs[1] : 0;\n"
  "CCTK_REAL const *restrict const dJ113 = use_jacobian ? jacobian_derivative_ptrs[2] : 0;\n"
  "CCTK_REAL const *restrict const dJ122 = use_jacobian ? jacobian_derivative_ptrs[3] : 0;\n"
  "CCTK_REAL const *restrict const dJ123 = use_jacobian ? jacobian_derivative_ptrs[4] : 0;\n"
  "CCTK_REAL const *restrict const dJ133 = use_jacobian ? jacobian_derivative_ptrs[5] : 0;\n"
  "CCTK_REAL const *restrict const dJ211 = use_jacobian ? jacobian_derivative_ptrs[6] : 0;\n"
  "CCTK_REAL const *restrict const dJ212 = use_jacobian ? jacobian_derivative_ptrs[7] : 0;\n"
  "CCTK_REAL const *restrict const dJ213 = use_jacobian ? jacobian_derivative_ptrs[8] : 0;\n"
  "CCTK_REAL const *restrict const dJ222 = use_jacobian ? jacobian_derivative_ptrs[9] : 0;\n"
  "CCTK_REAL const *restrict const dJ223 = use_jacobian ? jacobian_derivative_ptrs[10] : 0;\n"
  "CCTK_REAL const *restrict const dJ233 = use_jacobian ? jacobian_derivative_ptrs[11] : 0;\n"
  "CCTK_REAL const *restrict const dJ311 = use_jacobian ? jacobian_derivative_ptrs[12] : 0;\n"
  "CCTK_REAL const *restrict const dJ312 = use_jacobian ? jacobian_derivative_ptrs[13] : 0;\n"
  "CCTK_REAL const *restrict const dJ313 = use_jacobian ? jacobian_derivative_ptrs[14] : 0;\n"
  "CCTK_REAL const *restrict const dJ322 = use_jacobian ? jacobian_derivative_ptrs[15] : 0;\n"
  "CCTK_REAL const *restrict const dJ323 = use_jacobian ? jacobian_derivative_ptrs[16] : 0;\n"
  "CCTK_REAL const *restrict const dJ333 = use_jacobian ? jacobian_derivative_ptrs[17] : 0;\n"
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
  "LC_LOOP3VEC (EM_energy,\n"
  "  i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],\n"
  "  cctk_lsh[0],cctk_lsh[1],cctk_lsh[2],\n"
  "  CCTK_REAL_VEC_SIZE)\n"
  "{\n"
  "  ptrdiff_t const index = di*i + dj*j + dk*k;\n"
  "  \n"
  "  /* Assign local copies of grid functions */\n"
  "  \n"
  "  CCTK_REAL_VEC B1L = vec_load(B1[index]);\n"
  "  CCTK_REAL_VEC B2L = vec_load(B2[index]);\n"
  "  CCTK_REAL_VEC B3L = vec_load(B3[index]);\n"
  "  CCTK_REAL_VEC El1L = vec_load(El1[index]);\n"
  "  CCTK_REAL_VEC El2L = vec_load(El2[index]);\n"
  "  CCTK_REAL_VEC El3L = vec_load(El3[index]);\n"
  "  \n"
  "  \n"
  "  \n"
  "  /* Include user supplied include files */\n"
  "  \n"
  "  /* Precompute derivatives */\n"
  "  \n"
  "  switch(fdOrder)\n"
  "  {\n"
  "    case 2:\n"
  "      break;\n"
  "    \n"
  "    case 4:\n"
  "      break;\n"
  "  }\n"
  "  \n"
  "  /* Calculate temporaries and grid functions */\n"
  "  CCTK_REAL_VEC rhoL = \n"
  "    kmul(kadd(SQR(B1L),kadd(SQR(B2L),kadd(SQR(B3L),kadd(SQR(El1L),kadd(SQR(El2L),SQR(El3L)))))),ToReal(0.5));\n"
  "  \n"
  "  /* Copy local copies back to grid functions */\n"
  "  vec_store_nta_partial(rho[index],rhoL);\n"
  "}\n"
  "LC_ENDLOOP3VEC (EM_energy);\n"
  ""
  ;
  
  char const * const groups[] = {"My_New_Implementation::B_group","My_New_Implementation::El_group","My_New_Implementation::rho_group",NULL};
  
  static struct OpenCLKernel * kernel = NULL;
  char const * const sources[] = {differencing, source, NULL};
  OpenCLRunTime_CallKernel (cctkGH, CCTK_THORNSTRING, "EM_energy",
                            sources, groups, NULL, NULL, NULL, -1,
                            imin, imax, &kernel);
  
}

extern "C" void EM_energy(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Entering EM_energy_Body");
  }
  
  if (cctk_iteration % EM_energy_calc_every != EM_energy_calc_offset)
  {
    return;
  }
  
  const char *groups[] = {"My_New_Implementation::B_group","My_New_Implementation::El_group","My_New_Implementation::rho_group"};
  GenericFD_AssertGroupStorage(cctkGH, "EM_energy", 3, groups);
  
  switch(fdOrder)
  {
    case 2:
      break;
    
    case 4:
      break;
  }
  
  GenericFD_LoopOverEverything(cctkGH, &EM_energy_Body);
  
  if (verbose > 1)
  {
    CCTK_VInfo(CCTK_THORNSTRING,"Leaving EM_energy_Body");
  }
}
