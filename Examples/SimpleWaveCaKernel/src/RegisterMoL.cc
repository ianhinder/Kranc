/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void SimpleWaveCaKernel_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("SimpleWaveCaKernel::phi"),  CCTK_VarIndex("SimpleWaveCaKernel::phirhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("SimpleWaveCaKernel::pi"),  CCTK_VarIndex("SimpleWaveCaKernel::pirhs"));
  
  /* Register all the evolved Array functions with MoL */
  return;
}
