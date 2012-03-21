/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void WaveCaKernelScript_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("WaveCaKernelScript::phi"),  CCTK_VarIndex("WaveCaKernelScript::phirhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("WaveCaKernelScript::pi"),  CCTK_VarIndex("WaveCaKernelScript::pirhs"));
  
  /* Register all the evolved Array functions with MoL */
  return;
}
