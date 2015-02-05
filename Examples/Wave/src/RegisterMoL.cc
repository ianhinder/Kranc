/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void Wave_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("Wave::phi"),  CCTK_VarIndex("Wave::phirhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("Wave::pi"),  CCTK_VarIndex("Wave::pirhs"));
  /* Register all the evolved Array functions with MoL */
  return;
}
