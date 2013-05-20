/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void SimpleWave_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED  = 0;
  
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("SimpleWave::phi"),  CCTK_VarIndex("SimpleWave::phirhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("SimpleWave::pi"),  CCTK_VarIndex("SimpleWave::pirhs"));
  
  /* Register all the evolved Array functions with MoL */
  return;
}
