/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void EM_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EM::B1"),  CCTK_VarIndex("EM::B1rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EM::B2"),  CCTK_VarIndex("EM::B2rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EM::B3"),  CCTK_VarIndex("EM::B3rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EM::El1"),  CCTK_VarIndex("EM::El1rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EM::El2"),  CCTK_VarIndex("EM::El2rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EM::El3"),  CCTK_VarIndex("EM::El3rhs"));
  
  /* Register all the evolved Array functions with MoL */
  return;
}
