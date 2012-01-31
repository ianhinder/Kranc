/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void EMScript_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("My_New_Implementation::B1"),  CCTK_VarIndex("My_New_Implementation::B1rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("My_New_Implementation::B2"),  CCTK_VarIndex("My_New_Implementation::B2rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("My_New_Implementation::B3"),  CCTK_VarIndex("My_New_Implementation::B3rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("My_New_Implementation::El1"),  CCTK_VarIndex("My_New_Implementation::El1rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("My_New_Implementation::El2"),  CCTK_VarIndex("My_New_Implementation::El2rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("My_New_Implementation::El3"),  CCTK_VarIndex("My_New_Implementation::El3rhs"));
  
  /* Register all the evolved Array functions with MoL */
  return;
}
