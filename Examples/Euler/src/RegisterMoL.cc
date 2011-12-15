/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void Euler_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("Euler::Den"),  CCTK_VarIndex("Euler::Denrhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("Euler::En"),  CCTK_VarIndex("Euler::Enrhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("Euler::S1"),  CCTK_VarIndex("Euler::S1rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("Euler::S2"),  CCTK_VarIndex("Euler::S2rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("Euler::S3"),  CCTK_VarIndex("Euler::S3rhs"));
  return;
}
