/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void EulerSR_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED  = 0;
  
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerSR::Den"),  CCTK_VarIndex("EulerSR::Denrhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerSR::S1"),  CCTK_VarIndex("EulerSR::S1rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerSR::S2"),  CCTK_VarIndex("EulerSR::S2rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerSR::S3"),  CCTK_VarIndex("EulerSR::S3rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerSR::tau"),  CCTK_VarIndex("EulerSR::taurhs"));
  
  /* Register all the evolved Array functions with MoL */
  return;
}
