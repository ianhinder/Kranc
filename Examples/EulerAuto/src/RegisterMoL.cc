/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void EulerAuto_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerAuto::Den"),  CCTK_VarIndex("EulerAuto::Denrhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerAuto::En"),  CCTK_VarIndex("EulerAuto::Enrhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerAuto::S1"),  CCTK_VarIndex("EulerAuto::S1rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerAuto::S2"),  CCTK_VarIndex("EulerAuto::S2rhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("EulerAuto::S3"),  CCTK_VarIndex("EulerAuto::S3rhs"));
  return;
}
