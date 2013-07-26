/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

extern "C" void SimpleWaveODE_RegisterVars(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED  = 0;
  
  /* Register all the evolved grid functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("SimpleWaveODE::phi"),  CCTK_VarIndex("SimpleWaveODE::phirhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("SimpleWaveODE::pi"),  CCTK_VarIndex("SimpleWaveODE::pirhs"));
  
  /* Register all the evolved Array functions with MoL */
  ierr += MoLRegisterEvolved(CCTK_VarIndex("SimpleWaveODE::a"),  CCTK_VarIndex("SimpleWaveODE::arhs"));
  ierr += MoLRegisterEvolved(CCTK_VarIndex("SimpleWaveODE::b"),  CCTK_VarIndex("SimpleWaveODE::brhs"));
  return;
}
