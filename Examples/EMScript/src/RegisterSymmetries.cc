/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"
#include "Symmetry.h"

extern "C" void EMScript_RegisterSymmetries(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* array holding symmetry definitions */
  CCTK_INT sym[3];
  
  
  /* Register symmetries of grid functions */
  sym[0] = -1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "My_New_Implementation::B1");
  
  sym[0] = 1;
  sym[1] = -1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "My_New_Implementation::B2");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = -1;
  SetCartSymVN(cctkGH, sym, "My_New_Implementation::B3");
  
  sym[0] = -1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "My_New_Implementation::El1");
  
  sym[0] = 1;
  sym[1] = -1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "My_New_Implementation::El2");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = -1;
  SetCartSymVN(cctkGH, sym, "My_New_Implementation::El3");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "My_New_Implementation::CB");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "My_New_Implementation::CEl");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "My_New_Implementation::rho");
  
}
