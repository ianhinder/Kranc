/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"
#include "Symmetry.h"

extern "C" void EulerSR_RegisterSymmetries(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  
  /* array holding symmetry definitions */
  CCTK_INT sym[3];
  
  
  /* Register symmetries of grid functions */
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::Den");
  
  sym[0] = -1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S1");
  
  sym[0] = 1;
  sym[1] = -1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S2");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = -1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S3");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::tau");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::DenFlux");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::DenLeft");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::DenRight");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::epsi");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::epsiLeft");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::epsiRight");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::h");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::p");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::rho");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::rhoLeft");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::rhoRight");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S1Flux");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S1Left");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S1Right");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S2Flux");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S2Left");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S2Right");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S3Flux");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S3Left");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::S3Right");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::tauFlux");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::tauLeft");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::tauRight");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::v1Left");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::v1Right");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::v2Left");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::v2Right");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::v3Left");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::v3Right");
  
  sym[0] = -1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::v1");
  
  sym[0] = 1;
  sym[1] = -1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::v2");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = -1;
  SetCartSymVN(cctkGH, sym, "EulerSR::v3");
  
  sym[0] = 1;
  sym[1] = 1;
  sym[2] = 1;
  SetCartSymVN(cctkGH, sym, "EulerSR::W");
  
}
