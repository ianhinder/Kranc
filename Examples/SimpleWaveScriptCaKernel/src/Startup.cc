/*  File produced by Kranc */

#include "cctk.h"

#include "Chemora.h"

extern "C" int SimpleWaveScriptCaKernel_Startup(void)
{
  const char* banner CCTK_ATTRIBUTE_UNUSED = "SimpleWaveScriptCaKernel";
  CCTK_RegisterBanner(banner);
  chemora_cg_thorn_startup();
  return 0;
}
