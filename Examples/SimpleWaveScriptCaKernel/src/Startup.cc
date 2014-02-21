/*  File produced by Kranc */

#include "cctk.h"

extern "C" int SimpleWaveScriptCaKernel_Startup(void)
{
  const char* banner CCTK_ATTRIBUTE_UNUSED = "SimpleWaveScriptCaKernel";
  CCTK_RegisterBanner(banner);
  return 0;
}
