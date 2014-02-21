/*  File produced by Kranc */

#include "cctk.h"

extern "C" int WaveCaKernelScript_Startup(void)
{
  const char* banner CCTK_ATTRIBUTE_UNUSED = "WaveCaKernelScript";
  CCTK_RegisterBanner(banner);
  return 0;
}
