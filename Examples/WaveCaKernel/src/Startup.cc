/*  File produced by Kranc */

#include "cctk.h"

extern "C" int WaveCaKernel_Startup(void)
{
  const char* banner CCTK_ATTRIBUTE_UNUSED = "WaveCaKernel";
  CCTK_RegisterBanner(banner);
  return 0;
}
