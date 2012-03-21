/*  File produced by Kranc */

#include "cctk.h"

extern "C" int WaveCaKernelScript_Startup(void)
{
  const char * banner = "WaveCaKernelScript";
  CCTK_RegisterBanner(banner);
  return 0;
}
