/*  File produced by Kranc */

#include "cctk.h"

extern "C" int SimpleWaveCaKernel_Startup(void)
{
  const char * banner = "SimpleWaveCaKernel";
  CCTK_RegisterBanner(banner);
  return 0;
}
