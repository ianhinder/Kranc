/*  File produced by Kranc */

#include "cctk.h"

extern "C" int SimpleWaveOpenCL_Startup(void)
{
  const char * banner = "SimpleWaveOpenCL";
  CCTK_RegisterBanner(banner);
  return 0;
}
