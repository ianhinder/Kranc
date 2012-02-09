/*  File produced by Kranc */

#include "cctk.h"

extern "C" int SimpleWaveScript_Startup(void)
{
  const char * banner = "SimpleWaveScript";
  CCTK_RegisterBanner(banner);
  return 0;
}
