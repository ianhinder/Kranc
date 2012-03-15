/*  File produced by Kranc */

#include "cctk.h"

extern "C" int WaveHost_Startup(void)
{
  const char * banner = "WaveHost";
  CCTK_RegisterBanner(banner);
  return 0;
}
