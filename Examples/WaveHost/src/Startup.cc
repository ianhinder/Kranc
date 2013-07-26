/*  File produced by Kranc */

#include "cctk.h"

extern "C" int WaveHost_Startup(void)
{
  const char * banner CCTK_ATTRIBUTE_UNUSED  = "WaveHost";
  CCTK_RegisterBanner(banner);
  return 0;
}
