/*  File produced by Kranc */

#include "cctk.h"

extern "C" int SimpleWave_Startup(void)
{
  const char * banner CCTK_ATTRIBUTE_UNUSED  = "SimpleWave";
  CCTK_RegisterBanner(banner);
  return 0;
}
