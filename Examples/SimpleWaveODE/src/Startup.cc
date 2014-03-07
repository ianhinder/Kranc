/*  File produced by Kranc */

#include "cctk.h"

extern "C" int SimpleWaveODE_Startup(void)
{
  const char* banner CCTK_ATTRIBUTE_UNUSED = "SimpleWaveODE";
  CCTK_RegisterBanner(banner);
  return 0;
}
