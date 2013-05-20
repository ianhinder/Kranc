/*  File produced by Kranc */

#include "cctk.h"

extern "C" int Wave_Startup(void)
{
  const char * banner CCTK_ATTRIBUTE_UNUSED  = "Wave";
  CCTK_RegisterBanner(banner);
  return 0;
}
