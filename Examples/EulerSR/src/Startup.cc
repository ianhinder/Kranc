/*  File produced by Kranc */

#include "cctk.h"

extern "C" int EulerSR_Startup(void)
{
  const char* banner CCTK_ATTRIBUTE_UNUSED = "EulerSR";
  CCTK_RegisterBanner(banner);
  return 0;
}
