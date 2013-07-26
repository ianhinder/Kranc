/*  File produced by Kranc */

#include "cctk.h"

extern "C" int EulerAuto_Startup(void)
{
  const char * banner CCTK_ATTRIBUTE_UNUSED  = "EulerAuto";
  CCTK_RegisterBanner(banner);
  return 0;
}
