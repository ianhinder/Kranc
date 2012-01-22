/*  File produced by Kranc */

#include "cctk.h"

extern "C" int EulerAuto_Startup(void)
{
  const char * banner = "EulerAuto";
  CCTK_RegisterBanner(banner);
  return 0;
}
