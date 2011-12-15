/*  File produced by Kranc */

#include "cctk.h"

extern "C" int Euler_Startup(void)
{
  const char * banner = "Euler";
  CCTK_RegisterBanner(banner);
  return 0;
}
