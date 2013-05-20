/*  File produced by Kranc */

#include "cctk.h"

extern "C" int Euler_Startup(void)
{
  const char * banner CCTK_ATTRIBUTE_UNUSED  = "Euler";
  CCTK_RegisterBanner(banner);
  return 0;
}
