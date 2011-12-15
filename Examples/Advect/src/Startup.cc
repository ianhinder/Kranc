/*  File produced by Kranc */

#include "cctk.h"

extern "C" int Advect_Startup(void)
{
  const char * banner = "Advect";
  CCTK_RegisterBanner(banner);
  return 0;
}
