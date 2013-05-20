/*  File produced by Kranc */

#include "cctk.h"

extern "C" int Advect_Startup(void)
{
  const char * banner CCTK_ATTRIBUTE_UNUSED  = "Advect";
  CCTK_RegisterBanner(banner);
  return 0;
}
