/*  File produced by Kranc */

#include "cctk.h"

extern "C" int Burgers_Startup(void)
{
  const char * banner CCTK_ATTRIBUTE_UNUSED  = "Burgers";
  CCTK_RegisterBanner(banner);
  return 0;
}
