/*  File produced by Kranc */

#include "cctk.h"

extern "C" int Burgers_Startup(void)
{
  const char * banner = "Burgers";
  CCTK_RegisterBanner(banner);
  return 0;
}
