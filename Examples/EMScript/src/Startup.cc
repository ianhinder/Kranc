/*  File produced by Kranc */

#include "cctk.h"

extern "C" int EMScript_Startup(void)
{
  const char * banner = "EMScript";
  CCTK_RegisterBanner(banner);
  return 0;
}
