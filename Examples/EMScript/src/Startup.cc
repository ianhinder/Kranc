/*  File produced by Kranc */

#include "cctk.h"

extern "C" int EMScript_Startup(void)
{
  const char* banner CCTK_ATTRIBUTE_UNUSED = "EMScript";
  CCTK_RegisterBanner(banner);
  return 0;
}
