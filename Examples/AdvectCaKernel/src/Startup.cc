/*  File produced by Kranc */

#include "cctk.h"

extern "C" int AdvectCaKernel_Startup(void)
{
  const char * banner CCTK_ATTRIBUTE_UNUSED  = "AdvectCaKernel";
  CCTK_RegisterBanner(banner);
  return 0;
}
