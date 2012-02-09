/*  File produced by Kranc */

#include "cctk.h"

extern "C" int AdvectCaKernel_Startup(void)
{
  const char * banner = "AdvectCaKernel";
  CCTK_RegisterBanner(banner);
  return 0;
}
