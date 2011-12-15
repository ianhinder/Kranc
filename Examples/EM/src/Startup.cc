/*  File produced by Kranc */

#include "cctk.h"

extern "C" int EM_Startup(void)
{
  const char * banner = "EM";
  CCTK_RegisterBanner(banner);
  return 0;
}
