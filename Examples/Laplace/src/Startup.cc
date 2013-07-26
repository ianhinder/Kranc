/*  File produced by Kranc */

#include "cctk.h"

extern "C" int Laplace_Startup(void)
{
  const char * banner CCTK_ATTRIBUTE_UNUSED  = "Laplace";
  CCTK_RegisterBanner(banner);
  return 0;
}
