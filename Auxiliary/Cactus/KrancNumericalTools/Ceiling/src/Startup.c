/*  file produced by user shusa, 31/3/2004 */
/*  Produced with Mathematica Version 5.0 for Linux (June 9, 2003) */

/*  Mathematica script written by Ian Hinder and Sascha Husa */

/*  $Id$ */

#include "cctk.h"

int Ceiling_Startup(void)
{
  const char * banner = "Ceiling: abort when solution grows through the roof";
  CCTK_RegisterBanner(banner);
  return 0;
}
