#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder21(u) ((-KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o2dx)
#else
#  define PDstandardfdOrder21(u) (PDstandardfdOrder21_impl(u,p1o2dx,cdj,cdk))
static CCTK_REAL PDstandardfdOrder21_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dx, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder21_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dx, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o2dx;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder22(u) ((-KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o2dy)
#else
#  define PDstandardfdOrder22(u) (PDstandardfdOrder22_impl(u,p1o2dy,cdj,cdk))
static CCTK_REAL PDstandardfdOrder22_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder22_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o2dy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder23(u) (0)
#else
#  define PDstandardfdOrder23(u) (PDstandardfdOrder23_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder23_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder23_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder41(u) ((-8*KRANC_GFOFFSET3D(u,-1,0,0) + 8*KRANC_GFOFFSET3D(u,1,0,0) + KRANC_GFOFFSET3D(u,-2,0,0) - KRANC_GFOFFSET3D(u,2,0,0))*p1o12dx)
#else
#  define PDstandardfdOrder41(u) (PDstandardfdOrder41_impl(u,p1o12dx,cdj,cdk))
static CCTK_REAL PDstandardfdOrder41_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dx, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder41_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dx, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-8*KRANC_GFOFFSET3D(u,-1,0,0) + 8*KRANC_GFOFFSET3D(u,1,0,0) + KRANC_GFOFFSET3D(u,-2,0,0) - KRANC_GFOFFSET3D(u,2,0,0))*p1o12dx;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder42(u) ((-8*KRANC_GFOFFSET3D(u,0,-1,0) + 8*KRANC_GFOFFSET3D(u,0,1,0) + KRANC_GFOFFSET3D(u,0,-2,0) - KRANC_GFOFFSET3D(u,0,2,0))*p1o12dy)
#else
#  define PDstandardfdOrder42(u) (PDstandardfdOrder42_impl(u,p1o12dy,cdj,cdk))
static CCTK_REAL PDstandardfdOrder42_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder42_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-8*KRANC_GFOFFSET3D(u,0,-1,0) + 8*KRANC_GFOFFSET3D(u,0,1,0) + KRANC_GFOFFSET3D(u,0,-2,0) - KRANC_GFOFFSET3D(u,0,2,0))*p1o12dy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder43(u) (0)
#else
#  define PDstandardfdOrder43(u) (PDstandardfdOrder43_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder43_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder43_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder211(u) ((-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx2)
#else
#  define PDstandardfdOrder211(u) (PDstandardfdOrder211_impl(u,p1odx2,cdj,cdk))
static CCTK_REAL PDstandardfdOrder211_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder211_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder222(u) ((-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody2)
#else
#  define PDstandardfdOrder222(u) (PDstandardfdOrder222_impl(u,p1ody2,cdj,cdk))
static CCTK_REAL PDstandardfdOrder222_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1ody2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder222_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1ody2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder233(u) (0)
#else
#  define PDstandardfdOrder233(u) (PDstandardfdOrder233_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder233_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder233_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder411(u) ((30*KRANC_GFOFFSET3D(u,0,0,0) - 16*(KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0)) + KRANC_GFOFFSET3D(u,-2,0,0) + KRANC_GFOFFSET3D(u,2,0,0))*pm1o12dx2)
#else
#  define PDstandardfdOrder411(u) (PDstandardfdOrder411_impl(u,pm1o12dx2,cdj,cdk))
static CCTK_REAL PDstandardfdOrder411_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dx2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder411_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dx2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (30*KRANC_GFOFFSET3D(u,0,0,0) - 16*(KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0)) + KRANC_GFOFFSET3D(u,-2,0,0) + KRANC_GFOFFSET3D(u,2,0,0))*pm1o12dx2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder422(u) ((30*KRANC_GFOFFSET3D(u,0,0,0) - 16*(KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0)) + KRANC_GFOFFSET3D(u,0,-2,0) + KRANC_GFOFFSET3D(u,0,2,0))*pm1o12dy2)
#else
#  define PDstandardfdOrder422(u) (PDstandardfdOrder422_impl(u,pm1o12dy2,cdj,cdk))
static CCTK_REAL PDstandardfdOrder422_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dy2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder422_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dy2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (30*KRANC_GFOFFSET3D(u,0,0,0) - 16*(KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0)) + KRANC_GFOFFSET3D(u,0,-2,0) + KRANC_GFOFFSET3D(u,0,2,0))*pm1o12dy2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder433(u) (0)
#else
#  define PDstandardfdOrder433(u) (PDstandardfdOrder433_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder433_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder433_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder212(u) ((KRANC_GFOFFSET3D(u,-1,-1,0) - KRANC_GFOFFSET3D(u,-1,1,0) - KRANC_GFOFFSET3D(u,1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy)
#else
#  define PDstandardfdOrder212(u) (PDstandardfdOrder212_impl(u,p1o4dxdy,cdj,cdk))
static CCTK_REAL PDstandardfdOrder212_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder212_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,-1,-1,0) - KRANC_GFOFFSET3D(u,-1,1,0) - KRANC_GFOFFSET3D(u,1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder213(u) (0)
#else
#  define PDstandardfdOrder213(u) (PDstandardfdOrder213_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder213_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder213_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder221(u) ((KRANC_GFOFFSET3D(u,-1,-1,0) - KRANC_GFOFFSET3D(u,-1,1,0) - KRANC_GFOFFSET3D(u,1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy)
#else
#  define PDstandardfdOrder221(u) (PDstandardfdOrder221_impl(u,p1o4dxdy,cdj,cdk))
static CCTK_REAL PDstandardfdOrder221_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder221_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,-1,-1,0) - KRANC_GFOFFSET3D(u,-1,1,0) - KRANC_GFOFFSET3D(u,1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder223(u) (0)
#else
#  define PDstandardfdOrder223(u) (PDstandardfdOrder223_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder223_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder223_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder231(u) (0)
#else
#  define PDstandardfdOrder231(u) (PDstandardfdOrder231_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder231_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder231_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder232(u) (0)
#else
#  define PDstandardfdOrder232(u) (PDstandardfdOrder232_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder232_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder232_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder412(u) ((-64*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - KRANC_GFOFFSET3D(u,-2,2,0) - KRANC_GFOFFSET3D(u,2,-2,0) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy)
#else
#  define PDstandardfdOrder412(u) (PDstandardfdOrder412_impl(u,p1o144dxdy,cdj,cdk))
static CCTK_REAL PDstandardfdOrder412_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder412_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-64*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - KRANC_GFOFFSET3D(u,-2,2,0) - KRANC_GFOFFSET3D(u,2,-2,0) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder413(u) (0)
#else
#  define PDstandardfdOrder413(u) (PDstandardfdOrder413_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder413_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder413_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder421(u) ((-64*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - KRANC_GFOFFSET3D(u,-2,2,0) - KRANC_GFOFFSET3D(u,2,-2,0) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy)
#else
#  define PDstandardfdOrder421(u) (PDstandardfdOrder421_impl(u,p1o144dxdy,cdj,cdk))
static CCTK_REAL PDstandardfdOrder421_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder421_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-64*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - KRANC_GFOFFSET3D(u,-2,2,0) - KRANC_GFOFFSET3D(u,2,-2,0) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder423(u) (0)
#else
#  define PDstandardfdOrder423(u) (PDstandardfdOrder423_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder423_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder423_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder431(u) (0)
#else
#  define PDstandardfdOrder431(u) (PDstandardfdOrder431_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder431_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder431_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandardfdOrder432(u) (0)
#else
#  define PDstandardfdOrder432(u) (PDstandardfdOrder432_impl(u,p1o1,cdj,cdk))
static CCTK_REAL PDstandardfdOrder432_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandardfdOrder432_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return 0;
}
#endif

