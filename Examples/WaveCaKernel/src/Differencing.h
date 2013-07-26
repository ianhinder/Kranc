#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th1(u) ((-1.*KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o2dx)
#else
#  define PDstandard2th1(u) (PDstandard2th1_impl(u,p1o2dx,cdj,cdk))
static CCTK_REAL PDstandard2th1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dx, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dx, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-1.*KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o2dx;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th2(u) ((-1.*KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o2dy)
#else
#  define PDstandard2th2(u) (PDstandard2th2_impl(u,p1o2dy,cdj,cdk))
static CCTK_REAL PDstandard2th2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-1.*KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o2dy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th3(u) ((-1.*KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1o2dz)
#else
#  define PDstandard2th3(u) (PDstandard2th3_impl(u,p1o2dz,cdj,cdk))
static CCTK_REAL PDstandard2th3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-1.*KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1o2dz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th11(u) ((-2.*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx2)
#else
#  define PDstandard2th11(u) (PDstandard2th11_impl(u,p1odx2,cdj,cdk))
static CCTK_REAL PDstandard2th11_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th11_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-2.*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th22(u) ((-2.*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody2)
#else
#  define PDstandard2th22(u) (PDstandard2th22_impl(u,p1ody2,cdj,cdk))
static CCTK_REAL PDstandard2th22_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1ody2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th22_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1ody2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-2.*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th33(u) ((-2.*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1odz2)
#else
#  define PDstandard2th33(u) (PDstandard2th33_impl(u,p1odz2,cdj,cdk))
static CCTK_REAL PDstandard2th33_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odz2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th33_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odz2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-2.*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1odz2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th12(u) ((KRANC_GFOFFSET3D(u,-1,-1,0) - 1.*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy)
#else
#  define PDstandard2th12(u) (PDstandard2th12_impl(u,p1o4dxdy,cdj,cdk))
static CCTK_REAL PDstandard2th12_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th12_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,-1,-1,0) - 1.*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th13(u) ((KRANC_GFOFFSET3D(u,-1,0,-1) - 1.*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + KRANC_GFOFFSET3D(u,1,0,1))*p1o4dxdz)
#else
#  define PDstandard2th13(u) (PDstandard2th13_impl(u,p1o4dxdz,cdj,cdk))
static CCTK_REAL PDstandard2th13_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th13_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,-1,0,-1) - 1.*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + KRANC_GFOFFSET3D(u,1,0,1))*p1o4dxdz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th21(u) ((KRANC_GFOFFSET3D(u,-1,-1,0) - 1.*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy)
#else
#  define PDstandard2th21(u) (PDstandard2th21_impl(u,p1o4dxdy,cdj,cdk))
static CCTK_REAL PDstandard2th21_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th21_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,-1,-1,0) - 1.*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th23(u) ((KRANC_GFOFFSET3D(u,0,-1,-1) - 1.*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + KRANC_GFOFFSET3D(u,0,1,1))*p1o4dydz)
#else
#  define PDstandard2th23(u) (PDstandard2th23_impl(u,p1o4dydz,cdj,cdk))
static CCTK_REAL PDstandard2th23_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dydz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th23_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dydz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,0,-1,-1) - 1.*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + KRANC_GFOFFSET3D(u,0,1,1))*p1o4dydz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th31(u) ((KRANC_GFOFFSET3D(u,-1,0,-1) - 1.*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + KRANC_GFOFFSET3D(u,1,0,1))*p1o4dxdz)
#else
#  define PDstandard2th31(u) (PDstandard2th31_impl(u,p1o4dxdz,cdj,cdk))
static CCTK_REAL PDstandard2th31_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th31_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dxdz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,-1,0,-1) - 1.*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + KRANC_GFOFFSET3D(u,1,0,1))*p1o4dxdz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2th32(u) ((KRANC_GFOFFSET3D(u,0,-1,-1) - 1.*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + KRANC_GFOFFSET3D(u,0,1,1))*p1o4dydz)
#else
#  define PDstandard2th32(u) (PDstandard2th32_impl(u,p1o4dydz,cdj,cdk))
static CCTK_REAL PDstandard2th32_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dydz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2th32_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o4dydz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,0,-1,-1) - 1.*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + KRANC_GFOFFSET3D(u,0,1,1))*p1o4dydz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th1(u) ((-8.*KRANC_GFOFFSET3D(u,-1,0,0) + 8.*KRANC_GFOFFSET3D(u,1,0,0) + KRANC_GFOFFSET3D(u,-2,0,0) - 1.*KRANC_GFOFFSET3D(u,2,0,0))*p1o12dx)
#else
#  define PDstandard4th1(u) (PDstandard4th1_impl(u,p1o12dx,cdj,cdk))
static CCTK_REAL PDstandard4th1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dx, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dx, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-8.*KRANC_GFOFFSET3D(u,-1,0,0) + 8.*KRANC_GFOFFSET3D(u,1,0,0) + KRANC_GFOFFSET3D(u,-2,0,0) - 1.*KRANC_GFOFFSET3D(u,2,0,0))*p1o12dx;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th2(u) ((-8.*KRANC_GFOFFSET3D(u,0,-1,0) + 8.*KRANC_GFOFFSET3D(u,0,1,0) + KRANC_GFOFFSET3D(u,0,-2,0) - 1.*KRANC_GFOFFSET3D(u,0,2,0))*p1o12dy)
#else
#  define PDstandard4th2(u) (PDstandard4th2_impl(u,p1o12dy,cdj,cdk))
static CCTK_REAL PDstandard4th2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-8.*KRANC_GFOFFSET3D(u,0,-1,0) + 8.*KRANC_GFOFFSET3D(u,0,1,0) + KRANC_GFOFFSET3D(u,0,-2,0) - 1.*KRANC_GFOFFSET3D(u,0,2,0))*p1o12dy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th3(u) ((-8.*KRANC_GFOFFSET3D(u,0,0,-1) + 8.*KRANC_GFOFFSET3D(u,0,0,1) + KRANC_GFOFFSET3D(u,0,0,-2) - 1.*KRANC_GFOFFSET3D(u,0,0,2))*p1o12dz)
#else
#  define PDstandard4th3(u) (PDstandard4th3_impl(u,p1o12dz,cdj,cdk))
static CCTK_REAL PDstandard4th3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o12dz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-8.*KRANC_GFOFFSET3D(u,0,0,-1) + 8.*KRANC_GFOFFSET3D(u,0,0,1) + KRANC_GFOFFSET3D(u,0,0,-2) - 1.*KRANC_GFOFFSET3D(u,0,0,2))*p1o12dz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th11(u) ((30.*KRANC_GFOFFSET3D(u,0,0,0) - 16.*(KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0)) + KRANC_GFOFFSET3D(u,-2,0,0) + KRANC_GFOFFSET3D(u,2,0,0))*pm1o12dx2)
#else
#  define PDstandard4th11(u) (PDstandard4th11_impl(u,pm1o12dx2,cdj,cdk))
static CCTK_REAL PDstandard4th11_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dx2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th11_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dx2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (30.*KRANC_GFOFFSET3D(u,0,0,0) - 16.*(KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0)) + KRANC_GFOFFSET3D(u,-2,0,0) + KRANC_GFOFFSET3D(u,2,0,0))*pm1o12dx2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th22(u) ((30.*KRANC_GFOFFSET3D(u,0,0,0) - 16.*(KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0)) + KRANC_GFOFFSET3D(u,0,-2,0) + KRANC_GFOFFSET3D(u,0,2,0))*pm1o12dy2)
#else
#  define PDstandard4th22(u) (PDstandard4th22_impl(u,pm1o12dy2,cdj,cdk))
static CCTK_REAL PDstandard4th22_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dy2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th22_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dy2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (30.*KRANC_GFOFFSET3D(u,0,0,0) - 16.*(KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0)) + KRANC_GFOFFSET3D(u,0,-2,0) + KRANC_GFOFFSET3D(u,0,2,0))*pm1o12dy2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th33(u) ((30.*KRANC_GFOFFSET3D(u,0,0,0) - 16.*(KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1)) + KRANC_GFOFFSET3D(u,0,0,-2) + KRANC_GFOFFSET3D(u,0,0,2))*pm1o12dz2)
#else
#  define PDstandard4th33(u) (PDstandard4th33_impl(u,pm1o12dz2,cdj,cdk))
static CCTK_REAL PDstandard4th33_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dz2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th33_impl(CCTK_REAL const* restrict const u, CCTK_REAL const pm1o12dz2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (30.*KRANC_GFOFFSET3D(u,0,0,0) - 16.*(KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1)) + KRANC_GFOFFSET3D(u,0,0,-2) + KRANC_GFOFFSET3D(u,0,0,2))*pm1o12dz2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th12(u) ((-64.*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64.*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8.*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8.*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - 1.*(KRANC_GFOFFSET3D(u,-2,2,0) + KRANC_GFOFFSET3D(u,2,-2,0)) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy)
#else
#  define PDstandard4th12(u) (PDstandard4th12_impl(u,p1o144dxdy,cdj,cdk))
static CCTK_REAL PDstandard4th12_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th12_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-64.*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64.*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8.*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8.*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - 1.*(KRANC_GFOFFSET3D(u,-2,2,0) + KRANC_GFOFFSET3D(u,2,-2,0)) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th13(u) ((-64.*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + 64.*(KRANC_GFOFFSET3D(u,-1,0,-1) + KRANC_GFOFFSET3D(u,1,0,1)) + 8.*(KRANC_GFOFFSET3D(u,-1,0,2) + KRANC_GFOFFSET3D(u,1,0,-2) + KRANC_GFOFFSET3D(u,-2,0,1) + KRANC_GFOFFSET3D(u,2,0,-1)) - 8.*(KRANC_GFOFFSET3D(u,-1,0,-2) + KRANC_GFOFFSET3D(u,1,0,2) + KRANC_GFOFFSET3D(u,-2,0,-1) + KRANC_GFOFFSET3D(u,2,0,1)) + KRANC_GFOFFSET3D(u,-2,0,-2) - 1.*(KRANC_GFOFFSET3D(u,-2,0,2) + KRANC_GFOFFSET3D(u,2,0,-2)) + KRANC_GFOFFSET3D(u,2,0,2))*p1o144dxdz)
#else
#  define PDstandard4th13(u) (PDstandard4th13_impl(u,p1o144dxdz,cdj,cdk))
static CCTK_REAL PDstandard4th13_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th13_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-64.*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + 64.*(KRANC_GFOFFSET3D(u,-1,0,-1) + KRANC_GFOFFSET3D(u,1,0,1)) + 8.*(KRANC_GFOFFSET3D(u,-1,0,2) + KRANC_GFOFFSET3D(u,1,0,-2) + KRANC_GFOFFSET3D(u,-2,0,1) + KRANC_GFOFFSET3D(u,2,0,-1)) - 8.*(KRANC_GFOFFSET3D(u,-1,0,-2) + KRANC_GFOFFSET3D(u,1,0,2) + KRANC_GFOFFSET3D(u,-2,0,-1) + KRANC_GFOFFSET3D(u,2,0,1)) + KRANC_GFOFFSET3D(u,-2,0,-2) - 1.*(KRANC_GFOFFSET3D(u,-2,0,2) + KRANC_GFOFFSET3D(u,2,0,-2)) + KRANC_GFOFFSET3D(u,2,0,2))*p1o144dxdz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th21(u) ((-64.*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64.*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8.*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8.*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - 1.*(KRANC_GFOFFSET3D(u,-2,2,0) + KRANC_GFOFFSET3D(u,2,-2,0)) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy)
#else
#  define PDstandard4th21(u) (PDstandard4th21_impl(u,p1o144dxdy,cdj,cdk))
static CCTK_REAL PDstandard4th21_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th21_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-64.*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64.*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8.*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8.*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - 1.*(KRANC_GFOFFSET3D(u,-2,2,0) + KRANC_GFOFFSET3D(u,2,-2,0)) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th23(u) ((-64.*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + 64.*(KRANC_GFOFFSET3D(u,0,-1,-1) + KRANC_GFOFFSET3D(u,0,1,1)) + 8.*(KRANC_GFOFFSET3D(u,0,-1,2) + KRANC_GFOFFSET3D(u,0,1,-2) + KRANC_GFOFFSET3D(u,0,-2,1) + KRANC_GFOFFSET3D(u,0,2,-1)) - 8.*(KRANC_GFOFFSET3D(u,0,-1,-2) + KRANC_GFOFFSET3D(u,0,1,2) + KRANC_GFOFFSET3D(u,0,-2,-1) + KRANC_GFOFFSET3D(u,0,2,1)) + KRANC_GFOFFSET3D(u,0,-2,-2) - 1.*(KRANC_GFOFFSET3D(u,0,-2,2) + KRANC_GFOFFSET3D(u,0,2,-2)) + KRANC_GFOFFSET3D(u,0,2,2))*p1o144dydz)
#else
#  define PDstandard4th23(u) (PDstandard4th23_impl(u,p1o144dydz,cdj,cdk))
static CCTK_REAL PDstandard4th23_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dydz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th23_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dydz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-64.*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + 64.*(KRANC_GFOFFSET3D(u,0,-1,-1) + KRANC_GFOFFSET3D(u,0,1,1)) + 8.*(KRANC_GFOFFSET3D(u,0,-1,2) + KRANC_GFOFFSET3D(u,0,1,-2) + KRANC_GFOFFSET3D(u,0,-2,1) + KRANC_GFOFFSET3D(u,0,2,-1)) - 8.*(KRANC_GFOFFSET3D(u,0,-1,-2) + KRANC_GFOFFSET3D(u,0,1,2) + KRANC_GFOFFSET3D(u,0,-2,-1) + KRANC_GFOFFSET3D(u,0,2,1)) + KRANC_GFOFFSET3D(u,0,-2,-2) - 1.*(KRANC_GFOFFSET3D(u,0,-2,2) + KRANC_GFOFFSET3D(u,0,2,-2)) + KRANC_GFOFFSET3D(u,0,2,2))*p1o144dydz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th31(u) ((-64.*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + 64.*(KRANC_GFOFFSET3D(u,-1,0,-1) + KRANC_GFOFFSET3D(u,1,0,1)) + 8.*(KRANC_GFOFFSET3D(u,-1,0,2) + KRANC_GFOFFSET3D(u,1,0,-2) + KRANC_GFOFFSET3D(u,-2,0,1) + KRANC_GFOFFSET3D(u,2,0,-1)) - 8.*(KRANC_GFOFFSET3D(u,-1,0,-2) + KRANC_GFOFFSET3D(u,1,0,2) + KRANC_GFOFFSET3D(u,-2,0,-1) + KRANC_GFOFFSET3D(u,2,0,1)) + KRANC_GFOFFSET3D(u,-2,0,-2) - 1.*(KRANC_GFOFFSET3D(u,-2,0,2) + KRANC_GFOFFSET3D(u,2,0,-2)) + KRANC_GFOFFSET3D(u,2,0,2))*p1o144dxdz)
#else
#  define PDstandard4th31(u) (PDstandard4th31_impl(u,p1o144dxdz,cdj,cdk))
static CCTK_REAL PDstandard4th31_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th31_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dxdz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-64.*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + 64.*(KRANC_GFOFFSET3D(u,-1,0,-1) + KRANC_GFOFFSET3D(u,1,0,1)) + 8.*(KRANC_GFOFFSET3D(u,-1,0,2) + KRANC_GFOFFSET3D(u,1,0,-2) + KRANC_GFOFFSET3D(u,-2,0,1) + KRANC_GFOFFSET3D(u,2,0,-1)) - 8.*(KRANC_GFOFFSET3D(u,-1,0,-2) + KRANC_GFOFFSET3D(u,1,0,2) + KRANC_GFOFFSET3D(u,-2,0,-1) + KRANC_GFOFFSET3D(u,2,0,1)) + KRANC_GFOFFSET3D(u,-2,0,-2) - 1.*(KRANC_GFOFFSET3D(u,-2,0,2) + KRANC_GFOFFSET3D(u,2,0,-2)) + KRANC_GFOFFSET3D(u,2,0,2))*p1o144dxdz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard4th32(u) ((-64.*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + 64.*(KRANC_GFOFFSET3D(u,0,-1,-1) + KRANC_GFOFFSET3D(u,0,1,1)) + 8.*(KRANC_GFOFFSET3D(u,0,-1,2) + KRANC_GFOFFSET3D(u,0,1,-2) + KRANC_GFOFFSET3D(u,0,-2,1) + KRANC_GFOFFSET3D(u,0,2,-1)) - 8.*(KRANC_GFOFFSET3D(u,0,-1,-2) + KRANC_GFOFFSET3D(u,0,1,2) + KRANC_GFOFFSET3D(u,0,-2,-1) + KRANC_GFOFFSET3D(u,0,2,1)) + KRANC_GFOFFSET3D(u,0,-2,-2) - 1.*(KRANC_GFOFFSET3D(u,0,-2,2) + KRANC_GFOFFSET3D(u,0,2,-2)) + KRANC_GFOFFSET3D(u,0,2,2))*p1o144dydz)
#else
#  define PDstandard4th32(u) (PDstandard4th32_impl(u,p1o144dydz,cdj,cdk))
static CCTK_REAL PDstandard4th32_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dydz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard4th32_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o144dydz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-64.*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + 64.*(KRANC_GFOFFSET3D(u,0,-1,-1) + KRANC_GFOFFSET3D(u,0,1,1)) + 8.*(KRANC_GFOFFSET3D(u,0,-1,2) + KRANC_GFOFFSET3D(u,0,1,-2) + KRANC_GFOFFSET3D(u,0,-2,1) + KRANC_GFOFFSET3D(u,0,2,-1)) - 8.*(KRANC_GFOFFSET3D(u,0,-1,-2) + KRANC_GFOFFSET3D(u,0,1,2) + KRANC_GFOFFSET3D(u,0,-2,-1) + KRANC_GFOFFSET3D(u,0,2,1)) + KRANC_GFOFFSET3D(u,0,-2,-2) - 1.*(KRANC_GFOFFSET3D(u,0,-2,2) + KRANC_GFOFFSET3D(u,0,2,-2)) + KRANC_GFOFFSET3D(u,0,2,2))*p1o144dydz;
}
#endif

