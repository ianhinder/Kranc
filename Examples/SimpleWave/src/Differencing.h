#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2nd1(u) ((-KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o2dx)
#else
#  define PDstandard2nd1(u) (PDstandard2nd1_impl(u,p1o2dx,cdj,cdk))
static CCTK_REAL PDstandard2nd1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dx, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2nd1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dx, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o2dx;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2nd2(u) ((-KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o2dy)
#else
#  define PDstandard2nd2(u) (PDstandard2nd2_impl(u,p1o2dy,cdj,cdk))
static CCTK_REAL PDstandard2nd2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dy, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2nd2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dy, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o2dy;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2nd3(u) ((-KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1o2dz)
#else
#  define PDstandard2nd3(u) (PDstandard2nd3_impl(u,p1o2dz,cdj,cdk))
static CCTK_REAL PDstandard2nd3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2nd3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o2dz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1o2dz;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2nd11(u) ((-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx2)
#else
#  define PDstandard2nd11(u) (PDstandard2nd11_impl(u,p1odx2,cdj,cdk))
static CCTK_REAL PDstandard2nd11_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2nd11_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2nd22(u) ((-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody2)
#else
#  define PDstandard2nd22(u) (PDstandard2nd22_impl(u,p1ody2,cdj,cdk))
static CCTK_REAL PDstandard2nd22_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1ody2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2nd22_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1ody2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard2nd33(u) ((-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1odz2)
#else
#  define PDstandard2nd33(u) (PDstandard2nd33_impl(u,p1odz2,cdj,cdk))
static CCTK_REAL PDstandard2nd33_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odz2, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard2nd33_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odz2, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1odz2;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffPlus1(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o1)
#else
#  define DiffPlus1(u) (DiffPlus1_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffPlus1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffPlus1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffPlus2(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o1)
#else
#  define DiffPlus2(u) (DiffPlus2_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffPlus2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffPlus2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffPlus3(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,1))*p1o1)
#else
#  define DiffPlus3(u) (DiffPlus3_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffPlus3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffPlus3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,1))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffMinus1(u) ((KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,-1,0,0))*p1o1)
#else
#  define DiffMinus1(u) (DiffMinus1_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffMinus1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffMinus1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,-1,0,0))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffMinus2(u) ((KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,0,-1,0))*p1o1)
#else
#  define DiffMinus2(u) (DiffMinus2_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffMinus2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffMinus2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,0,-1,0))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffMinus3(u) ((KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,0,0,-1))*p1o1)
#else
#  define DiffMinus3(u) (DiffMinus3_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffMinus3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffMinus3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,0,0,-1))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define ShiftMinus1(u) (KRANC_GFOFFSET3D(u,-1,0,0)*p1o1)
#else
#  define ShiftMinus1(u) (ShiftMinus1_impl(u,p1o1,cdj,cdk))
static CCTK_REAL ShiftMinus1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL ShiftMinus1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return KRANC_GFOFFSET3D(u,-1,0,0)*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define ShiftMinus2(u) (KRANC_GFOFFSET3D(u,0,-1,0)*p1o1)
#else
#  define ShiftMinus2(u) (ShiftMinus2_impl(u,p1o1,cdj,cdk))
static CCTK_REAL ShiftMinus2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL ShiftMinus2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return KRANC_GFOFFSET3D(u,0,-1,0)*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define ShiftMinus3(u) (KRANC_GFOFFSET3D(u,0,0,-1)*p1o1)
#else
#  define ShiftMinus3(u) (ShiftMinus3_impl(u,p1o1,cdj,cdk))
static CCTK_REAL ShiftMinus3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL ShiftMinus3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1o1, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return KRANC_GFOFFSET3D(u,0,0,-1)*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDplus1(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx)
#else
#  define PDplus1(u) (PDplus1_impl(u,p1odx,cdj,cdk))
static CCTK_REAL PDplus1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDplus1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDplus2(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody)
#else
#  define PDplus2(u) (PDplus2_impl(u,p1ody,cdj,cdk))
static CCTK_REAL PDplus2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1ody, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDplus2_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1ody, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDplus3(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,1))*p1odz)
#else
#  define PDplus3(u) (PDplus3_impl(u,p1odz,cdj,cdk))
static CCTK_REAL PDplus3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odz, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDplus3_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odz, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,1))*p1odz;
}
#endif

