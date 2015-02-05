#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffPlus1(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o1)
#else
#  define DiffPlus1(u) (DiffPlus1_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffPlus1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffPlus1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffPlus2(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o1)
#else
#  define DiffPlus2(u) (DiffPlus2_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffPlus2_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffPlus2_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffPlus3(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,1))*p1o1)
#else
#  define DiffPlus3(u) (DiffPlus3_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffPlus3_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffPlus3_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return DiffPlus2_impl(u, p1o1, cdk, cdj);
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffMinus1(u) ((KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,-1,0,0))*p1o1)
#else
#  define DiffMinus1(u) (DiffMinus1_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffMinus1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffMinus1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,-1,0,0))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffMinus2(u) ((KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,0,-1,0))*p1o1)
#else
#  define DiffMinus2(u) (DiffMinus2_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffMinus2_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffMinus2_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,0,-1,0))*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define DiffMinus3(u) ((KRANC_GFOFFSET3D(u,0,0,0) - KRANC_GFOFFSET3D(u,0,0,-1))*p1o1)
#else
#  define DiffMinus3(u) (DiffMinus3_impl(u,p1o1,cdj,cdk))
static CCTK_REAL DiffMinus3_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL DiffMinus3_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return DiffMinus2_impl(u, p1o1, cdk, cdj);
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define ShiftMinus1(u) (KRANC_GFOFFSET3D(u,-1,0,0)*p1o1)
#else
#  define ShiftMinus1(u) (ShiftMinus1_impl(u,p1o1,cdj,cdk))
static CCTK_REAL ShiftMinus1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL ShiftMinus1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return KRANC_GFOFFSET3D(u,-1,0,0)*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define ShiftMinus2(u) (KRANC_GFOFFSET3D(u,0,-1,0)*p1o1)
#else
#  define ShiftMinus2(u) (ShiftMinus2_impl(u,p1o1,cdj,cdk))
static CCTK_REAL ShiftMinus2_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL ShiftMinus2_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return KRANC_GFOFFSET3D(u,0,-1,0)*p1o1;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define ShiftMinus3(u) (KRANC_GFOFFSET3D(u,0,0,-1)*p1o1)
#else
#  define ShiftMinus3(u) (ShiftMinus3_impl(u,p1o1,cdj,cdk))
static CCTK_REAL ShiftMinus3_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL ShiftMinus3_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1o1, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return ShiftMinus2_impl(u, p1o1, cdk, cdj);
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDplus1(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx)
#else
#  define PDplus1(u) (PDplus1_impl(u,p1odx,cdj,cdk))
static CCTK_REAL PDplus1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1odx, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDplus1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1odx, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDplus2(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody)
#else
#  define PDplus2(u) (PDplus2_impl(u,p1ody,cdj,cdk))
static CCTK_REAL PDplus2_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1ody, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDplus2_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1ody, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody;
}
#endif

#ifndef KRANC_DIFF_FUNCTIONS
#  define PDplus3(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,1))*p1odz)
#else
#  define PDplus3(u) (PDplus3_impl(u,p1odz,cdj,cdk))
static CCTK_REAL PDplus3_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1odz, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDplus3_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1odz, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return PDplus2_impl(u, p1odz, cdk, cdj);
}
#endif

