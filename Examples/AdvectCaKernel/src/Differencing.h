#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard1(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx)
#else
#  define PDstandard1(u) (PDstandard1_impl(u,p1odx,cdj,cdk))
static CCTK_REAL PDstandard1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1odx, const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard1_impl(const CCTK_REAL* restrict const u, const CCTK_REAL p1odx, const ptrdiff_t cdj, const ptrdiff_t cdk)
{
  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx;
}
#endif

