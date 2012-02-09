#ifndef KRANC_DIFF_FUNCTIONS
#  define PDstandard1(u) ((-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx)
#else
#  define PDstandard1(u) (PDstandard1_impl(u,p1odx,cdj,cdk))
static CCTK_REAL PDstandard1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx, ptrdiff_t const cdj, ptrdiff_t const cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL PDstandard1_impl(CCTK_REAL const* restrict const u, CCTK_REAL const p1odx, ptrdiff_t const cdj, ptrdiff_t const cdk)
{
  ptrdiff_t const cdi=sizeof(CCTK_REAL);
  return (-KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx;
}
#endif

