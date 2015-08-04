#ifndef STENCIL_OPS_HH
#define STENCIL_OPS_HH

#include <vectors.h>
#include <cctk.h>

#include <cstddef>
#include <type_traits>

static_assert(__cplusplus >= 201103, "");
static_assert(HAVE_CAPABILITY_Vectors, "");

namespace @THORN_NAME@ {
/******************************************************************************/
/* Convenient shortcuts */

using std::ptrdiff_t;
using std::size_t;

/******************************************************************************/
/* Explicitly unroll a loop */

// The loop ranges from I (inclusively) to N (exclusively) with a step
// size of S (must be positive, defaults to 1). The loop body is
// expected to be function object, such as a lamda expression.

template <ptrdiff_t I, ptrdiff_t N, ptrdiff_t S = 1, typename F>
inline typename std::enable_if<(I >= N), void>::type repeat_n(const F &f) {}

template <ptrdiff_t I, ptrdiff_t N, ptrdiff_t S = 1, typename F>
inline typename std::enable_if<(I < N), void>::type repeat_n(const F &f) {
  f(I);
  repeat_n<I + S, N, S>(f);
}

/******************************************************************************/
/* Alignment */

// Align x to the next multiple of y, either downwards or upwards
constexpr size_t aligndown(const size_t x, const size_t y) { return x / y * y; }
constexpr size_t alignup(const size_t x, const size_t y) {
  return (x + y - 1) / y * y;
}

/******************************************************************************/
/* Array access */

// Access an array "ptr" of type CCTK_REAL from a pointer to unsigned
// char. The index "idx" needs to be given in terms of unsigned char,
// not CCTK_REAL. This generally leads to better code, since the
// implicit multiplication by sizeof(CCTK_REAL) is avoided.
constexpr const CCTK_REAL &getelt(const unsigned char *restrict const ptr,
                                  const ptrdiff_t idx) {
  return *(const CCTK_REAL *)&ptr[idx];
}
constexpr CCTK_REAL &getelt(unsigned char *restrict const ptr,
                            const ptrdiff_t idx) {
  return *(CCTK_REAL *)&ptr[idx];
}

/******************************************************************************/
/* Finite differencing stencils */

// Odd stencils are e.g. used for first derivatives, even stencils for
// second derivatives. The last part "dirN" or "dirMN" of the name
// indicates the stencil direction, e.g. "dir0" is the x direction,
// "dir01" is a mixed xy stencil.

// Stencil coefficients are passed via types. For example, these types
// define fourth order accurate first and second derivatives:

/*
struct fdop_order4_d1 {
  constexpr static const ptrdiff_t stencil_radius = 2;
  typedef const CCTK_REAL coeffs_t[2 * stencil_radius + 1];
  constexpr static coeffs_t coeffs = {-1.0 / 12.0, 8.0 / 12.0, 0, -8.0 / 12.0,
                                      1.0 / 12.0};
};
constexpr typename fdop_order4_d1::coeffs_t fdop_order4_d1::coeffs;

struct fdop_order4_d2 {
  constexpr static const ptrdiff_t stencil_radius = 2;
  typedef const CCTK_REAL coeffs_t[2 * stencil_radius + 1];
  constexpr static coeffs_t coeffs = {-1.0 / 12.0, 16.0 / 12.0, -30.0 / 12.0,
                                      16.0 / 12.0, -1.0 / 12.0};
};
constexpr typename fdop_order4_d2::coeffs_t fdop_order4_d2::coeffs;
*/

// Note that the explicit definition of the stencil coefficients after
// the type definition leads to better code (at least with GCC), as
// the compiler does not need to make separate copies of the stencil
// coefficients.

// These loops range over fixed tiles of size (npoints_i, npoints_j,
// npoints_k). These should be small integers (e.g. 4 or 8). npoints_i
// should be chosen to be a multiple of the vector size
// CCTK_REAL_VEC_SIZE, and if should possibly also be aligned with the
// cache line size (e.g. 64 bytes on current Intel CPUs).

// TODO: Introduce a compile-time constant for the cache line size.

// Note that the stencils are evaluate symmetrically, which reduces
// the number of floating-point operations. Note that mixed
// derivatives are evaluated in two stages, which also reduces the
// necessary number of floating-point operations.

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE void
stencil_odd_dim3_dir0(const unsigned char *restrict const u,
                      unsigned char *restrict const du, const ptrdiff_t dj,
                      const ptrdiff_t dk) {
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeffs[n + fdop::stencil_radius];
          s = kmadd(vec_set1(c),
                    ksub(vec_loadu_maybe(n, getelt(u, offset + n * di)),
                         vec_loadu_maybe(-n, getelt(u, offset - n * di))),
                    s);
        }
        vec_store_nta_partial(getelt(du, offset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE void
stencil_even_dim3_dir0(const unsigned char *restrict const u,
                       unsigned char *restrict const du, const ptrdiff_t dj,
                       const ptrdiff_t dk) {
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = kmul(vec_set1(fdop::coeffs[fdop::stencil_radius]),
                               vec_load(getelt(u, offset)));
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeffs[n + fdop::stencil_radius];
          s = kmadd(vec_set1(c),
                    kadd(vec_loadu_maybe(-n, getelt(u, offset - n * di)),
                         vec_loadu_maybe(n, getelt(u, offset + n * di))),
                    s);
        }
        vec_store_nta_partial(getelt(du, offset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE void
stencil_odd_dim3_dir1(const unsigned char *restrict const u,
                      unsigned char *restrict const du, const ptrdiff_t dj,
                      const ptrdiff_t dk) {
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeffs[n + fdop::stencil_radius];
          s = kmadd(vec_set1(c), ksub(vec_load(getelt(u, offset + n * dj)),
                                      vec_load(getelt(u, offset - n * dj))),
                    s);
        }
        vec_store_nta_partial(getelt(du, offset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE void
stencil_even_dim3_dir1(const unsigned char *restrict const u,
                       unsigned char *restrict const du, const ptrdiff_t dj,
                       const ptrdiff_t dk) {
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = kmul(vec_set1(fdop::coeffs[fdop::stencil_radius]),
                               vec_load(getelt(u, offset)));
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeffs[n + fdop::stencil_radius];
          s = kmadd(vec_set1(c), kadd(vec_load(getelt(u, offset - n * dj)),
                                      vec_load(getelt(u, offset + n * dj))),
                    s);
        }
        vec_store_nta_partial(getelt(du, offset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE void
stencil_odd_dim3_dir01(const unsigned char *restrict const u,
                       unsigned char *restrict const du, const ptrdiff_t dj,
                       const ptrdiff_t dk) {
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  // Could also split k loop
  constexpr ptrdiff_t buf_ni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t buf_nj = npoints_j + 2 * fdop::stencil_radius;
  constexpr ptrdiff_t buf_nk = npoints_k;
  constexpr ptrdiff_t buf_di = di;
  constexpr ptrdiff_t buf_dj = buf_di * buf_ni;
  constexpr ptrdiff_t buf_dk = buf_dj * buf_nj;
  constexpr ptrdiff_t buf_sz = buf_dk * buf_nk;
  unsigned char buf[buf_sz] __attribute__((__aligned__(sizeof(CCTK_REAL_VEC))));
  // adapted from stencil_odd_dim3_dir0
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = -fdop::stencil_radius;
         j < npoints_j + fdop::stencil_radius; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t buf_offset =
            i * buf_di + (j + fdop::stencil_radius) * buf_dj + k * buf_dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeffs[n + fdop::stencil_radius];
          s = kmadd(vec_set1(c),
                    ksub(vec_loadu_maybe(n, getelt(u, offset + n * di)),
                         vec_loadu_maybe(-n, getelt(u, offset - n * di))),
                    s);
        }
        vec_store_nta_partial(getelt(buf, buf_offset), s);
      }
    }
  }
  // adapted from stencil_odd_dim3_dir1
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t buf_offset =
            i * buf_di + (j + fdop::stencil_radius) * buf_dj + k * buf_dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeffs[n + fdop::stencil_radius];
          s = kmadd(vec_set1(c),
                    ksub(vec_load(getelt(buf, buf_offset + n * buf_dj)),
                         vec_load(getelt(buf, buf_offset - n * buf_dj))),
                    s);
        }
        vec_store_nta_partial(getelt(du, offset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE void
stencil_odd_dim3_dir12(const unsigned char *restrict const u,
                       unsigned char *restrict const du, const ptrdiff_t dj,
                       const ptrdiff_t dk) {
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t buf_ni = alignup(npoints_i, vs);
  constexpr ptrdiff_t buf_nj = npoints_j + 2 * fdop::stencil_radius;
  constexpr ptrdiff_t buf_nk = npoints_k;
  constexpr ptrdiff_t buf_di = di;
  constexpr ptrdiff_t buf_dj = buf_di * buf_ni;
  constexpr ptrdiff_t buf_dk = buf_dj * buf_nj;
  constexpr ptrdiff_t buf_sz = buf_dk * buf_nk;
  unsigned char buf[buf_sz] __attribute__((__aligned__(sizeof(CCTK_REAL_VEC))));
  // TODO: Ensure the inner i loop covers at most one cache line; add
  // outer i loop if necessary
  // adapted from stencil_odd_dim3_dir1
  for (ptrdiff_t j = -fdop::stencil_radius;
       j < npoints_j + fdop::stencil_radius; ++j) {
    for (ptrdiff_t k = 0; k < npoints_k; ++k) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t buf_offset =
            i * buf_di + (j + fdop::stencil_radius) * buf_dj + k * buf_dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeffs[n + fdop::stencil_radius];
          s = kmadd(vec_set1(c), ksub(vec_load(getelt(u, offset + n * dk)),
                                      vec_load(getelt(u, offset - n * dk))),
                    s);
        }
        vec_store_nta_partial(getelt(buf, buf_offset), s);
      }
    }
  }
  // adapted from stencil_odd_dim3_dir1
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t buf_offset =
            i * buf_di + (j + fdop::stencil_radius) * buf_dj + k * buf_dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeffs[n + fdop::stencil_radius];
          s = kmadd(vec_set1(c),
                    ksub(vec_load(getelt(buf, buf_offset + n * buf_dj)),
                         vec_load(getelt(buf, buf_offset - n * buf_dj))),
                    s);
        }
        vec_store_nta_partial(getelt(du, offset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
void stencil_odd_dim3_dir2(const unsigned char *restrict const u,
                           unsigned char *restrict const du, const ptrdiff_t dj,
                           const ptrdiff_t dk) {
  stencil_odd_dim3_dir1<fdop, npoints_i, npoints_j, npoints_k>(u, du, dk, dj);
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
void stencil_even_dim3_dir2(const unsigned char *restrict const u,
                            unsigned char *restrict const du,
                            const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_even_dim3_dir1<fdop, npoints_i, npoints_j, npoints_k>(u, du, dk, dj);
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
void stencil_odd_dim3_dir02(const unsigned char *restrict const u,
                            unsigned char *restrict const du,
                            const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_odd_dim3_dir01<fdop, npoints_i, npoints_j, npoints_k>(u, du, dj, dk);
}

/******************************************************************************/
/* Discontinuous Galerkin (DG) differencing stencils */

// Stencil coefficients are passed via types. For example, this type
// defines a fourth order accurate first derivative:

/*
struct dgop_order4 {
  constexpr static const ptrdiff_t order = 4;
  typedef const CCTK_REAL
      coeffs_t[order + 3][alignup(order + 1, CCTK_REAL_VEC_SIZE)];
  constexpr static coeffs_t coeffs CCTK_ATTRIBUTE_ALIGNED(
      sizeof(CCTK_REAL_VEC)) = {
      {-5, 0, 0, 0, 0},
      {0, -1.24099025303098285784871934219, 0.375000000000000000000000000000,
       -0.259009746969017142151280657815, 0.50000000000000000000000000000},
      {6.7565024887242400038430275297, 0, -1.33658457769545333525484709817,
       0.76376261582597333443134119895, -1.41016417794242666282363913699},
      {-2.66666666666666666666666666667, 1.74574312188793905012877988332, 0,
       -1.74574312188793905012877988332, 2.66666666666666666666666666667},
      {1.41016417794242666282363913699, -0.76376261582597333443134119895,
       1.33658457769545333525484709817, 0, -6.7565024887242400038430275297},
      {-0.50000000000000000000000000000, 0.259009746969017142151280657815,
       -0.375000000000000000000000000000, 1.24099025303098285784871934219, 0},
      {0, 0, 0, 0, -5}};
};
constexpr dgop_order4::coeffs_t dgop_order4::coeffs;
*/

// Note that the explicit definition of the stencil coefficients after
// the type definition leads to better code (at least with GCC), as
// the compiler does not need to make separate copies of the stencil
// coefficients.

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE void load_dg_dim3(const unsigned char *restrict const u,
                                          unsigned char *restrict const du,
                                          const ptrdiff_t dj,
                                          const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t dni = alignup(dgop::order + 1, vs);
  constexpr ptrdiff_t dnj = dgop::order + 1;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k <= dgop::order; ++k) {
    for (ptrdiff_t j = 0; j <= dgop::order; ++j) {
      for (ptrdiff_t i = 0; i <= dgop::order; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        const CCTK_REAL_VEC s = vec_loadu(getelt(u, offset));
        vec_store(getelt(du, doffset), s);
      }
    }
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE void
store_dg_dim3(unsigned char *restrict const u,
              const unsigned char *restrict const du, const ptrdiff_t dj,
              const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t dni = alignup(dgop::order + 1, vs);
  constexpr ptrdiff_t dnj = dgop::order + 1;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k <= dgop::order; ++k) {
    for (ptrdiff_t j = 0; j <= dgop::order; ++j) {
      for (ptrdiff_t i = 0; i <= dgop::order; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, dgop::order + 1);
        const CCTK_REAL_VEC s = vec_load(getelt(du, doffset));
        vec_storeu_partial(getelt(u, offset), s);
      }
    }
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE void
stencil_dg_dim3_dir0(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const ptrdiff_t dj,
                     const ptrdiff_t dk) {
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  for (ptrdiff_t k = 0; k <= dgop::order; ++k) {
    for (ptrdiff_t j = 0; j <= dgop::order; ++j) {
      const ptrdiff_t offset0 = j * dj + k * dk;
      for (ptrdiff_t i = 0; i <= dgop::order; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        // vec_store_partial_prepare_fixed(i, 0, dgop::order + 1);
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = -1; n <= dgop::order + 1; ++n) {
          const CCTK_REAL_VEC cs = vec_load(dgop::coeffs[n + 1][i]);
          s = kmadd(cs, vec_set1(getelt(u, offset0 + n * di)), s);
        }
        vec_store(getelt(du, offset), s);
      }
    }
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE void
stencil_dg_dim3_dir1(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const ptrdiff_t dj,
                     const ptrdiff_t dk) {
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  for (ptrdiff_t k = 0; k <= dgop::order; ++k) {
    // unroll j loop
    repeat_n<0, dgop::order + 1>([=](ptrdiff_t j) {
      for (ptrdiff_t i = 0; i <= dgop::order; i += vs) {
        const ptrdiff_t offset0 = i * di + k * dk;
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        // vec_store_partial_prepare_fixed(i, 0, dgop::order + 1);
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = -1; n <= dgop::order + 1; ++n) {
          const CCTK_REAL c = dgop::coeffs[n + 1][j];
          if (c != 0.0) {
            s = kmadd(vec_set1(c), vec_load(getelt(u, offset0 + n * dj)), s);
          }
        }
        vec_store(getelt(du, offset), s);
      }
    });
  }
}

template <typename dgop>
void stencil_dg_dim3_dir2(const unsigned char *restrict const u,
                          unsigned char *restrict const du, const ptrdiff_t dj,
                          const ptrdiff_t dk) {
  stencil_dg_dim3_dir1<dgop>(u, du, dk, dj);
}

////////////////////////////////////////////////////////////////////////////////

// TODO: Auto-generate this, and don't declare it here, and change the name

// Note: These types are templates so that the static definitions of the
// coefficients can be placed here into a header file. Otherwise, they would
// need to go into a source file.

template <typename T> struct dgop_order4_nodeoffsets {
  constexpr static const ptrdiff_t order = 4;
  typedef const CCTK_REAL
      coeffs_t[order + 3][alignup(order + 1, CCTK_REAL_VEC_SIZE)];
  constexpr static coeffs_t coeffs
      CCTK_ATTRIBUTE_ALIGNED(sizeof(CCTK_REAL_VEC)) = {
          {0, 0, 0, 0, 0},
          {-1 - (-1), 0, 0, 0, 0},
          {0, -0.6546536707079771437982924562468583555692 - (-0.5), 0, 0, 0},
          {0, 0, 0 - (0), 0, 0},
          {0, 0, 0, 0.6546536707079771437982924562468583555692 - (0.5), 0},
          {0, 0, 0, 0, 1 - (1)},
          {0, 0, 0, 0, 0}};
};
template <typename T>
constexpr typename dgop_order4_nodeoffsets<T>::coeffs_t
    dgop_order4_nodeoffsets<T>::coeffs;

template <typename T> struct dgop_order4_weights {
  constexpr static const ptrdiff_t order = 4;
  typedef const CCTK_REAL
      coeffs_t[order + 3][alignup(order + 1, CCTK_REAL_VEC_SIZE)];
  constexpr static coeffs_t coeffs
      CCTK_ATTRIBUTE_ALIGNED(sizeof(CCTK_REAL_VEC)) = {
          {0, 0, 0, 0, 0},
          {0.100000000000000000000000000000, 0, 0, 0, 0},
          {0, 0.54444444444444444444444444444, 0, 0, 0},
          {0, 0, 0.71111111111111111111111111111, 0, 0},
          {0, 0, 0, 0.54444444444444444444444444444, 0},
          {0, 0, 0, 0, 0.100000000000000000000000000000},
          {0, 0, 0, 0, 0}};
};
template <typename T>
constexpr
    typename dgop_order4_weights<T>::coeffs_t dgop_order4_weights<T>::coeffs;

template <typename T> struct dgop_order4_derivs {
  constexpr static const ptrdiff_t order = 4;
  typedef const CCTK_REAL
      coeffs_t[order + 3][alignup(order + 1, CCTK_REAL_VEC_SIZE)];
  constexpr static coeffs_t coeffs CCTK_ATTRIBUTE_ALIGNED(
      sizeof(CCTK_REAL_VEC)) = {
      {-5, 0, 0, 0, 0},
      {0, -1.24099025303098285784871934219, 0.375000000000000000000000000000,
       -0.259009746969017142151280657815, 0.50000000000000000000000000000},
      {6.7565024887242400038430275297, 0, -1.33658457769545333525484709817,
       0.76376261582597333443134119895, -1.41016417794242666282363913699},
      {-2.66666666666666666666666666667, 1.74574312188793905012877988332, 0,
       -1.74574312188793905012877988332, 2.66666666666666666666666666667},
      {1.41016417794242666282363913699, -0.76376261582597333443134119895,
       1.33658457769545333525484709817, 0, -6.7565024887242400038430275297},
      {-0.50000000000000000000000000000, 0.259009746969017142151280657815,
       -0.375000000000000000000000000000, 1.24099025303098285784871934219, 0},
      {0, 0, 0, 0, -5}};
};
template <typename T>
constexpr
    typename dgop_order4_derivs<T>::coeffs_t dgop_order4_derivs<T>::coeffs;
}

#endif // #ifndef STENCIL_OPS_HH
