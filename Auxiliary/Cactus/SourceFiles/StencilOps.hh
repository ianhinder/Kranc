#ifndef STENCIL_OPS_HH
#define STENCIL_OPS_HH

#include <vectors.h>
#include <cctk.h>

#include <cstddef>
#include <type_traits>

static_assert(HAVE_CAPABILITY_Vectors, "");

namespace @THORN_NAME@ {
/******************************************************************************/
/* Convenient shortcuts */

using std::ptrdiff_t;
using std::size_t;

/******************************************************************************/
/* Evaluate an if condition at compile time */

template <bool C, typename F>
inline typename std::enable_if<!C, void>::type if_c(const F &f) {}

template <bool C, typename F>
inline typename std::enable_if<C, void>::type if_c(const F &f) {
  f();
}

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
// CCTK_REAL_VEC_SIZE (e.g. 4 CCTK_REALs for current Intel CPUs).

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
  unsigned char buf[buf_sz]
      __attribute__((__aligned__(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL))));
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
  constexpr ptrdiff_t buf_ni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t buf_nj = npoints_j + 2 * fdop::stencil_radius;
  constexpr ptrdiff_t buf_nk = npoints_k;
  constexpr ptrdiff_t buf_di = di;
  constexpr ptrdiff_t buf_dj = buf_di * buf_ni;
  constexpr ptrdiff_t buf_dk = buf_dj * buf_nj;
  constexpr ptrdiff_t buf_sz = buf_dk * buf_nk;
  unsigned char buf[buf_sz]
      __attribute__((__aligned__(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL))));
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
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL)) = {
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
  constexpr ptrdiff_t dni = alignup(dgop::order + 1, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dnj = dgop::order + 1;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < dgop::order + 1; ++k) {
    for (ptrdiff_t j = 0; j < dgop::order + 1; ++j) {
      for (ptrdiff_t i = 0; i < dgop::order + 1; i += vs) {
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
  constexpr ptrdiff_t dni = alignup(dgop::order + 1, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dnj = dgop::order + 1;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < dgop::order + 1; ++k) {
    for (ptrdiff_t j = 0; j < dgop::order + 1; ++j) {
      for (ptrdiff_t i = 0; i < dgop::order + 1; i += vs) {
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
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t dni = alignup(dgop::order + 1, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dnj = dgop::order + 1;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  static_assert(sizeof dgop::coeffs / sizeof *dgop::coeffs == dgop::order + 3,
                "");
  static_assert(sizeof *dgop::coeffs / sizeof **dgop::coeffs >= dgop::order + 1,
                "");
  for (ptrdiff_t k = 0; k < dgop::order + 1; ++k) {
    for (ptrdiff_t j = 0; j < dgop::order + 1; ++j) {
      const ptrdiff_t offset0 = j * dj + k * dk;
      for (ptrdiff_t i = 0; i < dgop::order + 1; i += vs) {
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = -1; n < dgop::order + 2; ++n) {
          // bool allzero = false;
          // for (ptrdiff_t ii = i; ii < i + vs; ++ii) {
          //   allzero = allzero && dgop::coeffs[n + 1][ii] == 0.0;
          // }
          // if (!allzero) {
          const CCTK_REAL_VEC cs = vec_load(dgop::coeffs[n + 1][i]);
          s = kmadd(cs, vec_set1(getelt(u, offset0 + n * di)), s);
          // }
        }
        vec_store(getelt(du, doffset), kmul(f, s));
      }
    }
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE void
stencil_dg_dim3_dir1(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t dni = alignup(dgop::order + 1, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dnj = dgop::order + 1;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  static_assert(sizeof dgop::coeffs / sizeof *dgop::coeffs == dgop::order + 3,
                "");
  static_assert(sizeof *dgop::coeffs / sizeof **dgop::coeffs >= dgop::order + 1,
                "");
  for (ptrdiff_t k = 0; k < dgop::order + 1; ++k) {
    // unroll j loop
    repeat_n<0, dgop::order + 1>([=](ptrdiff_t j) {
      for (ptrdiff_t i = 0; i < dgop::order + 1; i += vs) {
        const ptrdiff_t offset0 = i * di + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = -1; n < dgop::order + 2; ++n) {
          const CCTK_REAL c = dgop::coeffs[n + 1][j];
          if (c != 0.0) {
            // We could use an aligned access if grid functions are
            // suitably aligned, and if the element size is a multiple
            // of the vector size
            s = kmadd(vec_set1(c), vec_loadu(getelt(u, offset0 + n * dj)), s);
          }
        }
        vec_store(getelt(du, doffset), kmul(f, s));
      }
    });
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE void
stencil_dg_dim3_dir2(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  constexpr ptrdiff_t dni = alignup(dgop::order + 1, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dnj = dgop::order + 1;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  static_assert(sizeof dgop::coeffs / sizeof *dgop::coeffs == dgop::order + 3,
                "");
  static_assert(sizeof *dgop::coeffs / sizeof **dgop::coeffs >= dgop::order + 1,
                "");
  for (ptrdiff_t j = 0; j < dgop::order + 1; ++j) {
    // unroll k loop
    repeat_n<0, dgop::order + 1>([=](const ptrdiff_t k) {
      for (ptrdiff_t i = 0; i < dgop::order + 1; i += vs) {
        const ptrdiff_t offset0 = i * di + j * dj;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        CCTK_REAL_VEC s = vec_set1(0.0);
        for (ptrdiff_t n = -1; n < dgop::order + 2; ++n) {
          const CCTK_REAL c = dgop::coeffs[n + 1][k];
          if (c != 0.0) {
            // We could use an aligned access if grid functions are
            // suitably aligned, and if the element size is a multiple
            // of the vector size
            s = kmadd(vec_set1(c), vec_loadu(getelt(u, offset0 + n * dk)), s);
          }
        }
        vec_store(getelt(du, doffset), kmul(f, s));
      }
    });
  }
}

// template <typename dgop>
// void stencil_dg_dim3_dir2(const unsigned char *restrict const u,
//                           unsigned char *restrict const du,
//                           const CCTK_REAL_VEC f, const ptrdiff_t dj,
//                           const ptrdiff_t dk) {
//   stencil_dg_dim3_dir1<dgop>(u, du, f, dk, dj);
// }

////////////////////////////////////////////////////////////////////////////////

// TODO: Auto-generate this, and don't declare it here, and change the names

// Note: These types are templates so that the static definitions of the
// coefficients can be placed here into a header file. Otherwise, they would
// need to go into a source file.

// Should we change the normalization to include a factor 2/(order+1)?

template <std::size_t P, typename T> struct dgop_derivs;

template <typename T> struct dgop_derivs<2, T> {
  constexpr static const ptrdiff_t order = 2;
  typedef const CCTK_REAL
      coeffs_t[order + 3][alignup(order + 1, CCTK_REAL_VEC_SIZE)];
  constexpr static coeffs_t coeffs
      CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                             sizeof(CCTK_REAL)) = {{-1.5, 0, 0},
                                                   {0, -0.5, 0.5},
                                                   {2.0, 0, -2.0},
                                                   {-0.5, 0.5, 0},
                                                   {0, 0, 1.5}};
};
template <typename T>
constexpr typename dgop_derivs<2, T>::coeffs_t dgop_derivs<2, T>::coeffs;

template <typename T> struct dgop_derivs<4, T> {
  constexpr static const ptrdiff_t order = 4;
  typedef const CCTK_REAL
      coeffs_t[order + 3][alignup(order + 1, CCTK_REAL_VEC_SIZE)];
  constexpr static coeffs_t coeffs CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                          sizeof(CCTK_REAL)) = {
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
      {0, 0, 0, 0, 5}};
};
template <typename T>
constexpr typename dgop_derivs<4, T>::coeffs_t dgop_derivs<4, T>::coeffs;

template <typename T> struct dgop_derivs<6, T> {
  constexpr static const ptrdiff_t order = 6;
  typedef const CCTK_REAL
      coeffs_t[order + 3][alignup(order + 1, CCTK_REAL_VEC_SIZE)];
  constexpr static coeffs_t coeffs CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                          sizeof(CCTK_REAL)) = {
      {-10.5, 0, 0, 0, 0, 0, 0},
      {0, -2.44292601424428961264353508344, 0.62525666551534211425476493044,
       -0.312500000000000000000000000000, 0.226099400942574651735810251280,
       -0.226611870395445335165221916464, 0.50000000000000000000000000000},
      {14.2015766029198161670816977260, 0, -2.21580428316997131363803034549,
       0.90754447126882091882027943320, -0.61639083551757952864611929469,
       0.60224717963578568466640341307, -1.31737343570243448845609847333},
      {-5.6689852255455078785754090169, 3.45582821429428513260129789155, 0,
       -2.00696924058875308957204950201, 1.06644190400637468973550663446,
       -0.96133979728871166671127688438, 2.04996481307674277696238718244},
      {3.20000000000000000000000000000, -1.59860668809836683716785819589,
       2.26669808708599901220846300609, 0, -2.26669808708599901220846300609,
       1.59860668809836683716785819589, -3.20000000000000000000000000000},
      {-2.04996481307674277696238718244, 0.96133979728871166671127688438,
       -1.06644190400637468973550663446, 2.00696924058875308957204950201, 0,
       -3.45582821429428513260129789155, 5.6689852255455078785754090169},
      {1.31737343570243448845609847333, -0.60224717963578568466640341307,
       0.61639083551757952864611929469, -0.90754447126882091882027943320,
       2.21580428316997131363803034549, 0, -14.2015766029198161670816977260},
      {-0.50000000000000000000000000000, 0.226611870395445335165221916464,
       -0.226099400942574651735810251280, 0.312500000000000000000000000000,
       -0.62525666551534211425476493044, 2.44292601424428961264353508344, 0},
      {0, 0, 0, 0, 0, 0, 10.5}};
};
template <typename T>
constexpr typename dgop_derivs<6, T>::coeffs_t dgop_derivs<6, T>::coeffs;

template <typename T> struct dgop_derivs<8, T> {
  constexpr static const ptrdiff_t order = 8;
  typedef const CCTK_REAL
      coeffs_t[order + 3][alignup(order + 1, CCTK_REAL_VEC_SIZE)];
  constexpr static coeffs_t coeffs
      CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL)) = {
          {-18, 0, 0, 0, 0, 0, 0, 0, 0},
          {0, -4.0870137020336765889793098481, 0.98536009007450695190732750513,
           -0.44461344928109063495515603300, 0.273437500000000000000000000000,
           -0.207734512035597178904197341203, 0.189655591978356440316554839705,
           -0.215654018702498989385219122479, 0.50000000000000000000000000000},
          {24.3497451715930658264745651379, 0, -3.4883587534344548411598315601,
           1.28796075006390654800826405148, -0.74178239791625424057639927034,
           0.54730016053405140028685858270, -0.49235093831550741871977538918,
           0.55570498128371678540266599324, -1.28483063269958833334100425314},
          {-9.7387016572115472650292513563, 5.7868058166373116740903723405, 0,
           -2.83445891207942039532716103713, 1.26941308635814953242157409323,
           -0.85572618509267540469369404148, 0.73834927719038611665282848824,
           -0.81675638174138587811723062895, 1.87444087344698324367585844334},
          {5.5449639069493784995607676809, -2.69606544031405602899382047687,
           3.5766809401256153210044855557, 0, -2.65931021757391799292835532816,
           1.37696489376051208934447138421, -1.07980381128263048444543497939,
           1.14565373845513231901250100842, -2.59074567655935499218591558478},
          {-3.6571428571428571428571428571, 1.66522164500538518079547523473,
           -1.71783215719506277794780854135, 2.85191596846289538830749160288, 0,
           -2.85191596846289538830749160288, 1.71783215719506277794780854135,
           -1.66522164500538518079547523473, 3.6571428571428571428571428571},
          {2.59074567655935499218591558478, -1.14565373845513231901250100842,
           1.07980381128263048444543497939, -1.37696489376051208934447138421,
           2.65931021757391799292835532816, 0, -3.5766809401256153210044855557,
           2.69606544031405602899382047687, -5.5449639069493784995607676809},
          {-1.87444087344698324367585844334, 0.81675638174138587811723062895,
           -0.73834927719038611665282848824, 0.85572618509267540469369404148,
           -1.26941308635814953242157409323, 2.83445891207942039532716103713, 0,
           -5.7868058166373116740903723405, 9.7387016572115472650292513563},
          {1.28483063269958833334100425314, -0.55570498128371678540266599324,
           0.49235093831550741871977538918, -0.54730016053405140028685858270,
           0.74178239791625424057639927034, -1.28796075006390654800826405148,
           3.4883587534344548411598315601, 0, -24.3497451715930658264745651379},
          {-0.50000000000000000000000000000, 0.215654018702498989385219122479,
           -0.189655591978356440316554839705, 0.207734512035597178904197341203,
           -0.273437500000000000000000000000, 0.44461344928109063495515603300,
           -0.98536009007450695190732750513, 4.0870137020336765889793098481, 0},
          {0, 0, 0, 0, 0, 0, 0, 0, 18}};
};
template <typename T>
constexpr typename dgop_derivs<8, T>::coeffs_t dgop_derivs<8, T>::coeffs;
}

#endif // #ifndef STENCIL_OPS_HH
