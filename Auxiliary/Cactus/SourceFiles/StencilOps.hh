#ifndef STENCIL_OPS_HH
#define STENCIL_OPS_HH

#include <cctk.h>
#include <vectors.h>

#include <cassert>
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

template <bool C, typename F, typename G>
inline typename std::enable_if<C, void>::type cond(const F &f, const G &g) {
  f();
}

template <bool C, typename F, typename G>
inline typename std::enable_if<!C, void>::type cond(const F &f, const G &g) {
  g();
}

/******************************************************************************/
/* Explicitly unroll a loop */

// The loop ranges from I (inclusively) to N (exclusively) with a step
// size of S (must be positive, defaults to 1). The loop body is
// expected to be function object, such as a lamda expression.

template <ptrdiff_t I, ptrdiff_t N, ptrdiff_t S = 1, typename F>
inline typename std::enable_if<(I >= N), void>::type loop(const F &f) {}

template <ptrdiff_t I, ptrdiff_t N, ptrdiff_t S = 1, typename F>
inline typename std::enable_if<(I < N), void>::type loop(const F &f) {
  f(I);
  loop<I + S, N, S>(f);
}

#if 0
// Hierarchical loop decomposition (possibly better for superscalar parallelism)
// Note: This algorithm is broken, since kmadd implicily enforces a linear
// evaluation order

template <ptrdiff_t I, ptrdiff_t N, ptrdiff_t S = 1, typename R, typename F,
          typename Op>
inline typename std::enable_if<(I >= N), R>::type
mapreduce(const F &f, const Op &op, const R &z) {
  return z;
}

template <ptrdiff_t I, ptrdiff_t N, ptrdiff_t S = 1, typename R, typename F,
          typename Op>
inline typename std::enable_if<(I + S >= N && I < N), R>::type
mapreduce(const F &f, const Op &op, const R &z) {
  return f(I);
}

template <ptrdiff_t I, ptrdiff_t N, ptrdiff_t S = 1, typename R, typename F,
          typename Op>
inline typename std::enable_if<(I + S < N), R>::type
mapreduce(const F &f, const Op &op, const R &z) {
  constexpr ptrdiff_t J = I + (N - I) / S / 2 * S;
  static_assert(J >= I && J < N, "");
  R r1 = mapreduce<I, J, S>(f, z);
  R r2 = mapreduce<J, N, S>(f, z);
  return op(r1, r2);
}
#endif

template <ptrdiff_t I, ptrdiff_t N, ptrdiff_t S = 1, typename F>
inline typename std::enable_if<(I >= N), bool>::type all_true(const F &f) {
  return true;
}

template <ptrdiff_t I, ptrdiff_t N, ptrdiff_t S = 1, typename F>
inline typename std::enable_if<(I < N), bool>::type all_true(const F &f) {
  return f(I) && all_true<I + S, N, S>(f);
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
  return *(const CCTK_REAL *restrict) & ptr[idx];
}
constexpr CCTK_REAL &getelt(unsigned char *restrict const ptr,
                            const ptrdiff_t idx) {
  return *(CCTK_REAL * restrict) & ptr[idx];
}

/******************************************************************************/
/* Finite differencing stencils (always using grid functions) */

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
  constexpr static const coeffs_t coeffs =
  {-1.0 / 12.0, 8.0 / 12.0, 0, -8.0 / 12.0, 1.0 / 12.0};
  };
  constexpr typename fdop_order4_d1::coeffs_t fdop_order4_d1::coeffs;

  struct fdop_order4_d2 {
  constexpr static const ptrdiff_t stencil_radius = 2;
  typedef const CCTK_REAL coeffs_t[2 * stencil_radius + 1];
  constexpr static const coeffs_t coeffs =
  {-1.0 / 12.0, 16.0 / 12.0, -30.0 / 12.0, 16.0 / 12.0, -1.0 / 12.0};
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

// Note that the stencils are evaluated symmetrically, which reduces
// the number of floating-point operations. Note that mixed
// derivatives are evaluated in two stages, which also reduces the
// necessary number of floating-point operations.

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
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
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
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
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
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
          s = kmadd(vec_set1(c),
                    ksub(vec_load(getelt(u, offset + n * dj)),
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
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
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
          s = kmadd(vec_set1(c),
                    kadd(vec_load(getelt(u, offset - n * dj)),
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
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
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
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
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
          s = kmadd(vec_set1(c),
                    ksub(vec_load(getelt(u, offset + n * dk)),
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
inline void stencil_odd_dim3_dir2(const unsigned char *restrict const u,
                                  unsigned char *restrict const du,
                                  const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_odd_dim3_dir1<fdop, npoints_i, npoints_j, npoints_k>(u, du, dk, dj);
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
inline void stencil_even_dim3_dir2(const unsigned char *restrict const u,
                                   unsigned char *restrict const du,
                                   const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_even_dim3_dir1<fdop, npoints_i, npoints_j, npoints_k>(u, du, dk, dj);
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
inline void stencil_odd_dim3_dir02(const unsigned char *restrict const u,
                                   unsigned char *restrict const du,
                                   const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_odd_dim3_dir01<fdop, npoints_i, npoints_j, npoints_k>(u, du, dj, dk);
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
  constexpr static const coeffs_t coeffs =
  {-1.0 / 12.0, 8.0 / 12.0, 0, -8.0 / 12.0, 1.0 / 12.0};
  };
  constexpr typename fdop_order4_d1::coeffs_t fdop_order4_d1::coeffs;

  struct fdop_order4_d2 {
  constexpr static const ptrdiff_t stencil_radius = 2;
  typedef const CCTK_REAL coeffs_t[2 * stencil_radius + 1];
  constexpr static const coeffs_t coeffs =
  {-1.0 / 12.0, 16.0 / 12.0, -30.0 / 12.0, 16.0 / 12.0, -1.0 / 12.0};
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

// Note that the stencils are evaluated symmetrically, which reduces
// the number of floating-point operations. Note that mixed
// derivatives are evaluated in two stages, which also reduces the
// necessary number of floating-point operations.

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
load_fd_dim3(const unsigned char *restrict const u,
             unsigned char *restrict const du, const ptrdiff_t dj,
             const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = npoints_i;
  constexpr ptrdiff_t dnj = npoints_j;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        const CCTK_REAL_VEC s = vec_loadu(getelt(u, offset));
        // vec_store(getelt(du, doffset), s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
store_fd_dim3(unsigned char *restrict const u,
              const unsigned char *restrict const du, const ptrdiff_t dj,
              const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = npoints_i;
  constexpr ptrdiff_t dnj = npoints_j;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        // const CCTK_REAL_VEC s = vec_load(getelt(du, doffset));
        const CCTK_REAL_VEC s =
            vec_loadu_maybe3(i, npoints_j, npoints_k, getelt(du, doffset));
        vec_storeu_partial(getelt(u, offset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<fdop::antisymmetric, void>::type
stencil_fd_dim3_dir0(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  static_assert(fdop::antisymmetric, "");
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = npoints_i;
  constexpr ptrdiff_t dnj = npoints_j;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
#if 0
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c), ksub(vec_loadu(getelt(u, offset + n * di)),
                                      vec_loadu(getelt(u, offset - n * di))),
                    s);
        }
#else
        loop<1, fdop::stencil_radius + 1>([&](ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    ksub(vec_loadu(getelt(u, offset + n * di)),
                                      vec_loadu(getelt(u, offset - n * di))),
                    s);
        });
#endif
        s = kmul(f, s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<fdop::antisymmetric, void>::type
stencil_fd_dim3_dir1(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  static_assert(fdop::antisymmetric, "");
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = npoints_i;
  constexpr ptrdiff_t dnj = npoints_j;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
#if 0
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c), ksub(vec_loadu(getelt(u, offset + n * dj)),
                                      vec_loadu(getelt(u, offset - n * dj))),
                    s);
        }
#else
        loop<1, fdop::stencil_radius + 1>([&](ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    ksub(vec_loadu(getelt(u, offset + n * dj)),
                                      vec_loadu(getelt(u, offset - n * dj))),
                    s);
        });
#endif
        s = kmul(f, s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
inline typename std::enable_if<fdop::antisymmetric, void>::type
stencil_fd_dim3_dir2(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_fd_dim3_dir1<fdop, npoints_i, npoints_k, npoints_j>(u, du, f, dk, dj);
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<fdop::symmetric, void>::type
stencil_fd_dim3_dir0(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  static_assert(fdop::symmetric, "");
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = npoints_i;
  constexpr ptrdiff_t dnj = npoints_j;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s =
            kmul(vec_set1(fdop::coeff(0)), vec_loadu(getelt(u, offset)));
#if 0
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c), kadd(vec_loadu(getelt(u, offset - n * di)),
                                      vec_loadu(getelt(u, offset + n * di))),
                    s);
        }
#else
        loop<1, fdop::stencil_radius + 1>([&](ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    kadd(vec_loadu(getelt(u, offset - n * di)),
                                      vec_loadu(getelt(u, offset + n * di))),
                    s);
        });
#endif
        s = kmul(f, s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<fdop::symmetric, void>::type
stencil_fd_dim3_dir1(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  static_assert(fdop::symmetric, "");
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = npoints_i;
  constexpr ptrdiff_t dnj = npoints_j;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s =
            kmul(vec_set1(fdop::coeff(0)), vec_loadu(getelt(u, offset)));
#if 0
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c), kadd(vec_loadu(getelt(u, offset - n * dj)),
                                      vec_loadu(getelt(u, offset + n * dj))),
                    s);
        }
#else
        loop<1, fdop::stencil_radius + 1>([&](ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    kadd(vec_loadu(getelt(u, offset - n * dj)),
                                      vec_loadu(getelt(u, offset + n * dj))),
                    s);
        });
#endif
        s = kmul(f, s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
inline typename std::enable_if<fdop::symmetric, void>::type
stencil_fd_dim3_dir2(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_fd_dim3_dir1<fdop, npoints_i, npoints_k, npoints_j>(u, du, f, dk, dj);
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<fdop::antisymmetric, void>::type
stencil_fd_dim3_dir01(const unsigned char *restrict const u,
                      unsigned char *restrict const du, const CCTK_REAL_VEC f,
                      const ptrdiff_t dj, const ptrdiff_t dk) {
  static_assert(fdop::antisymmetric, "");
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = npoints_i;
  constexpr ptrdiff_t dnj = npoints_j;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  constexpr ptrdiff_t buf_ni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t buf_nj = npoints_j + 2 * fdop::stencil_radius;
  constexpr ptrdiff_t buf_di = di;
  constexpr ptrdiff_t buf_dj = buf_di * buf_ni;
  constexpr ptrdiff_t buf_sz = buf_dj * buf_nj;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    unsigned char buf[buf_sz]
        __attribute__((__aligned__(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL))));
    // adapted from stencil_fd_odd_dim3_dir0
    for (ptrdiff_t j = -fdop::stencil_radius;
         j < npoints_j + fdop::stencil_radius; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t buf_offset =
            i * buf_di + (j + fdop::stencil_radius) * buf_dj;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
#if 0
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c), ksub(vec_loadu(getelt(u, offset + n * di)),
                                      vec_loadu(getelt(u, offset - n * di))),
                    s);
        }
#else
        loop<1, fdop::stencil_radius + 1>([&](ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    ksub(vec_loadu(getelt(u, offset + n * di)),
                                      vec_loadu(getelt(u, offset - n * di))),
                    s);
        });
#endif
        vec_store_nta_partial(getelt(buf, buf_offset), s);
      }
    }
    // adapted from stencil_fd_odd_dim3_dir1
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        const ptrdiff_t buf_offset =
            i * buf_di + (j + fdop::stencil_radius) * buf_dj;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
#if 0
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    ksub(vec_load(getelt(buf, buf_offset + n * buf_dj)),
                         vec_load(getelt(buf, buf_offset - n * buf_dj))),
                    s);
        }
#else
        loop<1, fdop::stencil_radius + 1>([&](ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    ksub(vec_load(getelt(buf, buf_offset + n * buf_dj)),
                         vec_load(getelt(buf, buf_offset - n * buf_dj))),
                    s);
        });
#endif
        s = kmul(f, s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    }
  }
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
inline typename std::enable_if<fdop::antisymmetric, void>::type
stencil_fd_dim3_dir02(const unsigned char *restrict const u,
                      unsigned char *restrict const du, const CCTK_REAL_VEC f,
                      const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_fd_dim3_dir01<fdop, npoints_i, npoints_k, npoints_j>(u, du, f, dk,
                                                               dj);
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<fdop::antisymmetric, void>::type
stencil_fd_dim3_dir12(const unsigned char *restrict const u,
                      unsigned char *restrict const du, const CCTK_REAL_VEC f,
                      const ptrdiff_t dj, const ptrdiff_t dk) {
  static_assert(fdop::antisymmetric, "");
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = npoints_i;
  constexpr ptrdiff_t dnj = npoints_j;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  constexpr ptrdiff_t buf_ni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t buf_nj = npoints_j;
  constexpr ptrdiff_t buf_nk = npoints_k + 2 * fdop::stencil_radius;
  constexpr ptrdiff_t buf_di = di;
  constexpr ptrdiff_t buf_dj = buf_di * buf_ni;
  constexpr ptrdiff_t buf_dk = buf_dj * buf_nj;
  constexpr ptrdiff_t buf_sz = buf_dk * buf_nk;
  unsigned char buf[buf_sz]
      __attribute__((__aligned__(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL))));
  // TODO: Ensure the inner i loop covers at most one cache line; add
  // outer i loop if necessary
  // adapted from stencil_odd_dim3_dir1
  for (ptrdiff_t k = -fdop::stencil_radius;
       k < npoints_k + fdop::stencil_radius; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t buf_offset =
            i * buf_di + j * buf_dj + (k + fdop::stencil_radius) * buf_dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
#if 0
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c), ksub(vec_loadu(getelt(u, offset + n * dj)),
                                      vec_loadu(getelt(u, offset - n * dj))),
                    s);
        }
#else
        loop<1, fdop::stencil_radius + 1>([&](ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    ksub(vec_loadu(getelt(u, offset + n * dj)),
                                      vec_loadu(getelt(u, offset - n * dj))),
                    s);
        });
#endif
        vec_store_nta_partial(getelt(buf, buf_offset), s);
      }
    }
  }
  // adapted from stencil_odd_dim3_dir2
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        const ptrdiff_t buf_offset =
            i * buf_di + j * buf_dj + (k + fdop::stencil_radius) * buf_dk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC s = vec_set1(0.0);
#if 0
        for (ptrdiff_t n = 1; n <= fdop::stencil_radius; ++n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    ksub(vec_load(getelt(buf, buf_offset + n * buf_dk)),
                         vec_load(getelt(buf, buf_offset - n * buf_dk))),
                    s);
        }
#else
        loop<1, fdop::stencil_radius + 1>([&](ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          s = kmadd(vec_set1(c),
                    ksub(vec_load(getelt(buf, buf_offset + n * buf_dk)),
                         vec_load(getelt(buf, buf_offset - n * buf_dk))),
                    s);
        });
#endif
        s = kmul(f, s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////

#if 0
template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<fdop::antisymmetric, void>::type
stencil_fd_dim3(const unsigned char *restrict const u,
                unsigned char *restrict const dxu,
                unsigned char *restrict const dyu,
                unsigned char *restrict const dzu, const CCTK_REAL_VEC fx,
                const CCTK_REAL_VEC fy, const CCTK_REAL_VEC fz,
                const ptrdiff_t dj, const ptrdiff_t dk) {
  static_assert(fdop::antisymmetric, "");
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(npoints_i, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = npoints_i;
  constexpr ptrdiff_t dnj = npoints_j;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC sx = vec_set1(0.0);
        loop<1, fdop::stencil_radius + 1>([&](const ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          sx = kmadd(vec_set1(c), ksub(vec_loadu(getelt(u, offset + n * di)),
                                       vec_loadu(getelt(u, offset - n * di))),
                     sx);
        });
        sx = kmul(fx, sx);
        dni % vs == 0 ? vec_store_nta_partial(getelt(dxu, doffset), sx)
                      : vec_storeu_partial(getelt(dxu, doffset), sx);
      }
    }
  }
  for (ptrdiff_t k = 0; k < npoints_k; ++k) {
    for (ptrdiff_t j = 0; j < npoints_j; ++j) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC sy = vec_set1(0.0);
        loop<1, fdop::stencil_radius + 1>([&](const ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          sy = kmadd(vec_set1(c), ksub(vec_loadu(getelt(u, offset + n * dj)),
                                       vec_loadu(getelt(u, offset - n * dj))),
                     sy);
        });
        sy = kmul(fy, sy);
        dni % vs == 0 ? vec_store_nta_partial(getelt(dyu, doffset), sy)
                      : vec_storeu_partial(getelt(dyu, doffset), sy);
      }
    }
  }
  for (ptrdiff_t j = 0; j < npoints_j; ++j) {
    for (ptrdiff_t k = 0; k < npoints_k; ++k) {
      for (ptrdiff_t i = 0; i < npoints_i; i += vs) {
        const ptrdiff_t offset = i * di + j * dj + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, npoints_i);
        CCTK_REAL_VEC sz = vec_set1(0.0);
        loop<1, fdop::stencil_radius + 1>([&](const ptrdiff_t n) {
          const CCTK_REAL c = fdop::coeff(n);
          sz = kmadd(vec_set1(c), ksub(vec_loadu(getelt(u, offset + n * dk)),
                                       vec_loadu(getelt(u, offset - n * dk))),
                     sz);
        });
        sz = kmul(fz, sz);
        dni % vs == 0 ? vec_store_nta_partial(getelt(dzu, doffset), sz)
                      : vec_storeu_partial(getelt(dzu, doffset), sz);
      }
    }
  }
}
#endif

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
// CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<fdop::antisymmetric, void>::type stencil_fd_dim3(
    const unsigned char *restrict const u, unsigned char *restrict const dxu,
    unsigned char *restrict const dyu, unsigned char *restrict const dzu,
    const CCTK_REAL_VEC fx, const CCTK_REAL_VEC fy, const CCTK_REAL_VEC fz,
    const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_fd_dim3_dir0<fdop, npoints_i, npoints_j, npoints_k>(u, dxu, fx, dj,
                                                              dk);
  stencil_fd_dim3_dir1<fdop, npoints_i, npoints_j, npoints_k>(u, dyu, fy, dj,
                                                              dk);
  stencil_fd_dim3_dir2<fdop, npoints_i, npoints_j, npoints_k>(u, dzu, fz, dj,
                                                              dk);
}

template <typename fdop, ptrdiff_t npoints_i, ptrdiff_t npoints_j,
          ptrdiff_t npoints_k>
// CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<fdop::symmetric, void>::type stencil_fd_dim3(
    const unsigned char *restrict const u, unsigned char *restrict const dxu,
    unsigned char *restrict const dyu, unsigned char *restrict const dzu,
    const CCTK_REAL_VEC fx, const CCTK_REAL_VEC fy, const CCTK_REAL_VEC fz,
    const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_fd_dim3_dir0<fdop, npoints_i, npoints_j, npoints_k>(u, dxu, fx, dj,
                                                              dk);
  stencil_fd_dim3_dir1<fdop, npoints_i, npoints_j, npoints_k>(u, dyu, fy, dj,
                                                              dk);
  stencil_fd_dim3_dir2<fdop, npoints_i, npoints_j, npoints_k>(u, dzu, fz, dj,
                                                              dk);
}

template <typename fdop1, typename fdop2, ptrdiff_t npoints_i,
          ptrdiff_t npoints_j, ptrdiff_t npoints_k>
// CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__))
typename std::enable_if<(fdop1::antisymmetric && fdop2::symmetric), void>::type
stencil_fd_dim3(
    const unsigned char *restrict const u, unsigned char *restrict const dxu,
    unsigned char *restrict const dyu, unsigned char *restrict const dzu,
    unsigned char *restrict const dxxu, unsigned char *restrict const dxyu,
    unsigned char *restrict const dxzu, unsigned char *restrict const dyyu,
    unsigned char *restrict const dyzu, unsigned char *restrict const dzzu,
    const CCTK_REAL_VEC fx, const CCTK_REAL_VEC fy, const CCTK_REAL_VEC fz,
    const CCTK_REAL_VEC fxx, const CCTK_REAL_VEC fxy, const CCTK_REAL_VEC fxz,
    const CCTK_REAL_VEC fyy, const CCTK_REAL_VEC fyz, const CCTK_REAL_VEC fzz,
    const ptrdiff_t dj, const ptrdiff_t dk) {
  stencil_fd_dim3<fdop1, npoints_i, npoints_j, npoints_k>(u, dxu, dyu, dzu, fx,
                                                          fy, fz, dj, dk);
  stencil_fd_dim3_dir0<fdop2, npoints_i, npoints_j, npoints_k>(u, dxxu, fxx, dj,
                                                               dk);
  stencil_fd_dim3_dir01<fdop1, npoints_i, npoints_j, npoints_k>(u, dxyu, fxy,
                                                                dj, dk);
  stencil_fd_dim3_dir02<fdop1, npoints_i, npoints_j, npoints_k>(u, dxzu, fxz,
                                                                dj, dk);
  stencil_fd_dim3_dir1<fdop2, npoints_i, npoints_j, npoints_k>(u, dyyu, fyy, dj,
                                                               dk);
  stencil_fd_dim3_dir12<fdop1, npoints_i, npoints_j, npoints_k>(u, dyzu, fyz,
                                                                dj, dk);
  stencil_fd_dim3_dir2<fdop2, npoints_i, npoints_j, npoints_k>(u, dzzu, fzz, dj,
                                                               dk);
}

////////////////////////////////////////////////////////////////////////////////

// TODO: Auto-generate this, and don't declare it here, and change the names

template <std::size_t P> struct fdop_deriv;
template <std::size_t P> struct fdop_deriv2;

// Mattsson, Svärd, Norström, "Stable and Accurate Artificial
// Dissipation", Journal of Scientific Computing 21, page 57, 2004
template <std::size_t P> struct fdop_diss;

// order 2

template <> struct fdop_deriv<2> {
  constexpr static const ptrdiff_t order = 2;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool antisymmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {-1.0 / 2.0, 0, 1.0 / 2.0};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

template <> struct fdop_deriv2<2> {
  constexpr static const ptrdiff_t order = 2;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool symmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {1.0, -2.0, 1.0};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

template <> struct fdop_diss<2> {
  constexpr static const ptrdiff_t order = 2;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool symmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {1, -2, 1};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

// order 4

template <> struct fdop_deriv<4> {
  constexpr static const ptrdiff_t order = 4;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool antisymmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {1.0 / 12.0, -2.0 / 3.0, 0,
                                            2.0 / 3.0, -1.0 / 12.0};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

template <> struct fdop_deriv2<4> {
  constexpr static const ptrdiff_t order = 4;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool symmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {-1.0 / 12.0, 4.0 / 3.0, -5.0 / 2.0,
                                            4.0 / 3.0, -1.0 / 12.0};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

template <> struct fdop_diss<4> {
  constexpr static const ptrdiff_t order = 4;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool symmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {1, -4, 6, -4, 1};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

// order 6

template <> struct fdop_deriv<6> {
  constexpr static const ptrdiff_t order = 6;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool antisymmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {
        -1.0 / 60.0, 3.0 / 20.0,  -3.0 / 4.0, 0,
        3.0 / 4.0,   -3.0 / 20.0, 1.0 / 60.0};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

template <> struct fdop_deriv2<6> {
  constexpr static const ptrdiff_t order = 6;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool symmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {
        1.0 / 90.0, -3.0 / 20.0, 3.0 / 2.0, -49.0 / 18.0,
        3.0 / 2.0,  -3.0 / 20.0, 1.0 / 90.0};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

template <> struct fdop_diss<6> {
  constexpr static const ptrdiff_t order = 6;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool symmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {1, -6, 15, -20, 15, -6, 1};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

// order 8

template <> struct fdop_deriv<8> {
  constexpr static const ptrdiff_t order = 8;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool antisymmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {
        1.0 / 280.0, -4.0 / 105.0, 1.0 / 5.0,   -4.0 / 5.0,  0,
        4.0 / 5.0,   -1.0 / 5.0,   4.0 / 105.0, -1.0 / 280.0};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

template <> struct fdop_deriv2<8> {
  constexpr static const ptrdiff_t order = 8;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool symmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {
        -1.0 / 560.0, 8.0 / 315.0, -1.0 / 5.0,  8.0 / 5.0,   -205.0 / 72.0,
        8.0 / 5.0,    -1.0 / 5.0,  8.0 / 315.0, -1.0 / 560.0};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

template <> struct fdop_diss<8> {
  constexpr static const ptrdiff_t order = 8;
  constexpr static const ptrdiff_t stencil_radius = order / 2;
  constexpr static const ptrdiff_t nmin = -stencil_radius;
  constexpr static const ptrdiff_t nmax = stencil_radius + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs] CCTK_ATTRIBUTE_ALIGNED(
      CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));
  constexpr static const bool symmetric = true;
  // n: stencil point
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n) {
    constexpr static coeffs_t the_coeffs = {1, -8, 28, -56, 70, -56, 28, -8, 1};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    // assert(n >= nmin && n < nmax);
    return the_coeffs[n - nmin];
  }
};

/******************************************************************************/
/* Discontinuous Galerkin (DG) differencing stencils */

// Stencil coefficients are passed via types. For example, this type
// defines a fourth order accurate first derivative:

/*
template <> struct dgop_derivs<4> {
  constexpr static const ptrdiff_t order = 4;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = -1;
  constexpr static const ptrdiff_t nmax = npoints + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
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
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};
*/

// Note that the explicit definition of the stencil coefficients after
// the type definition leads to better code (at least with GCC), as
// the compiler does not need to make separate copies of the stencil
// coefficients.

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
load_dg_dim3(const unsigned char *restrict const u,
             unsigned char *restrict const du, const ptrdiff_t dj,
             const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(dgop::order + 1, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = dgop::order + 1;
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
        const CCTK_REAL_VEC s = vec_loadu(getelt(u, offset));
        // vec_store(getelt(du, doffset), s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    }
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
store_dg_dim3(unsigned char *restrict const u,
              const unsigned char *restrict const du, const ptrdiff_t dj,
              const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(dgop::order + 1, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = dgop::order + 1;
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
        // const CCTK_REAL_VEC s = vec_load(getelt(du, doffset));
        const CCTK_REAL_VEC s = vec_loadu_maybe3(
            i, dgop::order + 1, dgop::order + 1, getelt(du, doffset));
        vec_storeu_partial(getelt(u, offset), s);
      }
    }
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
stencil_dg_dim3_dir0(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(dgop::npoints, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = dgop::npoints;
  constexpr ptrdiff_t dnj = dgop::npoints;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < dgop::npoints; ++k) {
    // unroll i loop
    for (ptrdiff_t j = 0; j < dgop::npoints; ++j) {
      const ptrdiff_t offset0 = j * dj + k * dk;
      loop<0, dgop::npoints, vs>([&](ptrdiff_t i) {
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, dgop::npoints);
        CCTK_REAL_VEC s = vec_set1(0.0);
#if 0
        for (ptrdiff_t n = dgop::nmin; n < dgop::nmax; ++n) {
          const CCTK_REAL_VEC cs = vec_load(dgop::coeff(n, i));
          s = kmadd(cs, vec_set1(getelt(u, offset0 + n * di)), s);
        }
#else
        loop<dgop::nmin, dgop::nmax>([&](ptrdiff_t n) {
          const CCTK_REAL_VEC cs = vec_load(dgop::coeff(n, i));
          s = kmadd(cs, vec_set1(getelt(u, offset0 + n * di)), s);
        });
#endif
        s = kmul(f, s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      });
    }
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
stencil_dg_dim3_dir1(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(dgop::npoints, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = dgop::npoints;
  constexpr ptrdiff_t dnj = dgop::npoints;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t k = 0; k < dgop::npoints; ++k) {
    // unroll j loop
    loop<0, dgop::npoints>([&](ptrdiff_t j) {
      for (ptrdiff_t i = 0; i < dgop::npoints; i += vs) {
        const ptrdiff_t offset0 = i * di + k * dk;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, dgop::npoints);
        CCTK_REAL_VEC s = vec_set1(0.0);
#if 0
        for (ptrdiff_t n = dgop::nmin; n < dgop::nmax; ++n) {
          const CCTK_REAL c = dgop::coeff(n, j);
          if (c != 0.0) {
            // We could use an aligned access if grid functions are
            // suitably aligned, and if the element size is a multiple
            // of the vector size
            s = kmadd(vec_set1(c), vec_loadu(getelt(u, offset0 + n * dj)), s);
          }
        }
#else
        loop<dgop::nmin, dgop::nmax>([&](ptrdiff_t n) {
          const CCTK_REAL c = dgop::coeff(n, j);
          if (c != 0.0) {
            // We could use an aligned access if grid functions are
            // suitably aligned, and if the element size is a multiple
            // of the vector size
            s = kmadd(vec_set1(c), vec_loadu(getelt(u, offset0 + n * dj)), s);
          }
        });
#endif
        s = kmul(f, s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    });
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
stencil_dg_dim3_dir2(const unsigned char *restrict const u,
                     unsigned char *restrict const du, const CCTK_REAL_VEC f,
                     const ptrdiff_t dj, const ptrdiff_t dk) {
  constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(dgop::npoints, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = dgop::npoints;
  constexpr ptrdiff_t dnj = dgop::npoints;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  for (ptrdiff_t j = 0; j < dgop::npoints; ++j) {
    // unroll k loop
    loop<0, dgop::npoints>([&](const ptrdiff_t k) {
      for (ptrdiff_t i = 0; i < dgop::npoints; i += vs) {
        const ptrdiff_t offset0 = i * di + j * dj;
        const ptrdiff_t doffset = i * ddi + j * ddj + k * ddk;
        vec_store_partial_prepare_fixed(i, 0, dgop::npoints);
        CCTK_REAL_VEC s = vec_set1(0.0);
#if 0
        for (ptrdiff_t n = dgop::nmin; n < dgop::nmax; ++n) {
          const CCTK_REAL c = dgop::coeff(n, k);
          if (c != 0.0) {
            // We could use an aligned access if grid functions are
            // suitably aligned, and if the element size is a multiple
            // of the vector size
            s = kmadd(vec_set1(c), vec_loadu(getelt(u, offset0 + n * dk)), s);
          }
        }
#else
        loop<dgop::nmin, dgop::nmax>([&](ptrdiff_t n) {
          const CCTK_REAL c = dgop::coeff(n, k);
          if (c != 0.0) {
            // We could use an aligned access if grid functions are
            // suitably aligned, and if the element size is a multiple
            // of the vector size
            s = kmadd(vec_set1(c), vec_loadu(getelt(u, offset0 + n * dk)), s);
          }
        });
#endif
        s = kmul(f, s);
        dni % vs == 0 ? vec_store_nta_partial(getelt(du, doffset), s)
                      : vec_storeu_partial(getelt(du, doffset), s);
      }
    });
  }
}

template <typename dgop>
CCTK_ATTRIBUTE_NOINLINE __attribute__((__noclone__)) void
stencil_dg_dim3_dir012(const unsigned char *restrict const u,
                       unsigned char *restrict const du, const CCTK_REAL_VEC f,
                       const ptrdiff_t dj, const ptrdiff_t dk) {
  constexpr ptrdiff_t di = sizeof(CCTK_REAL);
  // constexpr ptrdiff_t dni = alignup(dgop::npoints, CCTK_REAL_VEC_SIZE);
  constexpr ptrdiff_t dni = dgop::npoints;
  constexpr ptrdiff_t dnj = dgop::npoints;
  constexpr ptrdiff_t dnk = dgop::npoints;
  constexpr ptrdiff_t ddi = di;
  constexpr ptrdiff_t ddj = ddi * dni;
  constexpr ptrdiff_t ddk = ddj * dnj;
  constexpr ptrdiff_t dsz = ddk * dnk;
  unsigned char buf[dsz]
      __attribute__((__aligned__(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL))));
  stencil_dg_dim3_dir0<dgop>(u, du, f, dj, dk);
  stencil_dg_dim3_dir1<dgop>(du, buf, vec_set1(1.0), dj, dk);
  stencil_dg_dim3_dir2<dgop>(buf, du, vec_set1(1.0), dj, dk);
}

////////////////////////////////////////////////////////////////////////////////

// TODO: Auto-generate this, and don't declare it here, and change the names

// Should we change the normalization to include a factor 2/(order+1)?

// Wide derivative operator
template <std::size_t P> struct dgop_derivs;

// Truncation operator, zeroing the highest mode
template <std::size_t P> struct dgop_trunc;

// Wide exponential RHS filter operator
template <std::size_t P> struct dgop_diss;

// order 1

template <> struct dgop_derivs<1> {
  constexpr static const ptrdiff_t order = 1;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = -1;
  constexpr static const ptrdiff_t nmax = npoints + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {-0.5, 0}, {0, -0.5}, {0.5, 0}, {0, 0.5}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

template <> struct dgop_trunc<1> {
  constexpr static const ptrdiff_t order = 1;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = 0;
  constexpr static const ptrdiff_t nmax = npoints;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {{0.5, 0.5}, {0.5, 0.5}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

// order 2

template <> struct dgop_derivs<2> {
  constexpr static const ptrdiff_t order = 2;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = -1;
  constexpr static const ptrdiff_t nmax = npoints + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {{-1.5, 0, 0},
                                            {0, -0.5, 0.5},
                                            {2.0, 0, -2.0},
                                            {-0.5, 0.5, 0},
                                            {0, 0, 1.5}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

template <> struct dgop_trunc<2> {
  constexpr static const ptrdiff_t order = 2;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = 0;
  constexpr static const ptrdiff_t nmax = npoints;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {0.666666666666666666666666666667, 0.166666666666666666666666666667,
         -0.333333333333333333333333333333},
        {0.666666666666666666666666666667, 0.666666666666666666666666666667,
         0.666666666666666666666666666667},
        {-0.333333333333333333333333333333, 0.166666666666666666666666666667,
         0.666666666666666666666666666667}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

// order 3

template <> struct dgop_derivs<3> {
  constexpr static const ptrdiff_t order = 3;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = -1;
  constexpr static const ptrdiff_t nmax = npoints + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {-3, 0, 0, 0},
        {0, -0.80901699437494742410229341718, 0.309016994374947424102293417183,
         -0.500000000000000000000000000000},
        {4.04508497187473712051146708591, 0, -1.11803398874989484820458683437,
         1.54508497187473712051146708591},
        {-1.54508497187473712051146708591, 1.11803398874989484820458683437, 0,
         -4.04508497187473712051146708591},
        {0.500000000000000000000000000000, -0.309016994374947424102293417183,
         0.80901699437494742410229341718, 0},
        {0, 0, 0, 3}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

template <> struct dgop_trunc<3> {
  constexpr static const ptrdiff_t order = 3;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = 0;
  constexpr static const ptrdiff_t nmax = npoints;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {0.750000000000000000000000000000, 0.111803398874989484820458683437,
         -0.111803398874989484820458683437, 0.250000000000000000000000000000},
        {0.559016994374947424102293417183, 0.750000000000000000000000000000,
         0.250000000000000000000000000000, -0.559016994374947424102293417183},
        {-0.559016994374947424102293417183, 0.250000000000000000000000000000,
         0.750000000000000000000000000000, 0.559016994374947424102293417183},
        {0.250000000000000000000000000000, -0.111803398874989484820458683437,
         0.111803398874989484820458683437, 0.750000000000000000000000000000}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

// order 4

template <> struct dgop_derivs<4> {
  constexpr static const ptrdiff_t order = 4;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = -1;
  constexpr static const ptrdiff_t nmax = npoints + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
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
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

template <> struct dgop_trunc<4> {
  constexpr static const ptrdiff_t order = 4;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = 0;
  constexpr static const ptrdiff_t nmax = npoints;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {0.800000000000000000000000000000, 0.0857142857142857142857142857143,
         -0.0750000000000000000000000000000, 0.0857142857142857142857142857143,
         -0.200000000000000000000000000000},
        {0.466666666666666666666666666667, 0.800000000000000000000000000000,
         0.175000000000000000000000000000, -0.200000000000000000000000000000,
         0.466666666666666666666666666667},
        {-0.533333333333333333333333333333, 0.228571428571428571428571428571,
         0.800000000000000000000000000000, 0.228571428571428571428571428571,
         -0.533333333333333333333333333333},
        {0.466666666666666666666666666667, -0.200000000000000000000000000000,
         0.175000000000000000000000000000, 0.800000000000000000000000000000,
         0.466666666666666666666666666667},
        {-0.200000000000000000000000000000, 0.0857142857142857142857142857143,
         -0.0750000000000000000000000000000, 0.0857142857142857142857142857143,
         0.800000000000000000000000000000}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

// order 5

template <> struct dgop_derivs<5> {
  constexpr static const ptrdiff_t order = 5;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = -1;
  constexpr static const ptrdiff_t nmax = npoints + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {-7.5, 0, 0, 0, 0, 0},
        {0, -1.78636494833909489397248387151, 0.48495104785356916930595719533,
         -0.269700610832038972472085277454, 0.237781177984231363805278620294,
         -0.50000000000000000000000000000},
        {10.1414159363196692802345292705, 0, -1.72125695283023338321606469405,
         0.78635667222324073743954873067, -0.65354750742980016720074791706,
         1.34991331419048809923129776324},
        {-4.0361872703053480052745286480, 2.52342677742945543190883765038, 0,
         -1.75296196636786597887757095193, 1.15282815853592934133182307563,
         -2.24468464817616682427129714069},
        {2.24468464817616682427129714069, -1.15282815853592934133182307563,
         1.75296196636786597887757095193, 0, -2.52342677742945543190883765038,
         4.0361872703053480052745286480},
        {-1.34991331419048809923129776324, 0.65354750742980016720074791706,
         -0.78635667222324073743954873067, 1.72125695283023338321606469405, 0,
         -10.1414159363196692802345292705},
        {0.50000000000000000000000000000, -0.237781177984231363805278620294,
         0.269700610832038972472085277454, -0.48495104785356916930595719533,
         1.78636494833909489397248387151, 0},
        {0, 0, 0, 0, 0, 7.5}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

template <> struct dgop_trunc<5> {
  constexpr static const ptrdiff_t order = 5;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = 0;
  constexpr static const ptrdiff_t nmax = npoints;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {0.833333333333333333333333333333, 0.0699494890218811980514866398801,
         -0.0577712875092362910051218592683, 0.0577712875092362910051218592683,
         -0.0699494890218811980514866398801, 0.166666666666666666666666666667},
        {0.397111947009198203244315970108, 0.833333333333333333333333333333,
         0.137650010784125976289789778629, -0.137650010784125976289789778629,
         0.166666666666666666666666666667, -0.397111947009198203244315970108},
        {-0.480823242399379698590772443519, 0.201800040694084394568584087573,
         0.833333333333333333333333333333, 0.166666666666666666666666666667,
         -0.201800040694084394568584087573, 0.480823242399379698590772443519},
        {0.480823242399379698590772443519, -0.201800040694084394568584087573,
         0.166666666666666666666666666667, 0.833333333333333333333333333333,
         0.201800040694084394568584087573, -0.480823242399379698590772443519},
        {-0.397111947009198203244315970108, 0.166666666666666666666666666667,
         -0.137650010784125976289789778629, 0.137650010784125976289789778629,
         0.833333333333333333333333333333, 0.397111947009198203244315970108},
        {0.166666666666666666666666666667, -0.0699494890218811980514866398801,
         0.0577712875092362910051218592683, -0.0577712875092362910051218592683,
         0.0699494890218811980514866398801, 0.833333333333333333333333333333}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

// order 6

template <> struct dgop_derivs<6> {
  constexpr static const ptrdiff_t order = 6;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = -1;
  constexpr static const ptrdiff_t nmax = npoints + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
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
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

template <> struct dgop_trunc<6> {
  constexpr static const ptrdiff_t order = 6;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = 0;
  constexpr static const ptrdiff_t nmax = npoints;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {0.857142857142857142857142857143, 0.0592500657683036564271051904732,
         -0.0474436903255645773243897241214, 0.0446428571428571428571428571429,
         -0.0474436903255645773243897241214, 0.0592500657683036564271051904732,
         -0.142857142857142857142857142857},
        {0.344441191763598831375069397121, 0.857142857142857142857142857143,
         0.114390928661804110748223308384, -0.107637872426124634804709186600,
         0.114390928661804110748223308384, -0.142857142857142857142857142857,
         0.344441191763598831375069397121},
        {-0.430155477477884545660783682835, 0.178407182318125050999120257141,
         0.857142857142857142857142857143, 0.134423586711838920518994900886,
         -0.142857142857142857142857142857, 0.178407182318125050999120257141,
         -0.430155477477884545660783682835},
        {0.457142857142857142857142857143, -0.189600210458571700566736609514,
         0.151819809041806647438047117188, 0.857142857142857142857142857143,
         0.151819809041806647438047117188, -0.189600210458571700566736609514,
         0.457142857142857142857142857143},
        {-0.430155477477884545660783682835, 0.178407182318125050999120257141,
         -0.142857142857142857142857142857, 0.134423586711838920518994900886,
         0.857142857142857142857142857143, 0.178407182318125050999120257141,
         -0.430155477477884545660783682835},
        {0.344441191763598831375069397121, -0.142857142857142857142857142857,
         0.114390928661804110748223308384, -0.107637872426124634804709186600,
         0.114390928661804110748223308384, 0.857142857142857142857142857143,
         0.344441191763598831375069397121},
        {-0.142857142857142857142857142857, 0.0592500657683036564271051904732,
         -0.0474436903255645773243897241214, 0.0446428571428571428571428571429,
         -0.0474436903255645773243897241214, 0.0592500657683036564271051904732,
         0.857142857142857142857142857143}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

// order 7

template <> struct dgop_derivs<7> {
  constexpr static const ptrdiff_t order = 7;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = -1;
  constexpr static const ptrdiff_t nmax = npoints + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {-14, 0, 0, 0, 0, 0, 0, 0},
        {0, -3.2099157030029903368153515716, 0.79247668132051452680135973613,
         -0.37215043572859486584709186266, 0.24333071272379100970784163159,
         -0.203284568900592744214055743889, 0.219957514771304363030019703909,
         -0.50000000000000000000000000000},
        {18.9375986071173705131546424941, 0, -2.8064757947364334364709223156,
         1.07894468879045270250846359969, -0.66115735090031122320143791015,
         0.53703958615766106093761934305, -0.57356541494026413730995712695,
         1.29768738832023198239705918927},
        {-7.5692898193484870087981787234, 4.5435850645665642173963830608, 0,
         -2.3781872335155058245697882514, 1.13535801688111144046472127434,
         -0.84502255650651048982986561020, 0.86944809833149293416121444553,
         -1.94165942554412230232935244532},
        {4.2979081642651752142490066598, -2.11206121431454222839296010401,
         2.8755174059725052174736362089, 0, -2.3889243591582392148795415102,
         1.37278583180602848091037564048, -1.29423205091350150769320563725,
         2.8101889892579490385377636866},
        {-2.8101889892579490385377636866, 1.29423205091350150769320563725,
         -1.37278583180602848091037564048, 2.3889243591582392148795415102, 0,
         -2.8755174059725052174736362089, 2.11206121431454222839296010401,
         -4.2979081642651752142490066598},
        {1.94165942554412230232935244532, -0.86944809833149293416121444553,
         0.84502255650651048982986561020, -1.13535801688111144046472127434,
         2.3781872335155058245697882514, 0, -4.5435850645665642173963830608,
         7.5692898193484870087981787234},
        {-1.29768738832023198239705918927, 0.57356541494026413730995712695,
         -0.53703958615766106093761934305, 0.66115735090031122320143791015,
         -1.07894468879045270250846359969, 2.8064757947364334364709223156, 0,
         -18.9375986071173705131546424941},
        {0.50000000000000000000000000000, -0.219957514771304363030019703909,
         0.203284568900592744214055743889, -0.24333071272379100970784163159,
         0.37215043572859486584709186266, -0.79247668132051452680135973613,
         3.2099157030029903368153515716, 0},
        {0, 0, 0, 0, 0, 0, 0, 14}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

template <> struct dgop_trunc<7> {
  constexpr static const ptrdiff_t order = 7;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = 0;
  constexpr static const ptrdiff_t nmax = npoints;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {0.875000000000000000000000000000, 0.0514629139204806524039369319530,
         -0.0404460106502039485171070478024, 0.0367824550735666539583712887445,
         -0.0367824550735666539583712887445, 0.0404460106502039485171070478024,
         -0.0514629139204806524039369319530, 0.125000000000000000000000000000},
        {0.303616698116694319981771003354, 0.875000000000000000000000000000,
         0.0982406736448606046730164464587, -0.0893421404644956651185537884383,
         0.0893421404644956651185537884383, -0.0982406736448606046730164464587,
         0.125000000000000000000000000000, -0.303616698116694319981771003354},
        {-0.386317457489993790708067046142, 0.159048176486291953017352954233,
         0.875000000000000000000000000000, 0.113677636194081538161944624272,
         -0.113677636194081538161944624272, 0.125000000000000000000000000000,
         -0.159048176486291953017352954233, 0.386317457489993790708067046142},
        {0.424794918358474420422658828620, -0.174889474538718203995378998343,
         0.137450078336634985758730054229, 0.875000000000000000000000000000,
         0.125000000000000000000000000000, -0.137450078336634985758730054229,
         0.174889474538718203995378998343, -0.424794918358474420422658828620},
        {-0.424794918358474420422658828620, 0.174889474538718203995378998343,
         -0.137450078336634985758730054229, 0.125000000000000000000000000000,
         0.875000000000000000000000000000, 0.137450078336634985758730054229,
         -0.174889474538718203995378998343, 0.424794918358474420422658828620},
        {0.386317457489993790708067046142, -0.159048176486291953017352954233,
         0.125000000000000000000000000000, -0.113677636194081538161944624272,
         0.113677636194081538161944624272, 0.875000000000000000000000000000,
         0.159048176486291953017352954233, -0.386317457489993790708067046142},
        {-0.303616698116694319981771003354, 0.125000000000000000000000000000,
         -0.0982406736448606046730164464587, 0.0893421404644956651185537884383,
         -0.0893421404644956651185537884383, 0.0982406736448606046730164464587,
         0.875000000000000000000000000000, 0.303616698116694319981771003354},
        {0.125000000000000000000000000000, -0.0514629139204806524039369319530,
         0.0404460106502039485171070478024, -0.0367824550735666539583712887445,
         0.0367824550735666539583712887445, -0.0404460106502039485171070478024,
         0.0514629139204806524039369319530, 0.875000000000000000000000000000}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

// order 8

template <> struct dgop_derivs<8> {
  constexpr static const ptrdiff_t order = 8;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = -1;
  constexpr static const ptrdiff_t nmax = npoints + 1;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
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
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};

template <> struct dgop_trunc<8> {
  constexpr static const ptrdiff_t order = 8;
  constexpr static const ptrdiff_t npoints = order + 1;
  constexpr static const ptrdiff_t nmin = 0;
  constexpr static const ptrdiff_t nmax = npoints;
  constexpr static const ptrdiff_t ncoeffs = nmax - nmin;
  typedef const CCTK_REAL coeffs_t[ncoeffs][alignup(
      npoints, CCTK_REAL_VEC_SIZE)] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE *
                                                           sizeof(CCTK_REAL));
  // n: stencil point, i: grid point (in element)
  static CCTK_ATTRIBUTE_ALWAYS_INLINE const CCTK_REAL &coeff(std::ptrdiff_t n,
                                                             std::ptrdiff_t i) {
    constexpr static coeffs_t the_coeffs = {
        {0.888888888888888888888888888889, 0.0455211606969650020027066489336,
         -0.0353430840776206841828537168104, 0.0314629490216813232057880935178,
         -0.0303819444444444444444444444444, 0.0314629490216813232057880935178,
         -0.0353430840776206841828537168104, 0.0455211606969650020027066489336,
         -0.111111111111111111111111111111},
        {0.271207474135623109302621175412, 0.888888888888888888888888888889,
         0.0862677770476901817810095172497, -0.0767968823972525928639268958777,
         0.0741582937089594439499354776518, -0.0767968823972525928639268958777,
         0.0862677770476901817810095172497, -0.111111111111111111111111111111,
         0.271207474135623109302621175412},
        {-0.349309612744378161211946528958, 0.143108811132583070906865188684,
         0.888888888888888888888888888889, 0.0989127948470365326215231138956,
         -0.0955143472347909034563916290121, 0.0989127948470365326215231138956,
         -0.111111111111111111111111111111, 0.143108811132583070906865188684,
         -0.349309612744378161211946528958},
        {0.392387852894469337623611067832, -0.160757554564315822603410027413,
         0.124813771882976578875887821411, 0.888888888888888888888888888889,
         0.107293553525831459506456151360, -0.111111111111111111111111111111,
         0.124813771882976578875887821411, -0.160757554564315822603410027413,
         0.392387852894469337623611067832},
        {-0.406349206349206349206349206349, 0.166477387691757721609898601814,
         -0.129254707483869930725865021478, 0.115064499279291696295453599151,
         0.888888888888888888888888888889, 0.115064499279291696295453599151,
         -0.129254707483869930725865021478, 0.166477387691757721609898601814,
         -0.406349206349206349206349206349},
        {0.392387852894469337623611067832, -0.160757554564315822603410027413,
         0.124813771882976578875887821411, -0.111111111111111111111111111111,
         0.107293553525831459506456151360, 0.888888888888888888888888888889,
         0.124813771882976578875887821411, -0.160757554564315822603410027413,
         0.392387852894469337623611067832},
        {-0.349309612744378161211946528958, 0.143108811132583070906865188684,
         -0.111111111111111111111111111111, 0.0989127948470365326215231138956,
         -0.0955143472347909034563916290121, 0.0989127948470365326215231138956,
         0.888888888888888888888888888889, 0.143108811132583070906865188684,
         -0.349309612744378161211946528958},
        {0.271207474135623109302621175412, -0.111111111111111111111111111111,
         0.0862677770476901817810095172497, -0.0767968823972525928639268958777,
         0.0741582937089594439499354776518, -0.0767968823972525928639268958777,
         0.0862677770476901817810095172497, 0.888888888888888888888888888889,
         0.271207474135623109302621175412},
        {-0.111111111111111111111111111111, 0.0455211606969650020027066489336,
         -0.0353430840776206841828537168104, 0.0314629490216813232057880935178,
         -0.0303819444444444444444444444444, 0.0314629490216813232057880935178,
         -0.0353430840776206841828537168104, 0.0455211606969650020027066489336,
         0.888888888888888888888888888889}};
    static_assert(sizeof the_coeffs / sizeof *the_coeffs == ncoeffs, "");
    static_assert(sizeof *the_coeffs / sizeof **the_coeffs >= npoints, "");
    // assert(n >= nmin && n < nmax);
    // assert(i >= 0 && i < npoints);
    return the_coeffs[n - nmin][i];
  }
};
}

#endif // #ifndef STENCIL_OPS_HH
