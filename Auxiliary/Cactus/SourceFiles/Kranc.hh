#ifndef KRANC_HH
#define KRANC_HH

#include <cctk.h>

#include <algorithm>
#include <cassert>
#include <cmath>
#include <math.h>
#include <stdlib.h>
#include <sys/time.h>

namespace @THORN_NAME@ {

/*********************************************************************
 * Types
 *********************************************************************/

struct KrancData { // Actual loop bounds
  int imin[3];
  int imax[3];
  // Region covered by this tile
  int tile_imin[3];
  int tile_imax[3];
  // Boundary information
  int dir;
  int face;
#if 0
  CCTK_REAL normal[3];
  CCTK_REAL tangentA[3];
  CCTK_REAL tangentB[3];
#endif
};

typedef void (*Kranc_Calculation)(cGH const *restrict cctkGH, int eir, int face,
                                  CCTK_REAL const normal[3],
                                  CCTK_REAL const tangentA[3],
                                  CCTK_REAL const tangentB[3], int const min[3],
                                  int const max[3], int n_subblock_gfs,
                                  CCTK_REAL *restrict const subblock_gfs[]);

/*********************************************************************
 * idiv
 *********************************************************************/

// Divide, rounding to minus infinity
inline int idiv(int x, int y) {
  // round down manually if the result is negative
  return (x ^ y) >= 0 ? x / y : (x - y + 1) / y;
}

/*********************************************************************
 * imod
 *********************************************************************/

// Modulo, rounding to minus infinity
inline int imod(int x, int y) {
  return (x ^ y) >= 0 ? x % y : (x - y + 1) % y + y - 1;
}

/*********************************************************************
 * ialign
 *********************************************************************/

// Align x to a multiple of y
inline int ialign(int x, int y) { return idiv(x, y) * y; }

/*********************************************************************
 * Function declarations
 *********************************************************************/

// Boundary information

int GetBoundaryWidth(cGH const *restrict const cctkGH);

void GetBoundaryInfo(cGH const *restrict cctkGH, int const *restrict cctk_ash,
                     int const *restrict cctk_lsh,
                     int const *restrict cctk_bbox,
                     int const *restrict cctk_nghostzones, int *restrict imin,
                     int *restrict imax, int *restrict is_symbnd,
                     int *restrict is_physbnd, int *restrict is_ipbnd);

// Looping

void LoopOverEverything(cGH const *restrict cctkGH, Kranc_Calculation calc);
void LoopOverBoundary(cGH const *restrict cctkGH, Kranc_Calculation calc);
void LoopOverBoundaryWithGhosts(cGH const *restrict cctkGH,
                                Kranc_Calculation calc);
void LoopOverInterior(cGH const *restrict cctkGH, Kranc_Calculation calc);

// Tiled looping

#if 0
void TiledLoop(cGH const *restrict const cctkGH,
               const KrancData &restrict kd_coarse,
               void(calc)(const cGH *restrict const cctkGH,
                          const KrancData &restrict kd));

void TiledLoopOverEverything(cGH const *restrict const cctkGH,
                             void(calc)(const cGH *restrict const cctkGH,
                                        const KrancData &restrict kd));

void TiledLoopOverInterior(cGH const *restrict const cctkGH,
                           void(calc)(const cGH *restrict const cctkGH,
                                      const KrancData &restrict kd));
#endif

/*********************************************************************
 * TiledLoop
 *********************************************************************/

template <typename F>
CCTK_ATTRIBUTE_NOINLINE void
TiledLoop(cGH const *restrict const cctkGH, const KrancData &restrict kd_coarse,
          const F &kernel
          // void(kernel)(const cGH *restrict const cctkGH,
          //              const KrancData &restrict kd)
          ) {
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;

  // Interior region
  int imin[3], imax[3];
  // Boundary types
  int is_symbnd[6], is_physbnd[6], is_ipbnd[6];

  GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox, cctk_nghostzones, imin,
                  imax, is_symbnd, is_physbnd, is_ipbnd);

  // Tile size
  assert(tile_size == -1);
  const int tile_size_l[3] = {tile_size_i, tile_size_j, tile_size_k};

  // Loop bounds covered by tiles (may be larger)
  int tiled_imin[3];
  int tiled_imax[3];
  for (int d = 0; d < 3; d++) {
    // Align with beginning of interior of domain
    tiled_imin[d] =
        imin[d] + ialign(kd_coarse.imin[d] - imin[d], tile_size_l[d]);
    tiled_imax[d] = kd_coarse.imax[d];
  }

#ifdef HAVE_CAPABILITY_FunHPC

  std::vector<qthread::future<void> > fs;
  const int dti = tile_size_l[0];
  const int dtj = tile_size_l[1];
  const int dtk = tile_size_l[2];
  for (int tk = tiled_imin[2]; tk < tiled_imax[2]; tk += dtk) {
    for (int tj = tiled_imin[1]; tj < tiled_imax[1]; tj += dtj) {
      for (int ti = tiled_imin[0]; ti < tiled_imax[0]; ti += dti) {
        fs.push_back(qthread::async(qthread::launch::async, [&, ti, tj, tk]() {
          KrancData kd = kd_coarse;

          kd.dir = 0;
          kd.face = 0;
          // TODO: initialise the rest, or use a constructor

          kd.tile_imin[0] = ti;
          kd.tile_imax[0] = ti + dti;
          kd.tile_imin[1] = tj;
          kd.tile_imax[1] = tj + dtj;
          kd.tile_imin[2] = tk;
          kd.tile_imax[2] = tk + dtk;

          kernel(cctkGH, kd);
        }));
      }
    }
  }
  for (auto &f : fs)
    f.get();

#else

  const int dti = tile_size_l[0];
  const int dtj = tile_size_l[1];
  const int dtk = tile_size_l[2];
#pragma omp parallel for collapse(3) schedule(dynamic)
  for (int tk = tiled_imin[2]; tk < tiled_imax[2]; tk += dtk) {
    for (int tj = tiled_imin[1]; tj < tiled_imax[1]; tj += dtj) {
      for (int ti = tiled_imin[0]; ti < tiled_imax[0]; ti += dti) {
        KrancData kd = kd_coarse;

        kd.dir = 0;
        kd.face = 0;
        // TODO: initialise the rest, or use a constructor

        kd.tile_imin[0] = ti;
        kd.tile_imax[0] = ti + dti;
        kd.tile_imin[1] = tj;
        kd.tile_imax[1] = tj + dtj;
        kd.tile_imin[2] = tk;
        kd.tile_imax[2] = tk + dtk;

        kernel(cctkGH, kd);
      }
    }
  }

#endif
}

void TiledCalculationOnEverything(cGH const *restrict const cctkGH,
                                  void(calc)(const cGH *restrict const cctkGH,
                                             const KrancData &restrict kd));

void TiledCalculationOnInterior(cGH const *restrict const cctkGH,
                                void(calc)(const cGH *restrict const cctkGH,
                                           const KrancData &restrict kd));

#if 0
void TiledCalculationOnBoundary(cGH const *restrict const cctkGH,
                                void (calc)(const cGH* restrict const cctkGH,
                                            const KrancData & restrict kd));
#endif

// Runtime checks

void EnsureStencilFits(cGH const *restrict const cctkGH, const char *calc,
                       int ni, int nj, int nk);
void GroupDataPointers(cGH const *restrict const cctkGH, const char *group_name,
                       int nvars, CCTK_REAL const *restrict *ptrs);
void AssertGroupStorage(cGH const *restrict const cctkGH, const char *calc,
                        int ngroups, const char *const group_names[]);

/*********************************************************************
 * Aligned memory allocation
 *********************************************************************/

template <typename T, size_t align> class aligned_vector {
  T *data_;

public:
  aligned_vector() noexcept : data_(nullptr) {}
  CCTK_ATTRIBUTE_NOINLINE aligned_vector(size_t size) {
    // Increase alignment if necessary
    size_t align1 = std::max(sizeof(void *), align);
    void *ptr = nullptr;
    int ierr = posix_memalign(&ptr, align1, size * sizeof(T));
    if (ierr)
      throw std::bad_alloc();
    data_ = static_cast<T *>(ptr);
    // for (size_t i = 0; i < size; ++i)
    //   data_[i] = T(NAN);
  }
  ~aligned_vector() { free(data_); }

  const T *data() const noexcept { return data_; }
  T *data() noexcept { return data_; }

  operator const T *() const noexcept { return data(); }
  operator T *() noexcept { return data(); }
  const T &operator[](ptrdiff_t ind) const { return data()[ind]; }
  T &operator[](ptrdiff_t ind) { return data()[ind]; }
};

/*********************************************************************
 * Gridfunction access macros
 *********************************************************************/

/* Grid function access */
/* var is a pointer to a grid point, i,j,k are offsets with respect
   to that point.
   For example: KRANC_GFINDEX3D_OFFSET(&u[ind3d],-1,-1,0) */
/* simple implementation */
/* #define KRANC_GFOFFSET3D(var,i,j,k) ((var)[di*(i)+dj*(j)+dk*(k)]) */
/* more efficient implementation for some compilers */
#ifndef KRANC_GFOFFSET3D
#define KRANC_GFOFFSET3D(var, i, j, k)                                         \
  (*(CCTK_REAL const *)&(                                                      \
      (char const *)(var))[cdi * (i) + cdj * (j) + cdk * (k)])
#endif

#define GFOffset(u, di, dj, dk) KRANC_GFOFFSET3D(&(u)[index], di, dj, dk)

/*********************************************************************
 * Macros used in Kranc expressions
 *********************************************************************/

#ifndef IfThen
#define IfThen(x, y, z) ((x) ? (y) : (z))
#endif
#define MinMod(x, y) ((x) * (y) < 0 ? 0 : (fabs((x)) < fabs((y)) ? (x) : (y)))
#define VanLeer(x, y)                                                          \
  ((x) * (y) < 0 ? 0 : (Min3(2 * fabs(x), 2 * fabs(y),                         \
                             0.5 * (fabs(x) + fabs(y))) *                      \
                        Sign((x) + (y))))
#define StepFunction(x) ((x) > 0)

static CCTK_REAL8 pown(CCTK_REAL8 x, int i) CCTK_ATTRIBUTE_UNUSED;
static CCTK_REAL8 pown(CCTK_REAL8 x, int i) {
  // return pow(x, i);
  if (i < 0) {
    x = 1 / x;
    i = -i;
  }
  CCTK_REAL8 r = 1;
  while (i != 0) {
    if (i & 1)
      r *= x;
    x *= x;
    i >>= 1;
  }
  return r;
}

/*********************************************************************
 * Numerical constants not defined in C++
 *********************************************************************/

#define E M_E
#define Pi M_PI

/*********************************************************************
 * Declare functions on the device if compiled for CUDA
 *********************************************************************/

#ifdef __CUDACC__
#define KRANC_WHERE __device__
#else
#define KRANC_WHERE
#endif

/*********************************************************************
 * Implement the signum function, used for Mathematica's Sign function
 *********************************************************************/

KRANC_WHERE static inline CCTK_REAL sgn(CCTK_REAL x) {
#ifdef __cplusplus
  using namespace std;
#endif
  return x == (CCTK_REAL)0.0 ? (CCTK_REAL)0.0 : copysign((CCTK_REAL)1.0, x);
}

KRANC_WHERE static inline int isgn(CCTK_REAL x) {
  if (x == (CCTK_REAL)0.0)
    return 0;
#ifdef __cplusplus
  using namespace std;
  int s = signbit(x);
#else
  int s = signbit(x);
#endif
  return s ? -1 : +1;
}

} // namespace @THORN_NAME@

#endif // #ifndef KRANC_HH
