#ifndef KRANC_HH
#define KRANC_HH

#include <cmath>
#include <math.h>
#include <sys/time.h>

#include <cctk.h>

namespace WaveCaKernel {

/*********************************************************************
 * Types
 *********************************************************************/

struct KrancData
{
  // Actual loop bounds
  int imin[3];
  int imax[3];
  // Region covered by this tile
  int tile_imin[3];
  int tile_imax[3];
  // Boundary information
  int dir;
  int face;
  CCTK_REAL normal[3];
  CCTK_REAL tangentA[3];
  CCTK_REAL tangentB[3];
};

typedef void(*Kranc_Calculation)(cGH const * restrict cctkGH,
                                 int eir,
                                 int face,
                                 CCTK_REAL const normal[3],
                                 CCTK_REAL const tangentA[3],
                                 CCTK_REAL const tangentB[3],
                                 int const min[3],
                                 int const max[3], 
                                 int n_subblock_gfs, 
                                 CCTK_REAL * restrict const subblock_gfs[]);

/*********************************************************************
 * Function declarations
 *********************************************************************/

// Boundary information

int GetBoundaryWidth(cGH const * restrict const cctkGH);

void GetBoundaryInfo(cGH const * restrict cctkGH,
                                  int const * restrict cctk_ash,
                                  int const * restrict cctk_lsh,
                                  int const * restrict cctk_bbox,
                                  int const * restrict cctk_nghostzones,
                                  int * restrict imin, 
                                  int * restrict imax,
                                  int * restrict is_symbnd, 
                                  int * restrict is_physbnd,
                                  int * restrict is_ipbnd);

// Looping

void LoopOverEverything(cGH const * restrict cctkGH,
                                     Kranc_Calculation calc);
void LoopOverBoundary(cGH const * restrict cctkGH,
                                   Kranc_Calculation calc);
void LoopOverBoundaryWithGhosts(cGH const * restrict cctkGH,
                                             Kranc_Calculation calc);
void LoopOverInterior(cGH const * restrict cctkGH,
                                   Kranc_Calculation calc);

// Tiled looping

void TiledLoop(
  cGH const * restrict const cctkGH,
  const KrancData & restrict kd_coarse,
  void (calc)(const cGH* restrict const cctkGH,
              const KrancData & restrict kd));

void TiledLoopOverEverything(
  cGH const * restrict const cctkGH,
  void (calc)(const cGH* restrict const cctkGH,
              const KrancData & restrict kd));

void TiledLoopOverInterior(
  cGH const * restrict const cctkGH,
  void (calc)(const cGH* restrict const cctkGH,
              const KrancData & restrict kd));

// void TiledLoopOverBoundary(
//   cGH const * restrict const cctkGH,
//   void (calc)(const cGH* restrict const cctkGH,
//               const KrancData & restrict kd));

// Runtime checks

void EnsureStencilFits(cGH const * restrict const cctkGH,
                                    const char *calc, int ni, int nj, int nk);
void GroupDataPointers(cGH const * restrict const cctkGH,
                                    const char *group_name, int nvars,
                                    CCTK_REAL const *restrict *ptrs);
void AssertGroupStorage(cGH const * restrict const cctkGH,
                                     const char *calc, int ngroups,
                                     const char *const group_names[]);


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
#define KRANC_GFOFFSET3D(var,i,j,k)                                     \
  (*(CCTK_REAL const*)&((char const*)(var))[cdi*(i)+cdj*(j)+cdk*(k)])

#define GFOffset(u,di,dj,dk) KRANC_GFOFFSET3D(&(u)[index],di,dj,dk)

/*********************************************************************
 * Macros used in Kranc expressions
 *********************************************************************/

#define IfThen(x,y,z) ((x) ? (y) : (z))
#define MinMod(x, y)  ((x) * (y) < 0 ? 0 : (fabs((x)) < fabs((y)) ? (x) : (y)))
#define VanLeer(x, y)  ((x) * (y) < 0 ? 0 : (Min3(2*fabs(x),2*fabs(y),0.5*(fabs(x)+fabs(y)))*Sign((x)+(y))))
#define StepFunction(x) ((x)>0)

/*********************************************************************
 * Numerical constants not defined in C++
 *********************************************************************/

#define E           M_E
#define Pi          M_PI

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

KRANC_WHERE static inline CCTK_REAL sgn(CCTK_REAL x)
{
#ifdef __cplusplus
  using namespace std;
#endif
  return x==(CCTK_REAL)0.0 ? (CCTK_REAL)0.0 : copysign((CCTK_REAL)1.0, x);
}

KRANC_WHERE static inline int isgn(CCTK_REAL x)
{
  if (x == (CCTK_REAL)0.0) return 0;
#ifdef __cplusplus
  using namespace std;
  int s = signbit(x);
#else
  int s = signbit(x);
#endif
  return s ? -1 : +1;
}

} // namespace WaveCaKernel

#endif  // #ifndef KRANC_HH