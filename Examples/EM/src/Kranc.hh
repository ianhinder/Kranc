#ifndef KRANC_HH
#define KRANC_HH

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

void EM_TiledLoop(
  cGH const * restrict const cctkGH,
  const KrancData & restrict kd_coarse,
  void (calc)(const cGH* restrict const cctkGH,
              const KrancData & restrict kd));

void EM_TiledLoopOverEverything(
  cGH const * restrict const cctkGH,
  void (calc)(const cGH* restrict const cctkGH,
              const KrancData & restrict kd));

void EM_TiledLoopOverInterior(
  cGH const * restrict const cctkGH,
  void (calc)(const cGH* restrict const cctkGH,
              const KrancData & restrict kd));

// void EM_TiledLoopOverBoundary(
//   cGH const * restrict const cctkGH,
//   void (calc)(const cGH* restrict const cctkGH,
//               const KrancData & restrict kd));

#define GFOffset(u,di,dj,dk) KRANC_GFOFFSET3D(&(u)[index],di,dj,dk)

#endif  // #ifndef KRANC_HH