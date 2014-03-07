
/*  Copyright 2014 Ian Hinder

    This file is part of Kranc.

    Kranc is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Kranc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Kranc; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <cctk.h>
#include <cctk_Arguments.h>
#include <cctk_Parameters.h>
            
#define KRANC_C
                                                
#include "GenericFD.h"
#include "Kranc.hh"



// Divide, rounding to minus infinity
static int idiv(int x, int y)
{
  // round down manually if the result is negative
  return (x^y) >= 0 ? x/y : (x-y+1)/y;
}
// Modulo, rounding to minus infinity
static int imod(int x, int y)
{
  return (x^y) >= 0 ? x%y : (x-y+1)%y + y-1;
}
// Align x to a multiple of y
static int ialign(int x, int y)
{
  return idiv(x, y) * y;
}



void Wave_TiledLoop(
  cGH const * restrict const cctkGH,
  const KrancData & restrict kd_coarse,
  void (calc)(const cGH* restrict const cctkGH,
              const KrancData & restrict kd))
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;

  // Interior region
  int imin[3], imax[3];
  // Boundary types
  int is_symbnd[6], is_physbnd[6], is_ipbnd[6];

  GenericFD_GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox,
                            cctk_nghostzones, 
                            imin, imax, is_symbnd, is_physbnd, is_ipbnd);

  // Tile size
  int tile_size_l[3];
  // Loop bounds covered by tiles (may be larger)
  int tiled_imin[3];
  int tiled_imax[3];

  if (tile_size == -1)
  {
    for (int d = 0; d < 3; d++)
    {
      // Loop in a single tile
      tiled_imin[d] = kd_coarse.imin[d];
      tiled_imax[d] = kd_coarse.imax[d];
      tile_size_l[d] = tiled_imax[d] - tiled_imin[d];
    }
  }
  else
  {
    for (int d = 0; d < 3; d++)
    {
      tile_size_l[d] = tile_size;
      // Align with beginning of interior of domain
      tiled_imin[d] =
        imin[d] + ialign(kd_coarse.imin[d] - imin[d], tile_size_l[d]);
      tiled_imax[d] = kd_coarse.imax[d];
    }
  }

  const int dti = tile_size_l[0];
  const int dtj = tile_size_l[1];
  const int dtk = tile_size_l[2];
#pragma omp parallel for collapse(3)
  for (int tk = tiled_imin[2]; tk < tiled_imax[2]; tk += dtk)
  {
    for (int tj = tiled_imin[1]; tj < tiled_imax[1]; tj += dtj)
    {
      for (int ti = tiled_imin[0]; ti < tiled_imax[0]; ti += dti)
      {
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

        calc(cctkGH, kd);
      }
    }
  }
}



void Wave_TiledLoopOverEverything(
  cGH const * restrict const cctkGH,
  void (calc)(const cGH* restrict const cctkGH,
              const KrancData & restrict kd))
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;

  KrancData kd_coarse;

  for (int d = 0; d < 3; d++)
  {
    kd_coarse.imin[d] = 0;
    kd_coarse.imax[d] = cctk_lsh[d];
  }

  Wave_TiledLoop(cctkGH, kd_coarse, calc);
}

void Wave_TiledLoopOverInterior(
  cGH const * restrict const cctkGH,
  void (calc)(const cGH* restrict const cctkGH,
              const KrancData & restrict kd))
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;

  // Interior region
  int imin[3], imax[3];
  // Boundary types
  int is_symbnd[6], is_physbnd[6], is_ipbnd[6];

  GenericFD_GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox,
                            cctk_nghostzones, 
                            imin, imax, is_symbnd, is_physbnd, is_ipbnd);

  KrancData kd_coarse;

  for (int d = 0; d < 3; d++)
  {
    kd_coarse.imin[d] = imin[d];
    kd_coarse.imax[d] = imax[d];
  }

  Wave_TiledLoop(cctkGH, kd_coarse, calc);
}