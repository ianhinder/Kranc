
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
                                             
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <cctk.h>
#include <cctk_Arguments.h>
#include <cctk_Parameters.h>
#include <util_Table.h>

#include <Symmetry.h>
            
#include "Kranc.hh"

namespace WaveHost {

/*********************************************************************
 * GetBoundaryWidths
 *********************************************************************/

void GetBoundaryWidths(cGH const * restrict const cctkGH, CCTK_INT nboundaryzones[6])
{
  CCTK_INT is_internal[6];
  CCTK_INT is_staggered[6];
  CCTK_INT shiftout[6];
  int ierr = -1;

  if (CCTK_IsFunctionAliased ("MultiPatch_GetBoundarySpecification")) {
    int const map = MultiPatch_GetMap (cctkGH);
    /* This doesn't make sense in level mode */
    if (map < 0)
    {
      static int have_warned = 0;
      if (!have_warned)
      {
        CCTK_WARN(1, "GetBoundaryWidths: Could not determine current map (can be caused by calling in LEVEL mode)");
        have_warned = 1;
      }
      for (int i = 0; i < 6; i++)
        nboundaryzones[i] = 0;
      return;
    }
    ierr = MultiPatch_GetBoundarySpecification
      (map, 6, nboundaryzones, is_internal, is_staggered, shiftout);
    if (ierr != 0)
      CCTK_WARN(0, "Could not obtain boundary specification");
  } else if (CCTK_IsFunctionAliased ("GetBoundarySpecification")) {
    ierr = GetBoundarySpecification
      (6, nboundaryzones, is_internal, is_staggered, shiftout);
    if (ierr != 0)
      CCTK_WARN(0, "Could not obtain boundary specification");
  } else {
    CCTK_WARN(0, "Could not obtain boundary specification");
  }
}

/*********************************************************************
 * GetBoundaryWidth
 *********************************************************************/

int GetBoundaryWidth(cGH const * restrict const cctkGH)
{
  CCTK_INT nboundaryzones[6];
  GetBoundaryWidths(cctkGH, nboundaryzones);

  int bw = nboundaryzones[0];

  for (int i = 1; i < 6; i++)
    if (nboundaryzones[i] != bw)
    CCTK_WARN(0, "Number of boundary points is different on different faces");

  return bw;
}

/*********************************************************************
 * GetBoundaryInfo
 *********************************************************************/

/* Return the array indices in imin and imax for looping over the
   interior of the grid. imin is the index of the first grid point.
   imax is the index of the last grid point plus 1.  So a loop over
   the interior of the grid would be

   for (i = imin; i < imax; i++)

   The indexing is C-style. Also return whether the boundary is a
   symmetry, physical or interprocessor boundary.  Carpet refinement
   boundaries are treated as interprocessor boundaries.
*/
void GetBoundaryInfo(cGH const * restrict const cctkGH,
                               int const * restrict const cctk_ash,
                               int const * restrict const cctk_lsh,
                               int const * restrict const cctk_bbox,
			       int const * restrict const cctk_nghostzones,
                               int * restrict const imin, 
			       int * restrict const imax,
                               int * restrict const is_symbnd, 
			       int * restrict const is_physbnd,
                               int * restrict const is_ipbnd)
{
  CCTK_INT bbox[6];
  CCTK_INT nboundaryzones[6];
  CCTK_INT is_internal[6];
  CCTK_INT is_staggered[6];
  CCTK_INT shiftout[6];
  CCTK_INT symbnd[6];

  int symtable = 0;
  int dir = 0;
  int face = 0;
  int npoints = 0;
  int iret = 0;
  int ierr = 0;

  if (CCTK_IsFunctionAliased ("MultiPatch_GetBbox")) {
    ierr = MultiPatch_GetBbox (cctkGH, 6, bbox);
    if (ierr != 0)
      CCTK_WARN(0, "Could not obtain bbox specification");
  } else {
    for (dir = 0; dir < 6; dir++)
    {
      bbox[dir] = 0;
    }
  }

  if (CCTK_IsFunctionAliased ("MultiPatch_GetBoundarySpecification")) {
    int const map = MultiPatch_GetMap (cctkGH);
    if (map < 0)
      CCTK_WARN(0, "Could not obtain boundary specification");
    ierr = MultiPatch_GetBoundarySpecification
      (map, 6, nboundaryzones, is_internal, is_staggered, shiftout);
    if (ierr != 0)
      CCTK_WARN(0, "Could not obtain boundary specification");
  } else if (CCTK_IsFunctionAliased ("GetBoundarySpecification")) {
    ierr = GetBoundarySpecification
      (6, nboundaryzones, is_internal, is_staggered, shiftout);
    if (ierr != 0)
      CCTK_WARN(0, "Could not obtain boundary specification");
  } else {
    CCTK_WARN(0, "Could not obtain boundary specification");
  }

  symtable = SymmetryTableHandleForGrid(cctkGH);
  if (symtable < 0) 
  {
    CCTK_WARN(0, "Could not obtain symmetry table");
  }  
  
  iret = Util_TableGetIntArray(symtable, 6, symbnd, "symmetry_handle");
  if (iret != 6) CCTK_WARN (0, "Could not obtain symmetry information");

  for (dir = 0; dir < 6; dir++)
  {
    is_ipbnd[dir] = (!cctk_bbox[dir]);
    is_symbnd[dir] = (!is_ipbnd[dir] && symbnd[dir] >= 0 && !bbox[dir]);
    is_physbnd[dir] = (!is_ipbnd[dir] && !is_symbnd[dir]);
  }

  for (dir = 0; dir < 3; dir++)
  {
    for (face = 0; face < 2; face++)
    {
      int index = dir*2 + face;
      if (is_ipbnd[index])
      {
	/* Inter-processor boundary */
	npoints = cctk_nghostzones[dir];
      }
      else
      {
	/* Symmetry or physical boundary */
	npoints = nboundaryzones[index];
             
	if (is_symbnd[index])
	{
	  /* Ensure that the number of symmetry zones is the same
	     as the number of ghost zones */
	  if (npoints != cctk_nghostzones[dir])
	  {
	    CCTK_WARN (1, "The number of symmetry points is different from the number of ghost points; this is probably an error");
	  }
	}
      }

      switch(face)
      {
      case 0: /* Lower boundary */
	imin[dir] = npoints;
	break;
      case 1: /* Upper boundary */
	imax[dir] = cctk_lsh[dir] - npoints;
	break;
      default:
	CCTK_WARN(0, "internal error");
      }
    }
  }
}

/*********************************************************************
 * LoopOverEverything
 *********************************************************************/

void LoopOverEverything(cGH const * restrict const cctkGH, Kranc_Calculation const calc)
{
  DECLARE_CCTK_ARGUMENTS

  int   dir = 0;
  int   face = 0;
  CCTK_REAL  normal[] = {0,0,0};
  CCTK_REAL  tangentA[] = {0,0,0};
  CCTK_REAL  tangentB[] = {0,0,0};
  int   bmin[] = {0,0,0};
  int   bmax[] = {cctk_lsh[0],cctk_lsh[1],cctk_lsh[2]};

  calc(cctkGH, dir, face, normal, tangentA, tangentB, bmin, bmax, 0, NULL);
  return;
}

/*********************************************************************
 * LoopOverBoundary
 *********************************************************************/

void LoopOverBoundary(cGH const * restrict const cctkGH, Kranc_Calculation const calc)
{
  DECLARE_CCTK_ARGUMENTS

  int   dir1, dir2, dir3;
  int   dir[3];
  CCTK_REAL  normal[3];
  CCTK_REAL  tangentA[3];
  CCTK_REAL  tangentB[3];
  int   bmin[3];
  int   bmax[3];
  int   have_bnd;
  int   all_physbnd;
  int   d;

  int   is_symbnd[6], is_physbnd[6], is_ipbnd[6];
  int   imin[3], imax[3];
  int        old_dir = 0;
  int        old_face = 0;

  GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox,
                  cctk_nghostzones, 
                  imin, imax, is_symbnd, is_physbnd, is_ipbnd);

 /* Loop over all faces */
  for (dir3 = -1; dir3 <= +1; dir3++)
  {
    for (dir2 = -1; dir2 <= +1; dir2++)
    {
      for (dir1 = -1; dir1 <= +1; dir1++)
      {
        dir[0] = dir1;
        dir[1] = dir2;
        dir[2] = dir3;

        have_bnd = 0;          /* one of the faces is a boundary */
        all_physbnd = 1;       /* all boundary faces are physical
                                  boundaries */

        for (d = 0; d < 3; d++)
        {
          switch(dir[d])
          {
          case -1:
            bmin[d] = 0;
            bmax[d] = imin[d];
            have_bnd = 1;
            all_physbnd = all_physbnd && is_physbnd[2*d+0];
            break;
          case 0:
            bmin[d] = imin[d];
            bmax[d] = imax[d];
            break;
          case +1:
            bmin[d] = imax[d];
            bmax[d] = cctk_lsh[d];
            have_bnd = 1;
            all_physbnd = all_physbnd && is_physbnd[2*d+1];
            break;
          }

          /* Choose a basis */
          normal[d] = dir[d];
          tangentA[d] = dir[(d+1)%3];
          tangentB[d] = dir[(d+2)%3];
        }

        if (have_bnd && all_physbnd)
        {
#if 0
          CCTK_REAL normal_norm = 0.0;
          for (d = 0; d < 3; d++)
          {
            normal_norm += pow(normal[d], 2);
          }
          normal_norm = sqrt(normal_norm);
          for (d = 0; d < 3; d++)
          {
            normal[d] /= normal_norm;
          }
#endif
          
          calc(cctkGH, old_dir, old_face, normal, tangentA, tangentB, bmin, bmax, 0, NULL);
        }

      }
    }
  }
  
  return;
}

/*********************************************************************
 * LoopOverBoundaryWithGhosts
 *********************************************************************/

void LoopOverBoundaryWithGhosts(cGH const * restrict const cctkGH, Kranc_Calculation const calc)
{
  DECLARE_CCTK_ARGUMENTS

  int   dir1, dir2, dir3;
  int   dir[3];
  CCTK_REAL  normal[3];
  CCTK_REAL  tangentA[3];
  CCTK_REAL  tangentB[3];
  int   bmin[3];
  int   bmax[3];
  int   have_bnd;
  int   have_physbnd;
  int   d;

  int   is_symbnd[6], is_physbnd[6], is_ipbnd[6];
  int   imin[3], imax[3];
  int        old_dir = 0;
  int        old_face = 0;

  GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox,
                  cctk_nghostzones, 
                  imin, imax, is_symbnd, is_physbnd, is_ipbnd);

 /* Loop over all faces */
  for (dir3 = -1; dir3 <= +1; dir3++)
  {
    for (dir2 = -1; dir2 <= +1; dir2++)
    {
      for (dir1 = -1; dir1 <= +1; dir1++)
      {
        dir[0] = dir1;
        dir[1] = dir2;
        dir[2] = dir3;

        have_bnd = 0;          /* one of the faces is a boundary */
        have_physbnd = 0;      /* one of the boundary faces is a physical
                                  boundary */

        for (d = 0; d < 3; d++)
        {
          switch(dir[d])
          {
          case -1:
            bmin[d] = 0;
            bmax[d] = imin[d];
            have_bnd = 1;
            have_physbnd = have_physbnd || is_physbnd[2*d+0];
            break;
          case 0:
            bmin[d] = imin[d];
            bmax[d] = imax[d];
            break;
          case +1:
            bmin[d] = imax[d];
            bmax[d] = cctk_lsh[d];
            have_bnd = 1;
            have_physbnd = have_physbnd || is_physbnd[2*d+1];
            break;
          }

          /* Choose a basis */
          normal[d] = dir[d];
          tangentA[d] = dir[(d+1)%3];
          tangentB[d] = dir[(d+2)%3];
        }

        if (have_bnd && have_physbnd)
        {
#if 0
          CCTK_REAL normal_norm = 0.0;
          for (d = 0; d < 3; d++)
          {
            normal_norm += pow(normal[d], 2);
          }
          normal_norm = sqrt(normal_norm);
          for (d = 0; d < 3; d++)
          {
            normal[d] /= normal_norm;
          }
#endif
          
          calc(cctkGH, old_dir, old_face, normal, tangentA, tangentB, bmin, bmax, 0, NULL);
        }

      }
    }
  }
  
  return;
}

/*********************************************************************
 * LoopOverInterior
 *********************************************************************/

void LoopOverInterior(cGH const * restrict const cctkGH, Kranc_Calculation const calc)
{
  DECLARE_CCTK_ARGUMENTS

  CCTK_REAL  normal[] = {0,0,0};
  CCTK_REAL  tangentA[] = {0,0,0};
  CCTK_REAL  tangentB[] = {0,0,0};

  int   is_symbnd[6], is_physbnd[6], is_ipbnd[6];
  int   imin[3], imax[3];
  int        dir = 0;
  int        face = 0;

  GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox,
                            cctk_nghostzones, 
                            imin, imax, is_symbnd, is_physbnd, is_ipbnd);

  calc(cctkGH, dir, face, normal, tangentA, tangentB, imin, imax, 0, NULL);
  
  return;
}

/*********************************************************************
 * AssertGroupStorage
 *********************************************************************/

void AssertGroupStorage(cGH const * restrict const cctkGH, const char *calc,
                                  int ngroups, const char *const group_names[])
{
  for (int i = 0; i < ngroups; i++)
  {
    int result = CCTK_QueryGroupStorage(cctkGH, group_names[i]);
    if (result == 0)
    {
      CCTK_VWarn(CCTK_WARN_ABORT, __LINE__, __FILE__, CCTK_THORNSTRING,
                 "Error in %s: Group \"%s\" does not have storage", calc, group_names[i]);
    }
    else if (result < 0)
    {
      CCTK_VWarn(CCTK_WARN_ABORT, __LINE__, __FILE__, CCTK_THORNSTRING,
                 "Error in %s: Invalid group name \"%s\"", calc, group_names[i]);
    }
  }
}

/*********************************************************************
 * GroupDataPointers
 *********************************************************************/

/* Return a list of pointers to the members of a named group */
void GroupDataPointers(cGH const * restrict const cctkGH, const char *group_name,
                                 int nvars, CCTK_REAL const *restrict *ptrs)
{
  int group_index, status;
  cGroup  group_info;

  group_index = CCTK_GroupIndex(group_name);
  if (group_index < 0)
    CCTK_VWarn(CCTK_WARN_ABORT,   __LINE__, __FILE__, CCTK_THORNSTRING,
               "Error return %d trying to get group index for group \'%s\'",
               group_index,
               group_name);

  status = CCTK_GroupData(group_index, &group_info);
  if (status < 0)
    CCTK_VWarn(CCTK_WARN_ABORT,   __LINE__, __FILE__, CCTK_THORNSTRING,
               "Error return %d trying to get info for group \'%s\'",
               status,
               group_name);

  if (group_info.numvars != nvars)
  {
    CCTK_VWarn(CCTK_WARN_ABORT,   __LINE__, __FILE__, CCTK_THORNSTRING,
               "Group \'%s\' has %d variables but %d were expected",
               group_name, group_info.numvars, nvars);
  }

  int v1 = CCTK_FirstVarIndex(group_name);

  for (int v = 0; v < nvars; v++)
  {
    ptrs[v] = (CCTK_REAL const *) CCTK_VarDataPtrI(cctkGH, 0 /* timelevel */, v1+v);
  }
}

/*********************************************************************
 * EnsureStencilFits
 *********************************************************************/

void EnsureStencilFits(cGH const * restrict const cctkGH, const char *calc, int ni, int nj, int nk)
{
  DECLARE_CCTK_ARGUMENTS

  CCTK_INT bws[6];
  GetBoundaryWidths(cctkGH, bws);

  int ns[] = {ni, nj, nk};
  const char *dirs[] = {"x", "y", "z"};
  const char *faces[] = {"lower", "upper"};
  int abort = 0;

  for (int dir = 0; dir < 3; dir++)
  {
    for (int face = 0; face < 2; face++)
    {
      int bw = bws[2*dir+face];
      if (bw < ns[dir])
      {
        CCTK_VInfo(CCTK_THORNSTRING,
                   "The stencil for %s requires %d points, but the %s %s boundary has only %d points.",
                   calc, ns[dir], faces[face], dirs[dir], bw);
        abort = 1;
      }
    }
    int gz = cctk_nghostzones[dir];
    if (gz < ns[dir])
    {
      CCTK_VInfo(CCTK_THORNSTRING,
                 "The stencil for %s requires %d points, but there are only %d ghost zones in the %s direction.",
                 calc, ns[dir], gz, dirs[dir]);
      abort = 1;
    }
  }

  if (abort)
  {
    CCTK_VWarn(CCTK_WARN_ABORT, __LINE__, __FILE__, CCTK_THORNSTRING,
               "Insufficient ghost or boundary points for %s", calc);
  }
}

/*********************************************************************
 * idiv
 *********************************************************************/

// Divide, rounding to minus infinity
static int idiv(int x, int y)
{
  // round down manually if the result is negative
  return (x^y) >= 0 ? x/y : (x-y+1)/y;
}

/*********************************************************************
 * imod
 *********************************************************************/

// // Modulo, rounding to minus infinity
// static int imod(int x, int y)
// {
//   return (x^y) >= 0 ? x%y : (x-y+1)%y + y-1;
// }

/*********************************************************************
 * ialign
 *********************************************************************/

// Align x to a multiple of y
static int ialign(int x, int y)
{
  return idiv(x, y) * y;
}

/*********************************************************************
 * TiledLoop
 *********************************************************************/

void TiledLoop(
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

  GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox,
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

/*********************************************************************
 * TiledLoopOverEverything
 *********************************************************************/

void TiledLoopOverEverything(
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

  TiledLoop(cctkGH, kd_coarse, calc);
}

/*********************************************************************
 * TiledLoopOverInterior
 *********************************************************************/

void TiledLoopOverInterior(
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

  GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox,
                            cctk_nghostzones, 
                            imin, imax, is_symbnd, is_physbnd, is_ipbnd);

  KrancData kd_coarse;

  for (int d = 0; d < 3; d++)
  {
    kd_coarse.imin[d] = imin[d];
    kd_coarse.imax[d] = imax[d];
  }

  TiledLoop(cctkGH, kd_coarse, calc);
}

} // namespace WaveHost