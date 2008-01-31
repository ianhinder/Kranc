/*@@                                         
   @file      GenericFD/src/GenericFD.c
   @date      June 16 2002
   @author    S. Husa                           
   @desc

   $Id$                                  
   
   @enddesc                                     
 @@*/                                           

/*  Copyright 2004 Sascha Husa, Ian Hinder, Christiane Lechner

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
    along with Foobar; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
                                              
#include "cctk.h"                             
#include "cctk_Arguments.h"                   
#include "cctk_Parameters.h"                  
#include "util_Table.h"
#include <assert.h>
#include <stdlib.h>
#include <math.h>
            
#include "Symmetry.h"                         


#define KRANC_C
                                                
#include "GenericFD.h"


/* TODO: provide functions for differencing, use FD macros to evaluate
   corresponding functions */

CCTK_INT sgn(CCTK_REAL x)
{
  if (x < 0)
    return -1;
  else if (x > 0)
    return 1;
  else 
    return 0;
}

/* Return the array indices in imin and imax for looping over the
   interior of the grid. imin is the index of the first grid point.
   imax is the index of the last grid point plus 1.  So a loop over
   the interior of the grid would be 

   for (i = imin; i < imax; i++)

   The indexing is C-style. Also return whether the boundary is a
   symmetry, physical or interprocessor boundary.  Carpet refinement
   boundaries are treated as interprocessor boundaries.
*/
void GenericFD_GetBoundaryInfo(cGH *cctkGH, CCTK_INT *cctk_lsh, CCTK_INT *cctk_bbox,
			       CCTK_INT *cctk_nghostzones, CCTK_INT *imin, 
			       CCTK_INT *imax, CCTK_INT *is_symbnd, 
			       CCTK_INT *is_physbnd, CCTK_INT *is_ipbnd)
{
  CCTK_INT nboundaryzones[6];
  CCTK_INT is_internal[6];
  CCTK_INT is_staggered[6];
  CCTK_INT shiftout[6];
  CCTK_INT symbnd[6];

  CCTK_INT symtable = 0;
  CCTK_INT dir = 0;
  CCTK_INT face = 0;
  CCTK_INT npoints = 0;
  CCTK_INT iret = 0;
  CCTK_INT ierr = 0;

  ierr = GetBoundarySpecification(6, nboundaryzones, is_internal, is_staggered, 
				  shiftout);
  if (ierr != 0)
    CCTK_WARN(0, "Could not obtain boundary specification");

  symtable = SymmetryTableHandleForGrid(cctkGH);
  if (symtable < 0) 
  {
    CCTK_WARN(0, "Could not obtain symmetry table");
  }  
  
  iret = Util_TableGetIntArray(symtable, 6, symbnd, "symmetry_handle");
  if (iret != 6) CCTK_WARN (0, "Could not obtain symmetry information");

  for (dir = 0; dir < 6; dir++)
  {
    is_symbnd[dir] = (symbnd[dir] >= 0);
    is_ipbnd[dir] = (cctk_bbox[dir] == 0);
    is_physbnd[dir] = (!is_ipbnd[dir] && !is_symbnd[dir]);
  }

  for (dir = 0; dir < 3; dir++)
  {
    for (face = 0; face < 2; face++)
    {
      CCTK_INT index = dir*2 + face;
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


void GenericFD_LoopOverEverything(cGH *cctkGH, Kranc_Calculation calc)
{
  DECLARE_CCTK_ARGUMENTS

  CCTK_INT   dir = 0;
  CCTK_INT   face = 0;
  CCTK_REAL  normal[] = {0,0,0};
  CCTK_REAL  tangentA[] = {0,0,0};
  CCTK_REAL  tangentB[] = {0,0,0};
  CCTK_INT   bmin[] = {0,0,0};
  CCTK_INT   bmax[] = {cctk_lsh[0], cctk_lsh[1], cctk_lsh[2]};

  calc(cctkGH, dir, face, normal, tangentA, tangentB, bmin, bmax, 0, NULL);
  return;
}


void GenericFD_LoopOverBoundary(cGH *cctkGH, Kranc_Calculation calc)
{
  DECLARE_CCTK_ARGUMENTS

  CCTK_INT   dir1, dir2, dir3;
  CCTK_INT   dir[3];
  CCTK_REAL  normal[3];
  CCTK_REAL  tangentA[3];
  CCTK_REAL  tangentB[3];
  CCTK_INT   bmin[3];
  CCTK_INT   bmax[3];
  CCTK_INT   here_is_physbnd;
  CCTK_INT   d;

  CCTK_INT   is_symbnd[6], is_physbnd[6], is_ipbnd[6];
  CCTK_INT   imin[3], imax[3];
  int        old_dir = 0;
  int        old_face = 0;

  GenericFD_GetBoundaryInfo(cctkGH, cctk_lsh, cctk_bbox, cctk_nghostzones, 
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

        here_is_physbnd = 0;

        for (d = 0; d < 3; d++)
        {
          switch(dir[d])
          {
          case -1:
            bmin[d] = 0;
            bmax[d] = imin[d];
            here_is_physbnd = here_is_physbnd || is_physbnd[2*d+0];
            break;
          case 0:
            bmin[d] = imin[d];
            bmax[d] = imax[d];
            break;
          case +1:
            bmin[d] = imax[d];
            bmax[d] = cctk_lsh[d];
            here_is_physbnd = here_is_physbnd || is_physbnd[2*d+1];
            break;
          }

          /* Choose a basis */
          normal[d] = dir[d];
          tangentA[d] = dir[(d+1)%3];
          tangentB[d] = dir[(d+2)%3];
        }

        if (here_is_physbnd)
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

void GenericFD_LoopOverInterior(cGH *cctkGH, Kranc_Calculation calc)
{
  DECLARE_CCTK_ARGUMENTS

  CCTK_REAL  normal[] = {0,0,0};
  CCTK_REAL  tangentA[] = {0,0,0};
  CCTK_REAL  tangentB[] = {0,0,0};

  CCTK_INT   is_symbnd[6], is_physbnd[6], is_ipbnd[6];
  CCTK_INT   imin[3], imax[3];
  int        dir = 0;
  int        face = 0;

  GenericFD_GetBoundaryInfo(cctkGH, cctk_lsh, cctk_bbox, cctk_nghostzones, 
                            imin, imax, is_symbnd, is_physbnd, is_ipbnd);

  calc(cctkGH, dir, face, normal, tangentA, tangentB, imin, imax, 0, NULL);
  
  return;
}

void GenericFD_PenaltyPrim2Char(cGH *cctkGH, CCTK_INT const dir,
                                CCTK_INT const face,
                                CCTK_REAL const * restrict const base,
                                CCTK_INT const * restrict const lbnd,
                                CCTK_INT const * restrict const lsh,
                                CCTK_INT const rhs_flag,
                                CCTK_INT const num_modes,
                                CCTK_POINTER const * restrict const modes,
                                CCTK_POINTER const * restrict const speeds,
                                Kranc_Calculation calc)
{
  DECLARE_CCTK_ARGUMENTS

  CCTK_REAL  normal[] = {0,0,0};
  CCTK_REAL  tangentA[] = {0,0,0};
  CCTK_REAL  tangentB[] = {0,0,0};
  CCTK_INT   bmin[] = {0,0,0};
  CCTK_INT   bmax[] = {cctk_lsh[0], cctk_lsh[1], cctk_lsh[2]};
  CCTK_REAL  **all_vars;
  int        i = 0;

  all_vars = malloc(num_modes*2*sizeof(CCTK_REAL *));
  assert(all_vars != NULL);

  for (i = 0; i < num_modes; i++)
  {
    all_vars[i] = (CCTK_REAL *) modes[i];
    all_vars[num_modes + i] = (CCTK_REAL *) speeds[i];
  }

  for (int d=0; d<3; ++d) {
    normal[d] = base[d];        /* A covector, index down */
    tangentA[d] = base[d+3];    /* A vector, index up */
    tangentB[d] = base[d+6];    /* A vector, index up */
  }

  calc(cctkGH, dir, face, normal, tangentA, tangentB, bmin, bmax, num_modes * 2, all_vars);

  free(all_vars);
  
  return;
}
