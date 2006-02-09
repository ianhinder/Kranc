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
            
#include "Symmetry.h"                         


#define KRANC_C
                                                
#include "GenericFD.h"


/* TODO: provide functions for differencing, use FD macros to
   evaluate == use macros to evaluate corresponding functions */

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
    is_physbnd[dir] = (!is_ipbnd[dir] && !is_symbnd[dir])
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

