/* $Header$ */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>



#include "perturb.h"

#include "cctk.h"
#include "cctk_Parameters.h"
#include "cctk_Arguments.h"
#include "cctk_FortranString.h"

#include "Symmetry.h"



/* #define DEBUG_BOUNDARY 1 */

static int ApplyBndperturb (const cGH *GH,
			  int stencil_dir,
			  const int *stencil,
			  int dir,
			  int first_var,
			  int num_vars);



int
BndperturbVI (const cGH *GH, const int *stencil, int vi)
{
  int retval;
  retval = ApplyBndperturb (GH, -1, stencil, 0, vi, 1);
  return retval;
}

void
CCTK_FCALL CCTK_FNAME (BndperturbVI) (int *ierr, const cGH **GH,
				    const int *stencil, const int *vi)
{
  *ierr = BndperturbVI (*GH, stencil, *vi);
}

int
BndperturbVN (const cGH *GH, const int *stencil, const char *vn)
{
  int vi, retval;
  vi = CCTK_VarIndex(vn);
  retval = BndperturbVI (GH, stencil, vi);
  return retval;
}

void
CCTK_FCALL CCTK_FNAME (BndperturbVN) (int *ierr, const cGH **GH,
				    const int *stencil, ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (vn);
  *ierr = BndperturbVN (*GH, stencil, vn);
  free (vn);
}



int
BndperturbGI (const cGH *GH, const int *stencil, int gi)
{
  int first_vi, retval;

  first_vi = CCTK_FirstVarIndexI (gi);
  if (first_vi >= 0)
    {
      retval = ApplyBndperturb (GH, -1, stencil, 0, first_vi,
			      CCTK_NumVarsInGroupI (gi));
    }
  else
    {
      CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
		  "Invalid group index %d in BndperturbGI", gi);
      retval = -1;
    }

  return (retval);
}



void
CCTK_FCALL CCTK_FNAME (BndperturbGI) (int *ierr, const cGH **GH,
				    const int *stencil, const int *gi)
{
  *ierr = BndperturbGI (*GH, stencil, *gi);
}


int
BndperturbGN (const cGH *GH, const int *stencil, const char *gn)
{
  int gi, retval;

  gi = CCTK_GroupIndex (gn);
  if (gi >= 0)
    {
      retval = BndperturbGI (GH, stencil, gi);
    }
  else
    {
      CCTK_VWarn (2, __LINE__, __FILE__, CCTK_THORNSTRING,
		  "Invalid group name '%s' in BndperturbGN", gn);
      retval = -1;
    }

  return (retval);
}

void CCTK_FCALL CCTK_FNAME (BndperturbGN)
     (int *ierr,
      const cGH **GH,
      const int *stencil,
      ONE_FORTSTRING_ARG)
{
  ONE_FORTSTRING_CREATE (gn)
    *ierr = BndperturbGN (*GH, stencil, gn);
  free (gn);
}





static int ApplyBndperturb (const cGH *GH,
			  int stencil_dir,
			  const int *stencil,
			  int dir,
			  int first_var,
			  int num_vars)
{
  DECLARE_CCTK_PARAMETERS;
  int i, j, k;
  int var, vtypesize, gindex, gdim, timelvl;
  int doBC[2*MAXDIM], dstag[MAXDIM], lsh[MAXDIM], lssh[MAXDIM];
  SymmetryGHex *sGHex;
  int type;

  /* This argument is unused an undocumented; better make sure people
     don't try to use it for something.  */
  assert (stencil_dir == -1);

  /* get the group index of the variables */
  gindex = CCTK_GroupIndexFromVarI (first_var);

  /* get the number of dimensions and the size of the variables' type */
  gdim      = CCTK_GroupDimI (gindex);
  vtypesize = CCTK_VarTypeSize (CCTK_VarTypeI (first_var));

  /* make sure we can deal with this number of dimensions */
  if (gdim > MAXDIM)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
		  "ApplyBndperturb: Variable dimension of %d not supported", gdim);
      return (-1);
    }

  /* check the direction parameter */
  if (abs (dir) > gdim)
    {
      CCTK_VWarn (1, __LINE__, __FILE__, CCTK_THORNSTRING,
		  "ApplyBndperturb: direction %d greater than dimension %d",
		  dir, gdim);
      return (-2);
    }

  /* initialize arrays for variables with less dimensions than MAXDIM
     so that we can use the INDEX_3D macro later on */
  for (i = gdim; i < MAXDIM; i++)
    {
      lsh[i]  = 1;
      lssh[i] = 1;
    }

  /* get the directional staggering of the group */
  CCTK_GroupStaggerDirArrayGI (dstag, gdim, gindex);

  /* get the current timelevel */
  timelvl = 0;

  /* see if we have a symmetry array */
  sGHex = (SymmetryGHex *) CCTK_GHExtension (GH, "Symmetry");


  /* now loop over all variables */
  for (var = first_var; var < first_var + num_vars; var++)
    {
      /* Apply condition if:
	 + boundary is not a symmetry boundary (no symmetry or unset(=unsed))
	 + boundary is a physical boundary
	 + have enough grid points
      */
      memset (doBC, 1, sizeof (doBC));
      if (sGHex)
	{
	  for (i = 0; i < 2 * gdim; i++)
	    {
	      doBC[i] = sGHex->GFSym[var][i] == GFSYM_NOSYM ||
		sGHex->GFSym[var][i] == GFSYM_UNSET;
	    }
	}
      for (i = 0; i < gdim; i++)
	{
	  lsh[i]       = GH->cctk_lsh[i];
	  lssh[i]      = GH->cctk_lssh[CCTK_LSSH_IDX (dstag[i], i)];
	  doBC[i*2]   &= GH->cctk_lsh[i] > 1 && GH->cctk_bbox[i*2];
	  doBC[i*2+1] &= GH->cctk_lsh[i] > 1 && GH->cctk_bbox[i*2+1];
	  if (dir != 0)
	    {
	      doBC[i*2]   &= i+1 == -dir;
	      doBC[i*2+1] &= i+1 ==  dir;
	    }
	}

      /* now apply the boundaries face by face */
      if (gdim > 0)
	{
#ifdef DEBUG_BOUNDARY
	  if (doBC[0])
	    {
	      printf("Boundary: Applying lower x perturb to boundary\n");
	    }
	  if (doBC[1])
	    {
	      printf("Boundary: Applying upper x perturb to boundary\n");
	    }
#endif /* DEBUG_BOUNDARY */
	  /* lower x */
	  BOUNDARY_PERTURB (doBC[0], stencil[0], lssh[1], lssh[2],
			  i, j, k);
	  /* upper x */
	  BOUNDARY_PERTURB (doBC[1], stencil[0], lssh[1], lssh[2],
			  lssh[0]-i-1, j, k);

	}
      if (gdim > 1)

	{
#ifdef DEBUG_BOUNDARY
	  if (doBC[2])
	    {
	      printf("Boundary: Applying lower y perturb to boundary\n");
	    }
	  if (doBC[3])
	    {
	      printf("Boundary: Applying upper y perturb to boundary\n");
	    }
#endif /* DEBUG_BOUNDARY */
	  /* lower y */
	  BOUNDARY_PERTURB (doBC[2], lssh[0], stencil[1], lssh[2],
			  i, j, k);
	  /* upper y */
	  BOUNDARY_PERTURB (doBC[3], lssh[0], stencil[1], lssh[2],
			  i, lssh[1]-j-1, k);
	}
      if (gdim > 2)
	{
#ifdef DEBUG_BOUNDARY
	  if (doBC[4])
	    {
	      printf("Boundary: Applying lower z perturb to boundary\n");
	    }
	  if (doBC[5])
	    {
	      printf("Boundary: Applying upper z perturb to boundary\n");
	    }
#endif /* DEBUG_BOUNDARY */
	  /* lower z */
	  BOUNDARY_PERTURB (doBC[4], lssh[0], lssh[1], stencil[2],
			  i, j, k);
	  /* upper z */
	  BOUNDARY_PERTURB (doBC[5], lssh[0], lssh[1], stencil[2],
			  i, j, lssh[2]-k-1);
	}
    }

  return(0);
}


static void
add_bc_perturb_to_var (int idx, const char* optstring, void* cctkGH)
{
  DECLARE_CCTK_PARAMETERS;
  cGH* GH = cctkGH;
  int sw[3];

  /* Change type from CCTK_INT to int */
  sw[0] = perturb_stencil[0];
  sw[1] = perturb_stencil[1];
  sw[2] = perturb_stencil[2];

  if (perturb_boundaries[0]) ApplyBndperturb (GH, -1, sw, -1, idx, 1);
  if (perturb_boundaries[1]) ApplyBndperturb (GH, -1, sw, +1, idx, 1);
  if (perturb_boundaries[2]) ApplyBndperturb (GH, -1, sw, -2, idx, 1);
  if (perturb_boundaries[3]) ApplyBndperturb (GH, -1, sw, +2, idx, 1);
  if (perturb_boundaries[4]) ApplyBndperturb (GH, -1, sw, -3, idx, 1);
  if (perturb_boundaries[5]) ApplyBndperturb (GH, -1, sw, +3, idx, 1);
}


void
bc_perturb(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS
  DECLARE_CCTK_PARAMETERS

/*   Boundary_MakeSureThatTheSelectionIsEmpty(); */
  if (CCTK_TraverseString(bc_vars, add_bc_perturb_to_var, cctkGH,
      		    CCTK_GROUP_OR_VAR) < 0)
    {
      CCTK_WARN (1, "Failed to parse 'perturb::bc_vars' parameter");
    }
}
