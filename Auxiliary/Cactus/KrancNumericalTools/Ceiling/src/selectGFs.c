/* $Header$ */

/* this code is based on Erik Schnetter's dissipation thorn */

#include <assert.h>
#include <stdlib.h>

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"

void CCTK_FCALL
CCTK_FNAME(apply_check_abs) (CCTK_REAL const * const var,
                             int       const * const ni,
                             int       const * const nj,
                             int       const * const nk,
                             CCTK_REAL const * const ceiling_value);


void CCTK_FCALL
CCTK_FNAME(apply_check_diff) (CCTK_REAL const * const var,
                              int       const * const ni,
                              int       const * const nj,
                              int       const * const nk,
                              CCTK_REAL const * const ceiling_value);

static void
call_apply_check (int const varindex, char const * const optstring, void * const arg);

void
check_ceiling (CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;

  CCTK_TraverseString (vars, call_apply_check, cctkGH, CCTK_GROUP_OR_VAR);
}


void
call_apply_check (int const varindex, char const * const optstring, void * const arg)
{
  cGH const * const cctkGH = (cGH const *) arg;
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;

  int vargroup;
  cGroup vardata;

  CCTK_REAL const * varptr;
  int ierr /* , terminate */ ;

  assert (varindex >= 0);

  if (verbose) {
    char * const fullvarname = CCTK_FullName (varindex);
    assert (fullvarname);
    CCTK_VInfo (CCTK_THORNSTRING,
                "Applying ceiling check to \"%s\" ",
                fullvarname);
    free (fullvarname);
  }

  vargroup = CCTK_GroupIndexFromVarI (varindex);
  assert (vargroup >= 0);

  ierr = CCTK_GroupData (vargroup, &vardata);
  assert (!ierr);

  assert (vardata.grouptype == CCTK_GF);
  assert (vardata.vartype == CCTK_VARIABLE_REAL);
  assert (vardata.dim == cctk_dim);

  varptr = CCTK_VarDataPtrI (cctkGH, 0, varindex);
  assert (varptr);

  if (type == "absolute")
  {
  CCTK_FNAME(apply_check_abs)
    (varptr, &cctk_lsh[0], &cctk_lsh[1], &cctk_lsh[2], &ceiling_value);
  } else if (type == "differential")
  {
  CCTK_FNAME(apply_check_diff)
    (varptr, &cctk_lsh[0], &cctk_lsh[1], &cctk_lsh[2], &ceiling_value);
  } else {
  CCTK_INFO("keyword ceiling::type only allows values 'absolute' and 'differential'");
  }

  /* if (terminate > 0) {CCTK_TerminateNext (cctkGH);}  */
}

