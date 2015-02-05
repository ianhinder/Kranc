/*  File produced by Kranc */

#include "cctk.h"
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"
#include "cctk_Faces.h"
#include "util_Table.h"
#include "Symmetry.h"


/* the boundary treatment is split into 3 steps:    */
/* 1. excision                                      */
/* 2. symmetries                                    */
/* 3. "other" boundary conditions, e.g. radiative */

/* to simplify scheduling and testing, the 3 steps  */
/* are currently applied in separate functions      */


extern "C" void WaveCaKernel_CheckBoundaries(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  return;
}

extern "C" void WaveCaKernel_SelectBoundConds(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  
  if (CCTK_EQUALS(phi_g_bound, "none"  ) ||
      CCTK_EQUALS(phi_g_bound, "static") ||
      CCTK_EQUALS(phi_g_bound, "flat"  ) ||
      CCTK_EQUALS(phi_g_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "WaveCaKernel::phi_g", phi_g_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register phi_g_bound BC for WaveCaKernel::phi_g!");
  }
  
  if (CCTK_EQUALS(pi_g_bound, "none"  ) ||
      CCTK_EQUALS(pi_g_bound, "static") ||
      CCTK_EQUALS(pi_g_bound, "flat"  ) ||
      CCTK_EQUALS(pi_g_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "WaveCaKernel::pi_g", pi_g_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register pi_g_bound BC for WaveCaKernel::pi_g!");
  }
  
  if (CCTK_EQUALS(phi_bound, "none"  ) ||
      CCTK_EQUALS(phi_bound, "static") ||
      CCTK_EQUALS(phi_bound, "flat"  ) ||
      CCTK_EQUALS(phi_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "WaveCaKernel::phi", phi_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register phi_bound BC for WaveCaKernel::phi!");
  }
  
  if (CCTK_EQUALS(pi_bound, "none"  ) ||
      CCTK_EQUALS(pi_bound, "static") ||
      CCTK_EQUALS(pi_bound, "flat"  ) ||
      CCTK_EQUALS(pi_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "WaveCaKernel::pi", pi_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register pi_bound BC for WaveCaKernel::pi!");
  }
  
  if (CCTK_EQUALS(phi_g_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_phi_g_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_phi_g_bound < 0) handle_phi_g_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_phi_g_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_phi_g_bound , phi_g_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_phi_g_bound ,phi_g_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_phi_g_bound, 
                      "WaveCaKernel::phi_g", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for WaveCaKernel::phi_g!");
  
  }
  
  if (CCTK_EQUALS(pi_g_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_pi_g_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_pi_g_bound < 0) handle_pi_g_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_pi_g_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_pi_g_bound , pi_g_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_pi_g_bound ,pi_g_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_pi_g_bound, 
                      "WaveCaKernel::pi_g", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for WaveCaKernel::pi_g!");
  
  }
  
  if (CCTK_EQUALS(phi_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_phi_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_phi_bound < 0) handle_phi_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_phi_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_phi_bound , phi_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_phi_bound ,phi_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_phi_bound, 
                      "WaveCaKernel::phi", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for WaveCaKernel::phi!");
  
  }
  
  if (CCTK_EQUALS(pi_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_pi_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_pi_bound < 0) handle_pi_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_pi_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_pi_bound , pi_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_pi_bound ,pi_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_pi_bound, 
                      "WaveCaKernel::pi", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for WaveCaKernel::pi!");
  
  }
  
  if (CCTK_EQUALS(phi_g_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_phi_g_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_phi_g_bound < 0) handle_phi_g_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_phi_g_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_phi_g_bound ,phi_g_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_phi_g_bound, 
                      "WaveCaKernel::phi_g", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for WaveCaKernel::phi_g!");
  
  }
  
  if (CCTK_EQUALS(pi_g_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_pi_g_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_pi_g_bound < 0) handle_pi_g_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_pi_g_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_pi_g_bound ,pi_g_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_pi_g_bound, 
                      "WaveCaKernel::pi_g", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for WaveCaKernel::pi_g!");
  
  }
  
  if (CCTK_EQUALS(phi_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_phi_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_phi_bound < 0) handle_phi_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_phi_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_phi_bound ,phi_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_phi_bound, 
                      "WaveCaKernel::phi", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for WaveCaKernel::phi!");
  
  }
  
  if (CCTK_EQUALS(pi_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_pi_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_pi_bound < 0) handle_pi_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_pi_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_pi_bound ,pi_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_pi_bound, 
                      "WaveCaKernel::pi", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for WaveCaKernel::pi!");
  
  }
  return;
}



/* template for entries in parameter file:
#$bound$#WaveCaKernel::phi_g_bound       = "skip"
#$bound$#WaveCaKernel::phi_g_bound_speed = 1.0
#$bound$#WaveCaKernel::phi_g_bound_limit = 0.0
#$bound$#WaveCaKernel::phi_g_bound_scalar = 0.0

#$bound$#WaveCaKernel::pi_g_bound       = "skip"
#$bound$#WaveCaKernel::pi_g_bound_speed = 1.0
#$bound$#WaveCaKernel::pi_g_bound_limit = 0.0
#$bound$#WaveCaKernel::pi_g_bound_scalar = 0.0

#$bound$#WaveCaKernel::phi_bound       = "skip"
#$bound$#WaveCaKernel::phi_bound_speed = 1.0
#$bound$#WaveCaKernel::phi_bound_limit = 0.0
#$bound$#WaveCaKernel::phi_bound_scalar = 0.0

#$bound$#WaveCaKernel::pi_bound       = "skip"
#$bound$#WaveCaKernel::pi_bound_speed = 1.0
#$bound$#WaveCaKernel::pi_bound_limit = 0.0
#$bound$#WaveCaKernel::pi_bound_scalar = 0.0

*/

