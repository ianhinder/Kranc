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


extern "C" void Wave_CheckBoundaries(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  return;
}

extern "C" void Wave_SelectBoundConds(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  
  if (CCTK_EQUALS(evolved_bound, "none"  ) ||
      CCTK_EQUALS(evolved_bound, "static") ||
      CCTK_EQUALS(evolved_bound, "flat"  ) ||
      CCTK_EQUALS(evolved_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "Wave::evolved", evolved_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register evolved_bound BC for Wave::evolved!");
  }
  
  if (CCTK_EQUALS(phi_bound, "none"  ) ||
      CCTK_EQUALS(phi_bound, "static") ||
      CCTK_EQUALS(phi_bound, "flat"  ) ||
      CCTK_EQUALS(phi_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "Wave::phi", phi_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register phi_bound BC for Wave::phi!");
  }
  
  if (CCTK_EQUALS(pi_bound, "none"  ) ||
      CCTK_EQUALS(pi_bound, "static") ||
      CCTK_EQUALS(pi_bound, "flat"  ) ||
      CCTK_EQUALS(pi_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "Wave::pi", pi_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register pi_bound BC for Wave::pi!");
  }
  
  if (CCTK_EQUALS(evolved_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_evolved_bound = -1;
    if (handle_evolved_bound < 0) handle_evolved_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_evolved_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_evolved_bound , evolved_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_evolved_bound ,evolved_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_evolved_bound, 
                      "Wave::evolved", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for Wave::evolved!");
  
  }
  
  if (CCTK_EQUALS(phi_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_phi_bound = -1;
    if (handle_phi_bound < 0) handle_phi_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_phi_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_phi_bound , phi_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_phi_bound ,phi_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_phi_bound, 
                      "Wave::phi", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for Wave::phi!");
  
  }
  
  if (CCTK_EQUALS(pi_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_pi_bound = -1;
    if (handle_pi_bound < 0) handle_pi_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_pi_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_pi_bound , pi_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_pi_bound ,pi_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_pi_bound, 
                      "Wave::pi", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for Wave::pi!");
  
  }
  
  if (CCTK_EQUALS(evolved_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_evolved_bound = -1;
    if (handle_evolved_bound < 0) handle_evolved_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_evolved_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_evolved_bound ,evolved_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_evolved_bound, 
                      "Wave::evolved", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for Wave::evolved!");
  
  }
  
  if (CCTK_EQUALS(phi_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_phi_bound = -1;
    if (handle_phi_bound < 0) handle_phi_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_phi_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_phi_bound ,phi_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_phi_bound, 
                      "Wave::phi", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for Wave::phi!");
  
  }
  
  if (CCTK_EQUALS(pi_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_pi_bound = -1;
    if (handle_pi_bound < 0) handle_pi_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_pi_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_pi_bound ,pi_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_pi_bound, 
                      "Wave::pi", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for Wave::pi!");
  
  }
  return;
}



/* template for entries in parameter file:
#$bound$#Wave::evolved_bound       = "skip"
#$bound$#Wave::evolved_bound_speed = 1.0
#$bound$#Wave::evolved_bound_limit = 0.0
#$bound$#Wave::evolved_bound_scalar = 0.0

#$bound$#Wave::phi_bound       = "skip"
#$bound$#Wave::phi_bound_speed = 1.0
#$bound$#Wave::phi_bound_limit = 0.0
#$bound$#Wave::phi_bound_scalar = 0.0

#$bound$#Wave::pi_bound       = "skip"
#$bound$#Wave::pi_bound_speed = 1.0
#$bound$#Wave::pi_bound_limit = 0.0
#$bound$#Wave::pi_bound_scalar = 0.0

*/

