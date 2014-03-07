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


extern "C" void Advect_CheckBoundaries(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  return;
}

extern "C" void Advect_SelectBoundConds(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  
  if (CCTK_EQUALS(rho_group_bound, "none"  ) ||
      CCTK_EQUALS(rho_group_bound, "static") ||
      CCTK_EQUALS(rho_group_bound, "flat"  ) ||
      CCTK_EQUALS(rho_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "Advect::rho_group", rho_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register rho_group_bound BC for Advect::rho_group!");
  }
  
  if (CCTK_EQUALS(rho_bound, "none"  ) ||
      CCTK_EQUALS(rho_bound, "static") ||
      CCTK_EQUALS(rho_bound, "flat"  ) ||
      CCTK_EQUALS(rho_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "Advect::rho", rho_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register rho_bound BC for Advect::rho!");
  }
  
  if (CCTK_EQUALS(rho_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_rho_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_rho_group_bound < 0) handle_rho_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_rho_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_rho_group_bound , rho_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_rho_group_bound ,rho_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_rho_group_bound, 
                      "Advect::rho_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for Advect::rho_group!");
  
  }
  
  if (CCTK_EQUALS(rho_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_rho_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_rho_bound < 0) handle_rho_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_rho_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_rho_bound , rho_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_rho_bound ,rho_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_rho_bound, 
                      "Advect::rho", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for Advect::rho!");
  
  }
  
  if (CCTK_EQUALS(rho_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_rho_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_rho_group_bound < 0) handle_rho_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_rho_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_rho_group_bound ,rho_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_rho_group_bound, 
                      "Advect::rho_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for Advect::rho_group!");
  
  }
  
  if (CCTK_EQUALS(rho_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_rho_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_rho_bound < 0) handle_rho_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_rho_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_rho_bound ,rho_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_rho_bound, 
                      "Advect::rho", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for Advect::rho!");
  
  }
  return;
}



/* template for entries in parameter file:
#$bound$#Advect::rho_group_bound       = "skip"
#$bound$#Advect::rho_group_bound_speed = 1.0
#$bound$#Advect::rho_group_bound_limit = 0.0
#$bound$#Advect::rho_group_bound_scalar = 0.0

#$bound$#Advect::rho_bound       = "skip"
#$bound$#Advect::rho_bound_speed = 1.0
#$bound$#Advect::rho_bound_limit = 0.0
#$bound$#Advect::rho_bound_scalar = 0.0

*/

