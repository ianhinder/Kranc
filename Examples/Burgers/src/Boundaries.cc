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


extern "C" void Burgers_CheckBoundaries(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  return;
}

extern "C" void Burgers_SelectBoundConds(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED  = 0;
  
  if (CCTK_EQUALS(u_group_bound, "none"  ) ||
      CCTK_EQUALS(u_group_bound, "static") ||
      CCTK_EQUALS(u_group_bound, "flat"  ) ||
      CCTK_EQUALS(u_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "Burgers::u_group", u_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register u_group_bound BC for Burgers::u_group!");
  }
  
  if (CCTK_EQUALS(u_bound, "none"  ) ||
      CCTK_EQUALS(u_bound, "static") ||
      CCTK_EQUALS(u_bound, "flat"  ) ||
      CCTK_EQUALS(u_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "Burgers::u", u_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register u_bound BC for Burgers::u!");
  }
  
  if (CCTK_EQUALS(u_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_u_group_bound CCTK_ATTRIBUTE_UNUSED  = -1;
    if (handle_u_group_bound < 0) handle_u_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_u_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_u_group_bound , u_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_u_group_bound ,u_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_u_group_bound, 
                      "Burgers::u_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for Burgers::u_group!");
  
  }
  
  if (CCTK_EQUALS(u_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_u_bound CCTK_ATTRIBUTE_UNUSED  = -1;
    if (handle_u_bound < 0) handle_u_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_u_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_u_bound , u_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_u_bound ,u_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_u_bound, 
                      "Burgers::u", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for Burgers::u!");
  
  }
  
  if (CCTK_EQUALS(u_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_u_group_bound CCTK_ATTRIBUTE_UNUSED  = -1;
    if (handle_u_group_bound < 0) handle_u_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_u_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_u_group_bound ,u_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_u_group_bound, 
                      "Burgers::u_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for Burgers::u_group!");
  
  }
  
  if (CCTK_EQUALS(u_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_u_bound CCTK_ATTRIBUTE_UNUSED  = -1;
    if (handle_u_bound < 0) handle_u_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_u_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_u_bound ,u_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_u_bound, 
                      "Burgers::u", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for Burgers::u!");
  
  }
  return;
}



/* template for entries in parameter file:
#$bound$#Burgers::u_group_bound       = "skip"
#$bound$#Burgers::u_group_bound_speed = 1.0
#$bound$#Burgers::u_group_bound_limit = 0.0
#$bound$#Burgers::u_group_bound_scalar = 0.0

#$bound$#Burgers::u_bound       = "skip"
#$bound$#Burgers::u_bound_speed = 1.0
#$bound$#Burgers::u_bound_limit = 0.0
#$bound$#Burgers::u_bound_scalar = 0.0

*/

