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


extern "C" void EulerSR_CheckBoundaries(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  return;
}

extern "C" void EulerSR_SelectBoundConds(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr = 0;
  
  if (CCTK_EQUALS(Den_group_bound, "none"  ) ||
      CCTK_EQUALS(Den_group_bound, "static") ||
      CCTK_EQUALS(Den_group_bound, "flat"  ) ||
      CCTK_EQUALS(Den_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerSR::Den_group", Den_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Den_group_bound BC for EulerSR::Den_group!");
  }
  
  if (CCTK_EQUALS(S_group_bound, "none"  ) ||
      CCTK_EQUALS(S_group_bound, "static") ||
      CCTK_EQUALS(S_group_bound, "flat"  ) ||
      CCTK_EQUALS(S_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerSR::S_group", S_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register S_group_bound BC for EulerSR::S_group!");
  }
  
  if (CCTK_EQUALS(tau_group_bound, "none"  ) ||
      CCTK_EQUALS(tau_group_bound, "static") ||
      CCTK_EQUALS(tau_group_bound, "flat"  ) ||
      CCTK_EQUALS(tau_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerSR::tau_group", tau_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register tau_group_bound BC for EulerSR::tau_group!");
  }
  
  if (CCTK_EQUALS(Den_bound, "none"  ) ||
      CCTK_EQUALS(Den_bound, "static") ||
      CCTK_EQUALS(Den_bound, "flat"  ) ||
      CCTK_EQUALS(Den_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerSR::Den", Den_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Den_bound BC for EulerSR::Den!");
  }
  
  if (CCTK_EQUALS(S1_bound, "none"  ) ||
      CCTK_EQUALS(S1_bound, "static") ||
      CCTK_EQUALS(S1_bound, "flat"  ) ||
      CCTK_EQUALS(S1_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerSR::S1", S1_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register S1_bound BC for EulerSR::S1!");
  }
  
  if (CCTK_EQUALS(S2_bound, "none"  ) ||
      CCTK_EQUALS(S2_bound, "static") ||
      CCTK_EQUALS(S2_bound, "flat"  ) ||
      CCTK_EQUALS(S2_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerSR::S2", S2_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register S2_bound BC for EulerSR::S2!");
  }
  
  if (CCTK_EQUALS(S3_bound, "none"  ) ||
      CCTK_EQUALS(S3_bound, "static") ||
      CCTK_EQUALS(S3_bound, "flat"  ) ||
      CCTK_EQUALS(S3_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerSR::S3", S3_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register S3_bound BC for EulerSR::S3!");
  }
  
  if (CCTK_EQUALS(tau_bound, "none"  ) ||
      CCTK_EQUALS(tau_bound, "static") ||
      CCTK_EQUALS(tau_bound, "flat"  ) ||
      CCTK_EQUALS(tau_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerSR::tau", tau_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register tau_bound BC for EulerSR::tau!");
  }
  
  if (CCTK_EQUALS(Den_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_Den_group_bound = -1;
    if (handle_Den_group_bound < 0) handle_Den_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_Den_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_Den_group_bound , Den_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_Den_group_bound ,Den_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_Den_group_bound, 
                      "EulerSR::Den_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerSR::Den_group!");
  
  }
  
  if (CCTK_EQUALS(S_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_S_group_bound = -1;
    if (handle_S_group_bound < 0) handle_S_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S_group_bound , S_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_S_group_bound ,S_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S_group_bound, 
                      "EulerSR::S_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerSR::S_group!");
  
  }
  
  if (CCTK_EQUALS(tau_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_tau_group_bound = -1;
    if (handle_tau_group_bound < 0) handle_tau_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_tau_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_tau_group_bound , tau_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_tau_group_bound ,tau_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_tau_group_bound, 
                      "EulerSR::tau_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerSR::tau_group!");
  
  }
  
  if (CCTK_EQUALS(Den_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_Den_bound = -1;
    if (handle_Den_bound < 0) handle_Den_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_Den_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_Den_bound , Den_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_Den_bound ,Den_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_Den_bound, 
                      "EulerSR::Den", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerSR::Den!");
  
  }
  
  if (CCTK_EQUALS(S1_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_S1_bound = -1;
    if (handle_S1_bound < 0) handle_S1_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S1_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S1_bound , S1_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_S1_bound ,S1_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S1_bound, 
                      "EulerSR::S1", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerSR::S1!");
  
  }
  
  if (CCTK_EQUALS(S2_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_S2_bound = -1;
    if (handle_S2_bound < 0) handle_S2_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S2_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S2_bound , S2_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_S2_bound ,S2_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S2_bound, 
                      "EulerSR::S2", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerSR::S2!");
  
  }
  
  if (CCTK_EQUALS(S3_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_S3_bound = -1;
    if (handle_S3_bound < 0) handle_S3_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S3_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S3_bound , S3_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_S3_bound ,S3_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S3_bound, 
                      "EulerSR::S3", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerSR::S3!");
  
  }
  
  if (CCTK_EQUALS(tau_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_tau_bound = -1;
    if (handle_tau_bound < 0) handle_tau_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_tau_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_tau_bound , tau_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_tau_bound ,tau_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_tau_bound, 
                      "EulerSR::tau", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerSR::tau!");
  
  }
  
  if (CCTK_EQUALS(Den_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_Den_group_bound = -1;
    if (handle_Den_group_bound < 0) handle_Den_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_Den_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_Den_group_bound ,Den_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_Den_group_bound, 
                      "EulerSR::Den_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for EulerSR::Den_group!");
  
  }
  
  if (CCTK_EQUALS(S_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_S_group_bound = -1;
    if (handle_S_group_bound < 0) handle_S_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S_group_bound ,S_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S_group_bound, 
                      "EulerSR::S_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for EulerSR::S_group!");
  
  }
  
  if (CCTK_EQUALS(tau_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_tau_group_bound = -1;
    if (handle_tau_group_bound < 0) handle_tau_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_tau_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_tau_group_bound ,tau_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_tau_group_bound, 
                      "EulerSR::tau_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for EulerSR::tau_group!");
  
  }
  
  if (CCTK_EQUALS(Den_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_Den_bound = -1;
    if (handle_Den_bound < 0) handle_Den_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_Den_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_Den_bound ,Den_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_Den_bound, 
                      "EulerSR::Den", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerSR::Den!");
  
  }
  
  if (CCTK_EQUALS(S1_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_S1_bound = -1;
    if (handle_S1_bound < 0) handle_S1_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S1_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S1_bound ,S1_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S1_bound, 
                      "EulerSR::S1", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerSR::S1!");
  
  }
  
  if (CCTK_EQUALS(S2_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_S2_bound = -1;
    if (handle_S2_bound < 0) handle_S2_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S2_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S2_bound ,S2_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S2_bound, 
                      "EulerSR::S2", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerSR::S2!");
  
  }
  
  if (CCTK_EQUALS(S3_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_S3_bound = -1;
    if (handle_S3_bound < 0) handle_S3_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S3_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S3_bound ,S3_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S3_bound, 
                      "EulerSR::S3", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerSR::S3!");
  
  }
  
  if (CCTK_EQUALS(tau_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_tau_bound = -1;
    if (handle_tau_bound < 0) handle_tau_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_tau_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_tau_bound ,tau_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_tau_bound, 
                      "EulerSR::tau", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerSR::tau!");
  
  }
  return;
}



/* template for entries in parameter file:
#$bound$#EulerSR::Den_group_bound       = "skip"
#$bound$#EulerSR::Den_group_bound_speed = 1.0
#$bound$#EulerSR::Den_group_bound_limit = 0.0
#$bound$#EulerSR::Den_group_bound_scalar = 0.0

#$bound$#EulerSR::S_group_bound       = "skip"
#$bound$#EulerSR::S_group_bound_speed = 1.0
#$bound$#EulerSR::S_group_bound_limit = 0.0
#$bound$#EulerSR::S_group_bound_scalar = 0.0

#$bound$#EulerSR::tau_group_bound       = "skip"
#$bound$#EulerSR::tau_group_bound_speed = 1.0
#$bound$#EulerSR::tau_group_bound_limit = 0.0
#$bound$#EulerSR::tau_group_bound_scalar = 0.0

#$bound$#EulerSR::Den_bound       = "skip"
#$bound$#EulerSR::Den_bound_speed = 1.0
#$bound$#EulerSR::Den_bound_limit = 0.0
#$bound$#EulerSR::Den_bound_scalar = 0.0

#$bound$#EulerSR::S1_bound       = "skip"
#$bound$#EulerSR::S1_bound_speed = 1.0
#$bound$#EulerSR::S1_bound_limit = 0.0
#$bound$#EulerSR::S1_bound_scalar = 0.0

#$bound$#EulerSR::S2_bound       = "skip"
#$bound$#EulerSR::S2_bound_speed = 1.0
#$bound$#EulerSR::S2_bound_limit = 0.0
#$bound$#EulerSR::S2_bound_scalar = 0.0

#$bound$#EulerSR::S3_bound       = "skip"
#$bound$#EulerSR::S3_bound_speed = 1.0
#$bound$#EulerSR::S3_bound_limit = 0.0
#$bound$#EulerSR::S3_bound_scalar = 0.0

#$bound$#EulerSR::tau_bound       = "skip"
#$bound$#EulerSR::tau_bound_speed = 1.0
#$bound$#EulerSR::tau_bound_limit = 0.0
#$bound$#EulerSR::tau_bound_scalar = 0.0

*/

