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


extern "C" void EulerAuto_CheckBoundaries(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  return;
}

extern "C" void EulerAuto_SelectBoundConds(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  
  if (CCTK_EQUALS(Den_group_bound, "none"  ) ||
      CCTK_EQUALS(Den_group_bound, "static") ||
      CCTK_EQUALS(Den_group_bound, "flat"  ) ||
      CCTK_EQUALS(Den_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerAuto::Den_group", Den_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Den_group_bound BC for EulerAuto::Den_group!");
  }
  
  if (CCTK_EQUALS(S_group_bound, "none"  ) ||
      CCTK_EQUALS(S_group_bound, "static") ||
      CCTK_EQUALS(S_group_bound, "flat"  ) ||
      CCTK_EQUALS(S_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerAuto::S_group", S_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register S_group_bound BC for EulerAuto::S_group!");
  }
  
  if (CCTK_EQUALS(En_group_bound, "none"  ) ||
      CCTK_EQUALS(En_group_bound, "static") ||
      CCTK_EQUALS(En_group_bound, "flat"  ) ||
      CCTK_EQUALS(En_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerAuto::En_group", En_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register En_group_bound BC for EulerAuto::En_group!");
  }
  
  if (CCTK_EQUALS(Den_bound, "none"  ) ||
      CCTK_EQUALS(Den_bound, "static") ||
      CCTK_EQUALS(Den_bound, "flat"  ) ||
      CCTK_EQUALS(Den_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerAuto::Den", Den_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Den_bound BC for EulerAuto::Den!");
  }
  
  if (CCTK_EQUALS(S1_bound, "none"  ) ||
      CCTK_EQUALS(S1_bound, "static") ||
      CCTK_EQUALS(S1_bound, "flat"  ) ||
      CCTK_EQUALS(S1_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerAuto::S1", S1_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register S1_bound BC for EulerAuto::S1!");
  }
  
  if (CCTK_EQUALS(S2_bound, "none"  ) ||
      CCTK_EQUALS(S2_bound, "static") ||
      CCTK_EQUALS(S2_bound, "flat"  ) ||
      CCTK_EQUALS(S2_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerAuto::S2", S2_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register S2_bound BC for EulerAuto::S2!");
  }
  
  if (CCTK_EQUALS(S3_bound, "none"  ) ||
      CCTK_EQUALS(S3_bound, "static") ||
      CCTK_EQUALS(S3_bound, "flat"  ) ||
      CCTK_EQUALS(S3_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerAuto::S3", S3_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register S3_bound BC for EulerAuto::S3!");
  }
  
  if (CCTK_EQUALS(En_bound, "none"  ) ||
      CCTK_EQUALS(En_bound, "static") ||
      CCTK_EQUALS(En_bound, "flat"  ) ||
      CCTK_EQUALS(En_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EulerAuto::En", En_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register En_bound BC for EulerAuto::En!");
  }
  
  if (CCTK_EQUALS(Den_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_Den_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_Den_group_bound < 0) handle_Den_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_Den_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_Den_group_bound , Den_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_Den_group_bound ,Den_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_Den_group_bound, 
                      "EulerAuto::Den_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerAuto::Den_group!");
  
  }
  
  if (CCTK_EQUALS(S_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_S_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_S_group_bound < 0) handle_S_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S_group_bound , S_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_S_group_bound ,S_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S_group_bound, 
                      "EulerAuto::S_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerAuto::S_group!");
  
  }
  
  if (CCTK_EQUALS(En_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_En_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_En_group_bound < 0) handle_En_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_En_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_En_group_bound , En_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_En_group_bound ,En_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_En_group_bound, 
                      "EulerAuto::En_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerAuto::En_group!");
  
  }
  
  if (CCTK_EQUALS(Den_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_Den_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_Den_bound < 0) handle_Den_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_Den_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_Den_bound , Den_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_Den_bound ,Den_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_Den_bound, 
                      "EulerAuto::Den", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerAuto::Den!");
  
  }
  
  if (CCTK_EQUALS(S1_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_S1_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_S1_bound < 0) handle_S1_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S1_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S1_bound , S1_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_S1_bound ,S1_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S1_bound, 
                      "EulerAuto::S1", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerAuto::S1!");
  
  }
  
  if (CCTK_EQUALS(S2_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_S2_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_S2_bound < 0) handle_S2_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S2_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S2_bound , S2_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_S2_bound ,S2_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S2_bound, 
                      "EulerAuto::S2", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerAuto::S2!");
  
  }
  
  if (CCTK_EQUALS(S3_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_S3_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_S3_bound < 0) handle_S3_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S3_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S3_bound , S3_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_S3_bound ,S3_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S3_bound, 
                      "EulerAuto::S3", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerAuto::S3!");
  
  }
  
  if (CCTK_EQUALS(En_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_En_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_En_bound < 0) handle_En_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_En_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_En_bound , En_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_En_bound ,En_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_En_bound, 
                      "EulerAuto::En", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EulerAuto::En!");
  
  }
  
  if (CCTK_EQUALS(Den_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_Den_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_Den_group_bound < 0) handle_Den_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_Den_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_Den_group_bound ,Den_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_Den_group_bound, 
                      "EulerAuto::Den_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for EulerAuto::Den_group!");
  
  }
  
  if (CCTK_EQUALS(S_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_S_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_S_group_bound < 0) handle_S_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S_group_bound ,S_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S_group_bound, 
                      "EulerAuto::S_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for EulerAuto::S_group!");
  
  }
  
  if (CCTK_EQUALS(En_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_En_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_En_group_bound < 0) handle_En_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_En_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_En_group_bound ,En_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_En_group_bound, 
                      "EulerAuto::En_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for EulerAuto::En_group!");
  
  }
  
  if (CCTK_EQUALS(Den_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_Den_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_Den_bound < 0) handle_Den_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_Den_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_Den_bound ,Den_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_Den_bound, 
                      "EulerAuto::Den", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerAuto::Den!");
  
  }
  
  if (CCTK_EQUALS(S1_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_S1_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_S1_bound < 0) handle_S1_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S1_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S1_bound ,S1_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S1_bound, 
                      "EulerAuto::S1", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerAuto::S1!");
  
  }
  
  if (CCTK_EQUALS(S2_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_S2_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_S2_bound < 0) handle_S2_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S2_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S2_bound ,S2_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S2_bound, 
                      "EulerAuto::S2", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerAuto::S2!");
  
  }
  
  if (CCTK_EQUALS(S3_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_S3_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_S3_bound < 0) handle_S3_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_S3_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_S3_bound ,S3_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_S3_bound, 
                      "EulerAuto::S3", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerAuto::S3!");
  
  }
  
  if (CCTK_EQUALS(En_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_En_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_En_bound < 0) handle_En_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_En_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_En_bound ,En_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_En_bound, 
                      "EulerAuto::En", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EulerAuto::En!");
  
  }
  return;
}



/* template for entries in parameter file:
#$bound$#EulerAuto::Den_group_bound       = "skip"
#$bound$#EulerAuto::Den_group_bound_speed = 1.0
#$bound$#EulerAuto::Den_group_bound_limit = 0.0
#$bound$#EulerAuto::Den_group_bound_scalar = 0.0

#$bound$#EulerAuto::S_group_bound       = "skip"
#$bound$#EulerAuto::S_group_bound_speed = 1.0
#$bound$#EulerAuto::S_group_bound_limit = 0.0
#$bound$#EulerAuto::S_group_bound_scalar = 0.0

#$bound$#EulerAuto::En_group_bound       = "skip"
#$bound$#EulerAuto::En_group_bound_speed = 1.0
#$bound$#EulerAuto::En_group_bound_limit = 0.0
#$bound$#EulerAuto::En_group_bound_scalar = 0.0

#$bound$#EulerAuto::Den_bound       = "skip"
#$bound$#EulerAuto::Den_bound_speed = 1.0
#$bound$#EulerAuto::Den_bound_limit = 0.0
#$bound$#EulerAuto::Den_bound_scalar = 0.0

#$bound$#EulerAuto::S1_bound       = "skip"
#$bound$#EulerAuto::S1_bound_speed = 1.0
#$bound$#EulerAuto::S1_bound_limit = 0.0
#$bound$#EulerAuto::S1_bound_scalar = 0.0

#$bound$#EulerAuto::S2_bound       = "skip"
#$bound$#EulerAuto::S2_bound_speed = 1.0
#$bound$#EulerAuto::S2_bound_limit = 0.0
#$bound$#EulerAuto::S2_bound_scalar = 0.0

#$bound$#EulerAuto::S3_bound       = "skip"
#$bound$#EulerAuto::S3_bound_speed = 1.0
#$bound$#EulerAuto::S3_bound_limit = 0.0
#$bound$#EulerAuto::S3_bound_scalar = 0.0

#$bound$#EulerAuto::En_bound       = "skip"
#$bound$#EulerAuto::En_bound_speed = 1.0
#$bound$#EulerAuto::En_bound_limit = 0.0
#$bound$#EulerAuto::En_bound_scalar = 0.0

*/

