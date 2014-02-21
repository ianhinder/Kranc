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


extern "C" void EMScript_CheckBoundaries(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  return;
}

extern "C" void EMScript_SelectBoundConds(CCTK_ARGUMENTS)
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;
  
  CCTK_INT ierr CCTK_ATTRIBUTE_UNUSED = 0;
  
  if (CCTK_EQUALS(El_group_bound, "none"  ) ||
      CCTK_EQUALS(El_group_bound, "static") ||
      CCTK_EQUALS(El_group_bound, "flat"  ) ||
      CCTK_EQUALS(El_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EMScript::El_group", El_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register El_group_bound BC for EMScript::El_group!");
  }
  
  if (CCTK_EQUALS(B_group_bound, "none"  ) ||
      CCTK_EQUALS(B_group_bound, "static") ||
      CCTK_EQUALS(B_group_bound, "flat"  ) ||
      CCTK_EQUALS(B_group_bound, "zero"  ) )
  {
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EMScript::B_group", B_group_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register B_group_bound BC for EMScript::B_group!");
  }
  
  if (CCTK_EQUALS(El1_bound, "none"  ) ||
      CCTK_EQUALS(El1_bound, "static") ||
      CCTK_EQUALS(El1_bound, "flat"  ) ||
      CCTK_EQUALS(El1_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EMScript::El1", El1_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register El1_bound BC for EMScript::El1!");
  }
  
  if (CCTK_EQUALS(El2_bound, "none"  ) ||
      CCTK_EQUALS(El2_bound, "static") ||
      CCTK_EQUALS(El2_bound, "flat"  ) ||
      CCTK_EQUALS(El2_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EMScript::El2", El2_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register El2_bound BC for EMScript::El2!");
  }
  
  if (CCTK_EQUALS(El3_bound, "none"  ) ||
      CCTK_EQUALS(El3_bound, "static") ||
      CCTK_EQUALS(El3_bound, "flat"  ) ||
      CCTK_EQUALS(El3_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EMScript::El3", El3_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register El3_bound BC for EMScript::El3!");
  }
  
  if (CCTK_EQUALS(B1_bound, "none"  ) ||
      CCTK_EQUALS(B1_bound, "static") ||
      CCTK_EQUALS(B1_bound, "flat"  ) ||
      CCTK_EQUALS(B1_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EMScript::B1", B1_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register B1_bound BC for EMScript::B1!");
  }
  
  if (CCTK_EQUALS(B2_bound, "none"  ) ||
      CCTK_EQUALS(B2_bound, "static") ||
      CCTK_EQUALS(B2_bound, "flat"  ) ||
      CCTK_EQUALS(B2_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EMScript::B2", B2_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register B2_bound BC for EMScript::B2!");
  }
  
  if (CCTK_EQUALS(B3_bound, "none"  ) ||
      CCTK_EQUALS(B3_bound, "static") ||
      CCTK_EQUALS(B3_bound, "flat"  ) ||
      CCTK_EQUALS(B3_bound, "zero"  ) )
  {
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,
                      "EMScript::B3", B3_bound);
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register B3_bound BC for EMScript::B3!");
  }
  
  if (CCTK_EQUALS(El_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_El_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_El_group_bound < 0) handle_El_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_El_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_El_group_bound , El_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_El_group_bound ,El_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_El_group_bound, 
                      "EMScript::El_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EMScript::El_group!");
  
  }
  
  if (CCTK_EQUALS(B_group_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_B_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_B_group_bound < 0) handle_B_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_B_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_B_group_bound , B_group_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_B_group_bound ,B_group_bound_speed, "SPEED") < 0)
       CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_B_group_bound, 
                      "EMScript::B_group", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EMScript::B_group!");
  
  }
  
  if (CCTK_EQUALS(El1_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_El1_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_El1_bound < 0) handle_El1_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_El1_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_El1_bound , El1_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_El1_bound ,El1_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_El1_bound, 
                      "EMScript::El1", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EMScript::El1!");
  
  }
  
  if (CCTK_EQUALS(El2_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_El2_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_El2_bound < 0) handle_El2_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_El2_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_El2_bound , El2_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_El2_bound ,El2_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_El2_bound, 
                      "EMScript::El2", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EMScript::El2!");
  
  }
  
  if (CCTK_EQUALS(El3_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_El3_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_El3_bound < 0) handle_El3_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_El3_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_El3_bound , El3_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_El3_bound ,El3_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_El3_bound, 
                      "EMScript::El3", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EMScript::El3!");
  
  }
  
  if (CCTK_EQUALS(B1_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_B1_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_B1_bound < 0) handle_B1_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_B1_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_B1_bound , B1_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_B1_bound ,B1_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_B1_bound, 
                      "EMScript::B1", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EMScript::B1!");
  
  }
  
  if (CCTK_EQUALS(B2_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_B2_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_B2_bound < 0) handle_B2_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_B2_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_B2_bound , B2_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_B2_bound ,B2_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_B2_bound, 
                      "EMScript::B2", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EMScript::B2!");
  
  }
  
  if (CCTK_EQUALS(B3_bound, "radiative"))
  {
   /* select radiation boundary condition */
    static CCTK_INT handle_B3_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_B3_bound < 0) handle_B3_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_B3_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_B3_bound , B3_bound_limit, "LIMIT") < 0)
       CCTK_WARN(0, "could not set LIMIT value in table!");
    if (Util_TableSetReal(handle_B3_bound ,B3_bound_speed, "SPEED") < 0)
        CCTK_WARN(0, "could not set SPEED value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_B3_bound, 
                      "EMScript::B3", "Radiation");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Radiation BC for EMScript::B3!");
  
  }
  
  if (CCTK_EQUALS(El_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_El_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_El_group_bound < 0) handle_El_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_El_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_El_group_bound ,El_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_El_group_bound, 
                      "EMScript::El_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for EMScript::El_group!");
  
  }
  
  if (CCTK_EQUALS(B_group_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_B_group_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_B_group_bound < 0) handle_B_group_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_B_group_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_B_group_bound ,B_group_bound_scalar, "SCALAR") < 0)
        CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, handle_B_group_bound, 
                      "EMScript::B_group", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Failed to register Scalar BC for EMScript::B_group!");
  
  }
  
  if (CCTK_EQUALS(El1_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_El1_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_El1_bound < 0) handle_El1_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_El1_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_El1_bound ,El1_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_El1_bound, 
                      "EMScript::El1", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EMScript::El1!");
  
  }
  
  if (CCTK_EQUALS(El2_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_El2_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_El2_bound < 0) handle_El2_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_El2_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_El2_bound ,El2_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_El2_bound, 
                      "EMScript::El2", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EMScript::El2!");
  
  }
  
  if (CCTK_EQUALS(El3_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_El3_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_El3_bound < 0) handle_El3_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_El3_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_El3_bound ,El3_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_El3_bound, 
                      "EMScript::El3", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EMScript::El3!");
  
  }
  
  if (CCTK_EQUALS(B1_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_B1_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_B1_bound < 0) handle_B1_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_B1_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_B1_bound ,B1_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_B1_bound, 
                      "EMScript::B1", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EMScript::B1!");
  
  }
  
  if (CCTK_EQUALS(B2_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_B2_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_B2_bound < 0) handle_B2_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_B2_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_B2_bound ,B2_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_B2_bound, 
                      "EMScript::B2", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EMScript::B2!");
  
  }
  
  if (CCTK_EQUALS(B3_bound, "scalar"))
  {
   /* select scalar boundary condition */
    static CCTK_INT handle_B3_bound CCTK_ATTRIBUTE_UNUSED = -1;
    if (handle_B3_bound < 0) handle_B3_bound = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);
    if (handle_B3_bound < 0) CCTK_WARN(0, "could not create table!");
    if (Util_TableSetReal(handle_B3_bound ,B3_bound_scalar, "SCALAR") < 0)
      CCTK_WARN(0, "could not set SCALAR value in table!");
  
    ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, handle_B3_bound, 
                      "EMScript::B3", "scalar");
  
    if (ierr < 0)
       CCTK_WARN(0, "Error in registering Scalar BC for EMScript::B3!");
  
  }
  return;
}



/* template for entries in parameter file:
#$bound$#EMScript::El_group_bound       = "skip"
#$bound$#EMScript::El_group_bound_speed = 1.0
#$bound$#EMScript::El_group_bound_limit = 0.0
#$bound$#EMScript::El_group_bound_scalar = 0.0

#$bound$#EMScript::B_group_bound       = "skip"
#$bound$#EMScript::B_group_bound_speed = 1.0
#$bound$#EMScript::B_group_bound_limit = 0.0
#$bound$#EMScript::B_group_bound_scalar = 0.0

#$bound$#EMScript::El1_bound       = "skip"
#$bound$#EMScript::El1_bound_speed = 1.0
#$bound$#EMScript::El1_bound_limit = 0.0
#$bound$#EMScript::El1_bound_scalar = 0.0

#$bound$#EMScript::El2_bound       = "skip"
#$bound$#EMScript::El2_bound_speed = 1.0
#$bound$#EMScript::El2_bound_limit = 0.0
#$bound$#EMScript::El2_bound_scalar = 0.0

#$bound$#EMScript::El3_bound       = "skip"
#$bound$#EMScript::El3_bound_speed = 1.0
#$bound$#EMScript::El3_bound_limit = 0.0
#$bound$#EMScript::El3_bound_scalar = 0.0

#$bound$#EMScript::B1_bound       = "skip"
#$bound$#EMScript::B1_bound_speed = 1.0
#$bound$#EMScript::B1_bound_limit = 0.0
#$bound$#EMScript::B1_bound_scalar = 0.0

#$bound$#EMScript::B2_bound       = "skip"
#$bound$#EMScript::B2_bound_speed = 1.0
#$bound$#EMScript::B2_bound_limit = 0.0
#$bound$#EMScript::B2_bound_scalar = 0.0

#$bound$#EMScript::B3_bound       = "skip"
#$bound$#EMScript::B3_bound_speed = 1.0
#$bound$#EMScript::B3_bound_limit = 0.0
#$bound$#EMScript::B3_bound_scalar = 0.0

*/

