
(* $Id$ *)

(*  Copyright 2004 Sascha Husa, Ian Hinder, Christiane Lechner

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
    along with Kranc; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(* This file provides:

   - Source files
   - List of implementations to inherit
   - List of files to include
   - Aliased functions to use
   - Groups to schedule
   - Functions to schedule
   - Parameters to declare

    *)

BeginPackage["CactusBoundary`", {"CodeGen`", "Thorn`",
 "MapLookup`", "KrancGroups`", "Errors`", "Helpers`", "Kranc`", "MoL`"}];

GetInheritedImplementations::usage = "";
GetIncludeFiles::usage = "";
GetUsedFunctions::usage = "";
GetScheduledGroups::usage = "";
GetScheduledFunctions::usage = "";
GetParameters::usage = "";
GetSources::usage = "";

Begin["`Private`"];


GetInheritedImplementations[] := {"Boundary"};

GetIncludeFiles[] := {"Boundary.h"};

GetUsedFunctions[] := 
{  
  {Name      -> "Boundary_SelectGroupForBC",
   Type      -> "CCTK_INT",
   ArgString -> "CCTK_POINTER_TO_CONST IN GH, CCTK_INT IN faces, CCTK_INT IN boundary_width, CCTK_INT IN table_handle, CCTK_STRING IN group_name, CCTK_STRING IN bc_name"},

  {Name      -> "Boundary_SelectVarForBC",
   Type      -> "CCTK_INT",
   ArgString -> "CCTK_POINTER_TO_CONST IN GH, CCTK_INT IN faces, CCTK_INT IN boundary_width, CCTK_INT IN table_handle, CCTK_STRING IN var_name, CCTK_STRING IN bc_name"}
};

boundariesName[thornName_] := thornName <> "_SelectBoundConds";
checkBoundariesName[thornName_] := thornName <> "_CheckBoundaries";

GetScheduledGroups[thornName_] :=
{
 {Name          -> "ApplyBCs",
  Language      -> "None", (* groups do not have a language *)
  SchedulePoint -> "as " <> thornName <> "_ApplyBCs in MoL_PostStep "
                   <> "after " <> boundariesName[thornName], 
  Comment       -> "Apply boundary conditions "
                   <> "controlled by thorn Boundary"
 }
};

GetScheduledFunctions[thornName_, declaredGroups_List, groups_List] :=
{
  {
    Name          -> boundariesName[thornName],
    SchedulePoint -> "in MoL_PostStep",
    SynchronizedGroups -> MoLEvolvedGroups[declaredGroups, groups],
    Language      -> "C",
    Options       -> "level", 
    Comment       -> "select boundary conditions"
  },

  {
    Name          -> checkBoundariesName[thornName],
    SchedulePoint -> "at BASEGRID",
    Options       -> "meta",
    Language      -> "C",
    Comment       -> "check boundaries treatment"
  }
};

createBoundTypeParam[groupOrGF_, def_] := {
                 Name          ->  ToString@groupOrGF <> "_bound",
                 Type          ->  "KEYWORD",
                 Default       ->  def,
                 Description   ->  "Boundary condition to implement",
                 Visibility    ->  "private",
                 AllowedValues ->  {
        {Value -> "flat",      Description -> "Flat boundary condition"},
        {Value -> "none",      Description -> "No boundary condition"},
        {Value -> "static",    Description -> "Boundaries held fixed"},
        {Value -> "radiative", Description -> "Radiation boundary condition"},
        {Value -> "scalar",    Description -> "Dirichlet boundary condition"},
        {Value -> "newrad",    Description -> "Improved radiative boundary condition"},
        {Value -> "skip",      Description -> "skip boundary condition code"}},
                 Steerable -> Always
};


createBoundSpeedParam[groupOrGF_] := {
                 Name          ->  ToString@groupOrGF <> "_bound_speed",
                 Type          ->  "CCTK_REAL",
                 Default       ->  1.0,
                 Description   ->  "characteristic speed at boundary",
                 Visibility    ->  "private",
                 AllowedValues ->  {{Value -> "0:*" ,
                      Description -> "outgoing characteristic speed > 0"}},
                 Steerable -> Always
};

createBoundLimitParam[groupOrGF_] := {
                 Name          ->  ToString@groupOrGF <> "_bound_limit",
                 Type          ->  "CCTK_REAL",
                 Default       ->  0.0,
                 Description   ->  "limit value for r -> infinity",
                 Visibility    ->  "private",
                 AllowedValues ->  {{Value -> "*:*" ,
                      Description -> "value of limit value is unrestricted"}},
                 Steerable -> Always
};

createBoundScalarParam[groupOrGF_] := {
                 Name          ->  ToString@groupOrGF <> "_bound_scalar",
                 Type          ->  "CCTK_REAL",
                 Default       ->  0.0,
                 Description   ->  "Dirichlet boundary value",
                 Visibility    ->  "private",
                 AllowedValues ->  {{Value -> "*:*" ,
                      Description -> "unrestricted"}},
                 Steerable -> Always
};

DefFn[
  GetParameters[declaredGroups_List, groups_List] :=
  Module[
    {evolvedGFs, evolvedGroups},
    evolvedGroups = MoLEvolvedGroups[declaredGroups, groups];
    evolvedGFs = variablesFromGroups[evolvedGroups, groups];

  Join[Map[createBoundTypeParam[#,"skip"] &, evolvedGFs],
       Map[createBoundTypeParam[#,"none"] &, Map[unqualifiedGroupName,evolvedGroups]],

       Map[createBoundSpeedParam, evolvedGFs],
       Map[createBoundSpeedParam, Map[unqualifiedGroupName,evolvedGroups]], 

       Map[createBoundLimitParam, evolvedGFs],
       Map[createBoundLimitParam, Map[unqualifiedGroupName,evolvedGroups]],

       Map[createBoundScalarParam, evolvedGFs],
       Map[createBoundScalarParam, Map[unqualifiedGroupName,evolvedGroups]]]]];
 

GetSources[evolvedGroups_, declaredGroups_, groups_, implementation_, thornName_] :=
  Module[{boundarySpec, evolvedGFs},
    evolvedGroups2 = MoLEvolvedGroups[declaredGroups, groups];

    If[Union@evolvedGroups2 =!= Union[evolvedGroups],
       Print["Group mismatch"];
       Quit[1]];

    evolvedGFs = variablesFromGroups[evolvedGroups2, groups];
    boundarySpec = 
    {
      Groups -> evolvedGroups2, 
      EvolvedGFs -> Map[qualifyGFName[#, groups, implementation] &, evolvedGFs],
      BaseImplementation -> implementation, 
      ThornName -> thornName,
      ThornImplementation -> implementation, 
      ExcisionGFs -> evolvedGFs
    };

    Return[{{Filename -> "Boundaries.cc", 
             Contents -> CreateMoLBoundariesSource[boundarySpec]}}]];

End[];
EndPackage[];
