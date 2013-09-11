
(*  Copyright 2004-2013 Sascha Husa, Ian Hinder, Christiane Lechner

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

BeginPackage[
  "MoL`",
  {"Errors`", "Helpers`", "Kranc`", "CodeGenKranc`", "MapLookup`", "CodeGenCactus`",
   "CodeGen`", "CodeGenC`", "KrancGroups`", "Calculation`"}];

CreateKrancMoLRegister;
CreateMoLBoundariesSource::usage = "";
CreateMoLExcisionSource::usage = "";
MoLReplaceDots;
EvolvedVariables;
MoLEvolvedGroups;
MoLNonevolvedGroups;
EvolvedGroupToRHSGroup::usage = "";
MoLRHSGroupDefinitions;
MoLRHSODEGroupDefinitions;
MoLUsedFunctions;
MoLProcessGroups;
MoLParameterStructures;
MoLUsedParameters;

Begin["`Private`"];

(* ------------------------------------------------------------------------ 
   MoL Registration
   ------------------------------------------------------------------------ *)

(* FIXME: This is still not quite right.  We only want to have those variables that
   we set as constrained, but I don't think this can hurt.*)

getConstrainedVariables[evolvedGroupNames_, groups_] :=
  Module[{evolvedGFs, allVariables, constrainedVariables},
    evolvedGFs = variablesFromGroups[evolvedGroupNames, groups];
    allVariables = Flatten[Map[groupVariables, groups],1];
    constrainedVariables = Complement[allVariables, Join[evolvedGFs, Map[Symbol[addrhs[#]] &, evolvedGFs]]];
    constrainedVariables];

CreateKrancMoLRegister[evolvedGroupNames_, nonevolvedGroupNames_, evolvedODEGroupNames_, nonevolvedODEGroupNames_, groups_, implementation_, thornName_] :=
  Module[{molspec, evolvedGFs, evolvedArrays, constrainedVariables},
    evolvedGFs = variablesFromGroups[evolvedGroupNames, groups];
    evolvedArrays = variablesFromGroups[evolvedODEGroupNames, groups];
    nonevolvedGFs = variablesFromGroups[nonevolvedGroupNames, groups];
    nonevolvedArrays = variablesFromGroups[nonevolvedGroupNames, groups];

    constrainedVariables = getConstrainedVariables[evolvedGroupNames, groups];
    
    molspec =
    {
      EvolvedGFs   -> Map[qualifyGFName[#, groups, implementation]& , evolvedGFs], 
      EvolvedArrays -> Map[qualifyGFName[#, groups, implementation]& , evolvedArrays], 
      PrimitiveGFs -> Map[qualifyGFName[#, groups, implementation]& , constrainedVariables],
      BaseImplementation -> implementation, 
      ThornName -> thornName
    };
    molregister = createMoLRegistrationSource[molspec, False];
    Return[molregister]];

(* MoL registration = {EvolvedGFs -> {h11, ...}, PrimitiveGFs -> {trK, ...}, 
                       BaseImplementation -> "ADMBase", ThornName -> "ADMMoL"} *)

(* Given a MoL registration structure as defined above, return a
   CodeGen structure of a source file which will register the
   variables given with MoL. *)
createMoLRegistrationSource[spec_, debug_] :=

  Module[{tmp, lang},

  If[debug,
    Print["Registering for MoL:"];
    Print[];
    Print["  Evolved   Gridfunctions: ", lookup[spec, EvolvedGFs]   ];
    Print["  Primitive Gridfunctions: ", lookup[spec, PrimitiveGFs] ];
    ];
	
    lang = CodeGenC`SOURCELANGUAGE;
    CodeGenC`SOURCELANGUAGE= "C";

    tmp = {FileHeader["C"],

    Map[IncludeFile, 
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],

    DefineCCTKFunction[lookup[spec,ThornName] <> "_RegisterVars", "void", 
      {DefineVariable["ierr", "CCTK_INT", "0"],

      CommentedBlock["Register all the evolved grid functions with MoL",

(* FIXME: We should clarify exactly what should happen with the implementation names here *)

(* OK. I think that the group name should be passed in qualified, as that is all that we can do. *)

      Map[{"ierr += MoLRegisterEvolved(CCTK_VarIndex(\"", #, "\"),  CCTK_VarIndex(\"",
            #, "rhs\"));\n"} &,
          lookup[spec, EvolvedGFs]]],

      CommentedBlock["Register all the evolved Array functions with MoL",
      Map[{"ierr += MoLRegisterEvolved(CCTK_VarIndex(\"", #, "\"),  CCTK_VarIndex(\"",
            #, "rhs\"));\n"} &,
          lookup[spec, EvolvedArrays]]],

      (* Registering all the remaining variables as constrained is
      just plain wrong.  Read the MoL documentation.  It is also not
      harmless, I think. *)

      (*CommentedBlock["Register all the primitive grid functions with MoL",
      (* We should check ierr *)
      Map[{"ierr += MoLRegisterConstrained(CCTK_VarIndex(\"", 
           #, "\"));\n"} &,
          lookup[spec, PrimitiveGFs]]],  *)
	"return;\n"}]};

      CodeGenC`SOURCELANGUAGE = lang;

tmp
];

(* ------------------------------------------------------------------------
   MoL Boundaries
   ------------------------------------------------------------------------ *)

(* boundaries spec = {Groups -> {trK, h11, ...},
                       BaseImplementation -> "ADMBase", ThornName -> "ADMMoL"} *)

(* the boundary treatment is split into 3 steps:
  1. excision                                      
  2. symmetries                                    
  3. "other" boundary conditions, e.g. flat, radiative   

To simplify scheduling, testing, etc. the 3 steps are currently applied in separate functions!*)

(* boundary conditions may have to be applied per GF or goup ; per group
should be more efficient, but sometimes there will be a GF-dependent parameter,
e.g. for radiation BCs *)

cleanCPP[x_] := Map[StringReplace[FlattenBlock[#],  "  #" -> "#"]&, x];

(* Given a BC registration structure as defined above, return a
   CodeGen structure of a source file which does nothing yet! *)
CreateMoLBoundariesSource[spec_] :=

  Module[{gfs, groups, unqualifiedGroups, tmp, lang},

  gfs = lookup[spec, EvolvedGFs];
  groups =  lookup[spec, Groups];  
  unqualifiedGroups = Map[unqualifiedGroupName, lookup[spec, Groups]];
  
    listBCparfileEntry[gforgroup_] := Module[{prefix, unqualName},
    (* include a comment block with template parameter file entries *)
    prefix =  "#$bound$#" <> lookup[spec, ThornImplementation] <> "::";
    unqualName = unqualifiedGroupName@ToString@gforgroup;

    {
     prefix <> unqualName <> "_bound       = \"skip\"\n",
     prefix <> unqualName <> "_bound_speed = 1.0\n",
     prefix <> unqualName <> "_bound_limit = 0.0\n",
     prefix <> unqualName <> "_bound_scalar = 0.0\n\n"
    }];


(*
    symmetryBCGroup[group_] := Module[{boundpar, fullgroupname},
    (* symmetry boundary conditions *)

    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];

    {"\n",

     "  ierr = CartSymGN(cctkGH, \"" <> fullgroupname <> "\");\n",

     "  if (ierr < 0)\n",
     "     CCTK_WARN(0, \"Failed to apply symmetry BC for " <> fullgroupname <> "!\");\n"}
     ];
*)

    trivialBCGroup[group_] := Module[{boundpar, fullgroupname},
    (* boundary conditions that do not have parameters besides their name *)

    boundpar      = unqualifiedGroupName@group <> "_bound";
    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"none\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"static\") ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"flat\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"zero\"  ) )\n",
     "{\n",

     "  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,\n",
     "                    \"" <> fullgroupname <> "\", " <> boundpar <> ");\n",

     "  if (ierr < 0)\n",
     "     CCTK_WARN(0, \"Failed to register "<>boundpar<>" BC for "<>fullgroupname<>"!\");\n",

     "}\n"}];


    trivialBCGF[gf_] := Module[{boundpar, fullgfname},
    (* boundary conditions that do not have parameters besides their name *)

    boundpar   = unqualifiedGroupName@ToString@gf <> "_bound";
    fullgfname = ToString@gf;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"none\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"static\") ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"flat\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"zero\"  ) )\n",
     "{\n",

     "  ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,\n",
     "                    \"" <> fullgfname <> "\", " <> boundpar <> ");\n",

     "  if (ierr < 0)\n",
     "     CCTK_WARN(0, \"Failed to register "<>boundpar<>" BC for "<>fullgfname<>"!\");\n",

     "}\n"}];

    radiationBCGroup[group_] := Module[{boundpar, fullgroupname, myhandle},
    (* a simple radiation boundary condition *)

    boundpar      = unqualifiedGroupName@ToString@group <> "_bound";
    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];

    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"radiative\"))\n",
     "{\n /* select radiation boundary condition */\n  ",

      DefineVariable[myhandle, "static CCTK_INT", "-1"],

      "  if ("<>myhandle<>" < 0) "<>myhandle<>" = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);\n",

      "  if ("<>myhandle<>" < 0) CCTK_WARN(0, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" , "<> boundpar <>"_limit, \"LIMIT\") < 0)\n",
      "     CCTK_WARN(0, \"could not set LIMIT value in table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_speed, \"SPEED\") < 0)\n",
      "     CCTK_WARN(0, \"could not set SPEED value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgroupname <> "\", \"Radiation\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(0, \"Failed to register Radiation BC for "<>fullgroupname<>"!\");\n",

      "\n}\n"}];


    radiationBCGF[gf_] := Module[{boundpar, fullgfname, myhandle},
    (* a simple radiation boundary condition *)

    boundpar   = unqualifiedGroupName@ToString@gf <> "_bound";
    fullgfname = ToString@gf;

    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"radiative\"))\n",
     "{\n /* select radiation boundary condition */\n  ",

      DefineVariable[myhandle, "static CCTK_INT", "-1"],

      "  if ("<>myhandle<>" < 0) "<>myhandle<>" = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);\n",

      "  if ("<>myhandle<>" < 0) CCTK_WARN(0, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" , "<> boundpar <>"_limit, \"LIMIT\") < 0)\n",
      "     CCTK_WARN(0, \"could not set LIMIT value in table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_speed, \"SPEED\") < 0)\n",
      "      CCTK_WARN(0, \"could not set SPEED value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgfname <> "\", \"Radiation\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(0, \"Failed to register Radiation BC for "<>fullgfname<>"!\");\n",

     "\n}\n"}];

    scalarBCGroup[group_] := Module[{boundpar, fullgroupname, myhandle},
    (* simple dirichlet boundary condition *)

    boundpar      = unqualifiedGroupName@group <> "_bound";
    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];
    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"scalar\"))\n",
     "{\n /* select scalar boundary condition */\n  ",

      DefineVariable[myhandle, "static CCTK_INT", "-1"],

      "  if ("<>myhandle<>" < 0) "<>myhandle<>" = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);\n",

      "  if ("<>myhandle<>" < 0) CCTK_WARN(0, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_scalar, \"SCALAR\") < 0)\n",
      "      CCTK_WARN(0, \"could not set SCALAR value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgroupname <> "\", \"scalar\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(0, \"Failed to register Scalar BC for "<>fullgroupname<>"!\");\n",

      "\n}\n"}];


    scalarBCGF[gf_] := Module[{boundpar, fullgfname, myhandle},
    (* simple dirichlet boundary condition *)

    boundpar   = unqualifiedGroupName@ToString@gf <> "_bound";
    fullgfname = ToString@gf;
    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"scalar\"))\n",
     "{\n /* select scalar boundary condition */\n  ",

      DefineVariable[myhandle, "static CCTK_INT", "-1"],

      "  if ("<>myhandle<>" < 0) "<>myhandle<>" = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);\n",

      "  if ("<>myhandle<>" < 0) CCTK_WARN(0, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_scalar, \"SCALAR\") < 0)\n",
      "    CCTK_WARN(0, \"could not set SCALAR value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgfname <> "\", \"scalar\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(0, \"Error in registering Scalar BC for "<>fullgfname<>"!\");\n",

     "\n}\n"}];


   lang = CodeGenC`SOURCELANGUAGE;
   CodeGenC`SOURCELANGUAGE = "C";

  tmp = {FileHeader["C"],


   Map[IncludeFile,
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h", 
         "cctk_Faces.h", "util_Table.h", "Symmetry.h"}],

   {"\n\n",
      "/* the boundary treatment is split into 3 steps:    */\n",
      "/* 1. excision                                      */\n",
      "/* 2. symmetries                                    */\n",
      "/* 3. \"other\" boundary conditions, e.g. radiative */\n\n",
      "/* to simplify scheduling and testing, the 3 steps  */\n",
      "/* are currently applied in separate functions      */\n\n"},


   cleanCPP@DefineCCTKFunction[lookup[spec,ThornName] <> "_CheckBoundaries",
   "void",
     {"return;\n"}],


   cleanCPP@DefineCCTKFunction[lookup[spec,ThornName] <> "_SelectBoundConds", 
   "void",
     {DefineVariable["ierr",   "CCTK_INT", "0"],

(*
       Map[symmetryBCGroup,  groups],
*)

       Map[trivialBCGroup,   groups],
       Map[trivialBCGF,      gfs],

       Map[radiationBCGroup, groups],
       Map[radiationBCGF,    gfs],

       Map[scalarBCGroup,    groups],
       Map[scalarBCGF,       gfs],

      "return;\n"
     }],

     "\n\n\n",
     "/* template for entries in parameter file:\n",
      Map[listBCparfileEntry, unqualifiedGroups],
      Map[listBCparfileEntry, gfs],
     "*/\n\n"
     };

	 CodeGenC`SOURCELANGUAGE = lang;
tmp
];

CreateMoLExcisionSource[spec_] :=

  Module[{gfs, currentlang, body, excisionExtrap},

  gfs = lookup[spec, ExcisionGFs];
 
  Print["Applying excision to GFs: ", gfs];
 
  currentlang = CodeGenC`SOURCELANGUAGE;
  CodeGenC`SOURCELANGUAGE = "Fortran";

  excisionExtrap[gf_] :=  "  call ExcisionExtrapolate(ierr, "
    <> ToString@gf <> ", " <> ToString@gf
    <> "_p, emask, exnormx, exnormy, exnormz, nx, ny, nz, "<> ToString@gf  <> "_bound_limit)\n";

  body = {FileHeader["Fortran"],

   Map[IncludeFile,
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],

   {"\n\n",
      "! the boundary treatment is split into 3 steps:    \n",
      "! 1. excision                                      \n",
      "! 2. symmetries                                    \n",
      "! 3. \"other\" boundary conditions, e.g. radiative \n",
      "! to simplify scheduling and testing, the 3 steps  \n",
      "! are currently applied in separate functions      \n\n"},

   cleanCPP@DefineCCTKSubroutine[lookup[spec,ThornName] <> "_FindBoundary",
     {"! APPLY EXCISION\n\n",
      DefineVariable["ierr", "CCTK_INT :: ", "0"],
      "",

      "integer  :: nx, ny, nz\n\n",

      "! grid parameters\n",

      "nx = cctk_lsh(1)\n",
      "ny = cctk_lsh(2)\n",
      "nz = cctk_lsh(3)\n\n",

      "if ( (excision .ne. 0).AND.(find_excision_boundary .ne. 0) ) then\n\n",

      "  call ExcisionFindBoundary(ierr, emask, nx, ny, nz)\n",
      "  if (ierr < 0) call CCTK_WARN(2, \"findboundary exited with an error\")\n\n",

     "endif\n\n",
     "return\n"}],

   cleanCPP@DefineCCTKSubroutine[lookup[spec,ThornName] <> "_FindNormals",
     {"! APPLY EXCISION\n\n",
      DefineVariable["ierr", "CCTK_INT :: ", "0"],
      "",

      "integer  :: nx, ny, nz\n\n",

      "! grid parameters\n",

      "nx = cctk_lsh(1)\n",
      "ny = cctk_lsh(2)\n",
      "nz = cctk_lsh(3)\n\n",

      "if ( (excision .ne. 0).AND.(find_excision_normals .ne. 0) ) then\n\n",

      "  call ExcisionFindNormals(ierr, emask, exnormx, exnormy, exnormz, nx, ny, nz)\n",
      "  if (ierr < 0) call CCTK_WARN(2, \"findnormals exited with an error\")\n\n",

     "endif\n\n",
     "return\n"}],


   cleanCPP@DefineCCTKSubroutine[lookup[spec,ThornName] <> "_ApplyExcision",
     {"! APPLY EXCISION\n\n",
      DefineVariable["ierr", "CCTK_INT :: ", "0"],
      "",

      "integer  :: nx, ny, nz\n\n",

      "! grid parameters\n",

      "nx = cctk_lsh(1)\n", 
      "ny = cctk_lsh(2)\n", 
      "nz = cctk_lsh(3)\n\n", 

      "if (excision .ne. 0) then\n",

      "  call CCTK_INFO(\"Applying LegoExcision\")\n\n",

     Map[excisionExtrap, gfs],
     "endif\n\n",
     "return\n"}]
};

CodeGenC`SOURCELANGUAGE = currentlang;

body
];

DefFn[MoLReplaceDots[x_] := 
  x /. (dot[y_] :> Symbol[ToString[y] <> "rhs"])];

EvolvedVariables[calc_] :=
  Module[{eqs, evolved, lhss},
    VerifyNewCalculation[calc];
    eqs = GetEquations[calc];
    lhss = Map[First, eqs];
    evolved = Cases[lhss, dot[v_] -> v];
    Return[evolved]];

DefFn[MoLEvolvedGroups[declaredGroups_, calcs_, groups_] :=
  Module[{evolvedVars, evolvedGroups},
    VerifyGroupNames[declaredGroups];
    VerifyGroups[groups];
    VerifyList[calcs];
    Map[VerifyNewCalculation, calcs];
    allVars = variablesFromGroups[declaredGroups, groups];
    evolvedVars = Apply[Join, Map[EvolvedVariables, calcs]];
    evolvedVars = Intersection[allVars, evolvedVars];
    evolvedGroups = containingGroups[evolvedVars, groups];
    Return[evolvedGroups]]];

DefFn[MoLNonevolvedGroups[declaredGroups_, calcs_, groups_] :=
  Module[{allVars, evolvedVars, evolvedGroups, nonevolvedGroups},
    VerifyGroupNames[declaredGroups];
    VerifyGroups[groups];
    VerifyList[calcs];
    Map[VerifyNewCalculation, calcs];

    allVars = variablesFromGroups[declaredGroups, groups];
    evolvedVars = Apply[Join, Map[EvolvedVariables, calcs]];
    evolvedGroups = containingGroups[evolvedVars, groups];
    nonevolvedGroups = Complement[declaredGroups, evolvedGroups];

    Return[nonevolvedGroups]]];

addrhs[x_] := ToString[x] <> "rhs";

EvolvedGroupToRHSGroup[name_, groups_] := 
  Module[{names, group},
    names = Map[groupName, groups];
    If[!MemberQ[names, name], ThrowError["evolvedGroupToRHSGroup: Group \"" <> groupName <> "\" not found in groups structure:", groups]];

    group = First[Select[groups, groupName[#] === name &]];

    oldVars = groupVariables[group];
    newVars = Map[Symbol[addrhs[ToString[#]]] &, oldVars];

    group = SetGroupName[group, addrhs[name]];
    group = SetGroupVariables[group, newVars];
    group = AddGroupTag[group, "Prolongation" -> "None"];
    Return[group]];

DefFn[
  MoLRHSGroupDefinitions[groups_List, evolvedGroups_List] :=
  Module[
    {evolvedGroupDefinitions},
    evolvedGroupDefinitions = Map[groupFromName[#, groups] &, evolvedGroups];
    Map[EvolvedGroupToRHSGroup[#, evolvedGroupDefinitions] &, evolvedGroups]]];

DefFn[
  MoLRHSODEGroupDefinitions[groups_List, evolvedODEGroups_List] :=
  Module[
    {evolvedODEGroupDefinitions},
    evolvedODEGroupDefinitions = Map[groupFromName[#, groups] &, evolvedODEGroups];
    Map[EvolvedGroupToRHSGroup[#, evolvedODEGroupDefinitions] &, evolvedODEGroups]]];

DefFn[
  MoLUsedFunctions[] :=
  {
    {
      Name      -> "MoLRegisterEvolved",
      Type      -> "CCTK_INT",
      ArgString -> "CCTK_INT IN EvolvedIndex, CCTK_INT IN RHSIndex"
    }

    (*
    {
      Name      -> "MoLRegisterConstrained",
      Type      -> "CCTK_INT",
      ArgString -> "CCTK_INT IN ConstrainedIndex"
    };
    *)
  }];

DefFn[
  MoLProcessGroups[declaredGroups_List, calcs_List, groups_List,
                   evolutionTimelevels_Integer] :=
  Module[
    {evolvedGroups, groups2},
    evolvedGroups = MoLEvolvedGroups[declaredGroups, calcs, groups];

    groups2 = Map[If[MemberQ[evolvedGroups, groupName[#]],
                     (* Print["Adding InterfaceTimelevels to ", groupName[#]]; *)
                     AddGroupExtra[
                       EnsureInterfaceTimelevels[#, evolutionTimelevels],
                       MoLEvolved -> True],
                     #] &, groups];
    groups2]];

DefFn[
  MoLParameterStructures[thornName_, evolvedGroups_, evolvedODEGroups_, groups_, evolutionTimelevels_,
                         defaultEvolutionTimelevels_] :=
  Module[
    {nEvolved, nevolvedODE},
    nEvolved   = Length[variablesFromGroups[evolvedGroups, groups]];
(*    nPrimitive = Length[variablesFromGroups[nonevolvedGroups, groups]];*)
(*    nPrimitive = Length[getConstrainedVariables[evolvedGroups, groups]];*)
    nEvolvedODE   = Length[variablesFromGroups[evolvedODEGroups, groups]];

    {{  Name -> thornName <> "_MaxNumEvolvedVars",
        Type -> "CCTK_INT",
        Default -> nEvolved,
        Description -> "Number of evolved variables used by this thorn",
        Visibility -> "restricted",
        AccumulatorBase -> "MethodofLines::MoL_Num_Evolved_Vars",
        AllowedValues -> {{Value -> ToString[nEvolved] <> ":" <> ToString[nEvolved] , 
                           Description -> "Number of evolved variables used by this thorn"}},
        Steerable -> Recover},
      { Name -> thornName <> "_MaxNumArrayEvolvedVars",
        Type -> "CCTK_INT",
        Default -> nEvolvedODE,
        Description -> "Number of Array evolved variables used by this thorn",
        Visibility -> "restricted",
        AccumulatorBase -> "MethodofLines::MoL_Num_ArrayEvolved_Vars",
        AllowedValues -> {{Value -> ToString[nEvolvedODE] <> ":" <> ToString[nEvolvedODE] , 
                           Description -> "Number of Array evolved variables used by this thorn"}},
        Steerable -> Recover},
     { Name -> "timelevels", (* For evolved variables, kind of *)
       Type -> "CCTK_INT",
       Default -> defaultEvolutionTimelevels,
       Description -> "Number of active timelevels",
       Visibility -> "restricted",
       AllowedValues -> {{Value -> ToString[0] <> ":" <> ToString[evolutionTimelevels],
                          Description -> ""}},
       Steerable -> Recover},
     (* { Name -> thornName <> "_MaxNumConstrainedVars", *)
     (*   Type -> "CCTK_INT", *)
     (*   Default -> nPrimitive, *)
     (*   Description -> "Number of constrained variables used by this thorn", *)
     (*   Visibility -> "restricted", *)
     (*   AccumulatorBase -> "MethodofLines::MoL_Num_Constrained_Vars", *)
     (*   AllowedValues -> {{Value -> ToString[nPrimitive] <> ":" <> ToString[nPrimitive] ,  *)
     (*                      Description -> "Number of constrained variables used by this thorn"}}}, *)
     { Name -> "rhs_timelevels",
       Type -> "CCTK_INT",
       Default -> 1,
       Description -> "Number of active RHS timelevels",
       Visibility -> "restricted",
       AllowedValues -> {{Value -> ToString[0] <> ":" <> ToString[evolutionTimelevels],
                          Description -> ""}},
       Steerable -> Recover}}]];

DefFn[
  MoLUsedParameters[] :=
  { Name -> "MethodOfLines",
    UsedParameters -> 
    {
      {Name -> "MoL_Num_Evolved_Vars",        Type -> "CCTK_INT"},
      {Name -> "MoL_Num_ArrayEvolved_Vars",   Type -> "CCTK_INT"}
      (* {Name -> "MoL_Num_Constrained_Vars", Type -> "CCTK_INT"} *)
    }}];

End[];

EndPackage[];
