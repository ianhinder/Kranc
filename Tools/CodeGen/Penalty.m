
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
    along with Foobar; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(****************************************************************************)
(* Generate Cactus Thorns from a high-level interface  *)
(****************************************************************************)


BeginPackage["sym`"];

{AllowedValues, BaseImplementation, BaseThornDefault, Calculation,
CollectList, Comment, Conditional, Contents, CreateExcisionCode, CustomSchedules,
DeBug, Default, Description,
Directory, Equations, EvolutionTimeLevels,
EvolvedGFs, Filename, GridFunctions, GridType, Group,
Groups, Implementation, Implementations, IntBaseParameters,
Interface, IntParameters, Language, Makefile, Name, NewParameters, Param,
Parameters, Parameter, PrimitiveDefinitions, PrimitiveGFs, PrimitiveGroups, Primitives,
RealBaseParameters, RealParameters, Schedule,
SchedulePoint, SetTime, ShorthandDefinitions, Shorthands, Sources, StartupName,
StorageGroups, SyncGroups, SynchronizedGroups, SystemDescription, SystemName,
SystemNameDefault, SystemParentDirectory, ThornArrangement, ThornGroups,
ThornImplementation,
ThornName, ThornParameters, ThornType, Timelevels, TranslatorInCalculation,
TranslatorOutCalculation, Type, UsedParameters, Value, Variables,
    VariableType, Visibility, WhereTrigger, InheritedImplementations, ZeroDimensions, Include, GuessTensorTypes, RHSGroups};

(* used in interface to AEI Black Hole Excision Thorns *)
{ExcisionGFs, exnormx, exnormy, exnormz};

EndPackage[];


BeginPackage["Penalty`", 
             {"CodeGen`", "sym`", "Thorn`", "MapLookup`", "KrancGroups`", "Differencing`", "CalculationFunction`", "Errors`", "Helpers`", "KrancThorns`"}];

CodeGen`SetSourceLanguage["C"];

(* Any symbols to be exported into the GenerateKrancThorns` context 
   should have their usage messages defined here. *)


CreateMPCharThorn::usage = 
"create a Cactus thorn that provides information on the characteristics in a format suitable to interface to the
AEIDevelopemt/MultiPatch thorn";


Begin["`Private`"];

(****************************************************************************)
(*      Initialization                                                      *)
(****************************************************************************)

(* Little Helpers *)

GetOptions[f_, optList_] :=
  Module[{allKeys},
    allKeys = Union[Join[Map[First, optList], Map[First, Options[f] ]]];

    Map[# -> lookupDefault[optList, #, 
                           lookupDefault[Options[f],#, "NOT FOUND"]] &, 
        allKeys]];

simpleGroupStruct[thisgroup_, thistimelevel_] := {Group      -> thisgroup, 
                                                  Timelevels -> thistimelevel};

implementationRealParamStruct[x_] := {Name        -> ToString@x, 
                                      Type        -> "CCTK_REAL", 
                                      Default     -> 0,
                                      Description -> ToString@x,
                                      Visibility  -> "restricted" };

implementationIntParamStruct[x_] := {Name        -> ToString@x, 
                                     Type        -> "CCTK_INT", 
                                     Default     -> 0,
                                     Description -> ToString@x,
                                     Visibility  -> "restricted" };

completeRealParamStruct[x_] := {Name          -> ToString@x, 
                                Type          -> "CCTK_REAL", 
                                Default       -> 0,
                                Description   -> ToString@x,
                                Visibility    -> "restricted",
                                AllowedValues -> {{Value       -> "*:*",
                                                   Description -> "no restrictions"}} };

completeIntParamStruct[x_] := {Name           -> ToString@x, 
                               Type           -> "CCTK_INT",
                               Default        -> 0,
                               Description    -> ToString@x,
                               Visibility     -> "restricted",
                               AllowedValues  -> {{Value       -> "*:*",
                                                   Description -> "no restrictions"}} };

CactusGroup[Thorns_, gr_] := Module[{newthorns, name},

  newthorns = Map[Flatten@{
    ThornImplementation /. #, ThornName /. #, ThornGroups /. #} &, Thorns];

  newthorns = newthorns /. {ThornImplementation, x_, y__} ->  {x, y};

  name[entry_] := If[MemberQ[entry, ToString@gr],
                     ToString@First@entry <> "::" <> ToString@gr];

  First@Cases[Map[name, newthorns], x_?StringQ]
];


assembleRHSGroups[groups_?ListQ]:= Module[{rhsGroups},
rhsGroups = Select[Flatten@groups, StringQ];
rhsGroups = Select[rhsGroups, StringMatchQ[#, "*rhs"]& ];

If[Length[Intersection[Map[ToString, Flatten@variablesFromGroups[rhsGroups, groups]],
                       Map[ToString, Flatten@Map[addrhs, evolvedGFs]]  ]] == 0,
    rhsGroups = Map[addrhs, evolvedGroups];,
    Print["Taking RHS groups from argument list: ", rhsGroups];
  ];

rhsGroups
];




(* "constants" used to specify default options *)

SystemNameDefault         = "myPDE";
BaseThornDefault          = "myBaseThorn";
SystemDescriptionDefault  = "myPDE";

(* more constants *)
StartupName               = "Startup";


(****************************************************************************)
(****************************************************************************)
(*         thorn generator functions                                        *)
(****************************************************************************)
(****************************************************************************)



(****************************************************************************)
(*         penalty multipatch characteristics thorn                         *)
(****************************************************************************)

Options[CreateMPCharThorn] = 
 (* The following have nontrivial defaults, so should not be included
      here: ThornName, Implementation, SystemDescription *)
  {RealBaseParameters    -> {},
   IntBaseParameters     -> {},
   RealParameters        -> {},
   IntParameters         -> {},
   SystemName            -> SystemNameDefault,
   CreateExcisionCode    -> False,
   EvolutionTimeLevels  -> 3,
   DeBug                 -> True,
   SystemParentDirectory -> ".", 
   PrimitiveGroups       -> {}};

CreateMPCharThorn[calculations_, groups_, optArgs___] :=
  Module[{baseParamsTrueQ, boundaryParam, calcrhsName,
          debug, evTimeLevels, file, globalStorageGroups, implementation, 
          intParameters, ListOfEvolvedGFs, ListOfEvolvedGroups, 
          ListOfPrimitiveGFs, ListOfPrimitiveGroups, ListOfRHSGroups, 
          opts, precompheaderName, primitiveGroups, realParameters, 
          selectBoundaryGroup, selectBoundaryVar, setterFileName, 
          genericFDImplementation, sourceFiles, sources, ThornList, thornName,
          whatitevolves, ext, useFuns, provideFuns, systemDesc, prim2char, char2prim,
          namedCharStruct, calculation},

Print["\n*** CreateMPCharThorn ***"];

(* processs optional arguments *)

opts = GetOptions[CreateMPCharThorn, {optArgs}];

(* These should be the same for all the thorns *)
parentDirectory    = lookup[opts, SystemParentDirectory];
systemName         = lookup[opts, SystemName];
systemDescription  = lookupDefault[opts, SystemDescription, systemName];

baseImplementation = systemName <> "Base";

(* thorn specific *)
realParameters     = lookup[opts, RealParameters];
intParameters      = lookup[opts, IntParameters];
realBaseParameters = lookup[opts, RealBaseParameters];
intBaseParameters  = lookup[opts, IntBaseParameters];
allParameters = Join[realParameters, intParameters, realBaseParameters, intBaseParameters];

primitiveGroups    = lookup[opts, PrimitiveGroups];

thornName          = lookupDefault[opts, ThornName, systemName <> "Evolve"];
implementation     = lookupDefault[opts, Implementation, thornName];

evTimeLevels       =  lookup[opts, EvolutionTimeLevels];
pddefs = lookupDefault[opts, PartialDerivatives, {}];

(* Find the evolved groups from the calculation *)
evolvedGFs    = Cases[calculations, dot[x_] -> x, Infinity];
evolvedGroups = containingGroups[evolvedGFs, groups];

(* All the variables in all the groups listed as "primitive" by the user *)
primitiveGFs = Flatten[Map[variablesInGroup[#, groups] &, primitiveGroups],1];


debug = lookup[opts, DeBug];
If[debug,
   Print["Debugging switched on"],
   Print["Debugging switched off"]
 ];

If[debug,
   Print["evolvedGroups   == ", evolvedGroups];
   Print["evolvedGFs      == ", evolvedGFs];
   Print["primitiveGroups == ", primitiveGroups];
   Print["primitiveGFs    == ", primitiveGFs];
 ];

(* define directories and create if needed *)
arrangementDirectory = parentDirectory <> "/" <> systemName;

EnsureDirectory[parentDirectory];
EnsureDirectory[arrangementDirectory];

(* create parameter, GF and Groups lists *)
baseParamsTrueQ = Length@realBaseParameters + Length@intBaseParameters > 0;


(* the list of thorns = return argument! *)

ThornList = {{ThornName -> "GenericFD", ThornArrangement -> "KrancNumericalTools",
              ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "CoordBase", ThornArrangement -> "CactusBase", ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "SymBase", ThornArrangement -> "CactusBase", ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "CartGrid3D", ThornArrangement -> "CactusBase", ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "SpaceMask", ThornArrangement -> "CactusEinstein", ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "MoL", ThornArrangement -> "CactusBase", ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "NaNChecker", ThornArrangement -> "CactusUtils", ThornGroups -> {},
	       ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "MultiPatch", ThornArrangement -> "AEIDevelopment", ThornGroups -> {},
	       ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "SummationByParts", ThornArrangement -> "AEIDevelopment", ThornGroups -> {},
	       ThornParameters -> {}, ThornType -> "External"}
};

(* define how we want to call files and functions *)
setterFileName   = thornName <> "_setCharInfo";
precompheaderName = "precomputations.h";

sourceFiles = {setterFileName <> ".c", StartupName <> ".c"};

(* assemble RHS groups *)
rhsGroups = assembleRHSGroups@groups;
Print["RHSGroups:\n", rhsGroups];

evolvedGroupDefinitions = Map[groupFromName[#, groups] &, evolvedGroups];

namedCharStruct = {Name       -> thornName, 
                   Groups     -> groups,
                   EvolvedGFs -> Map[qualifyGFName[#, groups, baseImplementation]& , evolvedGFs],
                   RHSGroups  -> rhsGroups};

(* create C & F90 source files for characteristic info *)
charDescSource = CreateMPCharSource[namedCharStruct, debug];

(* INTERFACE *)

inheritedImplementations = Join[{baseImplementation, "Grid", "MultiPatch", "SummationByParts",
                            "GenericFD"}, lookupDefault[opts, InheritedImplementations, {}]];

includeFiles             = {"Boundary.h", "GenericFD.h"};

systemDesc =
  {Name      -> "Multipatch_SystemDescription",
   Type      -> "FUNCTION",
   With      -> thornName <> "_Multipatch_SystemDescription",
   ArgString -> "CCTK_POINTER_TO_CONST IN cctkGH, \
         CCTK_INT IN nvars,               \
         CCTK_INT ARRAY OUT prim,         \
         CCTK_INT ARRAY OUT rhs,          \
         CCTK_REAL OUT sigma)"};

char2prim =
  {Name      -> "Multipatch_Char2Prim",
   Type      -> "FUNCTION",
   With      -> thornName <> "_Multipatch_Char2Prim",
   ArgString -> "CCTK_POINTER_TO_CONST IN cctkGH, \
         CCTK_INT IN dir,                      \
         CCTK_INT IN face,                     \
         CCTK_REAL ARRAY IN base,              \
         CCTK_INT ARRAY IN lbnd,               \
         CCTK_INT ARRAY IN lsh,                \
         CCTK_INT IN rhs_flag,                 \
         CCTK_INT IN num_modes,                \
         CCTK_POINTER_TO_CONST ARRAY IN modes"};

prim2char =
  {Name      -> "Multipatch_Prim2Char",
   Type      -> "FUNCTION",
   With      -> thornName <> "_Multipatch_Prim2Char",
   ArgString -> "CCTK_POINTER_TO_CONST IN cctkGH, \
         CCTK_INT IN dir,                 \
         CCTK_INT IN face,                \
         CCTK_REAL ARRAY IN base,         \
         CCTK_INT ARRAY IN lbnd,          \
         CCTK_INT ARRAY IN lsh,           \
         CCTK_INT IN rhs_flag,            \
         CCTK_INT IN num_modes,           \
         CCTK_POINTER ARRAY IN modes,     \
         CCTK_POINTER ARRAY IN speeds"};

useFuns = {};
provideFuns = {systemDesc, prim2char, char2prim};

interface = 
  CreateInterface[implementation, inheritedImplementations, includeFiles, {}, 
                  UsesFunctions     -> useFuns,
                  ProvidesFunctions -> provideFuns];


(* SCHEDULE *)
globalStorageGroups = {};

scheduledGroups     = {};

scheduledFunctions  = {{Name          -> thornName <> "_Startup",
                        SchedulePoint -> "at STARTUP",
                        Language      -> "C",
                        Options       -> "meta",
                        Comment       -> "create banner"}};

schedule = CreateSchedule[globalStorageGroups, scheduledGroups, scheduledFunctions];


(* PARAM *)

nEvolved   = Length[evolvedGFs];
nPrimitive = Length[primitiveGFs];

newParams = {};

genericFDImplementation = 
{Name -> "GenericFD",
 UsedParameters -> {{Name -> "stencil_width",    Type -> "CCTK_INT"},
                    {Name -> "stencil_width_x",  Type -> "CCTK_INT"},
                    {Name -> "stencil_width_y",  Type -> "CCTK_INT"},
                    {Name -> "stencil_width_z",  Type -> "CCTK_INT"}}};

baseImp = 
  {Name -> baseImplementation, 
   UsedParameters -> 
     Join[Map[implementationRealParamStruct, realBaseParameters],
          Map[implementationIntParamStruct,  intBaseParameters]]};

If[baseParamsTrueQ,
   implementations = {genericFDImplementation, baseImp},
   implementations = {genericFDImplementation}];

Map[AppendTo[newParams, #]&,
          Flatten[{Map[completeRealParamStruct, realParameters],
                   Map[completeIntParamStruct,  intParameters]}, 1]];

paramspec = {Implementations -> implementations,
             NewParameters -> newParams};

param = CreateParam[paramspec];


(* STARTUP  <> ": set RHSs for MoL" *)
startup = CreateStartupFile[thornName, thornName];

(* MAKEFILE *)
make = CreateMakefile[sourceFiles];

(* CREATE THORN *)

(* Write the differencing header file *)
diffHeader = CreateDifferencingHeader[pddefs, lookupDefault[opts, ZeroDimensions, {}]];


sources =  {
            {Filename -> StartupName            <> ".c",   Contents -> startup}, 
            {Filename -> setterFileName         <> ".c",   Contents -> charDescSource}, 
            {Filename -> precompheaderName,                Contents -> precompheader},
                         {Filename -> "Differencing.h",    Contents -> diffHeader}
};

thornspec = {Name      -> thornName, 
             Directory -> arrangementDirectory,
	     Interface -> interface, 
             Schedule  -> schedule, 
             Param     -> param, 
             Makefile  -> make,
             Sources   -> sources
};

CreateThorn[thornspec];

Print["Finished creating Multipatch Characteristics thorn"];

Append[ThornList,
       {ThornName          ->  thornName,
	ThornImplementation -> implementation, 
        ThornArrangement   ->  systemName,
        ThornGroups        ->  {}, 
        ThornParameters    ->  {"MISSING"},
        ThornType          ->  "Evolve"}]			    
];




End[];
EndPackage[];





