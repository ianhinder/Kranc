
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
CollectList, Comment, Conditional, Contents, CreateExcisionCode,
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


BeginPackage["KrancThorns`", 
             {"CodeGen`", "sym`", "Thorn`", "MapLookup`", "KrancGroups`", "Differencing`", "CalculationFunction`", "Helpers`"}];

CodeGen`SetSourceLanguage["C"];

(* Any symbols to be exported into the GenerateKrancThorns` context 
   should have their usage messages defined here. *)

CreateBaseThorn::usage       = 
"create a Cactus thorn which defines gridfunctions and parameters to be used in
a Kranc arrangement";
CreateMoLThorn::usage        = 
"create a Cactus thorn to evolve a set of gridfunctions with MoL";
CreateSetterThorn::usage     = 
"create a Cactus thorn to set gridfunctions to values";
CreateEvaluatorThorn::usage  = 
"create a Cactus thorn which evaluates gridfunctions in the ANALYSIS bin";
CreateTranslatorThorn::usage = 
"create a Cactus thorn which translates variables beween ADMBase and a Kranc
arrangement";
CreateMPCharThorn::usage = 
"create a Cactus thorn that provides information on the characteristics in a format suitable to interface to the
AEIDevelopemt/MultiPatch thorn";
CreateDifferencingThorn::usage = "";

CreateThornList::usage       = "create a Cactus THORNLIST file from information produced by Create*Thorn functions";

GetOptions::usage         = "utility to process optional arguments";

ThornName::usage          = 
"optional argument for Kranc thorn generator functions to specify a thorn name,
symbol to specify thorn name in ThornList structures";

RealParameters::usage     = 
"optional argument for Kranc thorn generator functions to specify real
parameters for this thorn";
IntParameters::usage      = 
"optional argument for Kranc thorn generator functions to specify integer 
parameters for this thorn";
RealBaseParameters::usage = 
"optional argument for Kranc thorn generator functions to specify real 
parameters inherited from a base thorn";
IntBaseParameters::usage  = 
"optional argument for Kranc thorn generator functions specifying integer 
parameters inherited from a base thorn";


SystemDescription::usage  = 
"optional (short) text argument for Kranc thorn generator functions to describe
the Kranc arrangement ";

ThornParameters::usage  = "symbol to specify thorn parameters in ThornList structures";
ThornGroups::usage      = "symbol to specify thorn groups in ThornList structures";
ThornArrangement::usage = "symbol to specify arrangements in ThornList structures";
ThornType::usage        = "symbol to specify thorn types in ThornList structures";

CactusGroup::usage   = "CactusGroup[Thorns_, groupname_] extracts a Cactus
style group name from a Kranc thorns list, i.e. metric -> ADMBase::metric";

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
(*         mol thorn                                                        *)
(****************************************************************************)

replaceDots[x_] := 
  x /. (dot[y_] :> Symbol[ToString[y] <> "rhs"]);


Options[CreateMoLThorn] = 
 (* The following have nontrivial defaults, so should not be included
      here: ThornName, Implementation, SystemDescription *)
  {RealBaseParameters    -> {},
   IntBaseParameters     -> {},
   RealParameters        -> {},
   IntParameters         -> {},
   SystemName            -> SystemNameDefault,
   CreateExcisionCode    -> False,
   UseLSUSummationByParts -> False,
   EvolutionTimeLevels  -> 3,
   DeBug                 -> False,
   SystemParentDirectory -> ".", 
   PrimitiveGroups       -> {}};

(* removed evolvedGroups *)

CreateMoLThorn[calculation_, groups_, optArgs___] :=
  Module[{baseParamsTrueQ, boundaryParam, calcrhsName, createExcisionCode,
          debug, evTimeLevels, file, globalStorageGroups, implementation, 
          intParameters, ListOfEvolvedGFs, ListOfEvolvedGroups, 
          ListOfPrimitiveGFs, ListOfPrimitiveGroups, ListOfRHSGroups, 
          molboundaries, molboundariesFileName, molexcision,
          molexcisionName, molImplementation, MoLParameters, molregisterName, 
          opts, precompheaderName, primitiveGroups, realParameters, 
          selectBoundaryGroup, selectBoundaryVar, setterFileName, 
          genericFDImplementation, sourceFiles, sources, ThornList, thornName,
          whatitevolves, ext, useFuns, provideFuns, useLSUSBP},

Print["\n*** CreateMoLThorn ***"];

(* processs optional arguments *)

opts = GetOptions[CreateMoLThorn, {optArgs}];

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

createExcisionCode =  lookup[opts, CreateExcisionCode];
useLSUSBP          =  lookup[opts, UseLSUSummationByParts];
evTimeLevels       =  lookup[opts, EvolutionTimeLevels];
pddefs = lookupDefault[opts, PartialDerivatives, {}];

(* Find the evolved groups from the calculation *)
evolvedGFs    = Cases[calculation, dot[x_] -> x, Infinity];
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
              ThornParameters -> {}, ThornType -> "External"}
};



(* define how we want to call files and functions *)
setterFileName        = thornName <> "_CalcRHS";
molregisterName       = thornName <> "_RegisterVars";
molboundariesFileName = thornName <> "_Boundaries";
molboundariesName     = thornName <> "_ApplyBoundConds";
molfindboundaryName   = thornName <> "_FindBoundary";
molfindnormalsName    = thornName <> "_FindNormals";
molexcisionName       = thornName <> "_ApplyExcision";
checkboundariesName   = thornName <> "_CheckBoundaries";
precompheaderName     = "precomputations.h";

ext = CodeGen`SOURCESUFFIX;

sourceFiles = {setterFileName  <> ext, StartupName <> ".c", 
               molregisterName <> ".c", 
               molboundariesFileName <> ".c"};
If[createExcisionCode, AppendTo[sourceFiles,  molexcisionName <> ".F90"] ];


(* assemble RHS groups and namedCalc structure  *)
rhsGroups = assembleRHSGroups@groups;

evolvedGroupDefinitions = Map[groupFromName[#, groups] &, evolvedGroups];
rhsGroupDefinitions = Map[evolvedGroupToRHSGroup[#, evolvedGroupDefinitions] &, evolvedGroups];

allowedToSync = Map[groupName, groups];

calc2 = mapEnsureKey[calculation, SyncGroups, allowedToSync];

namedCalc = augmentCalculation[calc2, thornName <> "_CalcRHS", 
                               baseImplementation, Join[groups, rhsGroupDefinitions], 
                               allParameters, pddefs];

Print["number of equations in calculation: ", numeq = Length@lookup[namedCalc, Equations]];


calcrhsName    = lookup[namedCalc, Name];


(* RHS CALCULATION and PRECOMP MACROS *)

calcrhs       = CreateSetterSource[{namedCalc}, debug];
precompheader = CreatePrecompMacros[namedCalc];

(* INTERFACE *)

inheritedImplementations = Join[{baseImplementation, "Grid", "Boundary", "SpaceMask",
                            "GenericFD"}, lookupDefault[opts, InheritedImplementations, {}]];

includeFiles             = {"Boundary.h", "Symmetry.h", "GenericFD.h"};

registerEvolved = 
  {Name      -> "MoLRegisterEvolved",
   Type      -> "CCTK_INT",
   ArgString -> "CCTK_INT IN EvolvedIndex, CCTK_INT IN RHSIndex"};

registerConstrained = 
  {Name      -> "MoLRegisterConstrained",
   Type      -> "CCTK_INT",
   ArgString -> "CCTK_INT IN ConstrainedIndex"};

selectBoundaryGroup =
  {Name      -> "Boundary_SelectGroupForBC",
   Type      -> "CCTK_INT",
   ArgString -> "CCTK_POINTER_TO_CONST IN GH, CCTK_INT IN faces, CCTK_INT IN boundary_width, CCTK_INT IN table_handle, CCTK_STRING IN group_name, CCTK_STRING IN bc_name"};

selectBoundaryVar =
  {Name      -> "Boundary_SelectVarForBC",
   Type      -> "CCTK_INT",
   ArgString -> "CCTK_POINTER_TO_CONST IN GH, CCTK_INT IN faces, CCTK_INT IN boundary_width, CCTK_INT IN table_handle, CCTK_STRING IN var_name, CCTK_STRING IN bc_name"};

excisionFindBoundary =
  {Name      -> "ExcisionFindBoundary",
   Type      -> "SUBROUTINE",
   ArgString -> "CCTK_INT OUT ierr, CCTK_REAL INOUT ARRAY mask, CCTK_INT IN ni, CCTK_INT IN nj, CCTK_INT IN nk"};

excisionExtrapolate =
  {Name      -> "ExcisionExtrapolate",
   Type      -> "SUBROUTINE",
   ArgString -> "CCTK_INT OUT ierr, CCTK_REAL INOUT ARRAY var, CCTK_REAL IN ARRAY oldvar, CCTK_REAL IN ARRAY mask, CCTK_REAL IN ARRAY dirx, CCTK_REAL IN ARRAY diry, CCTK_REAL IN ARRAY dirz, CCTK_INT IN ni, CCTK_INT IN nj, CCTK_INT IN nk, CCTK_REAL IN var0"};

excisionFindNormals =
  {Name      -> "ExcisionFindNormals",
   Type      -> "SUBROUTINE",
   ArgString -> "CCTK_INT OUT ierr, CCTK_REAL IN ARRAY mask, CCTK_REAL IN ARRAY dirx, CCTK_REAL IN ARRAY diry, CCTK_REAL IN ARRAY dirz, CCTK_INT IN ni, CCTK_INT IN nj, CCTK_INT IN nk"};

If[createExcisionCode, 

 useFuns = {registerEvolved, registerConstrained, selectBoundaryGroup, selectBoundaryVar,
            excisionFindBoundary, excisionExtrapolate, excisionFindNormals},
 useFuns = {registerEvolved, registerConstrained, selectBoundaryGroup, selectBoundaryVar}
];

interface = 
  CreateInterface[implementation, inheritedImplementations, includeFiles, {}, 
                  UsesFunctions     -> useFuns,
                  ProvidesFunctions -> {}];


(* SCHEDULE *)

(* in the following the Union takes care of the case when rhs groups have been explicitly
   declared as primitive groups *)
globalStorageGroups =
  Union@Join[Map[simpleGroupStruct[#,evTimeLevels]&, evolvedGroups],
             Map[simpleGroupStruct[#,1           ]&, rhsGroups],
             Map[simpleGroupStruct[#,1           ]&, primitiveGroups]];

scheduledGroups     = {{Name          -> "ApplyBCs",
                        Language      -> "None", (* groups do not have a language *)
                        SchedulePoint -> "as " <> thornName <> "_ApplyBCs in MoL_PostStep "
                                      <> " after " <> molboundariesName, 
                        Comment       -> "Apply boundary conditions "
                                      <> "controlled by thorn Boundary"}};

scheduledFunctions  = {{Name          -> thornName <> "_Startup",
                        SchedulePoint -> "at STARTUP",
                        Language      -> "C",
                        Options       -> "meta",
                        Comment       -> "create banner"},
(* *)
                       {Name          -> molregisterName,
                        SchedulePoint -> "in MoL_Register", 
                        Language      -> "C", 
                        Options       -> "meta",
                        Comment       -> "Register Variables for MoL"},
(* *)
                       {Name          -> calcrhsName,
                        SchedulePoint -> "in MoL_CalcRHS", 
                        Language      -> CodeGen`SOURCELANGUAGE, 
                        Comment       -> "Perform RHS calculation for MoL"},
(* *)
                       {Name          -> molboundariesName,
                        SchedulePoint -> "in MoL_PostStep",
                        SynchronizedGroups -> evolvedGroups,
                        Language      -> "C",
                        Options       -> "level", 
                        Comment       -> "apply boundary conditions"},
(* *)
                       {Name          -> checkboundariesName,
                        SchedulePoint -> "at BASEGRID",
                        Options       -> "meta",
                        Language      -> "C",
                        Comment       -> "check boundaries treatment"}
};

If[createExcisionCode, 
          AppendTo[scheduledFunctions, 
                   {Name               -> molfindboundaryName,
                    SchedulePoint      -> "in MoL_PostStep",
                    SynchronizedGroups -> {"spacemask::mask"},
                    Language           -> "Fortran",
                    Comment            -> "find excision boundary"}];

          AppendTo[scheduledFunctions,
                   {Name               -> molfindnormalsName,
                    SchedulePoint      -> "in MoL_PostStep AFTER " <> molfindboundaryName,
                    SynchronizedGroups -> {"excisionnormals"},
                    Language           -> "Fortran",
                    Comment            -> "find normals to excision boundary"}];

          AppendTo[scheduledFunctions,
                   {Name               -> molexcisionName,
                    SchedulePoint      -> "in MoL_PostStep AFTER " <> molfindnormalsName,
                    SynchronizedGroups -> evolvedGroups,
                    Language           -> "Fortran",
                    Comment            -> "apply excision"}]
];
schedule = CreateSchedule[globalStorageGroups, scheduledGroups, scheduledFunctions];


(* PARAM *)

createBoundTypeParam[groupOrGF_] := {
                 Name          ->  ToString@groupOrGF <> "_bound",
                 Type          ->  "KEYWORD",
                 Default       ->  "skip",
                 Description   ->  "Boundary condition to implement",
                 Visibility    ->  "private",
                 AllowedValues ->  {
        {Value -> "flat",      Description -> "Flat boundary condition"},
        {Value -> "none",      Description -> "No boundary condition"},
        {Value -> "static",    Description -> "Boundaries held fixed"},
        {Value -> "radiative", Description -> "Radiation boundary condition"},
        {Value -> "scalar",    Description -> "Dirichlet boundary condition"},
        {Value -> "newrad",    Description -> "Improved radiative boundary condition"},
        {Value -> "skip",      Description -> "skip boundary condition code"}
}};


createBoundSpeedParam[groupOrGF_] := {
                 Name          ->  ToString@groupOrGF <> "_bound_speed",
                 Type          ->  "CCTK_REAL",
                 Default       ->  1.0,
                 Description   ->  "characteristic speed at boundary",
                 Visibility    ->  "private",
                 AllowedValues ->  {{Value -> "0:*" ,
                      Description -> "outgoing characteristic speed > 0"}}
};

createBoundLimitParam[groupOrGF_] := {
                 Name          ->  ToString@groupOrGF <> "_bound_limit",
                 Type          ->  "CCTK_REAL",
                 Default       ->  0.0,
                 Description   ->  "limit value for r -> infinity",
                 Visibility    ->  "private",
                 AllowedValues ->  {{Value -> "*:*" ,
                      Description -> "value of limit value is unrestricted"}}
};

createBoundScalarParam[groupOrGF_] := {
                 Name          ->  ToString@groupOrGF <> "_bound_scalar",
                 Type          ->  "CCTK_REAL",
                 Default       ->  0.0,
                 Description   ->  "Dirichlet boundary value",
                 Visibility    ->  "private",
                 AllowedValues ->  {{Value -> "*:*" ,
                      Description -> "unrestricted"}}
};

(* boundaryParam = Join[Map[createBoundTypeParam, Join[evolvedGFs,    primitiveGFs]],
                     Map[createBoundTypeParam, Join[evolvedGroups, primitiveGroups]],
 
                     Map[createBoundSpeedParam, Join[evolvedGFs,    primitiveGFs]],
                     Map[createBoundSpeedParam, Join[evolvedGroups, primitiveGroups]],

                     Map[createBoundLimitParam, Join[evolvedGFs,    primitiveGFs]],
                     Map[createBoundLimitParam, Join[evolvedGroups, primitiveGroups]],

                     Map[createBoundScalarParam, Join[evolvedGFs,    primitiveGFs]],
                     Map[createBoundScalarParam, Join[evolvedGroups, primitiveGroups]]
*)

boundaryParam = Join[Map[createBoundTypeParam, evolvedGFs],
                     Map[createBoundTypeParam, Map[unqualifiedGroupName,evolvedGroups]],

                     Map[createBoundSpeedParam, evolvedGFs],
                     Map[createBoundSpeedParam, Map[unqualifiedGroupName,evolvedGroups]], 

                     Map[createBoundLimitParam, evolvedGFs],
                     Map[createBoundLimitParam, Map[unqualifiedGroupName,evolvedGroups]],

                     Map[createBoundScalarParam, evolvedGFs],
                     Map[createBoundScalarParam, Map[unqualifiedGroupName,evolvedGroups]] 
];
 

nEvolved   = Length[evolvedGFs];
nPrimitive = Length[primitiveGFs];

evolvedMoLParam =
  {Name -> thornName <> "_MaxNumEvolvedVars",
   Type -> "CCTK_INT",
   Default -> nEvolved,
   Description -> "Number of evolved variables used by this thorn",
   Visibility -> "restricted",
   AccumulatorBase -> "MethodofLines::MoL_Num_Evolved_Vars",
   AllowedValues -> {{Value -> ToString[nEvolved] <> ":" <> ToString[nEvolved] , 
                      Description -> "Number of evolved variables used by this thorn"}}};
constrainedMoLParam =
  {Name -> thornName <> "_MaxNumConstrainedVars",
   Type -> "CCTK_INT",
   Default -> nPrimitive,
   Description -> "Number of constrained variables used by this thorn",
   Visibility -> "restricted",
   AccumulatorBase -> "MethodofLines::MoL_Num_Constrained_Vars",
   AllowedValues -> {{Value -> ToString[nPrimitive] <> ":" <> ToString[nPrimitive] , 
                      Description -> "Number of constrained variables used by this thorn"}}};

excisionParam =
  {Name -> "excision",
   Type -> "BOOLEAN",
   Default -> "\"true\"",
   Description -> "whether to apply excision or not",
   Visibility -> "restricted"};

findboundaryParam =
  {Name -> "find_excision_boundary",
   Type -> "BOOLEAN",
   Default -> "\"true\"",
   Description -> "whether to locate excision boundary",
   Visibility -> "restricted"};

findnormalsParam =
  {Name -> "find_excision_normals",
   Type -> "BOOLEAN",
   Default -> "\"true\"",
   Description -> "whether to compute normals to excision boundary",
   Visibility -> "restricted"};


If[createExcisionCode,
 newParams = Join[boundaryParam, {excisionParam, findboundaryParam, findnormalsParam},
                                 {evolvedMoLParam, constrainedMoLParam}];,
 newParams = Join[boundaryParam, {evolvedMoLParam, constrainedMoLParam}];
];

molImplementation =
{Name -> "MethodOfLines",
 UsedParameters -> {{Name -> "MoL_Num_Evolved_Vars",     Type -> "CCTK_INT"},
                    {Name -> "MoL_Num_Constrained_Vars", Type -> "CCTK_INT"}}};

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
   implementations = {molImplementation, genericFDImplementation, baseImp},
   implementations = {molImplementation, genericFDImplementation}];

Map[AppendTo[newParams, #]&,
          Flatten[{Map[completeRealParamStruct, realParameters],
                   Map[completeIntParamStruct,  intParameters]}, 1]];

paramspec = {Implementations -> implementations,
             NewParameters -> newParams};

param = CreateParam[paramspec];


(* STARTUP  <> ": set RHSs for MoL" *)
startup = CreateStartupFile[thornName, thornName];


(* MOL REGISTER *)
molspec = {EvolvedGFs   -> Map[qualifyGFName[#, groups, baseImplementation]& , evolvedGFs], 
           PrimitiveGFs -> primitiveGFs,
           BaseImplementation -> baseImplementation, ThornName -> thornName};

molregister = CreateMoLRegistrationSource[molspec, debug];


(* BOUNDARIES *)
molspec = {Groups -> evolvedGroups, 
           EvolvedGFs -> Map[qualifyGFName[#, groups, baseImplementation]& , evolvedGFs],
           BaseImplementation -> baseImplementation, ThornName -> thornName,
           ThornImplementation -> implementation, ExcisionGFs -> evolvedGFs};

molboundaries = CreateMoLBoundariesSource[molspec];

If[createExcisionCode, molexcision   = CreateMoLExcisionSource[molspec]];

(* MAKEFILE *)
make = CreateMakefile[sourceFiles];


(* CREATE THORN *)
Print["create excision files: ", createExcisionCode];

ext = CodeGen`SOURCESUFFIX;

(* Write the differencing header file *)
diffHeader = CreateDifferencingHeader[pddefs, lookupDefault[opts, ZeroDimensions, {}]];

If[createExcisionCode, 
sources =  {
            {Filename -> StartupName            <> ".c",   Contents -> startup}, 
            {Filename -> molregisterName        <> ".c",   Contents -> molregister},
            {Filename -> molboundariesFileName  <> ".c",   Contents -> molboundaries},
            {Filename -> molexcisionName        <> ".F90", Contents -> molexcision},
            {Filename -> setterFileName         <> ext,    Contents -> calcrhs}, 
            {Filename -> precompheaderName,                Contents -> precompheader},
                         {Filename -> "Differencing.h",    Contents -> diffHeader}
           },
sources =  {
            {Filename -> StartupName            <> ".c",   Contents -> startup}, 
            {Filename -> molregisterName        <> ".c",   Contents -> molregister},
            {Filename -> molboundariesFileName  <> ".c",   Contents -> molboundaries},
            {Filename -> setterFileName         <> ext,    Contents -> calcrhs}, 
            {Filename -> precompheaderName,                Contents -> precompheader},
                         {Filename -> "Differencing.h",    Contents -> diffHeader}
           }
];

thornspec = {Name      -> thornName, 
             Directory -> arrangementDirectory,
	     Interface -> interface, 
             Schedule  -> schedule, 
             Param     -> param, 
             Makefile  -> make,
             Sources   -> sources
};
CreateThorn[thornspec];

Print["Finished creating MoL thorn"];

Append[ThornList,
       {ThornName          ->  thornName,
	ThornImplementation -> implementation, 
        ThornArrangement   ->  systemName,
        ThornGroups        ->  {}, 
        ThornParameters    ->  {"MISSING"},
        ThornType          ->  "Evolve"}]			    
];


(****************************************************************************)
(*         setter thorn                                                     *)
(****************************************************************************)

Options[CreateSetterThorn] = 
   (* The following have nontrivial defaults, so should not be included
      here: ThornName, Implementation, SystemDescription *)
  {RealBaseParameters    -> {},
   IntBaseParameters     -> {},
   RealParameters        -> {},
   IntParameters         -> {},
   SystemName            -> SystemNameDefault,
   DeBug                 -> False,
   SystemParentDirectory -> ".",
   SetTime               -> "initial_and_poststep",
   Groups                -> {},
   Include               -> {}};

CreateSetterThorn[calc_, groups_, optArgs___] :=

Module[{after, allowedSetTimes, baseImplementation, baseParamsTrueQ, before, calcrhsName, debug,
        file, GFs, globalStorageGroups, implementation, implementations, intParameters,
        namedCalc, precompheaderName, realParameters, RHSs, setgroups, setTime,
        ThornList, ext, include, genericFDImplementation, baseImp, numeq, grepSYNC, scheduledStartup,
        sheduledINITIAL, scheduledPOSTSTEP},

    Print["\n*** CreateSetterThorn ***"];

(* process optional arguments *)
opts = GetOptions[CreateSetterThorn, {optArgs}];

(* These should be the same for all the thorns *)
parentDirectory   = lookup[opts, SystemParentDirectory];
systemName        = lookup[opts, SystemName];
systemDescription = lookupDefault[opts, SystemDescription, systemName];


(* thorn specific values *)
realParameters     = lookup[opts, RealParameters];
intParameters      = lookup[opts, IntParameters];
realBaseParameters = lookup[opts, RealBaseParameters];
intBaseParameters  = lookup[opts, IntBaseParameters];
allParameters = Join[realParameters, intParameters, realBaseParameters, intBaseParameters];

thornName          = lookupDefault[opts, ThornName, systemName <> "Setter"];

implementation     = lookupDefault[opts, Implementation, thornName];
baseImplementation = systemName <> "Base";

setTime            = lookup[opts, SetTime];
pddefs             = lookupDefault[opts, PartialDerivatives, {}];

debug              = lookup[opts, DeBug];
include            = lookupDefault[opts, Include, {}];


If[debug,
  Print["Debugging switched on"],
  Print["Debugging switched off"]
 ];

  allowedSetTimes = {"initial_only", "poststep_only", "initial_and_poststep"};

  If[!MemberQ[allowedSetTimes, setTime],
     Module[{},
       Print["Unknown value for option SetTime: ", SetTime];
       Throw["Allowed values for option SetTime are: \"initial_only\", \"poststep_only\" and \"initial_and_poststep\""]]];



baseParamsTrueQ = Length@realBaseParameters + Length@intBaseParameters > 0;


(* define directories and create if needed *)
arrangementDirectory = parentDirectory <> "/" <> systemName;

EnsureDirectory[parentDirectory];
EnsureDirectory[arrangementDirectory];


(* the list of thorns = return argument! *)

ThornList = {{ThornName -> "GenericFD", ThornArrangement -> "KrancNumericalTools",
              ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "CartGrid3D", ThornArrangement -> "CactusBase", ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"}
(* *)
};


namedCalc = augmentCalculation[calc, thornName <> "_Set", 
                               baseImplementation, groups, allParameters, pddefs];

Print["number of equations in calculation: ", numeq = Length@lookup[namedCalc, Equations]];


setgroups = calculationSetGroups[namedCalc];

Print["setgroups: ", setgroups];

calcrhsName       = lookup[namedCalc, Name];
precompheaderName = "precomputations.h";

before = If[mapContains[namedCalc, Before],
            " BEFORE (" <> FlattenBlock @ SpaceSeparated @ lookup[namedCalc, Before] <> ") ",
            "" ];

after  = If[mapContains[namedCalc, After],
            " AFTER (" <> FlattenBlock@SpaceSeparated@lookup[namedCalc, After] <> ") ",
            "" ];


(* INTERFACE *)
inheritedImplementations = Join[{baseImplementation, "Grid", "GenericFD"}, 
                                lookupDefault[opts, InheritedImplementations, {}]];

includeFiles             = {"GenericFD.h"};


newgroups = {}; 
interface = CreateInterface[implementation, inheritedImplementations, includeFiles, newgroups];


(* PARAM *)
newparams = {}; 

Map[AppendTo[newparams, #]&,
          Flatten[{Map[completeRealParamStruct, realParameters],
                   Map[completeIntParamStruct,  intParameters]}, 1]];

If[(setTime == "initial_and_poststep"),

  AppendTo[newparams, {Name -> "set_initial_data", Type -> "BOOLEAN", Default -> "\"true\"",
  Description -> "whether to set initial data", Visibility -> "private"}];

  AppendTo[newparams, {Name -> "set_poststep", Type -> "BOOLEAN", Default -> "\"true\"",
  Description -> "whether to set data after intermediate MoL steps", Visibility -> "private"}]];


genericFDImplementation =
{Name -> "GenericFD",
 UsedParameters -> {{Name -> "stencil_width",    Type -> "CCTK_INT"},
                    {Name -> "stencil_width_x",  Type -> "CCTK_INT"},
                    {Name -> "stencil_width_y",  Type -> "CCTK_INT"},
                    {Name -> "stencil_width_z",  Type -> "CCTK_INT"}}};


baseImp = {Name -> baseImplementation,
           UsedParameters -> Flatten[{Map[implementationRealParamStruct, realBaseParameters],
                                      Map[implementationIntParamStruct,  intBaseParameters]   }, 1]};

If[baseParamsTrueQ,
   implementations = {genericFDImplementation, baseImp},
   implementations = {genericFDImplementation}
];


paramspec = {Implementations -> implementations,
             NewParameters   -> newparams};

param = CreateParam[paramspec];


(* STARTUP *)
startup = CreateStartupFile[thornName, thornName <> ": set values"];


(* CALCULATION and PRECOMP MACROS *)
setrhs        = CreateSetterSource[{namedCalc}, debug, Include -> include];
precompheader = CreatePrecompMacros[ namedCalc ];

ext = CodeGen`SOURCESUFFIX;

(* search for SYNCs *)
If[numeq <= 1, 
   grepSYNC = GrepSyncGroups[setrhs, calcrhsName]; ,
   grepSYNC = {};
   setRHS = UncommentSourceSync[setrhs, calcrhsName];
   Print["> 1 loop in thorn -> scheduling in source code, incompatible with Multipatch!"];
];


(* SCHEDULE *)
scheduledGroups     = {};
scheduledFunctions  = {};
globalStorageGroups = Map[simpleGroupStruct[#, 1]&, setgroups];

scheduledStartup = {Name          -> thornName <> "_Startup",
                    SchedulePoint -> "at STARTUP",
                    Language      -> "C",
                    Options       -> "meta",
                    Comment       -> "create banner"};


scheduledINITIAL  = {Name               -> calcrhsName,
                     SchedulePoint      -> "AT POSTINITIAL" <> before <> after,
                     SynchronizedGroups -> grepSYNC,
                     Language           -> CodeGen`SOURCELANGUAGE, 
                     Conditional        -> If[setTime == "initial_and_poststep", 
                                                    {Textual -> "set_initial_data"}, 
                                                    {}
                                                 ],
                     Comment            -> "set initial values"};

scheduledPOSTSTEP  = {Name               -> calcrhsName,
                      SchedulePoint      -> "in MoL_PostStep" <> before <> after, 
                      SynchronizedGroups -> grepSYNC,
                      Language           -> CodeGen`SOURCELANGUAGE, 
                      Conditional        -> If[setTime == "initial_and_poststep", 
                                                 {Textual -> "set_poststep"}, 
                                                 {}
                                              ],
                      Comment            -> "set values"};

If[(setTime == "initial_only"),
   scheduledFunctions = {scheduledINITIAL};
];
 
If[(setTime == "poststep_only"),
   scheduledFunctions = {scheduledPOSTSTEP};
];

If[(setTime == "initial_and_poststep"),
   scheduledFunctions = {scheduledINITIAL, scheduledPOSTSTEP};
];


schedule = CreateSchedule[globalStorageGroups, {}, scheduledFunctions];



(* Write the differencing header file *)
diffHeader = CreateDifferencingHeader[pddefs, lookupDefault[opts, ZeroDimensions, {}]];


(* MAKEFILE *)
make = CreateMakefile[{StartupName <> ".c", calcrhsName <> ext}];

(* CREATE THORN *)
thornspec = {Name      -> thornName, 
             Directory -> arrangementDirectory,
	     Interface -> interface, 
             Schedule  -> schedule, 
             Param     -> param, 
             Makefile  -> make, 
             Sources   -> {
                  {Filename -> StartupName       <> ".c", Contents -> startup},
                  {Filename -> calcrhsName       <> ext,  Contents -> setrhs}, 
                  {Filename -> precompheaderName,         Contents -> precompheader},
                         {Filename -> "Differencing.h",    Contents -> diffHeader}
                          }
};
CreateThorn[thornspec];

Append[ThornList,
       {ThornName           -> thornName, 
	ThornImplementation -> implementation,
        ThornArrangement    -> systemName,
        ThornGroups         -> {}, 
        ThornParameters     -> paramspec,
        ThornType           -> "Set"}]
];

(****************************************************************************)
(*         evaluator thorn                                                  *)
(****************************************************************************)

 (* The following have nontrivial defaults, so should not be included
      here: ThornName, Implementation, SystemDescription *)

Options[CreateEvaluatorThorn] = {RealParameters        -> {},
                                 IntParameters         -> {},
                                 RealBaseParameters    -> {},
                                 IntBaseParameters     -> {},
                                 DeBug                 -> False,
                                 SystemName            -> SystemNameDefault,
                                 SystemParentDirectory -> "."
                           };

CreateEvaluatorThorn[unqualifiedEvaluationDefinitions_, unqualifiedGroups_, optArgs___] :=

Module[{file, implementation, debug, newgroupnames,
        baseImplementation, ThornList, EvaluateParameters, globalStorageGroups,
        shorthandMember, definitions, allexpressions, 
        calcrhsName, precompheaderName, evals, namedcalc, 
        realParameters, intParameters,
        realBaseParameters, intBaseParameters, baseParamsTrueQ,
        parentDirectory, systemName, thornName, systemDescriptipion, 
        arrangementDirectory, genericFDImplementation, baseImp, implementations},

opts = GetOptions[CreateEvaluatorThorn, {optArgs}];

Print["\n*** CreateEvaluatorThorn ***"];

debug               = lookup[opts, DeBug];

(* These should be the same for all the thorns *)
parentDirectory     = lookup[opts, SystemParentDirectory];
systemName          = lookup[opts, SystemName];
systemDescription   = lookupDefault[opts, SystemDescription, systemName];

baseImplementation  = systemName <> "Base";

realParameters      = lookup[opts, RealParameters];
intParameters       = lookup[opts, IntParameters];
realBaseParameters  = lookup[opts, RealBaseParameters];
intBaseParameters   = lookup[opts, IntBaseParameters];
allParameters = Join[realParameters, intParameters, realBaseParameters, intBaseParameters];

thornName           = lookupDefault[opts, ThornName, systemName <> "Evaluator"];
implementation      = lookupDefault[opts, Implementation, thornName];

pddefs = lookupDefault[opts, PartialDerivatives, {}];

arrangementDirectory = parentDirectory <> "/" <> systemName;

EnsureDirectory[parentDirectory];
EnsureDirectory[arrangementDirectory];

Print["Creating files in directory " <> arrangementDirectory];


baseParamsTrueQ = Length@realBaseParameters + Length@intBaseParameters > 0;

If[debug,
  Print["Debugging switched on"],
  Print["Debugging switched off"]
 ];

(* Need to make sure that all the new evaluated group names in
   "groups" are fully qualified with this implementation.  The
   remaining groups will be sorted out in augmentCalculation.  We may
   prefer to do that here, eventually. *)

groups = Module[{simpleEvaluatedGroupNames = 
                  Map[unqualifiedGroupName[First[#]] &, 
                      unqualifiedEvaluationDefinitions]},
           qualifyGroups[unqualifiedGroups, simpleEvaluatedGroupNames, 
                         implementation, baseImplementation]];

If[debug, Print["groups (qualified) == ", groups]];

qualifyEvaluationDefinition[{name_, calc_}, imp_] :=
  {qualifyGroupName[name, imp], calc};

evaluationDefinitions = Map[qualifyEvaluationDefinition[#, implementation] &,
                            unqualifiedEvaluationDefinitions];

(* Print["evaluationDefinitions (qualified) == ", evaluationDefinitions]; *)

evaluatedGroupNames = Map[First, evaluationDefinitions];

interfaceGroupStructure[name_] := 
  {Name           ->  unqualifiedGroupName[name], 
   VariableType   ->  "CCTK_REAL", 
   Timelevels     ->  1,  
   GridType       ->  "GF tags='Prolongation=\"None\"'",
   Comment        ->  unqualifiedGroupName[name], 
   Visibility     ->  "private", 
   Variables      ->  variablesInGroup[name, groups]};


newGroupInterfaceStructures = Map[interfaceGroupStructure, evaluatedGroupNames];

(* the list of thorns = return argument! *)

ThornList = {{ThornName -> "GenericFD", ThornArrangement -> "KrancNumericalTools",
              ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "CartGrid3D", ThornArrangement -> "CactusBase", ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"}
};

(* this list collects all parameters and default values *)
EvaluateParameters = {};


(* This is the name for the source file *)
calcrhsName       = thornName <> "_Eval";
precompheaderName = "precomputations.h";

newparams = {};


(* RHS CALCULATION *)

augmentEvaluationDefinition[{gName_, calc_}] :=
  {gName, 
   augmentCalculation[calc, thornName <> "_" <> unqualifiedGroupName[gName] <> "_Eval", 
                      implementation, groups, allParameters, pddefs]};

augmentedEvaluationDefinitions = 
  Map[augmentEvaluationDefinition, evaluationDefinitions];

evalCalcs = Map[Last, augmentedEvaluationDefinitions];

setrhs = CreateSetterSource[evalCalcs, debug];


(* PRECOMP MACROS *)

(* this is currenly not functional, is kept here for future use *)
precompheader = CreatePrecompMacros[calculation];


(* INTERFACE *)


inheritedImplementations = Join[{baseImplementation, "Grid", "GenericFD"}, 
                                 lookupDefault[opts, InheritedImplementations, {}]];

includeFiles             = {"GenericFD.h"};

interface = CreateInterface[implementation, inheritedImplementations, 
                            includeFiles, newGroupInterfaceStructures];

(* SCHEDULE *)

(* globalStorageGroups = {Map[simpleGroupStruct[#, 1]&, Map[ToString, evaluationGroupStructs]]};*)

scheduledGroups = {};

newGroupScheduleStructure[{groupName_, calc_}] := 
  {Name               -> lookup[calc, Name],
   SchedulePoint      -> "at ANALYSIS",
   SynchronizedGroups -> GrepSyncGroups[setrhs, lookup[calc, Name]],
   Language           -> CodeGen`SOURCELANGUAGE,
   TriggerGroups      -> {groupName},
   allGroups = Union[{groupName}, calculationUsedGroups[calc]];
   allStorageGroups = Map[{Group -> #, Timelevels -> 1} &, allGroups];
   StorageGroups      -> allStorageGroups,
   Comment            -> "evaluate GFs"};

scheduledFunctions = Map[newGroupScheduleStructure, augmentedEvaluationDefinitions];

AppendTo[scheduledFunctions, {Name          -> thornName <> "_Startup",
                              SchedulePoint -> "at STARTUP",
                              Language      -> "C",
                              Options       -> "meta",
                              Comment       -> "create banner"}
        ];


Print["TriggerGroups: ", evaluatedGroupNames];

globalStorageGroups =
Module[{cs},
  cs = Map[#[[2]] &, augmentedEvaluationDefinitions];
  gs = Flatten[Map[calculationUsedGroups[#] &, cs]];
  Map[{Group -> #, Timelevels -> 1} &, gs]];

Print["globalStorageGroups == ", globalStorageGroups];

schedule = CreateSchedule[globalStorageGroups, scheduledGroups, scheduledFunctions];

(* PARAM *)

Map[AppendTo[newparams, #]&, 
          Flatten[{Map[completeRealParamStruct, realParameters],
                   Map[completeIntParamStruct,  intParameters]}, 1]];

genericFDImplementation =
{Name -> "GenericFD",
 UsedParameters -> {{Name -> "stencil_width",    Type -> "CCTK_INT"},
                    {Name -> "stencil_width_x",  Type -> "CCTK_INT"},
                    {Name -> "stencil_width_y",  Type -> "CCTK_INT"},
                    {Name -> "stencil_width_z",  Type -> "CCTK_INT"}}};

baseImp = {Name -> baseImplementation, 
           UsedParameters -> Flatten[{Map[implementationRealParamStruct, realBaseParameters],
                                      Map[implementationIntParamStruct,  intBaseParameters]   }, 1]};

If[baseParamsTrueQ,
   implementations = {genericFDImplementation, baseImp},
   implementations = {genericFDImplementation}
];

paramspec = {Implementations -> implementations, NewParameters -> newparams};

param = CreateParam[paramspec];


(* STARTUP *)

startup = CreateStartupFile[thornName, thornName <> ": evaluate grid functions"];

(* MAKEFILE *)
ext  = CodeGen`SOURCESUFFIX;
make = CreateMakefile[{StartupName <> ".c", calcrhsName <> ext}];


(* Write the differencing header file *)
diffHeader = CreateDifferencingHeader[pddefs, lookupDefault[opts, ZeroDimensions, {}]];

(* CREATE THORN *)

thornspec = {Name      -> thornName, 
             Directory -> arrangementDirectory,
	     Interface -> interface, 
             Schedule  -> schedule, 
             Param     -> param, 
             Makefile  -> make, 
             Sources   -> {
                         {Filename -> StartupName <> ".c", Contents -> startup},
                         {Filename -> calcrhsName <> ext,  Contents -> setrhs}, 
                         {Filename -> precompheaderName,   Contents -> precompheader},
                         {Filename -> "Differencing.h",    Contents -> diffHeader}
                          }
};

CreateThorn[thornspec];



Append[ThornList,
       {ThornName -> thornName, ThornImplementation -> implementation,
        ThornArrangement -> systemName,
        ThornGroups -> Map[unqualifiedGroupName, evaluatedGroupNames], 
        ThornParameters -> paramspec,
        ThornType -> "Evaluate"}]
];

(****************************************************************************)
(*         base thorn                                                       *)
(****************************************************************************)

Options[CreateBaseThorn] = 
  {RealBaseParameters -> {},
   IntBaseParameters -> {},
   SystemName -> SystemNameDefault,
   DeBug -> False,
   SystemParentDirectory -> ".",
   EvolutionTimeLevels  -> 3,
   CreateExcisionCode -> False,
   GuessTensorTypes -> False
   (* The following have nontrivial defaults, so should not be included
      here: ThornName, Implementation, SystemDescription *)};

CreateBaseThorn[groups1_, evolvedGroupNames_, primitiveGroupNames_, optArgs___] :=
  Module[{debug,directory,systemName,thornName,
          implementation,systemDescription,thornList,completePrimitiveGroups,
          evolvedGFs,primitiveGFs,allGFs, newGroupNames,
          rhsGroups,rhsGroupNames,allGroupNames, localPrimitiveGroupNames,
          completeEvolvedGroupStruct,completePrimitiveGroupStruct,groupStructures,
          inheritedImplementations,includeFiles,interface,storageGroups,
          scheduledGroups,scheduledFunctions,schedule,paramSpec,param,symmetries,
          startup,make,evTimeLevels,thornSpec,thisThorn, groups, createExcisionCode},

  Print["\n*** CreateBaseThorn ***"];

(* We do not want the implementation names in these group names.  It
   may not have been supplied, but if it has been, we strip it. *)
groups = Map[{unqualifiedGroupName[groupName[#]], Last[#]} &, groups1];

opts = GetOptions[CreateBaseThorn, {optArgs}];

(* These should be the same for all the thorns *)
debug                = lookup[opts, DeBug];
parentDirectory      = lookup[opts, SystemParentDirectory];
systemName           = ToString@lookup[opts, SystemName];
thornName            = lookupDefault[opts, ThornName, systemName <> "Base"];
implementation       = lookupDefault[opts, Implementation, thornName];
systemDescription    = lookupDefault[opts, SystemDescription, systemName];
evTimeLevels         = lookupDefault[opts, EvolutionTimeLevels, 3];
createExcisionCode   = lookupDefault[opts, CreateExcisionCode, False];
arrangementDirectory = parentDirectory <> "/" <> systemName;

Print["Creating files in directory " <> arrangementDirectory];

EnsureDirectory[parentDirectory];
EnsureDirectory[arrangementDirectory];

(* the list of thorns = return argument! *)
thornList = 
  {{ThornName -> "CartGrid3D", ThornArrangement -> "CactusBase", ThornGroups -> {}, 
    ThornParameters -> {}, ThornType -> "External"},

    {ThornName -> "PUGH", ThornArrangement -> "CactusPUGH", ThornGroups -> {},
     ThornParameters -> {}, ThornType -> "Driver"},

    {ThornName -> "Boundary", ThornArrangement -> "CactusBase", ThornGroups -> {},
     ThornParameters -> {}, ThornType -> "External"},

    {ThornName -> "Time", ThornArrangement -> "CactusBase", ThornGroups -> {},
     ThornParameters -> {}, ThornType -> "External"},

    {ThornName -> "IOBasic", ThornArrangement -> "CactusBase", ThornGroups -> {},
     ThornParameters -> {}, ThornType -> "External"},

    {ThornName -> "IOUtil", ThornArrangement -> "CactusBase", ThornGroups -> {},
     ThornParameters -> {}, ThornType -> "External"},

    {ThornName -> "IOASCII", ThornArrangement -> "CactusBase", ThornGroups -> {},
     ThornParameters -> {}, ThornType -> "External"},

    {ThornName -> "PUGHSlab", ThornArrangement -> "CactusPUGH", ThornGroups -> {},
     ThornParameters -> {}, ThornType -> "Driver"},

    {ThornName -> "PUGHReduce", ThornArrangement -> "CactusPUGH", ThornGroups -> {},
     ThornParameters -> {}, ThornType -> "Driver"}};


(*****************************************************************)
(* Add some new groups that the user does not need to know about *)
(*****************************************************************)

(* This is needed for excision *)
excisionGroup = {"ExcisionNormals", {exnormx, exnormy, exnormz}};

(* These are needed for MoL *)
rhsGroups     = Map[evolvedGroupToRHSGroup[#, groups] &, evolvedGroupNames];
rhsGroupNames = Map[groupName, rhsGroups];

allGroups     = Join[groups, {excisionGroup}, rhsGroups];
allGroupNames = Map[groupName, allGroups];
newGroupNames = Join[evolvedGroupNames, primitiveGroupNames , Map[groupName, Join[{excisionGroup}, rhsGroups]]];

groups = allGroups;

(* All the grid functions that are either evolved or primitive *)
If[createExcisionCode, Print["Creating Excision Code"];,
                       Print["Not Creating Excision Code"];];

If[createExcisionCode, localPrimitiveGroupNames = Flatten@{primitiveGroupNames, "ExcisionNormals"};,
                       localPrimitiveGroupNames = primitiveGroupNames;];

allGFs       = Join[variablesFromGroups[evolvedGroupNames,   allGroups],
                    variablesFromGroups[localPrimitiveGroupNames, allGroups]];

If[debug,
  Print["newGroupNames == ", newGroupNames];
  Print["rhsGroups     == ", rhsGroups];
  Print["rhsGroupNames == ", rhsGroupNames];
  Print["allGroups     == ", allGroups];
  Print["allGFs        == ", allGFs];
];

(* INTERFACE *)

tensorType[group_] := Module[{comps, type, tmetric, tweight, tspecial},
    comps = Length@group[[2]]  ;
 
    If[   StringQ@Global`UserSetTensorWeight[group[[1]]], 
       tweight = " tensorweight=" <> Global`UserSetTensorWeight[group[[1]]], 
       tweight = ""];


    If[    StringQ@Global`UserSetTensorSpecial[group[[1]]],
       tspecial = " tensorspecial=\"" <> Global`UserSetTensorSpecial[group[[1]]] <> "\"", 
       tspecial = ""];

    If[   StringQ@Global`UserSetTensorMetric[group[[1]]],
       tmetric = " tensormetric=\"" <> Global`UserSetTensorMetric[group[[1]]] <> "\"", 
       tmetric = ""];

    type = tType@group;
    Print["Group ", group[[1]],  " marked as tensor type " <> type];
    
    " tags='tensortypealias=\"" <> type <> "\"" <> tmetric <> tweight <> tspecial <>"'"
    ];


tType[group_] := Module[{comps, indices, type},

    comps = Map[ToString,     group[[2]] ] ;

    indices = Map[BreakTensorComponentName, comps];
    indices = Map[#[[2]] &, indices];

    type = "scalar"; (* the default *)

    If [Sort@indices == Sort@{"11", "21", "22", "31", "32", "33", "12", "13", "23"}, type = "DD"];
    If [Sort@indices == Sort@{"11", "21", "22", "31", "32", "33"},                   type = "DD_sym"];
    If [Sort@indices == Sort@{"33", "23", "22", "13", "12", "11"},                   type = "UU_sym"];


    If [indices == {"1", "2", "3"}, type = "D"];
    If [indices == {"3", "2", "1"}, type = "U"];


    If[ValueQ@Global`UserSetTensorType[x], type = Global`UserSetTensorType[group[[1]]] ];

    type
    ];


completeEvolvedGroupStruct[group_] := 
  {Name -> First@group, VariableType -> "CCTK_REAL", 
   Timelevels -> evTimeLevels,  
   If[lookupDefault[opts, GuessTensorTypes, False],
      GridType -> "GF" <> tensorType[group],
      GridType -> "GF"],
   Comment -> First@group, Visibility -> "public", 
   Variables -> SortTensorComponentsCactusStyle[Map[ToString, Last@group], tType@group]};

completePrimitiveGroupStruct[group_] := 
  {Name -> First@group, VariableType -> "CCTK_REAL",
   Timelevels -> 1, 
   GridType -> "GF" <> StringReplace[tensorType[group], "tags='" -> "tags='Prolongation=\"None\" "],
   Comment -> First@group, Visibility -> "public",
   Variables -> SortTensorComponentsCactusStyle[Map[ToString, Last@group], tType@group]};

evolvedBlock   = Map[completeEvolvedGroupStruct[  groupFromName[#, allGroups]] &, evolvedGroupNames];
primitiveBlock = Map[completePrimitiveGroupStruct[groupFromName[#, allGroups]] &, localPrimitiveGroupNames];
rhsBlock       = Map[completePrimitiveGroupStruct[groupFromName[#, allGroups]] &, rhsGroupNames];

groupStructures = Join[evolvedBlock, primitiveBlock, rhsBlock];

inheritedImplementations = Join[{"Grid"},lookupDefault[opts, InheritedImplementations, {}]];
includeFiles             = {};
interface = CreateInterface[implementation, inheritedImplementations, 
                            includeFiles, groupStructures];
(* SCHEDULE *)
scheduledGroups = {};
storageGroups   = {};
If[createExcisionCode, storageGroups = {{Group      -> "ExcisionNormals",
                                         Timelevels -> 1}}];


scheduledFunctions = {{Name          -> thornName <> "_Startup",
                       SchedulePoint -> "at STARTUP", 
                       Language      -> "C",
                       Options       -> "meta",
                       Comment       -> "create banner"},

                      {Name          -> thornName <> "_RegisterSymmetries",
                       SchedulePoint -> "at BASEGRID", 
                       Language      -> "C",
                       Options       -> "meta",
                       Comment       -> "register symmetries"}};

schedule = CreateSchedule[storageGroups, scheduledGroups, scheduledFunctions];

(* PARAM *)
paramSpec = {NewParameters -> Join[Map[completeRealParamStruct, 
                                       lookup[opts, RealBaseParameters]],
                                   Map[completeIntParamStruct, 
                                       lookup[opts, IntBaseParameters]]]};

param = CreateParam[paramSpec];

(* SYMMETRIES REGISTRATION *)
symmetries = CreateSymmetriesRegistrationSource[thornName, implementation, allGFs, debug];

(* STARTUP *)
startup = CreateStartupFile[thornName, thornName <> ": base thorn"];

(* MAKEFILE *)
make = CreateMakefile[{StartupName <> ".c", "RegisterSymmetries.c"}];

(* CREATE THORN *)
thornSpec = {Name      -> thornName, 
             Directory -> arrangementDirectory,
	     Interface -> interface, 
             Schedule  -> schedule, 
             Param     -> param, 
             Makefile  -> make, 
             Sources   -> {{Filename -> StartupName <> ".c",
                            Contents -> startup},
                           {Filename -> "RegisterSymmetries.c", 
                            Contents -> symmetries}}};

CreateThorn[thornSpec];

thisThorn = {ThornName -> thornName, ThornImplementation -> implementation,
             ThornArrangement -> systemName,
             ThornGroups -> newGroupNames, 
             ThornParameters -> paramSpec,
             ThornType -> "Base"};

Append[thornList, thisThorn]
];


(* imp is the implementation to be used for any groups whose
   implementation is not included in its name *)
augmentCalculation[calc_, name_, imp_, groups_, parameters_, pddefs_] :=
  Module[{c, imps, simpleGroups, coordGroup, fullGroups},
    coordGroup = {"grid::coordinates", {sym`x,sym`y,sym`z,sym`r}};

    fullGroups = Map[renameGroup[#, qualifyGroupName[groupName[#], imp]] &, 
                     groups];

    fullGroups = Join[fullGroups, {coordGroup}];
    c = mapEnsureKey[calc, Name, name];
    c = mapEnsureKey[c, Groups, fullGroups];
    c = mapEnsureKey[c, Parameters, parameters];
    c = mapEnsureKey[c, PartialDerivatives, pddefs];

    If[mapContains[c, SyncGroups],
       (* Make sure they are fully qualified *)
       c = mapReplace[c, SyncGroups, Map[qualifyGroupName[#, imp] &, lookup[c, SyncGroups]]],
       (* Allow all groups to be synchronized *)
       c = mapEnsureKey[c, SyncGroups, Map[groupName, fullGroups]]];

    (* Change the dot[gf] syntax into gf <> rhs notation *)
    c = replaceDots[c];

    c
  ];

allGroupVariables[groups_] :=
  Flatten[Map[Last, groups],1];


(* Return the group names of any gridfunctions set in the calculation *)
calculationSetGroups[calc_] :=
  Module[{equations, groups, allVars, lhss, gfLhss},
    equations = Flatten[lookup[calc, Equations],1];
    groups    = lookup[calc,Groups];
    allVars   = allGroupVariables[groups];
    lhss      = Map[First, equations];
    gfLhss    = Intersection[lhss,allVars];
 
   containingGroups[gfLhss, groups]];


(****************************************************************************)
(*         translator thorn                                                 *)
(****************************************************************************)

Options[CreateTranslatorThorn] =  
  {RealBaseParameters    -> {},
   IntBaseParameters     -> {},
   RealParameters        -> {},
   IntParameters         -> {},
   SystemName            -> SystemNameDefault,
   DeBug                 -> False,
   SystemParentDirectory -> ".", 

   (* The following have nontrivial defaults, so should not be included
      here: ThornName, Implementation, SystemDescription *)

   TranslatorInCalculation  -> {},
   TranslatorOutCalculation -> {}};


CreateTranslatorThorn[groups_, optArgs___] :=

Module[{after, baseImplementation, before, cleanADMBaseRules, dirName,
          options, thornName, translatedToADMGroups, toEvolveSetgroups,
          translatedFromADMGroups, ThornList, TranslatorParameters,
          gridFunctions, shorthandDefinitions, shorthands, InGFs,
          OutGFs, GFs, RHSs, setgroups, translatorInCalculation,
          translatorOutCalculation, scheduledStartup,
          scheduledADMToEvolve, scheduledEvolveToADM, setterFileName,
          baseParamsTrueQ, genericFDImplementation, baseImp, implementations},

    Print["\n*** CreateTranslatorThorn ***"];

    If[debug, Print["groups == ", groups]];

    opts = GetOptions[CreateTranslatorThorn, {optArgs}];

    (* These should be the same for all the thorns *)
    debug                 = lookup[opts, DeBug];
    parentDirectory = lookup[opts, SystemParentDirectory];
    systemName = lookup[opts, SystemName];
    thornName = lookupDefault[opts, ThornName, systemName <> "Translator"];
    implementation = lookupDefault[opts, Implementation, thornName];
    systemDescription = lookupDefault[opts, SystemDescription, systemName];
    arrangementDirectory = parentDirectory <> "/" <> systemName;

    realParameters     = lookup[opts, RealParameters];
    intParameters      = lookup[opts, IntParameters];
    realBaseParameters = lookup[opts, RealBaseParameters];
    intBaseParameters  = lookup[opts, IntBaseParameters];
    allParameters      = Join[realParameters, intParameters, realBaseParameters, intBaseParameters];
    pddefs             = lookupDefault[opts, PartialDerivatives, {}];

    baseParamsTrueQ = Length@realBaseParameters + Length@intBaseParameters > 0;

    EnsureDirectory[parentDirectory];
    EnsureDirectory[arrangementDirectory];

    Print["Creating files in directory " <> arrangementDirectory];

    translatorInCalculation  = lookup[opts, TranslatorInCalculation];
    translatorOutCalculation = lookup[opts, TranslatorOutCalculation];

    baseImplementation = systemName <> "Base";

    cleanADMBaseRules = {baseImplementation <> "::curv"   -> "ADMBase::curv",
                         baseImplementation <> "::metric" -> "ADMBase::metric",
                         baseImplementation <> "::lapse"  -> "ADMBase::lapse",
                         baseImplementation <> "::shift"  -> "ADMBase::shift"};

    If[debug, Print["cleanADMBaseRules == ", cleanADMBaseRules]];

    namedTranslatorInCalculation = 
      augmentCalculation[translatorInCalculation,
                         thornName <> "_ADMToEvolve", 
                         baseImplementation, 
                         groups, allParameters, pddefs] /. cleanADMBaseRules;

    namedTranslatorOutCalculation = 
      augmentCalculation[translatorOutCalculation,
                         thornName <> "_EvolveToADM", 
                         baseImplementation, 
                         groups, allParameters, pddefs] /. cleanADMBaseRules;


(* the list of thorns = return argument! *)

ThornList = {{ThornName -> "ADMBase", ThornArrangement -> "CactusEinstein", 
              ThornGroups -> {"metric", "curv", "lapse", "shift"},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "GenericFD", ThornArrangement -> "KrancNumericalTools",
              ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"},
(* *)
              {ThornName -> "CartGrid3D", ThornArrangement -> "CactusBase", ThornGroups -> {},
              ThornParameters -> {}, ThornType -> "External"}
(* *)         
};

(* Which groups do we need to allocate storage for?  Any that are
   being set by the calculation. *)

setgroups = Join[calculationSetGroups[namedTranslatorInCalculation], 
                 calculationSetGroups[namedTranslatorOutCalculation]];
	


setgroups = setgroups /. cleanADMBaseRules;

If[debug, Print["setgroups == ", setgroups]];


(* this list collects all parameters and default values *)
TranslatorParameters = {};

setterFileName    = thornName <> "_Setter";
precompheaderName = "precomputations.h";



(* RHS CALCULATION and PRECOMP MACROS *)
Print["Creating setter source"];
setrhs        = CreateSetterSource[{namedTranslatorInCalculation, namedTranslatorOutCalculation}, debug];
precompheader = CreatePrecompMacros[{namedTranslatorInCalculation, namedTranslatorOutCalculation}];


(* INTERFACE *)

inheritedImplementations = Join[{baseImplementation, "Grid", "GenericFD", "ADMBase"}, 
                                lookupDefault[opts, InheritedImplementations, {}]];

includeFiles             = {"GenericFD.h"};

newgroups = {}; 

interface = CreateInterface[implementation, inheritedImplementations, includeFiles, newgroups];



(* SCHEDULE *)

storageGroups = Map[simpleGroupStruct[#, 1] &, setgroups];

(* this code is currently not needed, but will probably be used in the future
toEvolveSetgroups = Select[setgroups,         Not@StringMatchQ["ADMBase::metric", #]& ];
toEvolveSetgroups = Select[toEvolveSetgroups, Not@StringMatchQ["ADMBase::curv",   #]& ];
*)

If[debug, Print["storageGroups: ", storageGroups]];


(* Conditional -> {Parameter -> "", Value -> ""}}  *)

scheduledStartup = {Name          -> thornName <> "_Startup",
                    SchedulePoint -> "at STARTUP",
                    Language      -> "C",
                    Options       -> "meta",
                    Comment       -> "create banner"};

textLookup[s_, t_] :=  FlattenBlock@SpaceSeparated@lookup[s, t];


before = If[mapContains[translatorInCalculation, Before],
         " BEFORE (" <> textLookup[translatorInCalculation, Before] <> ") ",
            ""];

after  = If[mapContains[translatorInCalculation, After],
            " AFTER (" <> textLookup[translatorInCalculation, After] <> ") ",
            "" ];


scheduledADMToEvolve  = {Name          -> lookup[namedTranslatorInCalculation, Name],
                         SchedulePoint -> "at POSTINITIAL as ADMToEvolve"  <> before <> after,
                         Comment       -> "ADMBase -> Evolution vars translation",
                         StorageGroups -> storageGroups,
                         Language      -> CodeGen`SOURCELANGUAGE,
                         SynchronizedGroups -> GrepSyncGroups[setrhs, 
                                                          lookup[namedTranslatorInCalculation, Name]] };

before = If[mapContains[translatorOutCalculation, Before],
            " BEFORE (" <> textLookup[translatorOutCalculation, Before] <> ") ",
            "" ];


after  = If[mapContains[translatorOutCalculation, After],
            " AFTER (" <> textLookup[translatorOutCalculation, After] <> ") ",
            "" ];


scheduledEvolveToADM  = {Name               -> lookup[namedTranslatorOutCalculation, Name],
                         SchedulePoint      -> "at POSTSTEP as EvolveToADM"  <> before <> after,
                         StorageGroups      -> storageGroups,
                         SynchronizedGroups ->  GrepSyncGroups[setrhs, 
                                                          lookup[namedTranslatorInCalculation, Name]]
                         (* {"ADMBase::metric", "ADMbase::curv"} *),
                         Conditional        -> {Textual -> "translateToADM"},
                         Language           -> CodeGen`SOURCELANGUAGE,
                         Comment            -> "Evolution vars -> ADMBase translation"};


scheduledFunctions = {scheduledStartup, scheduledADMToEvolve, scheduledEvolveToADM};

schedule = CreateSchedule[{}, {}, scheduledFunctions];


(* PARAM *)

newparams = {};

AppendTo[newparams, {Name -> "translateToADM", Type -> "BOOLEAN", Default -> "\"true\"",
         Description -> "whether to translate back to ADM variables", Visibility -> "private"}];

genericFDImplementation =
{Name -> "GenericFD",
 UsedParameters -> {{Name -> "stencil_width",    Type -> "CCTK_INT"},
                    {Name -> "stencil_width_x",  Type -> "CCTK_INT"},
                    {Name -> "stencil_width_y",  Type -> "CCTK_INT"},
                    {Name -> "stencil_width_z",  Type -> "CCTK_INT"}}};

baseImp = {Name -> baseImplementation,
           UsedParameters -> Flatten[{Map[implementationRealParamStruct, realBaseParameters],
                                      Map[implementationIntParamStruct,  intBaseParameters]   }, 1]};

If[baseParamsTrueQ,
   implementations = {genericFDImplementation, baseImp},
   implementations = {genericFDImplementation}
];


(* Should we have the boundaryParam stuff above *)
paramspec = {Implementations -> implementations,
             NewParameters   -> newparams};
param = CreateParam[paramspec];


(* STARTUP *)
startup = CreateStartupFile[thornName, thornName <> ": translate to/from ADMBase"];

(* MAKEFILE *)
ext = CodeGen`SOURCESUFFIX;
make = CreateMakefile[{StartupName <> ".c", setterFileName <> ext}];


(* Write the differencing header file *)
diffHeader = CreateDifferencingHeader[pddefs, lookupDefault[opts, ZeroDimensions, {}]];

(* CREATE THORN *)

thornspec = {Name      -> thornName, 
             Directory -> arrangementDirectory,
	     Interface -> interface, 
             Schedule  -> schedule, 
             Param     -> param, 
             Makefile  -> make, 
             Sources   -> {
                  {Filename -> StartupName       <> ".c", Contents -> startup},
                  {Filename -> setterFileName    <> ext,  Contents -> setrhs}, 
                  {Filename -> precompheaderName,         Contents -> precompheader},
                         {Filename -> "Differencing.h",    Contents -> diffHeader}
                          }
};

CreateThorn[thornspec];


Append[ThornList,
       {ThornName          -> ToString@thornName,
	ThornImplementation -> implementation, 
        ThornArrangement   -> ToString@systemName,
        ThornGroups        -> {}, 
        ThornParameters    -> SetParameters,
        ThornType          -> "Translate"}]
];


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



(****************************************************************************)
(*         thornlist                                                        *)
(****************************************************************************)

Options[CreateThornList] = {SystemName            -> SystemNameDefault,
                            SystemParentDirectory -> ".",
                            DeBug                 -> False
                           };



CreateThornList[Thorns_, optArgs___] :=
  Module[{file, dirname, opts, thornlist, thornspaths, debug,
          parentDirectory, systemName, arrangementDirectory},

opts = GetOptions[CreateThornList, {optArgs}];

parentDirectory     = ToString@lookup[opts, SystemParentDirectory];
systemName          = ToString@lookup[opts, SystemName];
debug               = lookup[opts, DeBug];

arrangementDirectory = parentDirectory <> "/" <> systemName;

EnsureDirectory[parentDirectory];
EnsureDirectory[arrangementDirectory];

Print["Creating files in directory " <> arrangementDirectory];


If[debug,
  Print["Debugging switched on"],
  Print["Debugging switched off"]
 ];


(* write file *)

file = arrangementDirectory <> "/" <> systemName <> ".th";

SafeDelete@file;

filepointer = OpenAppend[file, PageWidth -> 74];

pr[x_] := WriteString[filepointer, x];

pr["# Thorn list file for " <> systemName <> "\n"];
pr["# $Id" <> "$\n\n"];

take2[x_] := {ToString[ThornArrangement /. x], ToString[ThornName /. x]};

thornspaths = Map[take2, Thorns];

concatPathName[x_] := First@x <> "/" <> Last@x <> "\n";

thornspaths = Union@Map[concatPathName, thornspaths];


(* file init stuff goes here *)

Map[Print, thornspaths];
Map[pr,    thornspaths];
];




End[];
EndPackage[];





