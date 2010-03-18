
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

ThornOptions =
 {Calculations -> {},
  DeclaredGroups -> {},
  Implementation -> None,
  InheritedImplementations -> {},
  EvolutionTimelevels -> 3,
  DefaultEvolutionTimelevels -> None,
  RealParameters -> {},
  IntParameters -> {},
  KeywordParameters -> {},
  InheritedRealParameters -> {},
  InheritedIntParameters -> {},
  InheritedKeywordParameters -> {},
  ExtendedRealParameters -> {},
  ExtendedIntParameters -> {},
  ExtendedKeywordParameters -> {},
  PartialDerivatives -> {},
  ReflectionSymmetries -> {},
  ZeroDimensions -> {},
  UseLoopControl -> False,
  UseCSE -> False,
  ProhibitAssignmentToGridFunctionsRead -> False,
  IncludeFiles -> {}};

{ConditionalOnKeyword, ConditionalOnKeywords, CollectList, Interior,
InteriorNoSync, Boundary, BoundaryWithGhosts, Where, PreDefinitions,
AllowedSymbols, Parameters};

EndPackage[];

BeginPackage["KrancThorn`", {"CodeGen`", "sym`", "Thorn`",
 "MapLookup`", "KrancGroups`", "Differencing`",
 "CalculationFunction`", "Errors`", "Helpers`", "CactusBoundary`",
 "TensorTools`"}];

CreateKrancThorn::usage = "Construct a Kranc thorn";
CreateKrancThornTT::usage = "Construct a Kranc thorn using TensorTools";
CreateGroupFromTensor::usage = "";

Begin["`Private`"];

VerifyGroups[gs_] := 
  If[!ListQ[gs],
   ThrowError["Not a list of group definitions: ", gs],
   Map[VerifyGroup, gs]];

VerifyGroupNames[gns_] := 
  If[!ListQ[gns],
   ThrowError["Not a list of group names: ", gns],
   Map[VerifyGroupName, gns]];

VerifyNewCalculation[calc_] :=
  Module[{calcName},
    If[Head[calc] != List,
      ThrowError["Invalid Calculation structure: " <> ToString[calc]]];

    calcName = lookupDefault[calc, Name, "<unknown>"];

    VerifyListContent[calc, Rule, " while checking the calculation called " <> ToString[calcName]];

    If[mapContains[calc, Shorthands],
      VerifyListContent[lookup[calc, Shorthands], Symbol," while checking the calculation called " <> ToString[calcName] ]];

    If[mapContains[calc, Equations],
      VerifyListContent[lookup[calc, Equations], Rule," while checking the calculation called " <> ToString[calcName]],
      ThrowError["Invalid Calculation structure. Must contain Equations element: " <> ToString[calc]]]];

cktCheckNamedArgs[l_] := 
Module[{used, unrecognized},
    used = Map[First, l];
    unrecognized = Complement[used, Map[First, ThornOptions]];
    If[Length[unrecognized] > 0,
      ThrowError["Unrecognized named arguments: ", unrecognized]]];

replaceDots[x_] := 
  x /. (dot[y_] :> Symbol[ToString[y] <> "rhs"]);

Options[CreateKrancThorn] = ThornOptions;

CreateKrancThorn[groupsOrig_, parentDirectory_, thornName_, opts:OptionsPattern[]] :=
  Module[{calcs, declaredGroups, implementation,
    inheritedImplementations, includeFiles,
    evolutionTimelevels, defaultEvolutionTimelevels,
    realParams, intParams, keywordParams,
    inheritedRealParams, inheritedIntParams, inheritedKeywordParams,
    extendedRealParams, extendedIntParams, extendedKeywordParams,
    configuration,
    partialDerivs, coordGroup, evolvedGroups, rhsGroups, nonevolvedGroups,
    interface, evolvedGroupDefinitions, rhsGroupDefinitions, thornspec,
    allParams, boundarySources, reflectionSymmetries,
    realParamDefs, intParamDefs,
    pDefs, useCSE},

    (* Parse named arguments *)

    InfoMessage[Terse, "Processing arguments to CreateKrancThorn"];
    cktCheckNamedArgs[{opts}];

    calcs = OptionValue[Calculations];
    declaredGroups = OptionValue[DeclaredGroups];
    implementation = 
      If[OptionValue[Implementation] =!= None, 
        OptionValue[Implementation],
        thornName];
    inheritedImplementations = OptionValue[InheritedImplementations];
    includeFiles = OptionValue[IncludeFiles];
    evolutionTimelevels = OptionValue[EvolutionTimelevels]; (* Redundant *)
    defaultEvolutionTimelevels = lookupDefault[{opts}, DefaultEvolutionTimelevels, evolutionTimelevels];
    realParams = OptionValue[RealParameters];
    intParams = OptionValue[IntParameters];
    realParamDefs = makeFullParamDefs[realParams];
    intParamDefs = makeFullParamDefs[intParams];
    keywordParams = OptionValue[KeywordParameters];
    inheritedRealParams = OptionValue[InheritedRealParameters];
    inheritedIntParams = OptionValue[InheritedIntParameters];
    inheritedKeywordParams = OptionValue[InheritedKeywordParameters];
    extendedRealParams = OptionValue[ExtendedRealParameters];
    extendedIntParams = OptionValue[ExtendedIntParameters];
    extendedKeywordParams = OptionValue[ExtendedKeywordParameters];
    partialDerivs = OptionValue[PartialDerivatives];
    reflectionSymmetries = OptionValue[ReflectionSymmetries];
    useCSE = OptionValue[UseCSE];

    coordGroup = {"grid::coordinates", {sym`x,sym`y,sym`z,sym`r}};
    groups = Join[groupsOrig, {coordGroup}];
    includeFiles = Join[includeFiles, {"GenericFD.h", "Symmetry.h", "sbp_calc_coeffs.h"}];

    inheritedImplementations = Join[inheritedImplementations, {"Grid",
     "GenericFD"}, CactusBoundary`GetInheritedImplementations[]];

    InfoMessage[Terse, "Verifying arguments"];

    (* Check parameters *)
    VerifyGroups[groups];
    VerifyString[parentDirectory];
    VerifyString[thornName];
    VerifyString[implementation];
    VerifyGroupNames[declaredGroups];

    InfoMessage[Terse, "Creating startup file"];
    startup = CreateStartupFile[thornName, thornName];

    (* Get the different types of group *)
    evolvedGroups = extractEvolvedGroups[declaredGroups, calcs, groups];
    nonevolvedGroups = extractNonevolvedGroups[declaredGroups, calcs, groups];

    (* Replace the dots in the calculation *)
    calcs = replaceDots[calcs];

    (* Add the RHS groups *)
    evolvedGroupDefinitions = Map[groupFromName[#, groups] &, evolvedGroups];
    rhsGroupDefinitions = Map[evolvedGroupToRHSGroup[#, evolvedGroupDefinitions] &, evolvedGroups];
    groups = Join[groups, rhsGroupDefinitions];

    (* Add the groups into the calcs *)
    calcs = Map[Join[#, {Groups -> groups}] &, calcs];

    rhsGroups = Map[groupName, rhsGroupDefinitions];

    (* Construct the configuration file *)
    InfoMessage[Terse, "Creating configuration file"];
    configuration = createKrancConfiguration[opts];

    (* Construct the interface file *)
    InfoMessage[Terse, "Creating interface file"];
    interface = createKrancInterface[nonevolvedGroups,
      evolvedGroups, rhsGroups, groups,
      implementation, inheritedImplementations, includeFiles, opts];

    (* Construct the param file *)
    InfoMessage[Terse, "Creating param file"];
    param = createKrancParam[evolvedGroups, nonevolvedGroups, groups, thornName,
      realParamDefs, intParamDefs, keywordParams,
      inheritedRealParams, inheritedIntParams, inheritedKeywordParams,
      extendedRealParams, extendedIntParams, extendedKeywordParams,
      evolutionTimelevels, defaultEvolutionTimelevels,
      calcs];

    (* Construct the schedule file *)
    InfoMessage[Terse, "Creating schedule file"];
    schedule = createKrancScheduleFile[calcs, groups, evolvedGroups,
      rhsGroups, nonevolvedGroups, thornName,
      evolutionTimelevels];

    boundarySources = CactusBoundary`GetSources[evolvedGroups, groups, 
                                            implementation, thornName];

    (* Create the MoL registration file (we do this for every thorn,
       even if it does not evolve any variables). This could be fixed
       later. *)
    InfoMessage[Terse, "Creating MoL registration file"];
    molregister = createKrancMoLRegister[evolvedGroups, nonevolvedGroups, groups, implementation, thornName];

    Module[{allGFs = Join[variablesFromGroups[evolvedGroups, groups],
                          variablesFromGroups[nonevolvedGroups, groups]]},
      InfoMessage[Terse, "Creating symmetry registration file"];
      symregister = CreateSymmetriesRegistrationSource[thornName, implementation, 
        allGFs, reflectionSymmetries, False]];

    (* Write the differencing header file *)
    InfoMessage[Terse, "Creating differencing header file"];
    {pDefs, diffHeader} = CreateDifferencingHeader[partialDerivs, OptionValue[ZeroDimensions]];

    (* Add the predefinitions into the calcs *)
    calcs = Map[Join[#, {PreDefinitions -> pDefs}] &, calcs];

    ext = CodeGen`SOURCESUFFIX;

    (* Construct a source file for each calculation *)
    allParams = Join[Map[paramName, realParamDefs],
                     Map[paramName, intParamDefs],
                     Map[unqualifiedName, inheritedRealParams], 
                     Map[unqualifiedName, inheritedIntParams], 
                     Map[unqualifiedName, inheritedKeywordParams]];

    InfoMessage[Terse, "Creating calculation source files"];
    calcSources = Map[CreateSetterSourceWrapper[#, allParams, partialDerivs, useCSE, opts] &, calcs];
    calcFilenames = Map[lookup[#, Name] <> ext &, calcs];

    (* Makefile *)
    InfoMessage[Terse, "Creating make file"];
    make = CreateMakefile[Join[{"Startup.c", "RegisterMoL.c", "RegisterSymmetries.c"}, calcFilenames, 
      Map[lookup[#, Filename] &, boundarySources]]];

    (* Put all the above together and generate the Cactus thorn *)
    thornspec = {Name          -> thornName, 
                 Directory     -> parentDirectory,
                 Configuration -> configuration,
	         Interface     -> interface, 
                 Schedule      -> schedule, 
                 Param         -> param,
                 Makefile      -> make,
                 Sources       -> Join[{
                  {Filename -> "Startup.c", Contents -> startup}, 
                  {Filename -> "RegisterMoL.c", Contents -> molregister},
                  {Filename -> "RegisterSymmetries.c", Contents -> symregister},
                  {Filename -> "Differencing.h", Contents -> diffHeader}},
                  MapThread[{Filename -> #1, Contents -> #2} &, 
                            {calcFilenames, calcSources}], boundarySources]};
    InfoMessage[Terse, "Creating thorn"];
    CreateThorn[thornspec]];

CalculationEvolvedVars[calc_] :=
  Module[{eqs, evolved, lhss},
    VerifyNewCalculation[calc];
    eqs = lookup[calc, Equations];
    lhss = Map[First, eqs];
    evolved = Cases[lhss, dot[v_] -> v];
    Return[evolved]];

extractEvolvedGroups[declaredGroups_, calcs_, groups_] :=
  Module[{evolvedVars, evolvedGroups},
    VerifyGroupNames[declaredGroups];
    VerifyGroups[groups];
    VerifyList[calcs];
    Map[VerifyNewCalculation, calcs];
    evolvedVars = Apply[Join, Map[CalculationEvolvedVars, calcs]];
    evolvedGroups = containingGroups[evolvedVars, groups];
    Return[evolvedGroups]];

extractNonevolvedGroups[declaredGroups_, calcs_, groups_] :=
  Module[{allVars, evolvedVars, evolvedGroups, nonevolvedGroups},
    VerifyGroupNames[declaredGroups];
    VerifyGroups[groups];
    VerifyList[calcs];
    Map[VerifyNewCalculation, calcs];

    allVars = variablesFromGroups[declaredGroups, groups];
    evolvedVars = Apply[Join, Map[CalculationEvolvedVars, calcs]];
    evolvedGroups = containingGroups[evolvedVars, groups];
    nonevolvedGroups = Complement[declaredGroups, evolvedGroups];

    Return[nonevolvedGroups]];

nonevolvedGroupInterfaceStructure[group_] := 
{
  Name -> groupName[group], 
  VariableType -> "CCTK_REAL",
  Timelevels -> nonevolvedTimelevels[group],
  GridType -> "GF",
  Comment -> groupName[group], 
  Visibility -> "public",
  Tags -> Join[GroupTags[group]],
  Variables -> groupVariables[group]
}

evolvedGroupInterfaceStructure[group_, timelevels_] := 
{
  Name -> groupName[group], 
  VariableType -> "CCTK_REAL",
  Timelevels -> timelevels, 
  GridType -> "GF",
  Comment -> groupName[group], 
  Visibility -> "public",
  Tags -> GroupTags[group],
  Variables -> groupVariables[group]
}

rhsGroupInterfaceStructure[group_, timelevels_] := 
{
  Name -> groupName[group], 
  VariableType -> "CCTK_REAL",
  Timelevels -> timelevels, 
  GridType -> "GF",
  Comment -> groupName[group], 
  Visibility -> "public",
  Tags -> GroupTags[group],
  Variables -> groupVariables[group]
}

nonevolvedTimelevels[group_] :=
  Module[{tls = GroupTimelevels[group]},
    If[ tls === False, 1, tls]];

createKrancConfiguration[opts:OptionsPattern[]] :=
  Module[{configuration},
    configuration = CreateConfiguration[opts];
    Return[configuration]];

Options[createKrancInterface] = ThornOptions;

createKrancInterface[nonevolvedGroups_, evolvedGroups_, rhsGroups_, groups_,
  implementation_, inheritedImplementations_,
  includeFiles_, opts:OptionsPattern[]] :=

  Module[{registerEvolved, (*registerConstrained,*)
    nonevolvedGroupStructures, evolvedGroupStructures, rhsGroupStructures,
    groupStructures, interface},
    VerifyGroupNames[nonevolvedGroups];
    VerifyGroupNames[evolvedGroups];
    VerifyGroupNames[rhsGroups];
    VerifyGroups[groups];
    VerifyString[implementation];
    VerifyStringList[inheritedImplementations];
    VerifyStringList[includeFiles];
    (* These are the aliased functions that are USED by this thorn from other thorns *)
    registerEvolved = 
    {
      Name      -> "MoLRegisterEvolved",
      Type      -> "CCTK_INT",
      ArgString -> "CCTK_INT IN EvolvedIndex, CCTK_INT IN RHSIndex"
    };

    (*
    registerConstrained = 
    {
      Name      -> "MoLRegisterConstrained",
      Type      -> "CCTK_INT",
      ArgString -> "CCTK_INT IN ConstrainedIndex"
    };
    *)

    diffCoeff = 
    {
      Name -> "Diff_coeff",
      Type -> "SUBROUTINE",
      ArgString -> "CCTK_POINTER_TO_CONST IN cctkGH, CCTK_INT IN dir, CCTK_INT IN nsize, CCTK_INT OUT ARRAY imin, CCTK_INT OUT ARRAY imax, CCTK_REAL OUT ARRAY q, CCTK_INT IN table_handle"
    };


    (* For each group declared in this thorn, we need an entry in the
        interface file.  Each evolved group needs an associated rhs
        group, but these are constructed at a higher level and are
        listed in the nonevolved groups. *)
    nonevolvedGroupStructures = 
      Map[nonevolvedGroupInterfaceStructure[groupFromName[#, groups]] &, 
          nonevolvedGroups];

    evolvedGroupStructures =
      Map[evolvedGroupInterfaceStructure[groupFromName[#, groups],
          OptionValue[EvolutionTimelevels]] &, evolvedGroups];

    rhsGroupStructures =
      Map[rhsGroupInterfaceStructure[groupFromName[#, groups],
          OptionValue[EvolutionTimelevels]] &, rhsGroups];

    groupStructures = Join[nonevolvedGroupStructures,
                           evolvedGroupStructures, rhsGroupStructures];

    interface = CreateInterface[implementation, inheritedImplementations, 
      Join[includeFiles, {CactusBoundary`GetIncludeFiles[]},
           If[OptionValue[UseLoopControl], {"loopcontrol.h"}, {}]],
      groupStructures,
      UsesFunctions ->
        Join[{registerEvolved, (*registerConstrained,*) diffCoeff}, 
             CactusBoundary`GetUsedFunctions[]]];
    Return[interface]];

VerifyQualifiedName[name_] :=
  If[! StringQ[name] || ! StringMatchQ[name, "*::*"],
    ThrowError["Not a name with an implementation:", name]];

implementationFromQualifiedName[name_] :=
  Module[{colon},
    VerifyQualifiedName[name];
    colon = First[First[StringPosition[name, ":", 1]]];
    StringDrop[name, colon - 1 - StringLength[name]]];

unqualifiedName[name_] :=
  Module[{colon},
    VerifyQualifiedName[name];
    colon = First[First[StringPosition[name, ":", 1]]];
    Return[StringDrop[name, colon + 1]]];

krancParamStruct[definition_, type_, inherited_] :=
  Module[{description, name},
    name = lookup[definition, Name];
    description = lookupDefault[definition, Description, name];
    Join[
    {Name        -> name,
     Type        -> type, 
     Description -> description,
     Default     -> lookup[definition, Default],
     Visibility  -> "restricted"},
    If[inherited,
      {},
      {AllowedValues -> {{Value -> "*:*", Description -> ""}}}]]];

krancParamStructExtended[definition_, type_] :=
  Module[{allowedValues, description, name},
    name = unqualifiedName[lookup[definition, Name]];
    description = lookupDefault[definition, Description, name];
    allowedValues = lookup[definition, AllowedValues];
    {Name        -> name,
     Type        -> type, 
     Description -> description,
     Default     -> "",
     Visibility  -> "restricted",
     AllowedValues -> Map[{Value -> #, Description -> ""} &, allowedValues]}];

krancKeywordParamStruct[struct_] :=
{
  Name -> lookup[struct, Name],
  Type -> "KEYWORD",
  Default -> lookup[struct, Default],
  Description -> lookupDefault[struct, Description, lookup[struct, Name]],
  Visibility -> lookupDefault[struct, Visibility, "private"],
  AllowedValues -> Map[{Value -> #, Description -> #} &, lookup[struct, AllowedValues]]
};

makeFullParamDefs[params_] :=
  Module[{p},
    p = Map[If[!ListQ[#], {Name -> #, Default -> 0}, #] &, params];
    p];

paramName[paramDef_] :=
  lookup[paramDef, Name];

inheritParameters[imp_, reals_, ints_, keywords_] :=
  Module[{theseReals, theseInts, theseKeywords, theseRealsNoImp, theseIntsNoImp, theseKeywordsNoImp, realStructs, intStructs, keywordStructs},
    theseReals = Select[reals, implementationFromQualifiedName[#] == imp &];
    theseInts = Select[ints, implementationFromQualifiedName[#] == imp &];
    theseKeywords = Select[keywords, implementationFromQualifiedName[#] == imp &];
    theseRealsNoImp = makeFullParamDefs[Map[unqualifiedName, theseReals]];
    theseIntsNoImp = makeFullParamDefs[Map[unqualifiedName, theseInts]];
    theseKeywordsNoImp = makeFullParamDefs[Map[unqualifiedName, theseKeywords]];
    realStructs = Map[krancParamStruct[#, "CCTK_REAL", True] &, theseRealsNoImp];
    intStructs = Map[krancParamStruct[#, "CCTK_INT", True] &, theseIntsNoImp];
    keywordStructs = Map[krancParamStruct[#, "CCTK_KEYWORD", True] &, theseKeywordsNoImp];
    If[(Length[theseReals] + Length[theseInts] + Length[theseKeywords]) > 0,
      Return[{Name -> imp, UsedParameters -> Join[realStructs, intStructs, keywordStructs]}], 
      Return[{}]]];

extendParameters[imp_, reals_, ints_, keywords_] :=
  Module[{theseReals, theseInts, theseKeywords, realStructs, intStructs, keywordStructs},
    theseReals = Select[reals, implementationFromQualifiedName[lookup[#, Name]] == imp &];
    theseInts = Select[ints, implementationFromQualifiedName[lookup[#, Name]] == imp &];
    theseKeywords = Select[keywords, implementationFromQualifiedName[lookup[#, Name]] == imp &];
    realStructs = Map[krancParamStructExtended[#, "CCTK_REAL"] &, theseReals];
    intStructs = Map[krancParamStructExtended[#, "CCTK_INT"] &, theseInts];
    keywordStructs = Map[krancParamStructExtended[#, "CCTK_KEYWORD"] &, theseKeywords];
    If[(Length[theseReals] + Length[theseInts] + Length[theseKeywords]) > 0,
      Return[{Name -> imp, ExtendedParameters -> Join[realStructs, intStructs, keywordStructs]}],
      Return[{}]]];

createKrancParam[evolvedGroups_, nonevolvedGroups_, groups_, thornName_, 
  reals_, ints_, keywords_,
  inheritedReals_, inheritedInts_, inheritedKeywords_,
  extendedReals_, extendedInts_, extendedKeywords_,
  evolutionTimelevels_, defaultEvolutionTimelevels_,
  calcs_] :=
  Module[{nEvolved, evolvedMoLParam, evolvedGFs,
    (*constrainedMoLParam,*) genericfdStruct, realStructs, intStructs,
    allInherited, allExtended, implementationNames, molImplementation,
    userImplementations, implementations, params, paramspec, param,
    verboseStruct, calcOffsetStructs, calcEveryStructs},

    (* reals and ints are symbols containing parameter names.  The
       inherited ones have implementation names as well *)

    evolvedGFs = variablesFromGroups[evolvedGroups, groups];

    nEvolved   = Length[variablesFromGroups[evolvedGroups, groups]];
(*    nPrimitive = Length[variablesFromGroups[nonevolvedGroups, groups]];*)
(*    nPrimitive = Length[getConstrainedVariables[evolvedGroups, groups]];*)

    evolvedMoLParam =
    {
      Name -> thornName <> "_MaxNumEvolvedVars",
      Type -> "CCTK_INT",
      Default -> nEvolved,
      Description -> "Number of evolved variables used by this thorn",
      Visibility -> "restricted",
      AccumulatorBase -> "MethodofLines::MoL_Num_Evolved_Vars",
      AllowedValues -> {{Value -> ToString[nEvolved] <> ":" <> ToString[nEvolved] , 
                         Description -> "Number of evolved variables used by this thorn"}}
    };

    (*
    constrainedMoLParam =
    {
      Name -> thornName <> "_MaxNumConstrainedVars",
      Type -> "CCTK_INT",
      Default -> nPrimitive,
      Description -> "Number of constrained variables used by this thorn",
      Visibility -> "restricted",
      AccumulatorBase -> "MethodofLines::MoL_Num_Constrained_Vars",
      AllowedValues -> {{Value -> ToString[nPrimitive] <> ":" <> ToString[nPrimitive] , 
                         Description -> "Number of constrained variables used by this thorn"}}
    };
    *)

    timelevelsParam =
    {
      Name -> "timelevels",
      Type -> "CCTK_INT",
      Default -> defaultEvolutionTimelevels,
      Description -> "Number of active timelevels",
      Visibility -> "restricted",
      AllowedValues -> {{Value -> ToString[0] <> ":" <> ToString[evolutionTimelevels],
                         Description -> ""}}
    };

    rhsTimelevelsParam =
    {
      Name -> "rhs_timelevels",
      Type -> "CCTK_INT",
      Default -> 1,
      Description -> "Number of active RHS timelevels",
      Visibility -> "restricted",
      AllowedValues -> {{Value -> ToString[0] <> ":" <> ToString[evolutionTimelevels],
                         Description -> ""}}
    };

    genericfdStruct =
    {
      Name -> "GenericFD",
      UsedParameters -> 
        {{Name -> "stencil_width",    Type -> "CCTK_INT"},
         {Name -> "stencil_width_x",  Type -> "CCTK_INT"},
         {Name -> "stencil_width_y",  Type -> "CCTK_INT"},
         {Name -> "stencil_width_z",  Type -> "CCTK_INT"},
         {Name -> "boundary_width",   Type -> "CCTK_INT"}}
    };

    realStructs = Map[krancParamStruct[#, "CCTK_REAL", False] &, reals];
    verboseStruct = krancParamStruct[{Name -> "verbose", Default -> 0}, "CCTK_INT", False];
    intStructs = Map[krancParamStruct[#, "CCTK_INT", False] &, ints];
    calcEveryStructs = Map[krancParamStruct[{Name -> lookup[#, Name] <> "_calc_every", Default -> 1}, "CCTK_INT", False] &, calcs];
    calcOffsetStructs = Map[krancParamStruct[{Name -> lookup[#, Name] <> "_calc_offset", Default -> 0}, "CCTK_INT", False] &, calcs];
    keywordStructs = Map[krancKeywordParamStruct, keywords];

    allInherited = Join[inheritedReals, inheritedInts, inheritedKeywords];
    allExtended = Join[extendedReals, extendedInts, extendedKeywords];

    implementationNames = Union[Map[implementationFromQualifiedName, allInherited],
                                Map[implementationFromQualifiedName[lookup[#, Name]] &, allExtended]];

    molImplementation =
    {
      Name -> "MethodOfLines",
      UsedParameters -> 
      {
        {Name -> "MoL_Num_Evolved_Vars",     Type -> "CCTK_INT"}
        (* {Name -> "MoL_Num_Constrained_Vars", Type -> "CCTK_INT"} *)
      }
    };

    userImplementations = Map[inheritParameters[#, inheritedReals,inheritedInts,inheritedKeywords] &, 
                              implementationNames];
    userImplementations2 = Map[extendParameters[#, extendedReals,extendedInts,extendedKeywords] &, 
                               implementationNames];

    userImplementations = If[userImplementations=={{}},{},userImplementations];
    userImplementations2 = If[userImplementations2=={{}},{},userImplementations2];

    implementations = Join[userImplementations, userImplementations2, {genericfdStruct, molImplementation}];
    params = Join[{verboseStruct}, realStructs, intStructs, keywordStructs, {evolvedMoLParam, (*constrainedMoLParam,*) timelevelsParam, rhsTimelevelsParam},
                  calcEveryStructs, calcOffsetStructs,
      CactusBoundary`GetParameters[evolvedGFs, evolvedGroups]];

    paramspec = {Implementations -> implementations,
                 NewParameters   -> params};

    param = CreateParam[paramspec];
    Return[param]
  ];

simpleGroupStruct[groupName_, timelevels_] := 
{
  Group -> groupName, 
  Timelevels -> timelevels
};

evolvedGroupStruct[groupName_, timelevels_, maxtimelevels_] := 
{
  Group -> groupName, 
  Timelevels -> timelevels,
  MaxTimelevels -> "timelevels"
};

rhsGroupStruct[groupName_, timelevels_, maxtimelevels_] := 
{
  Group -> groupName, 
  Timelevels -> timelevels,
  MaxTimelevels -> "rhs_timelevels"
};

groupsSetInCalc[calc_, groups_] :=
  Module[{gfs, eqs, lhss, gfsInLHS, lhsGroupNames},
    gfs = allVariables[groups];
    eqs = lookup[calc, Equations];
    lhss = Map[First, eqs];
    gfsInLHS = Union[Cases[lhss, _ ? (MemberQ[gfs,#] &), Infinity]];

    lhsGroupNames = containingGroups[gfsInLHS, groups];
    Return[lhsGroupNames]
  ];

(* Each calculation can be scheduled at multiple points, so this
   function returns a LIST of schedule structures for each calculation
   *)
scheduleCalc[calc_, groups_] :=
  Module[{points, conditional, conditionals, keywordConditional, keywordConditionals, triggered, keyword, value, keywordvaluepairs, groupsToSync},
    conditional = mapContains[calc, ConditionalOnKeyword];
    conditionals = mapContains[calc, ConditionalOnKeywords];
    triggered = mapContains[calc, TriggerGroups];
    If[conditional,
      keywordConditional = lookup[calc, ConditionalOnKeyword];
      If[! MatchQ[keywordConditional, {lhs_String, rhs_String}],
        ThrowError["ConditionalOnKeyword entry in calculation expected to be of the form {parameter, value}, but was ", keywordConditional, "Calculation is ", calc]];

      keyword = keywordConditional[[1]];
      value = keywordConditional[[2]];
      ];
    If[conditionals,
      keywordConditionals = lookup[calc, ConditionalOnKeywords];
      If[! MatchQ[keywordConditionals, {{_, _} ...}],
        ThrowError["ConditionalOnKeywords entry in calculation expected to be of the form {{parameter, value}}, but was ", keywordConditionals, "Calculation is ", calc]];

      keywordvaluepairs =
        Map[# /. {keyword_, value_} -> {Parameter -> keyword, Value -> value} &,
            keywordConditionals];
      ];

    groupsToSync = If[lookupDefault[calc, Where, Everywhere] === Interior || 
                      lookupDefault[calc, Where, Everywhere] === Boundary,
                      groupsSetInCalc[calc, groups],
                      {}];

    Map[
      Join[
      {
        Name               -> lookup[calc, Name],
        SchedulePoint      -> #,
        SynchronizedGroups -> If[StringMatchQ[#, "*MoL_CalcRHS*", IgnoreCase -> True] || StringMatchQ[#, "*MoL_RHSBoundaries*", IgnoreCase -> True],
                                 {},
                                 groupsToSync],
        Language           -> CodeGen`SOURCELANGUAGE, 
        Comment            -> lookup[calc, Name]
      },
       If[triggered, {TriggerGroups -> lookup[calc, TriggerGroups]},
          {}],
       If[conditional, {Conditional -> {Parameter -> keyword, Value -> value}},
          {}],
       If[conditionals, {Conditionals -> keywordvaluepairs},
          {}]
      ] &,
      lookup[calc, Schedule]]];

createKrancScheduleFile[calcs_, groups_, evolvedGroups_, rhsGroups_, nonevolvedGroups_, thornName_, 
                        evolutionTimelevels_] :=
  Module[{scheduledCalcs, scheduledStartup, scheduleMoLRegister, globalStorageGroups, scheduledFunctions, schedule},

    scheduledCalcs = Flatten[Map[scheduleCalc[#, groups] &, calcs], 1];

    scheduledStartup = 
    {
      Name          -> thornName <> "_Startup",
      SchedulePoint -> "at STARTUP",
      Language      -> "C",
      Options       -> "meta",
      Comment       -> "create banner"
    };

    scheduleMoLRegister =
    {
      Name          -> thornName <> "_RegisterVars",
      SchedulePoint -> "in MoL_Register", 
      Language      -> "C", 
      Options       -> "meta",
      Comment       -> "Register Variables for MoL"
    };
    
    scheduleRegisterSymmetries =
    {
      Name          -> thornName <> "_RegisterSymmetries",
      SchedulePoint -> "in SymmetryRegister", 
      Language      -> "C",
      Options       -> "meta",
      Comment       -> "register symmetries"
    };

    globalStorageGroups = Join[Map[simpleGroupStruct[#, nonevolvedTimelevels[groupFromName[#, groups]]] &, nonevolvedGroups], 
                               Map[evolvedGroupStruct[#, evolutionTimelevels, evolutionTimelevels] &, evolvedGroups], 
                               Map[rhsGroupStruct[#, evolutionTimelevels, evolutionTimelevels] &, rhsGroups]];

    scheduledFunctions = 
      Join[{scheduledStartup, scheduleMoLRegister, scheduleRegisterSymmetries}, 
        scheduledCalcs, CactusBoundary`GetScheduledFunctions[thornName, evolvedGroups]];

    schedule = CreateSchedule[globalStorageGroups, 
      CactusBoundary`GetScheduledGroups[thornName], scheduledFunctions];

    Return[schedule]];

Options[CreateSetterSourceWrapper] = ThornOptions;

CreateSetterSourceWrapper[calc_, parameters_, derivs_, useCSE_, opts:OptionsPattern[]] :=
  Module[{modCalc},
    modCalc = Join[calc,
      {Parameters -> parameters},
      {PartialDerivatives -> derivs}];

    source = CreateSetterSource[{modCalc}, False, useCSE,
      If[OptionValue[UseLoopControl], {"loopcontrol.h"}, {}], opts];
    Return[source]];

(* FIXME: This is still not quite right.  We only want to have those variables that
   we set as constrained, but I don't think this can hurt.*)

getConstrainedVariables[evolvedGroupNames_, groups_] :=
  Module[{evolvedGFs, allVariables, constrainedVariables},
    evolvedGFs = variablesFromGroups[evolvedGroupNames, groups];
    allVariables = Flatten[Map[groupVariables, groups],1];
    constrainedVariables = Complement[allVariables, Join[evolvedGFs, Map[Symbol[addrhs[#]] &, evolvedGFs]]];
    constrainedVariables];

createKrancMoLRegister[evolvedGroupNames_, nonevolvedGroupNames_, groups_, implementation_, thornName_] :=
  Module[{molspec, evolvedGFs, constrainedVariables},
    evolvedGFs = variablesFromGroups[evolvedGroupNames, groups];
    nonevolvedGFs = variablesFromGroups[nonevolvedGroupNames, groups];

    constrainedVariables = getConstrainedVariables[evolvedGroupNames, groups];
    
    molspec =
    {
      EvolvedGFs   -> Map[qualifyGFName[#, groups, implementation]& , evolvedGFs], 
      PrimitiveGFs -> Map[qualifyGFName[#, groups, implementation]& , constrainedVariables],
      BaseImplementation -> implementation, 
      ThornName -> thornName
    };
    molregister = CreateMoLRegistrationSource[molspec, False];
    Return[molregister]];

(* Tensorial wrapper *)

CreateKrancThornTT[groups_, parentDirectory_, thornName_, opts___] :=
  Module[{calcs, expCalcs, expGroups, options, derivs, expDerivs, reflectionSymmetries, declaredGroups},
    InfoMessage[Terse, "Processing tensorial arguments"];
    calcs = lookup[{opts}, Calculations];
    derivs = lookupDefault[{opts}, PartialDerivatives, {}];
    Map[CheckCalculationTensors, calcs];
    expCalcs = Map[makeCalculationExplicit, calcs];

    InfoMessage[Info, "Group definitions:", groups];

    expDerivs = Flatten[Map[MakeExplicit,derivs],1];
    expGroups = Map[makeGroupExplicit, groups];
    options = Join[DeleteCases[{opts}, Calculations -> _], {Calculations -> expCalcs}];
    options = Join[DeleteCases[options, PartialDerivatives -> _], {PartialDerivatives -> expDerivs}];

    declaredGroups = lookupDefault[{opts}, DeclaredGroups, {}];
    evolutionTimelevels = lookupDefault[{opts}, EvolutionTimelevels, 3];
    defaultEvolutionTimelevels = lookupDefault[{opts}, DefaultEvolutionTimelevels, evolutionTimelevels];
    InfoMessage[Info, "Declared groups: " <> ToString[declaredGroups]];
    InfoMessage[Terse, "Computing reflection symmetries"];
    reflectionSymmetries = computeReflectionSymmetries[declaredGroups, groups];
    InfoMessage[Info, "Reflection symmetries: ", reflectionSymmetries];

    InfoMessage[Terse, "Creating (component-based) Kranc thorn"];
    CreateKrancThorn[expGroups, parentDirectory, thornName, Apply[Sequence, options], 
      ReflectionSymmetries -> reflectionSymmetries]];

computeReflectionSymmetries[declaredGroups_, groups_] :=
  Module[{variables, syms},
    variables = variablesFromGroups[declaredGroups, groups];
    syms = Flatten[Map[ReflectionSymmetriesOfTensor, variables], 1];
    syms];

makeCalculationExplicit[calc_] :=
  mapValueMapMultiple[calc, 
    {Shorthands -> MakeExplicit, 
     CollectList -> MakeExplicit, 
     Equations -> MakeExplicit}];

makeGroupExplicit[g_] :=
  Module[{variables, newVariables, newGroup},
    variables = groupVariables[g];
    newVariables = RemoveDuplicates[MakeExplicit[variables]];
    newGroup = SetGroupVariables[g, newVariables];
    newGroup];

tensorTypeString[k_, inds_] :=
  Module[{},
  InfoMessage[InfoFull, "Tensor attributes of " <> ToString[k], TensorAttributes[k]];
  If[HasTensorAttribute[k, TensorManualCartesianParities],
    "ManualCartesian",
    If[Length[inds] == 0, 
      "Scalar",
      Apply[StringJoin, Map[If[IndexIsUpper[#], "U", "D"] &, inds]]]]];

CreateGroupFromTensor[T:Tensor[k_, is__]] :=
  CreateGroupFromTensor[k, {is}];

reflectionParityString[l_] :=
  Module[{chars},
    If[!ListQ[l] || !Length[l] == 3,
      ThrowError["Expecting a list of three parities for TensorManualCartesianParities, must be 1 or -1"]];
      
      chars= Map[Switch[#, -1, "-", +1, "+", _, 
                        ThrowError["Expecting a list of three parities for TensorManualCartesianParities, must be 1 or -1"]] &,
                 l];

      Apply[StringJoin, chars]];

CreateGroupFromTensor[k_, inds_] :=
  Module[{ttypeString, nInds, tags, group, vars},
    InfoMessage[InfoFull, "Creating group from tensor with kernel " <> ToString[k] <> " and indices " <> ToString[inds]];
    ttypeString = tensorTypeString[k, inds];
    InfoMessage[InfoFull, "Tensor type string: ", ttypeString];
    nInds = Length[inds];
    If[nInds == 2 && GetTensorAttribute[k, Symmetries] == {{2,1},1}, 
        ttypeString = ttypeString <> "_sym"];
    tags = {"tensortypealias" -> ttypeString, "tensorweight" -> GetTensorAttribute[k, TensorWeight]};
    If[HasTensorAttribute[k, TensorSpecial],
      tags = Append[tags, "tensorspecial" -> GetTensorAttribute[k, TensorSpecial]]];
    If[HasTensorAttribute[k, TensorManualCartesianParities],
      tags = Append[tags, "cartesianreflectionparities" -> 
                          reflectionParityString[GetTensorAttribute[k, TensorManualCartesianParities]]]];
    vars = If[nInds == 0, {k}, {Apply[Tensor, {k, Apply[Sequence,inds]}]}];
    group = CreateGroup[ToString[k] <> "_group", vars, {Tags -> tags}];
    Return[group]];

CreateGroupFromTensor[x_] :=
  If[IsTensor[x],
    CreateGroupFromTensor[x, {}],
    ThrowError["CreateGroupFromTensor: Not a tensor", x]];

CheckEquationTensors[eq_] :=
  Module[{},
    CheckTensors[eq]];

CheckCalculationTensors[calc_] :=
  Module[{eqs},

  If[mapContains[calc, Shorthands],
  CheckTensors[lookup[calc, Shorthands]]];

  eqs = lookup[calc, Equations];
  Map[CheckEquationTensors, eqs]];

End[];
EndPackage[];
