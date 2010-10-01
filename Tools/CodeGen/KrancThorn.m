
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

(****************************************************************************)
(* Generate Cactus Thorns from a high-level interface  *)
(****************************************************************************)

BeginPackage["KrancThorn`", {"CodeGen`", "Thorn`",
 "MapLookup`", "KrancGroups`", "Differencing`",
 "CalculationFunction`", "Errors`", "Helpers`", "CactusBoundary`",
 "TensorTools`", "Param`", "Schedule`", "Interface`", "Kranc`",
 "ConservationCalculation`"}];

CreateKrancThorn::usage = "Construct a Kranc thorn";
CreateKrancThornTT::usage = "Construct a Kranc thorn using TensorTools";
CreateGroupFromTensor::usage = "";

Begin["`Private`"];

(* --------------------------------------------------------------------------
   Utility functions
   -------------------------------------------------------------------------- *)

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

(* --------------------------------------------------------------------------
   Thorn generation (main entry point for non-tensorial thorns)
   -------------------------------------------------------------------------- *)

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
    pDefs, useCSE, consCalcs, consCalcsIn, consGroups},

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
    realParams = OptionValue[RealParameters] ~Join~ ConservationDifferencingRealParameters[];
    intParams = OptionValue[IntParameters];
    realParamDefs = MakeFullParamDefs[realParams];
    intParamDefs = MakeFullParamDefs[intParams];
    keywordParams = OptionValue[KeywordParameters];
    inheritedRealParams = OptionValue[InheritedRealParameters];
    inheritedIntParams = OptionValue[InheritedIntParameters];
    inheritedKeywordParams = OptionValue[InheritedKeywordParameters];
    extendedRealParams = OptionValue[ExtendedRealParameters];
    extendedIntParams = OptionValue[ExtendedIntParameters];
    extendedKeywordParams = OptionValue[ExtendedKeywordParameters];
    partialDerivs = OptionValue[PartialDerivatives] ~Join~
      ConservationDifferencingOperators[];
    reflectionSymmetries = OptionValue[ReflectionSymmetries];
    useCSE = OptionValue[UseCSE];

    coordGroup = {"grid::coordinates", {Kranc`x,Kranc`y,Kranc`z,Kranc`r}};
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

    consCalcsIn = Append[#,Groups -> groups]& /@
                    OptionValue[ConservationCalculations];

    (* Add in calculations to solve any conservation laws that have
       been provided *)
    calcs = Join[calcs,
      consCalcs = Flatten[Map[ProcessConservationCalculation[#,thornName] &,
        consCalcsIn],1]];
    (* Print["consCalcs = ", consCalcs]; *)

    consGroups = Union@Flatten[
      Map[ConservationCalculationDeclaredGroups, consCalcsIn],1];

    groups = Join[groups, consGroups];
    declaredGroups = Join[declaredGroups, Map[groupName, consGroups]];

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
    configuration = CreateConfiguration[opts];

    (* Construct the interface file *)
    InfoMessage[Terse, "Creating interface file"];
    interface = CreateKrancInterface[nonevolvedGroups,
      evolvedGroups, rhsGroups, groups,
      implementation, inheritedImplementations, includeFiles, opts];

    (* Construct the param file *)
    InfoMessage[Terse, "Creating param file"];
    param = CreateKrancParam[evolvedGroups, nonevolvedGroups, groups, thornName,
      realParamDefs, intParamDefs, keywordParams,
      inheritedRealParams, inheritedIntParams, inheritedKeywordParams,
      extendedRealParams, extendedIntParams, extendedKeywordParams,
      evolutionTimelevels, defaultEvolutionTimelevels,
      calcs];

    (* Construct the schedule file *)
    InfoMessage[Terse, "Creating schedule file"];
    schedule = CreateKrancScheduleFile[calcs, groups, evolvedGroups,
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
    allParams = Join[Map[ParamName, realParamDefs],
                     Map[ParamName, intParamDefs],
                     Map[unqualifiedName, inheritedRealParams], 
                     Map[unqualifiedName, inheritedIntParams], 
                     Map[unqualifiedName, inheritedKeywordParams]];

    InfoMessage[Terse, "Creating calculation source files"];
    calcSources = Map[CreateSetterSource[
      {Join[#, {Parameters -> allParams, PartialDerivatives -> partialDerivs}]},
      False, useCSE, {}, implementation, opts] &, calcs];
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

(* --------------------------------------------------------------------------
   Functions related to calculations
   -------------------------------------------------------------------------- *)

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

(* --------------------------------------------------------------------------
   Tensors
   -------------------------------------------------------------------------- *)

(*Options[CreateKrancThornTT] = ThornOptions;*)

CreateKrancThornTT[groups_, parentDirectory_, thornName_, opts___] :=
  Module[{calcs, expCalcs, expGroups, options, derivs, expDerivs, reflectionSymmetries, declaredGroups, consCalcs, expConsCalcs},
    InfoMessage[Terse, "Processing tensorial arguments"];
    calcs = lookup[{opts}, Calculations];
    consCalcs = lookupDefault[{opts}, ConservationCalculations, {}];
    derivs = lookupDefault[{opts}, PartialDerivatives, {}];
    Map[CheckCalculationTensors, calcs];
    expCalcs = Map[makeCalculationExplicit, calcs];
    expConsCalcs = Map[makeCalculationExplicit, consCalcs];

    InfoMessage[Info, "Group definitions:", groups];

    expDerivs = Flatten[Map[MakeExplicit,derivs],1];
    expGroups = Map[makeGroupExplicit, groups];
    options = {opts};
    options = Join[DeleteCases[options, Calculations -> _], {Calculations -> expCalcs}];
    options = Join[DeleteCases[options, ConservationCalculations -> _],
      {ConservationCalculations -> expConsCalcs}];
    options = Join[DeleteCases[options, PartialDerivatives -> _],
      {PartialDerivatives -> expDerivs}];

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
     Equations -> MakeExplicit,
     PrimitiveEquations -> MakeExplicit,
     ConservedEquations -> MakeExplicit}];

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
    If[nInds == 3 && GetTensorAttribute[k, Symmetries] == {{1,3,2},1}, 
        ttypeString = ttypeString <> "_sym"];
    tags = {"tensortypealias" -> ttypeString, "tensorweight" -> GetTensorAttribute[k, TensorWeight]};
    If[HasTensorAttribute[k, TensorSpecial],
      tags = Append[tags, "tensorspecial" -> GetTensorAttribute[k, TensorSpecial]]];
    If[HasTensorAttribute[k, TensorManualCartesianParities],
      tags = Append[tags, "cartesianreflectionparities" -> 
                          reflectionParityString[GetTensorAttribute[k, TensorManualCartesianParities]]]];
    If[HasTensorAttribute[k, TensorParity],
      tags = Append[tags, "tensorparity" -> GetTensorAttribute[k, TensorParity]]];

    If[HasTensorAttribute[k, Checkpoint],
      tags = Append[tags, "checkpoint" -> GetTensorAttribute[k, Checkpoint]]];

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
