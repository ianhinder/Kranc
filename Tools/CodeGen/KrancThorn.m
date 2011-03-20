(* ::Package:: *)

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
 "Param`", "Schedule`", "Interface`", "Kranc`"}];

CreateKrancThorn::usage = "Construct a Kranc thorn";

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
    pDefs},

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
    realParamDefs = MakeFullParamDefs[realParams];
    intParamDefs = MakeFullParamDefs[intParams];
    keywordParams = OptionValue[KeywordParameters];
    inheritedRealParams = OptionValue[InheritedRealParameters];
    inheritedIntParams = OptionValue[InheritedIntParameters];
    inheritedKeywordParams = OptionValue[InheritedKeywordParameters];
    extendedRealParams = OptionValue[ExtendedRealParameters];
    extendedIntParams = OptionValue[ExtendedIntParameters];
    extendedKeywordParams = OptionValue[ExtendedKeywordParameters];
    partialDerivs = OptionValue[PartialDerivatives];
    reflectionSymmetries = OptionValue[ReflectionSymmetries];

    coordGroup = {"grid::coordinates", {Kranc`x,Kranc`y,Kranc`z,Kranc`r}};

    CheckGroups[groupsOrig];

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
    {pDefs, diffHeader} = CreateDifferencingHeader[partialDerivs, OptionValue[ZeroDimensions], OptionValue[UseVectors]];

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
      False, {}, implementation, opts] &, calcs];
    calcFilenames = Map[lookup[#, Name] <> ext &, calcs];

    (* Makefile *)
    InfoMessage[Terse, "Creating make file"];
    make = CreateMakefile[Join[{"Startup.cc", "RegisterMoL.cc", "RegisterSymmetries.cc"}, calcFilenames, 
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
                  {Filename -> "Startup.cc", Contents -> startup}, 
                  {Filename -> "RegisterMoL.cc", Contents -> molregister},
                  {Filename -> "RegisterSymmetries.cc", Contents -> symregister},
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

End[];
EndPackage[];
