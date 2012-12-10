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
 "KrancTensor`", "Param`", "Schedule`", "Interface`", "Kranc`", "Jacobian`",
 "ConservationCalculation`", "CaKernel`", "Calculation`", "ParamCheck`"}];

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
  Module[{calcs, declaredGroups, odeGroups, implementation,
    inheritedImplementations, includeFiles,
    evolutionTimelevels, defaultEvolutionTimelevels,
    realParams, intParams, keywordParams,
    inheritedRealParams, inheritedIntParams, inheritedKeywordParams,
    extendedRealParams, extendedIntParams, extendedKeywordParams,
    configuration,
    partialDerivs, coordGroup, evolvedGroups, rhsGroups, nonevolvedGroups,
    interface, evolvedGroupDefinitions, rhsGroupDefinitions, thornspec,
    evolvedODEGroups, nonevolvedODEGroups,
    evolvedODEGroupDefinitions, rhsODEGroupDefinitions, rhsODEGroups,
    allParams, boundarySources, reflectionSymmetries,
    realParamDefs, intParamDefs,
    pDefs, consCalcs, consCalcsIn, consGroups, cakernel,
    hostCals, deviceCalcs, incFilenames},

    (* Parse named arguments *)

    InfoMessage[Terse, "Processing arguments to CreateKrancThorn"];
    cktCheckNamedArgs[{opts}];

    calcs = OptionValue[Calculations];

    calcs = Map[mapReplaceAdd[#, Shorthands, Join[lookup[#,Shorthands,{}],OptionValue[Shorthands]]] &, calcs];

    declaredGroups = OptionValue[DeclaredGroups];
    odeGroups = OptionValue[ODEGroups];
    implementation = 
      If[OptionValue[Implementation] =!= None, 
        OptionValue[Implementation],
        thornName];

    calcs = Map[Append[#, Implementation -> implementation] &, calcs];

    inheritedImplementations = OptionValue[InheritedImplementations];
    includeFiles = OptionValue[IncludeFiles];
    evolutionTimelevels = OptionValue[EvolutionTimelevels]; (* Redundant *)
    defaultEvolutionTimelevels = lookupDefault[{opts}, DefaultEvolutionTimelevels, evolutionTimelevels];
    realParams = OptionValue[RealParameters];
    If[OptionValue[ConservationCalculations] =!= {},
       realParams = Join[realParams,ConservationDifferencingRealParameters[]]];
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
    If[OptionValue[ConservationCalculations] =!= {},
       partialDerivs = Join[partialDerivs, ConservationDifferencingOperators[]]];
    reflectionSymmetries = OptionValue[ReflectionSymmetries];

    (* Make the CaKernel option calculation-specific *)
    calcs = Map[Append[#,UseCaKernel -> OptionValue[UseCaKernel]] &, calcs];

    If[OptionValue[GenerateHostCode] && OptionValue[UseCaKernel],
       calcs = WithHostCalculations[calcs]];

    If[!And@@Map[ListQ, calcs], Print[Short[calcs//InputForm]]; ThrowError["Result of WithHostCalculations is not a list of lists"]];


    calcs = Map[Append[#, PartialDerivatives -> partialDerivs] &, calcs];

    coordGroup = {"grid::coordinates", {Kranc`x,Kranc`y,Kranc`z,Kranc`r}};

    CheckGroups[groupsOrig];

    groups = Union[groupsOrig, {coordGroup},
                   SameTest->(ToLowerCase[#1]==ToLowerCase[#2]&)];

    calcs = SeparateDerivatives[calcs];

    groups = DeleteDuplicates[Join[groups, Flatten[Map[lookup[#,LocalGroups,{}] &, calcs],1]]];
    includeFiles = Join[includeFiles, {"GenericFD.h", "Symmetry.h", "sbp_calc_coeffs.h"}];

    If[OptionValue[UseCaKernel],
       includeFiles = Append[includeFiles, "CaCUDALib_driver_support.h"]];

    inheritedImplementations = Join[inheritedImplementations, {"Grid",
     "GenericFD"}, CactusBoundary`GetInheritedImplementations[]];

    If[OptionValue[UseCaKernel],
       inheritedImplementations = Append[inheritedImplementations, "Accelerator"]];

    InfoMessage[Terse, "Verifying arguments"];

    (* Check parameters *)
    VerifyGroups[groups];
    VerifyString[parentDirectory];
    VerifyString[thornName];
    VerifyString[implementation];
    VerifyGroupNames[declaredGroups];
    VerifyGroupNames[odeGroups];

    If[OptionValue[UseJacobian], JacobianCheckGroups[groups]];

    InfoMessage[Terse, "Creating startup file"];
    startup = CreateStartupFile[thornName, thornName];

    consCalcsIn = Append[#,Groups -> groups]& /@
                    OptionValue[ConservationCalculations];

    (* Add in calculations to solve any conservation laws that have
       been provided *)

    consCalcs = Flatten[Map[ProcessConservationCalculation[#,thornName] &,
                            consCalcsIn],1];

    consCalcs = Map[Join[#, {PartialDerivatives -> partialDerivs,
                             Implementation -> implementation}] &, consCalcs];

    calcs = Join[calcs,consCalcs];
    (* Print["consCalcs = ", consCalcs]; *)

    consGroups = Union@Flatten[
      Map[ConservationCalculationDeclaredGroups, consCalcsIn],1];

    groups = Join[groups, consGroups];
    declaredGroups = Join[declaredGroups, Map[groupName, consGroups]];

    declaredGroups = DeleteDuplicates[Join[declaredGroups, Flatten[Map[Map[groupName,lookup[#,LocalGroups,{}]] &, calcs],1]]];

    (* Get the different types of group *)
    evolvedGroups = extractEvolvedGroups[declaredGroups, calcs, groups];
    nonevolvedGroups = extractNonevolvedGroups[declaredGroups, calcs, groups];

    evolvedODEGroups = extractEvolvedGroups[odeGroups, calcs, groups];
    nonevolvedODEGroups = extractNonevolvedGroups[odeGroups, calcs, groups];

    (* Replace the dots in the calculation *)
    calcs = replaceDots[calcs];

    (* Add the RHS groups *)
    evolvedGroupDefinitions = Map[groupFromName[#, groups] &, evolvedGroups];
    rhsGroupDefinitions = Map[evolvedGroupToRHSGroup[#, evolvedGroupDefinitions] &, evolvedGroups];
    groups = Join[groups, rhsGroupDefinitions];

    evolvedODEGroupDefinitions = Map[groupFromName[#, groups] &, evolvedODEGroups];
    rhsODEGroupDefinitions = Map[evolvedGroupToRHSGroup[#, evolvedODEGroupDefinitions] &, evolvedODEGroups];
    groups = Join[groups, rhsODEGroupDefinitions];

    (* Add the groups into the calcs *)
    calcs = Map[Join[#, {Groups -> groups}] &, calcs];

    calcs = SplitCalculations[calcs];

    rhsGroups = Map[groupName, rhsGroupDefinitions];
    rhsODEGroups = Map[groupName, rhsODEGroupDefinitions];

    calcs = Map[Append[#, ODEGroups -> Join[odeGroups, rhsODEGroups]] &, calcs];

    (* Construct a source file for each calculation *)
    allParams = Join[Map[ParamName, realParamDefs],
                     Map[ParamName, intParamDefs],
                     Map[unqualifiedName, inheritedRealParams], 
                     Map[unqualifiedName, inheritedIntParams], 
                     Map[unqualifiedName, inheritedKeywordParams]];

    calcs = Map[Append[#, Parameters -> allParams] &, calcs];

    calcs = Map[If[!lookup[#,UseCaKernel,False], #, If[mapContains[#,ExecuteOn], #, Append[#,ExecuteOn->Device]]] &, calcs];

    (* Construct the configuration file *)
    InfoMessage[Terse, "Creating configuration file"];
    configuration = CreateConfiguration[opts];

    (* Construct the interface file *)
    InfoMessage[Terse, "Creating interface file"];
    interface = CreateKrancInterface[nonevolvedGroups,
      evolvedGroups, rhsGroups, nonevolvedODEGroups, evolvedODEGroups,
      rhsODEGroups, groups,
      implementation, inheritedImplementations, includeFiles, opts];

    (* Construct the param file *)
    InfoMessage[Terse, "Creating param file"];
    param = CreateKrancParam[evolvedGroups, nonevolvedGroups,
      evolvedODEGroups, nonevolvedODEGroups,
      groups, thornName,
      realParamDefs, intParamDefs, keywordParams,
      inheritedRealParams, inheritedIntParams, inheritedKeywordParams,
      extendedRealParams, extendedIntParams, extendedKeywordParams,
      evolutionTimelevels, defaultEvolutionTimelevels,
      calcs, opts];

    (* Construct the schedule file *)
    InfoMessage[Terse, "Creating schedule file"];
    schedule = CreateKrancScheduleFile[calcs, groups, Join[evolvedGroups,evolvedODEGroups],
      Join[rhsGroups,rhsODEGroups], Join[nonevolvedGroups,nonevolvedODEGroups], thornName,
      evolutionTimelevels,opts];

    (* Construct the cakernel file *)
    If[OptionValue[UseCaKernel],
       InfoMessage[Terse, "Creating CaKernel file"];
       cakernel = CaKernelCCL[calcs, opts];
    ,
       cakernel = None;
    ];

    boundarySources = CactusBoundary`GetSources[evolvedGroups, groups, 
                                            implementation, thornName];

    (* Create the MoL registration file (we do this for every thorn,
       even if it does not evolve any variables). This could be fixed
       later. *)
    InfoMessage[Terse, "Creating MoL registration file"];
    molregister = createKrancMoLRegister[evolvedGroups, nonevolvedGroups, evolvedODEGroups, nonevolvedODEGroups, groups, implementation, thornName];

    Module[{allGFs = Join[variablesFromGroups[evolvedGroups, groups],
                          variablesFromGroups[nonevolvedGroups, groups]]},
      InfoMessage[Terse, "Creating symmetry registration file"];
      symregister = CreateSymmetriesRegistrationSource[thornName, implementation, 
        allGFs, reflectionSymmetries, False]];

    (* Write the differencing header file *)
    InfoMessage[Terse, "Creating differencing header file"];
    {pDefs, diffHeader} = CreateDifferencingHeader[partialDerivs, OptionValue[ZeroDimensions], OptionValue[UseVectors], OptionValue[IntParameters]];
    diffHeader = Join[
        If[OptionValue[UseVectors] && ! OptionValue[UseOpenCL],
           {"#include <assert.h>\n",
            "#include \"vectors.h\"\n",
            "\n"},
           {}],
        diffHeader];
    diffHeader = If[OptionValue[UseOpenCL],
                    "static char const *const differencing =\n" <>
                    Stringify[diffHeader] <>
                    ";\n",
                    diffHeader];

    (* Add the predefinitions into the calcs *)
    calcs = Map[Join[#, {PreDefinitions -> pDefs}] &, calcs];

    ext = CodeGenC`SOURCESUFFIX;


    InfoMessage[Terse, "Creating calculation source files"];

    hostCalcs = Select[calcs, !CalculationOnDevice[#] &];
    deviceCalcs = Select[calcs, CalculationOnDevice];

    calcSources = Join[Map[CreateSetterSource[{#}, False, {}, opts] &, hostCalcs],
                       Map[CaKernelCode[#,opts] &, deviceCalcs]];

    calcFilenames = Join[Map[lookup[#, Name] <> ext &, hostCalcs],
                         Map["CaKernel__"<>lookup[#, Name] <> ".code" &, deviceCalcs]];

    incFilenames = Map[lookup[#, Name] <> ext &, hostCalcs];

    (* Makefile *)
    InfoMessage[Terse, "Creating make file"];
    make = CreateMakefile[Join[{"Startup.cc", "RegisterSymmetries.cc"},
                               {"RegisterMoL.cc"}, If[Length[OptionValue[ParameterConditions]] > 0, {"ParamCheck.cc"}, {}],
                               incFilenames,
                               Map[lookup[#, Filename] &, boundarySources]]];

    (* Put all the above together and generate the Cactus thorn *)
    thornspec = {Name          -> thornName, 
                 Directory     -> parentDirectory,
                 Configuration -> configuration,
	         Interface     -> interface, 
                 Schedule      -> schedule, 
                 Param         -> param,
                 CaKernel      -> cakernel,
                 Makefile      -> make,
                 Sources       -> Join[{
                  {Filename -> "Startup.cc", Contents -> startup}, 
                  {Filename -> "RegisterMoL.cc", Contents -> molregister},
                  {Filename -> "RegisterSymmetries.cc", Contents -> symregister},
                  {Filename -> "Differencing.h", Contents -> diffHeader}},
                  MapThread[{Filename -> #1, Contents -> #2} &, 
                            {calcFilenames, calcSources}], boundarySources, 
                  If[Length[OptionValue[ParameterConditions]] > 0,
                     {{Filename -> "ParamCheck.cc",
                      Contents -> ParameterCheckSource[thornName, OptionValue[ParameterConditions]]}},
                     {}]]};
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
    allVars = variablesFromGroups[declaredGroups, groups];
    evolvedVars = Apply[Join, Map[CalculationEvolvedVars, calcs]];
    evolvedVars = Intersection[allVars, evolvedVars];
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

createKrancMoLRegister[evolvedGroupNames_, nonevolvedGroupNames_, evolvedODEGroupNames_, nonevolvedODEGroupNames_, groups_, implementation_, thornName_] :=
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
    molregister = CreateMoLRegistrationSource[molspec, False];
    Return[molregister]];

End[];
EndPackage[];
