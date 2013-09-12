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
 "CodeGenCalculation`", "Errors`", "Helpers`", "CactusBoundary`",
 "KrancTensor`", "Param`", "Schedule`", "Interface`", "Kranc`", "Jacobian`",
 "ConservationCalculation`", "CaKernel`", "Calculation`", "ParamCheck`",
 "OpenCL`", "CodeGenConfiguration`", "CodeGenMakefile`", "CodeGenSymmetries`", "MoL`",
 "CodeGenStartup`", "CodeGenCalculation`"}];

CreateKrancThorn::usage = "Construct a Kranc thorn";

Begin["`Private`"];

(* --------------------------------------------------------------------------
   Utility functions
   -------------------------------------------------------------------------- *)

cktCheckNamedArgs[l_] := 
Module[{used, unrecognized},
    used = Map[First, l];
    unrecognized = Complement[used, Map[First, ThornOptions]];
    If[Length[unrecognized] > 0,
      ThrowError["Unrecognized named arguments: ", unrecognized]]];

DefFn[
  processODEGroups[odeGroups_List, groups_List] :=
  Map[If[MemberQ[odeGroups, groupName[#]],
         (* Print["Adding grid type array to ", groupName[#]]; *)
         Append[#, GridType -> "array"],
         #] &, groups]];

(* --------------------------------------------------------------------------
   Thorn generation (main entry point for non-tensorial thorns)
   -------------------------------------------------------------------------- *)

Options[CreateKrancThorn] = ThornOptions;

CreateKrancThorn[groupsOrig_, parentDirectory_, thornName_, opts:OptionsPattern[]] :=
  Module[{calcs, declaredGroups, odeGroups, implementation,
    inheritedImplementations, includeFiles,
    evolutionTimelevels, defaultEvolutionTimelevels,
    parameters,
    configuration,
    partialDerivs, coordGroup, evolvedGroups, rhsGroups, nonevolvedGroups,
    interface, evolvedGroupDefinitions, thornspec,
    evolvedODEGroups, nonevolvedODEGroups,
    evolvedODEGroupDefinitions, rhsODEGroups,
    reflectionSymmetries,
    consCalcs, consCalcsIn, consGroups, cakernel,
    sources = {}},

    InfoMessage[Terse, "Processing arguments to CreateKrancThorn"];

    (* ------------------------------------------------------------------------ 
       Read named arguments
       ------------------------------------------------------------------------ *)

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

    parameters = ParameterDatabase[opts];

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

    (* ------------------------------------------------------------------------ 
       Add coordinates group
       ------------------------------------------------------------------------ *)

    coordGroup = {"grid::coordinates", {Kranc`x,Kranc`y,Kranc`z,Kranc`r}};

    CheckGroups[groupsOrig];

    groups = Union[groupsOrig, {coordGroup},
                   SameTest->(ToLowerCase[#1]==ToLowerCase[#2]&)];

    (* ------------------------------------------------------------------------ 
       Separate derivatives
       ------------------------------------------------------------------------ *)

    calcs = SeparateDerivatives[calcs];

    (* ------------------------------------------------------------------------ 
       Add groups defined in calculations to thorn groups
       ------------------------------------------------------------------------ *)

    groups = DeleteDuplicates[Join[groups, Flatten[Map[lookup[#,LocalGroups,{}] &, calcs],1]]];

    (* ------------------------------------------------------------------------ 
       Add include files
       ------------------------------------------------------------------------ *)

    includeFiles = Join[includeFiles, {"GenericFD.h", "Symmetry.h", "sbp_calc_coeffs.h"}];

    If[OptionValue[UseCaKernel],
       includeFiles = Append[includeFiles, "CaCUDALib_driver_support.h"]];

    (* ------------------------------------------------------------------------ 
       Inherited implementations
       ------------------------------------------------------------------------ *)

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

    (* ------------------------------------------------------------------------ 
       Conservation Calculations
       ------------------------------------------------------------------------ *)

    Module[
      {inputConsCalcs, outputConsCalcs, consGroups},

      inputConsCalcs = Map[Append[#, Groups -> groups] &, OptionValue[ConservationCalculations]];

      outputConsCalcs = 
      Flatten[
        Map[
          ProcessConservationCalculation[#, thornName] &,
          inputConsCalcs],
        1];

      outputConsCalcs = Map[Join[#, {PartialDerivatives -> partialDerivs,
                                     Implementation -> implementation}] &,
                            outputConsCalcs];

      consGroups = Union@Flatten[
        Map[ConservationCalculationDeclaredGroups, inputConsCalcs],1];
      
      calcs = Join[calcs,outputConsCalcs];
      groups = Join[groups, consGroups];
      declaredGroups = Join[declaredGroups, Map[groupName, consGroups]]];

    (* ------------------------------------------------------------------------ 
       ODEs
       ------------------------------------------------------------------------ *)

    groups = processODEGroups[odeGroups, groups];

    declaredGroups = DeleteDuplicates[Join[declaredGroups, Flatten[Map[Map[groupName,lookup[#,LocalGroups,{}]] &, calcs],1]]];

    (* ------------------------------------------------------------------------ 
       MoL
       ------------------------------------------------------------------------ *)

    groups = MoLProcessGroups[declaredGroups,
                              calcs, groups, evolutionTimelevels];

    (* Get the different types of group *)
    evolvedGroups = MoLEvolvedGroups[declaredGroups, calcs, groups];
    nonevolvedGroups = MoLNonevolvedGroups[declaredGroups, calcs, groups];

    evolvedODEGroups = MoLEvolvedGroups[odeGroups, calcs, groups];
    nonevolvedODEGroups = MoLNonevolvedGroups[odeGroups, calcs, groups];

    (* Replace the dots in the calculation *)
    calcs = MoLReplaceDots[calcs];

    Module[
      {rhsGroupDefinitions = MoLRHSGroupDefinitions[groups, evolvedGroups],
       rhsODEGroupDefinitions = MoLRHSODEGroupDefinitions[groups, evolvedODEGroups]},

      (* Add the RHS groups *)
      groups = Join[groups, rhsGroupDefinitions, rhsODEGroupDefinitions];

      rhsGroups = Map[groupName, rhsGroupDefinitions];
      rhsODEGroups = Map[groupName, rhsODEGroupDefinitions]];

    declaredGroups = Join[declaredGroups, rhsGroups, odeGroups, rhsODEGroups];

    InfoMessage[Terse, "Creating MoL registration file"];
    (* TODO: only do this for thorns with evolved variables *)

    AppendTo[
      sources, 
      {Filename -> "RegisterMoL.cc",
       Contents -> CreateKrancMoLRegister[
         evolvedGroups, nonevolvedGroups, evolvedODEGroups,
         nonevolvedODEGroups, groups, implementation, thornName]}];

    (* ------------------------------------------------------------------------ 
       Add options to calculations
       ------------------------------------------------------------------------ *)

    calcs = Map[Join[#, {Groups -> groups}] &, calcs];

    calcs = Map[Append[#, ODEGroups -> Join[odeGroups, rhsODEGroups]] &, calcs];

    calcs = Map[Append[#, Parameters -> AllNumericParameters[parameters]] &, calcs];

    calcs = Map[If[!lookup[#,UseCaKernel,False], #, If[mapContains[#,ExecuteOn], #, Append[#,ExecuteOn->Device]]] &, calcs];

    (* ------------------------------------------------------------------------ 
       Split calculations according to SplitVars option
       ------------------------------------------------------------------------ *)

    calcs = SplitCalculations[calcs];

    (* ------------------------------------------------------------------------ 
       Startup source file
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Creating startup file"];
    AppendTo[
      sources,
      {Filename -> "Startup.cc",
       Contents -> CreateStartupFile[thornName, thornName]}];

    (* ------------------------------------------------------------------------ 
       Create CCL files
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Creating configuration file"];
    configuration = CreateConfiguration[opts];

    InfoMessage[Terse, "Creating interface file"];
    interface = CreateKrancInterface[declaredGroups, groups,
      implementation, inheritedImplementations, includeFiles, opts];

    InfoMessage[Terse, "Creating param file"];
    param = CreateKrancParam[declaredGroups,
                             groups,
                             thornName,
                             parameters,
                             evolutionTimelevels,
                             defaultEvolutionTimelevels,
                             calcs, opts];

    InfoMessage[Terse, "Creating schedule file"];
    schedule = CreateKrancScheduleFile[calcs, groups, Join[evolvedGroups,evolvedODEGroups],
      Join[rhsGroups,rhsODEGroups], Join[nonevolvedGroups,nonevolvedODEGroups], thornName,
      evolutionTimelevels,opts];

    If[OptionValue[UseCaKernel],
       InfoMessage[Terse, "Creating CaKernel file"];
       cakernel = CaKernelCCL[calcs, opts];
    ,
       cakernel = None;
    ];

    (* ------------------------------------------------------------------------ 
       Create Boundary source files
       ------------------------------------------------------------------------ *)

    sources = Join[
      sources,
      CactusBoundary`GetSources[evolvedGroups, groups, implementation, thornName]];

    (* ------------------------------------------------------------------------ 
       Create symmetry registration source file
       ------------------------------------------------------------------------ *)

    Module[{allGFs = Join[variablesFromGroups[evolvedGroups, groups],
                          variablesFromGroups[nonevolvedGroups, groups]]},
      InfoMessage[Terse, "Creating symmetry registration file"];
      AppendTo[
        sources,
        {Filename -> "RegisterSymmetries.cc",
         Contents -> CreateSymmetriesRegistrationSource[
           thornName, implementation, 
           allGFs, reflectionSymmetries, False]}]];

    (* ------------------------------------------------------------------------ 
       Add parameter check source file
       ------------------------------------------------------------------------ *)
   
    If[Length[OptionValue[ParameterConditions]] > 0,
       AppendTo[sources,
                {Filename -> "ParamCheck.cc",
                 Contents -> ParameterCheckSource[thornName, OptionValue[ParameterConditions]]}]];

    (* ------------------------------------------------------------------------ 
       Create finite differencing header file
       ------------------------------------------------------------------------ *)

    Module[
      {diffHeader, pDefs},
      InfoMessage[Terse, "Creating differencing header file"];
      {pDefs, diffHeader} = CreateDifferencingHeader[
        partialDerivs, OptionValue[ZeroDimensions],
        OptionValue[UseVectors], OptionValue[IntParameters]];
      calcs = Map[Join[#, {PreDefinitions -> pDefs}] &, calcs];
      diffHeader = Join[
        If[OptionValue[UseVectors] && ! OptionValue[UseOpenCL],
           {"#include <assert.h>\n",
            "#include \"vectors.h\"\n",
            "\n"},
           {}],
        diffHeader];
      If[OptionValue[UseOpenCL], diffHeader = OpenCLProcessDifferencingHeader[diffHeader]];
      AppendTo[sources, {Filename -> "Differencing.h", Contents -> diffHeader}]];

    (* ------------------------------------------------------------------------ 
       Add predefinitions to calculations
       ------------------------------------------------------------------------ *)


    (* ------------------------------------------------------------------------ 
       Create calculation source files
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Creating calculation source files"];

    sources = Join[sources,
                   Map[{Filename -> lookup[#, Name] <> ".cc",
                        Contents -> CreateSetterSource[{#}, False, {}, opts]} &,
                       Select[calcs, !CalculationOnDevice[#] &]]];

    sources = Join[sources,
                   Map[{Filename -> "CaKernel__"<>lookup[#, Name] <> ".code",
                        Contents -> CaKernelCode[#,opts]} &,
                       Select[calcs, CalculationOnDevice]]];

    (* ------------------------------------------------------------------------ 
       Create Makefile
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Creating make file"];
    make = CreateMakefile[Sort[Select[lookup[#, Filename] & /@ sources, StringMatchQ[#, "*.cc"] &]]];

    (* ------------------------------------------------------------------------ 
       Create thorn
       ------------------------------------------------------------------------ *)

    (* Put all the above together and generate the Cactus thorn *)
    thornspec = {Name          -> thornName, 
                 Directory     -> parentDirectory,
                 Configuration -> configuration,
	         Interface     -> interface, 
                 Schedule      -> schedule, 
                 Param         -> param,
                 CaKernel      -> cakernel,
                 Makefile      -> make,
                 Sources       -> sources};
    InfoMessage[Terse, "Creating thorn"];
    CreateThorn[thornspec]];

End[];
EndPackage[];
