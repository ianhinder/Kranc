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
 "CodeGenStartup`", "CodeGenCalculation`", "Code`", "Object`"}];

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
    partialDerivs, evolvedGroups, rhsGroups, nonevolvedGroups,
    interface, evolvedGroupDefinitions,
    evolvedODEGroups, nonevolvedODEGroups,
    evolvedODEGroupDefinitions, rhsODEGroups,
    cakernel,
    sources = {},
    c},

    InfoMessage[Terse, "Processing arguments to CreateKrancThorn"];

    (* ------------------------------------------------------------------------ 
       Read named arguments and apply nontrivial defaults
       ------------------------------------------------------------------------ *)

    cktCheckNamedArgs[{opts}];

    calcs = OptionValue[Calculations];
    declaredGroups = OptionValue[DeclaredGroups];
    odeGroups = OptionValue[ODEGroups];
    implementation = If[OptionValue[Implementation] =!= None,
                        OptionValue[Implementation],
                        thornName];
    inheritedImplementations = OptionValue[InheritedImplementations];
    includeFiles = OptionValue[IncludeFiles];
    evolutionTimelevels = OptionValue[EvolutionTimelevels]; (* Redundant *)
    defaultEvolutionTimelevels = lookupDefault[{opts}, DefaultEvolutionTimelevels, evolutionTimelevels];
    partialDerivs = OptionValue[PartialDerivatives];

    c = NewObject[Code, {"Name" -> thornName,
                         "Groups" -> groupsOrig,
                         "DeclaredGroups" -> declaredGroups,
                         "Calculations" -> calcs,
                         "ODEGroups" -> odeGroups,
                         "Implementation" -> implementation,
                         "InheritedImplementations" -> inheritedImplementations,
                         "IncludeFiles" -> includeFiles,
                         "EvolutionTimelevels" -> evolutionTimelevels,
                         "DefaultEvolutionTimelevels" -> defaultEvolutionTimelevels,
                         "PartialDerivatives" -> partialDerivs,
                         "Sources" -> {}}];

    (* ------------------------------------------------------------------------ 
       Add required include files
       ------------------------------------------------------------------------ *)

    c = AppendObjectField[c, "IncludeFiles", "GenericFD.h"];

    (* ------------------------------------------------------------------------ 
       Add conservation differencing operators to partialDerivs
       ------------------------------------------------------------------------ *)

    If[OptionValue[ConservationCalculations] =!= {},
       c = JoinObjectField[c, "PartialDerivatives", ConservationDifferencingOperators[]]];

    (* ------------------------------------------------------------------------ 
       Construct parameter database from named arguments
       ------------------------------------------------------------------------ *)

    c = SetObjectField[c, "Parameters", ParameterDatabase[opts]];

    (* ------------------------------------------------------------------------ 
       Add thorn-global options to calculations
       ------------------------------------------------------------------------ *)

    c = Module[
      {calcs = GetObjectField[c, "Calculations"]},

      calcs = Map[mapReplaceAdd[#, Shorthands, Join[lookup[#,Shorthands,{}],OptionValue[Shorthands]]] &, calcs];
      calcs = Map[Append[#, Implementation -> implementation] &, calcs];
      calcs = Map[Append[#, PartialDerivatives -> partialDerivs] &, calcs];
      SetObjectField[c, "Calculations", calcs]];
     
    (* ------------------------------------------------------------------------ 
       CaKernel
       ------------------------------------------------------------------------ *)

    c = Module[
      {calcs = GetObjectField[c, "Calculations"]},

      (* Make the CaKernel option calculation-specific *)
      calcs = Map[Append[#,UseCaKernel -> OptionValue[UseCaKernel]] &, calcs];
      
      If[OptionValue[GenerateHostCode] && OptionValue[UseCaKernel],
         calcs = WithHostCalculations[calcs]];

      If[!And@@Map[ListQ, calcs], Print[Short[calcs//InputForm]]; ThrowError["Result of WithHostCalculations is not a list of lists"]];

      (* Add ExecuteOn -> Device to any CaKernel calculation that has no ExecuteOn option *)
      calcs = Map[If[!lookup[#,UseCaKernel,False], #, If[mapContains[#,ExecuteOn], #, Append[#,ExecuteOn->Device]]] &, calcs];

      SetObjectField[c, "Calculations", calcs]];

    If[OptionValue[UseCaKernel],
       c = AppendObjectField[c, "IncludeFiles", "CaCUDALib_driver_support.h"]];

    If[OptionValue[UseCaKernel],
       c = AppendObjectField[c, "InheritedImplementations", "Accelerator"]];

    (* ------------------------------------------------------------------------ 
       Add coordinates group
       ------------------------------------------------------------------------ *)

    CheckGroups[GetObjectField[c, "Groups"]];

    (* TODO: this should just be an Append.  The order of the groups
       should not matter.  We could also check that the input groups
       do not contain the coordinates. *)

    c = SetObjectField[c, "Groups", 
                       Union[GetObjectField[c, "Groups"],
                             {{"grid::coordinates", {Kranc`x,Kranc`y,Kranc`z,Kranc`r}}},
                             SameTest->(ToLowerCase[#1]==ToLowerCase[#2]&)]];

    (* ------------------------------------------------------------------------ 
       Separate derivatives
       ------------------------------------------------------------------------ *)

    c = SetObjectField[c, "Calculations",
                       SeparateDerivatives[GetObjectField[c, "Calculations"]]];

    (* ------------------------------------------------------------------------ 
       SummationByParts thorn
       ------------------------------------------------------------------------ *)

    If[Cases[{pddefs}, SBPDerivative[_], Infinity] != {},
       c = AppendObjectField[c, "IncludeFiles", "sbp_calc_coeffs.h"]];

    (* ------------------------------------------------------------------------ 
       Add groups defined in calculations to thorn groups
       ------------------------------------------------------------------------ *)

    c = SetObjectField[
      c, "Groups",
      DeleteDuplicates[Join[GetObjectField[c, "Groups"],
                            Flatten[Map[lookup[#,LocalGroups,{}] &,
                                        GetObjectField[c, "Calculations"]],1]]]];

    (* ------------------------------------------------------------------------ 
       Inherited implementations
       ------------------------------------------------------------------------ *)

    c = JoinObjectField[c, "InheritedImplementations",
                        Join[{"Grid", "GenericFD"},
                             CactusBoundary`GetInheritedImplementations[]]];

    (* ------------------------------------------------------------------------ 
       Check input parameters
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Verifying arguments"];
    VerifyGroups[GetObjectField[c, "Groups"]];
    VerifyString[parentDirectory];
    VerifyString[GetObjectField[c, "Name"]];
    VerifyString[GetObjectField[c, "Implementation"]];
    VerifyGroupNames[GetObjectField[c, "DeclaredGroups"]];
    VerifyGroupNames[GetObjectField[c, "ODEGroups"]];

    If[OptionValue[UseJacobian], JacobianCheckGroups[GetObjectField[c, "Groups"]]];

    (* ------------------------------------------------------------------------ 
       Conservation Calculations
       ------------------------------------------------------------------------ *)

    Module[
      {inputConsCalcs, outputConsCalcs, consGroups},

      inputConsCalcs = Map[Append[#, Groups -> GetObjectField[c, "Groups"]] &,
                           OptionValue[ConservationCalculations]];

      outputConsCalcs = 
      Flatten[
        Map[
          ProcessConservationCalculation[#, GetObjectField[c, "Name"]] &,
          inputConsCalcs],
        1];

      outputConsCalcs =
      Map[Join[#, {PartialDerivatives -> GetObjectField[c, "PartialDerivatives"],
                   Implementation -> GetObjectField[c, "Implementation"]}] &,
          outputConsCalcs];

      consGroups = Union@Flatten[
        Map[ConservationCalculationDeclaredGroups, inputConsCalcs],1];
      
      c = JoinObjectField[c, "Calculations", outputConsCalcs];
      c = JoinObjectField[c, "Groups", consGroups];
      c = JoinObjectField[c, "DeclaredGroups", Map[groupName, consGroups]]];


    (* ------------------------------------------------------------------------ 
       ODEs
       ------------------------------------------------------------------------ *)

    c = SetObjectField[c, "Groups", processODEGroups[GetObjectField[c, "ODEGroups"],
                                                     GetObjectField[c, "Groups"]]];

    c = SetObjectField[
      c, "DeclaredGroups", 
      DeleteDuplicates[Join[GetObjectField[c, "DeclaredGroups"],
                            Flatten[Map[Map[groupName,lookup[#,LocalGroups,{}]] &,
                                        GetObjectField[c, "Calculations"]],1]]]];

    (* ------------------------------------------------------------------------ 
       MoL
       ------------------------------------------------------------------------ *)

    Module[
      {evolvedGroups, nonevolvedGroups, evolvedODEGroups,
       nonevolvedODEGroups, rhsGroupDefinitions,
       rhsODEGroupDefinitions, rhsGroups, rhsODEGroups,

       groups, declaredGroups, calcs, odeGroups},

    groups = GetObjectField[c, "Groups"];
    declaredGroups = GetObjectField[c, "DeclaredGroups"];
    calcs = GetObjectField[c, "Calculations"];
    odeGroups = GetObjectField[c, "ODEGroups"];

    groups = MoLProcessGroups[declaredGroups,
                              calcs, groups, GetObjectField[c,"EvolutionTimelevels"]];

    (* Get the different types of group *)
    evolvedGroups = MoLEvolvedGroups[declaredGroups, calcs, groups];
    nonevolvedGroups = MoLNonevolvedGroups[declaredGroups, calcs, groups];

    evolvedODEGroups = MoLEvolvedGroups[odeGroups, calcs, groups];
    nonevolvedODEGroups = MoLNonevolvedGroups[odeGroups, calcs, groups];

    (* Replace the dots in the calculation *)
    calcs = MoLReplaceDots[calcs];

    rhsGroupDefinitions = MoLRHSGroupDefinitions[groups, evolvedGroups];
    rhsODEGroupDefinitions = MoLRHSODEGroupDefinitions[groups, evolvedODEGroups];

    (* Add the RHS groups *)
    groups = Join[groups, rhsGroupDefinitions, rhsODEGroupDefinitions];

    rhsGroups = Map[groupName, rhsGroupDefinitions];
    rhsODEGroups = Map[groupName, rhsODEGroupDefinitions];

    (* This possibly shouldn't be in MoL but under ODEs instead *)
    calcs = Map[Append[#, ODEGroups -> Join[odeGroups, rhsODEGroups]] &, calcs];

    declaredGroups = Join[declaredGroups, rhsGroups, odeGroups, rhsODEGroups];

    InfoMessage[Terse, "Creating MoL registration file"];
    (* TODO: only do this for thorns with evolved variables *)

    c = SetObjectField[c, "Groups", groups];
    c = SetObjectField[c, "DeclaredGroups", declaredGroups];
    c = SetObjectField[c, "Calculations", calcs];
    c = AppendObjectField[
      c, "Sources",
      {Filename -> "RegisterMoL.cc",
       Contents -> CreateKrancMoLRegister[
         evolvedGroups, nonevolvedGroups, evolvedODEGroups,
         nonevolvedODEGroups, groups, implementation, thornName]}]];

    (* ------------------------------------------------------------------------ 
       Add options to calculations
       ------------------------------------------------------------------------ *)

    c = SetObjectField[c, "Calculations", 
                       Map[Join[#, {Groups -> GetObjectField[c, "Groups"]}] &,
                           GetObjectField[c, "Calculations"]]];

    c = SetObjectField[
      c, "Calculations", 
      Map[Append[#, Parameters -> AllNumericParameters[GetObjectField[c, "Parameters"]]] &,
          GetObjectField[c, "Calculations"]]];

    includeFiles = GetObjectField[c, "IncludeFiles"];
    partialDerivs = GetObjectField[c, "PartialDerivatives"];
    parameters = GetObjectField[c, "Parameters"];
    calcs = GetObjectField[c, "Calculations"];
    inheritedImplementations = GetObjectField[c, "InheritedImplementations"];
    groups = GetObjectField[c, "Groups"];
    declaredGroups = GetObjectField[c, "DeclaredGroups"];
    sources = GetObjectField[c, "Sources"];

    (* ------------------------------------------------------------------------ 
       Split calculations according to SplitVars option
       ------------------------------------------------------------------------ *)

    calcs = SplitCalculations[calcs];

    (* ------------------------------------------------------------------------ 
       Symmetries
       ------------------------------------------------------------------------ *)

    AppendTo[includeFiles, "Symmetry.h"];

    InfoMessage[Terse, "Creating symmetry registration file"];
    AppendTo[
      sources,
      {Filename -> "RegisterSymmetries.cc",
       Contents -> CreateSymmetriesRegistrationSource[
         thornName, implementation, 
         declaredGroups, groups, OptionValue[ReflectionSymmetries], False]}];

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
    schedule = CreateKrancScheduleFile[calcs, declaredGroups, groups, thornName,
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
      CactusBoundary`GetSources[declaredGroups, groups, implementation, thornName]];

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

    Module[
      {thornspec},
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
      CreateThorn[thornspec]]];

End[];
EndPackage[];
