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
  Module[{configuration, interface, schedule, param, make, cakernel, c},

    InfoMessage[Terse, "Processing arguments to CreateKrancThorn"];

    (* ------------------------------------------------------------------------ 
       Read named arguments and apply nontrivial defaults
       ------------------------------------------------------------------------ *)

    cktCheckNamedArgs[{opts}];

    c = NewObject[
      Code, 
      {"Name" -> thornName,
       "Groups" -> groupsOrig,
       "DeclaredGroups" -> OptionValue[DeclaredGroups],
       "Calculations" -> OptionValue[Calculations],
       "ODEGroups" -> OptionValue[ODEGroups],
       "Implementation" -> If[OptionValue[Implementation] =!= None,
                              OptionValue[Implementation],
                              thornName],
       "InheritedImplementations" -> OptionValue[InheritedImplementations],
       "IncludeFiles" -> OptionValue[IncludeFiles],
       "EvolutionTimelevels" -> OptionValue[EvolutionTimelevels],
       "DefaultEvolutionTimelevels" -> lookupDefault[{opts}, DefaultEvolutionTimelevels,
                                                     OptionValue[EvolutionTimelevels]],
       "PartialDerivatives" -> OptionValue[PartialDerivatives],
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
      calcs = Map[Append[#, Implementation -> GetObjectField[c, "Implementation"]] &, calcs];
      calcs = Map[Append[#, PartialDerivatives -> GetObjectField[c, "PartialDerivatives"]] &, calcs];
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
         nonevolvedODEGroups, 
         Sequence@@(GetObjectField[c,#]& /@ 
                    {"Groups", "Implementation", "Name"})]}]];

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

    (* ------------------------------------------------------------------------ 
       Split calculations according to SplitVars option
       ------------------------------------------------------------------------ *)

    c = ApplyToObjectField[c, "Calculations", SplitCalculations];

    (* ------------------------------------------------------------------------ 
       Symmetries
       ------------------------------------------------------------------------ *)

    c = AppendObjectField[c, "IncludeFiles", "Symmetry.h"];

    InfoMessage[Terse, "Creating symmetry registration file"];

    c = AppendObjectField[
      c, "Sources", 
      {Filename -> "RegisterSymmetries.cc",
       Contents -> CreateSymmetriesRegistrationSource[
         GetObjectField[c, "Name"], GetObjectField[c,"Implementation"], 
         GetObjectField[c, "DeclaredGroups"], GetObjectField[c, "Groups"],
         OptionValue[ReflectionSymmetries], False]}];

    (* ------------------------------------------------------------------------ 
       Startup source file
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Creating startup file"];

    c = AppendObjectField[
      c, "Sources",
      {Filename -> "Startup.cc",
       Contents -> CreateStartupFile[GetObjectField[c, "Name"],
                                     GetObjectField[c, "Name"]]}];

    (* ------------------------------------------------------------------------ 
       Create CCL files
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Creating configuration file"];
    configuration = CreateConfiguration[opts];

    (* TODO: Pass Code object directly into these functions *)

    InfoMessage[Terse, "Creating interface file"];
    interface = CreateKrancInterface[Sequence@@(GetObjectField[c,#]& /@ {"DeclaredGroups", "Groups",
      "Implementation", "InheritedImplementations", "IncludeFiles"}), opts];

    InfoMessage[Terse, "Creating param file"];
    param = CreateKrancParam[
      Sequence@@
      (GetObjectField[c,#]& /@
       {"DeclaredGroups", "Groups", "Name", "Parameters",
        "EvolutionTimelevels", "DefaultEvolutionTimelevels",
        "Calculations"}), opts];

    InfoMessage[Terse, "Creating schedule file"];
    schedule = CreateKrancScheduleFile[
      Sequence@@
      (GetObjectField[c,#]& /@
       {"Calculations", "DeclaredGroups", "Groups", "Name",
        "EvolutionTimelevels"}), opts];

    If[OptionValue[UseCaKernel],
       InfoMessage[Terse, "Creating CaKernel file"];
       cakernel = CaKernelCCL[GetObjectField[c, "Calculations"], opts];
    ,
       cakernel = None;
    ];

    (* ------------------------------------------------------------------------ 
       Create Boundary source files
       ------------------------------------------------------------------------ *)

    c = JoinObjectField[
      c, "Sources",
      CactusBoundary`GetSources[
        Sequence@@
        (GetObjectField[c,#]& /@
         {"DeclaredGroups", "Groups", "Implementation", "Name"})]];

    (* ------------------------------------------------------------------------ 
       Add parameter check source file
       ------------------------------------------------------------------------ *)
   
    If[Length[OptionValue[ParameterConditions]] > 0,
       c = AppendObjectField[
         c, "Sources",
         {Filename -> "ParamCheck.cc",
          Contents -> ParameterCheckSource[GetObjectField[c, "Name"], 
                                           OptionValue[ParameterConditions]]}]];

    (* ------------------------------------------------------------------------ 
       Create finite differencing header file
       ------------------------------------------------------------------------ *)

    Module[
      {diffHeader, pDefs},
      InfoMessage[Terse, "Creating differencing header file"];
      {pDefs, diffHeader} = CreateDifferencingHeader[
        GetObjectField[c, "PartialDerivatives"], OptionValue[ZeroDimensions],
        OptionValue[UseVectors], OptionValue[IntParameters]];
      c = SetObjectField[c, "Calculations", Map[Join[#, {PreDefinitions -> pDefs}] &, GetObjectField[c, "Calculations"]]];
      diffHeader = Join[
        If[OptionValue[UseVectors] && ! OptionValue[UseOpenCL],
           {"#include <assert.h>\n",
            "#include \"vectors.h\"\n",
            "\n"},
           {}],
        diffHeader];
      If[OptionValue[UseOpenCL], diffHeader = OpenCLProcessDifferencingHeader[diffHeader]];
      c = AppendObjectField[
        c, "Sources",
        {Filename -> "Differencing.h", Contents -> diffHeader}]];

    (* ------------------------------------------------------------------------ 
       Create calculation source files
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Creating calculation source files"];

    c = JoinObjectField[
      c, "Sources", 
      Join[Map[{Filename -> lookup[#, Name] <> ".cc",
                Contents -> CreateSetterSource[{#}, False, {}, opts]} &,
               Select[GetObjectField[c, "Calculations"], !CalculationOnDevice[#] &]],
           Map[{Filename -> "CaKernel__"<>lookup[#, Name] <> ".code",
                Contents -> CaKernelCode[#,opts]} &,
               Select[GetObjectField[c, "Calculations"], CalculationOnDevice]]]];

    (* ------------------------------------------------------------------------ 
       Create Makefile
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Creating make file"];
    make = CreateMakefile[Sort[Select[lookup[#, Filename] & /@ 
                                      GetObjectField[c, "Sources"],
                                      StringMatchQ[#, "*.cc"] &]]];

    (* ------------------------------------------------------------------------ 
       Create thorn
       ------------------------------------------------------------------------ *)

    (* Put all the above together and generate the Cactus thorn *)

    Module[
      {thornspec},
      thornspec = {Name          -> GetObjectField[c, "Name"], 
                   Directory     -> parentDirectory,
                   Configuration -> configuration,
                   Interface     -> interface, 
                   Schedule      -> schedule, 
                   Param         -> param,
                   CaKernel      -> cakernel,
                   Makefile      -> make,
                   Sources       -> GetObjectField[c, "Sources"]};
      InfoMessage[Terse, "Creating thorn"];
      CreateThorn[thornspec]]];

End[];
EndPackage[];
