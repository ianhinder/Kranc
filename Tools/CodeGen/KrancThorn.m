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
 "CodeGenStartup`", "CodeGenCalculation`", "Code`", "Object`", "OperationCount`"}];

CreateKrancThorn::usage = "Construct a Kranc thorn";

Begin["`Private`"];

(* --------------------------------------------------------------------------
   Utility functions
   -------------------------------------------------------------------------- *)

DefFn[cktCheckNamedArgs[l_] := 
Module[{used, unrecognized},
    used = Map[First, l];
    unrecognized = Complement[used, Map[First, ThornOptions]];
    If[Length[unrecognized] > 0,
      ThrowError["Unrecognized named arguments: ", unrecognized]]]];

DefFn[
  processODEGroups[odeGroups_List, groups_List] :=
  Map[If[MemberQ[odeGroups, groupName[#]],
         (* Print["Adding grid type array to ", groupName[#]]; *)
         Append[#, GridType -> "array"],
         #] &, groups]];

Options[ODEProcessCode] = ThornOptions;

DefFn[
  ODEProcessCode[cIn_Code, opts:OptionsPattern[]] :=
  Module[
    {c = cIn},
    c = SetObjectField[c, "Groups", processODEGroups[GetObjectField[c, "ODEGroups"],
                                                     GetObjectField[c, "Groups"]]];
    c]];

DefFn[
  coordinatesProcessCode[cIn_Code, opts___] :=
  Module[
    {c = cIn},
    (* TODO: this should just be an Append.  The order of the groups
       should not matter.  We could also check that the input groups
       do not contain the coordinates. *)

    c = SetObjectField[c, "Groups", 
                       Union[GetObjectField[c, "Groups"],
                             {{"grid::coordinates", {x,y,z,r}}},
                             SameTest->(ToLowerCase[#1]==ToLowerCase[#2]&)]];
    c = AppendObjectField[c, "InheritedImplementations", "Grid"];
    c]];

DefFn[
  embeddedFile[name_String,c_, opts___] :=
    StringReplace[Import[FileNameJoin[{KrancDirectory,"Auxiliary/Cactus/SourceFiles",name}], "Text"], "@THORN_NAME@" -> GetObjectField[c,"Name"]]];

DefFn[
  genericFDProcessCode[cIn_Code, opts___] :=
  Module[
    {c = cIn, krancSource, krancHeader},
    c = AppendObjectField[c, "InheritedImplementations", "GenericFD"];

    krancSource =
    {Filename -> "Kranc.cc",
     Contents -> embeddedFile["Kranc.cc",c,opts]};

    krancHeader =
    {Filename -> "Kranc.hh",
     Contents -> embeddedFile["Kranc.hh",c, opts]};

    stencilHeader =
    {Filename -> "StencilOps.hh",
     Contents -> embeddedFile["StencilOps.hh",c, opts]};

    c = AppendObjectField[c, "Sources", krancSource];
    c = AppendObjectField[c, "Sources", krancHeader];
    c = AppendObjectField[c, "Sources", stencilHeader];
    c]];

DefFn[
  splitCalculationsProcessCode[cIn_Code, opts___] :=
  ApplyToObjectField[cIn, "Calculations", SplitCalculations]];

DefFn[
  distributeOptionsProcessCode[cIn_Code, opts___] :=
  Module[
    {c = cIn},
    c = SetObjectField[
      c, "Calculations", 
      Map[Join[#, {Groups -> GetObjectField[c, "Groups"]}] &,
          GetObjectField[c, "Calculations"]]];

    c = SetObjectField[
      c, "Calculations", 
      Map[Append[#, Parameters -> AllNumericParameters[GetObjectField[c, "Parameters"]]] &,
          GetObjectField[c, "Calculations"]]];
    c]];

DefFn[
  declaredGroupsProcessCode[cIn_Code, opts___] :=
  Module[
    {c = cIn},
    c = SetObjectField[
      c, "DeclaredGroups", 
      DeleteDuplicates[Join[GetObjectField[c, "DeclaredGroups"],
                            Flatten[Map[Map[groupName,lookup[#,LocalGroups,{}]] &,
                                        GetObjectField[c, "Calculations"]],1]]]];
    c]];

Options[loopControlProcessCode] = ThornOptions;
DefFn[
  loopControlProcessCode[cIn_Code, opts:OptionsPattern[]] :=
  Module[
    {},
    If[OptionValue[UseVectors] && !OptionValue[UseLoopControl],
      ThrowError["UseVectors -> True requires UseLoopControl -> True"]];
    SetObjectField[cIn, "Calculations", SetCalculationLoopControl[#,opts] & /@ GetObjectField[cIn, "Calculations"]]]];


(* --------------------------------------------------------------------------
   Thorn generation (main entry point for non-tensorial thorns)
   -------------------------------------------------------------------------- *)

Options[CreateKrancThorn] = ThornOptions;

DefFn[CreateKrancThorn[groupsOrig_, parentDirectory_, thornName_, opts:OptionsPattern[]] :=
  Module[{configuration, interface, schedule, param, make, cakernel, c},

    InfoMessage[Terse, "Processing arguments to CreateKrancThorn"];

    (* ------------------------------------------------------------------------ 
       Read named arguments and apply nontrivial defaults
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Verifying arguments"];
    cktCheckNamedArgs[{opts}];
    CheckGroups[groupsOrig];
    VerifyGroups[groupsOrig];
    VerifyString[parentDirectory];
    VerifyString[thornName];

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
       "Sources" -> {},
       "Files" -> {}}];

    VerifyString[GetObjectField[c, "Implementation"]];
    VerifyGroupNames[GetObjectField[c, "DeclaredGroups"]];
    VerifyGroupNames[GetObjectField[c, "ODEGroups"]];

    (* ------------------------------------------------------------------------ 
       GenericFD
       ------------------------------------------------------------------------ *)

    c = genericFDProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       Coordinates
       ------------------------------------------------------------------------ *)

    c = coordinatesProcessCode[c, opts];

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
      calcs = Map[Append[#, ThornName -> GetObjectField[c, "Name"]] &, calcs];

      (* TODO: Replace this with the parameter database, and change
         the code in CalculationStencilSize to get the list of integer
         parameters from the parameter database *)
      calcs = Map[Append[#, IntParameters -> OptionValue[IntParameters]] &, calcs];

      SetObjectField[c, "Calculations", calcs]];

    (* ------------------------------------------------------------------------ 
       CaKernel
       ------------------------------------------------------------------------ *)

    c = CaKernelProcessCode[c, opts];

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
       Jacobian
       ------------------------------------------------------------------------ *)

    c = JacobianProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       Conservation Calculations
       ------------------------------------------------------------------------ *)

    c = ConservationCalculationProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       ODEs
       ------------------------------------------------------------------------ *)

    c = ODEProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       LoopControl
       ------------------------------------------------------------------------ *)

    c = loopControlProcessCode[c, opts];
     
    (* ------------------------------------------------------------------------ 
       Declared groups
       ------------------------------------------------------------------------ *)

    c = declaredGroupsProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       MoL
       ------------------------------------------------------------------------ *)

    c = MoLProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       Add selected code options to calculations
       ------------------------------------------------------------------------ *)

    c = distributeOptionsProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       Split calculations according to SplitVars option
       ------------------------------------------------------------------------ *)

    c = splitCalculationsProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       Symmetries
       ------------------------------------------------------------------------ *)

    c = SymmetriesProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       Boundary thorn
       ------------------------------------------------------------------------ *)

    c = CactusBoundaryProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       Add parameter check source file
       ------------------------------------------------------------------------ *)
   
    c = ParamCheckProcessCode[c, opts];

    (* ------------------------------------------------------------------------ 
       Create finite differencing header file
       ------------------------------------------------------------------------ *)

    c = DifferencingProcessCode[c, opts];

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
       Create calculation source files
       ------------------------------------------------------------------------ *)

    InfoMessage[Terse, "Creating calculation source files"];

    Module[{opCounts},
      opCounts=Reap[

    c = JoinObjectField[
      c, "Sources", 
      Join[Map[{Filename -> lookup[#, Name] <> ".cc",
                Contents -> CreateSetterSource[{#}, False, {}, thornName, opts]} &,
               Select[GetObjectField[c, "Calculations"], !CalculationOnDevice[#] &]],
           Map[{Filename -> "CaKernel__"<>lookup[#, Name] <> ".code",
                Contents -> CaKernelCode[#,opts]} &,
               Select[GetObjectField[c, "Calculations"], CalculationOnDevice]]]],

        ProcessOperationCount][[2]];

      c = OperationCountProcessCode[c, Flatten[opCounts], opts]];

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
                   MergeFiles    -> OptionValue[MergeFiles],
                   Sources       -> GetObjectField[c, "Sources"],
                   Files         -> GetObjectField[c, "Files"]};
      InfoMessage[Terse, "Creating thorn"];
      CreateThorn[thornspec]]]];

End[];
EndPackage[];
