(* ::Package:: *)

(*  Copyright 2004-2010
    Sascha Husa, Ian Hinder, Christiane Lechner, Barry Wardell

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
(* Wrapper providing tensor support to Kranc (from TensorTools or xTensor)  *)
(****************************************************************************)

$KrancTensorPackage = "TensorToolsKranc`";

BeginPackage["KrancTensor`", {"Errors`", "KrancThorn`", "MapLookup`", "KrancGroups`",
                              "Kranc`", $KrancTensorPackage, "ConservationCalculation`",
                              "TensorTools`", "KrancGroups`", "Differencing`",
                              "Piraha`"}];

CreateKrancThornTT::usage = "Construct a Kranc thorn using tensor expressions.";
CreateKrancThornTT2::usage = "Construct a Kranc thorn using tensor expressions.";

(* FIXME: Move CreateGroupFromTensor here *)

Begin["`Private`"];

(* --------------------------------------------------------------------------
   Tensor Tools
   -------------------------------------------------------------------------- *)

CreateKrancThornTT[groups_, parentDirectory_, thornName_, opts___] :=
  Module[{calcs, expCalcs, expGroups, options, derivs, expDerivs, reflectionSymmetries, declaredGroups, consCalcs, expConsCalcs},
    InfoMessage[Terse, "Creating thorn "<>thornName];
    InfoMessage[Terse, "Processing tensorial arguments"];
    calcs = lookup[{opts}, Calculations];
    consCalcs = lookupDefault[{opts}, ConservationCalculations, {}];
    derivs = lookupDefault[{opts}, PartialDerivatives, {}];
    Map[CheckCalculationTensors, calcs];
    expCalcs = Map[makeCalculationExplicit, calcs];
    expConsCalcs = Map[makeCalculationExplicit, consCalcs];

    InfoMessage[Info, "Group definitions:", groups];
    VerifyGroups[groups];

    expDerivs = Flatten[Map[ExpandComponents,derivs],1];
    expGroups = Map[makeGroupExplicit, groups];
    options = Join[DeleteCases[{opts}, Calculations -> _], {Calculations -> expCalcs}];
    options = mapReplace[options, Shorthands, ExpandComponents[lookup[options,Shorthands,{}]]];
    options = Join[DeleteCases[options, ConservationCalculations -> _],
      {ConservationCalculations -> expConsCalcs}];
    options = Join[DeleteCases[options, PartialDerivatives -> _], {PartialDerivatives -> expDerivs}];

    declaredGroups = lookupDefault[{opts}, DeclaredGroups, {}];
    odeGroups = lookupDefault[{opts}, ODEGroups, {}];
    evolutionTimelevels = lookupDefault[{opts}, EvolutionTimelevels, 3];
    defaultEvolutionTimelevels = lookupDefault[{opts}, DefaultEvolutionTimelevels, evolutionTimelevels];
    InfoMessage[Info, "Declared groups: " <> ToString[declaredGroups]];
    InfoMessage[Info, "ODE groups: " <> ToString[odeGroups]];
    InfoMessage[Terse, "Computing reflection symmetries"];
    reflectionSymmetries = computeReflectionSymmetries[declaredGroups, groups];
    InfoMessage[Info, "Reflection symmetries: ", reflectionSymmetries];

    InfoMessage[Terse, "Creating (component-based) Kranc thorn"];

    (* It is necessary to include the KrancThorn context here due to some annoying Needs[] dependency issue *)
    KrancThorn`CreateKrancThorn[expGroups, parentDirectory, thornName,
      Apply[Sequence, options], ReflectionSymmetries -> reflectionSymmetries]];

computeReflectionSymmetries[declaredGroups_, groups_] :=
  Module[{variables, syms},
    variables = variablesFromGroups[declaredGroups, groups];
    syms = Flatten[Map[ReflectionSymmetries, variables], 1];
    syms];

makeCalculationExplicit[calc_] :=
  mapValueMapMultiple[calc, 
    {Shorthands -> ExpandComponents,
     CollectList -> ExpandComponents,
     Equations -> ExpandComponents,
     PrimitiveEquations -> MakeExplicit,
     ConservedEquations -> MakeExplicit,
     Primitives -> MakeExplicit}];

(* DeleteDuplicates is not available in Mathematica before version 7 *)
deleteDuplicates[l_] :=
 Tally[Join@l][[All, 1]];

makeGroupExplicit[g_] :=
  Module[{variables, newVariables, newGroup},
    variables = groupVariables[g];
    newVariables = deleteDuplicates[ExpandComponents[variables]];
    newGroup = SetGroupVariables[g, newVariables];
    newGroup];

printStruct[x:h_[args___], indent_:0] :=
  Module[
    {ind},
    ind = StringJoin@ConstantArray[" ",indent];
    If[Length[x] === 0, 
       Print[ind,h,"[]"],
       (* else *)
       If[And@@Map[StringQ,x],
          Print[ind,h,"[",StringJoin@Riffle[List@@x,","],"]"],
          (* else *)
          Print[ind,h,"["];
          Scan[printStruct[#,indent+2]&, x];
          Print[StringJoin@ConstantArray[" ",indent+StringLength[h]],"]"]]]];

printStruct[x_String, indent_:0] :=
  Module[
    {},
    Print[StringJoin@ConstantArray[" ",indent],x]];

DefFn[
  InheritedGroups[imp_String] :=
  Module[
    {interfaceFiles, arrangements},

    arrangements = FileNameJoin[{KrancDirectory,"..","..","arrangements"}];
    interfaceFiles = Take[FileNames[FileNameJoin[{arrangements,"*","*","interface.ccl"}]],All];

    Print[StringForm["Parsing interface.ccl files in `1`", FileNameJoin[{KrancDirectory,"..","..","arrangements"}]]];

    interfaces = Catch[(WriteString["stdout","."]; # -> Parse["intrfccl.peg", "intr", #]),_,
                       Function[{val,tag},#->val]] & /@ interfaceFiles; Print[];

    badInterfaces = Select[interfaces, (Head[#[[2]]] === KrancError) &];

    (* dir = "failed-interface-files"; *)
    (* CreateDirectory[dir]; *)
    (* Map[Print[CopyFile[#, FileNameJoin[{dir,FileNameSplit[#][[-2]]}]<>"_interface.ccl"]] &, Map[First, badInterfaces]]; *)

    goodInterfaces = Select[interfaces, (!(Head[#] === KrancError)) &];

    If[Length[badInterfaces] > 0,
       InfoMessage[Terse, StringForm["Warning: Could not parse the interface files for thorns `1`",StringJoin@Riffle[Map[FileNameSplit[#[[1]]][[-2]] &,badInterfaces]," "]]]];

    (* Map[PrintError,badInterfaces]; *)

    If[Length[badInterfaces] > 0,
       Print[StringForm["`1`/`2` interface.ccl files could not be parsed", 
                        Length[badInterfaces], Length[interfaces]]]];

    interfaces2 = goodInterfaces //. {("startIndex" -> _) :> Sequence[],
                                      ("endIndex" -> _) :> Sequence[]};

    stringRules = {(a_-> XMLObject["Document"][_,data_,___]) :> a->data, 
                   XMLElement[s_,_,c_] :> s@@c};

    interfaces3 = interfaces2 //. stringRules;

    structGet[expr_, path_List] :=
    Module[
      {results, pat,x},
      pat = Fold[#2[___, #1, ___] &, x:Last[path], Reverse@Drop[path,-1]];

      (* pat = "intr"[___, "entries"[___, "GROUP_VARS"[___], ___], ___]; *)
      (* pat = "intr"[___, "entries"[___, "GROUP_VARS"[x__], ___], ___]; *)

      (* Print["pattern = ", pat//InputForm]; *)
      (* Print["expression = ", expr//InputForm]; *)
      results = Cases[{expr}, pat :> {x}];

      If[Length[results] === 0, 
         ThrowError[ToString@StringForm["Cannot find `1` in expression `2`", path, expr]]];

      If[Length[results] > 1,
         ThrowError[ToString@StringForm["More than one instance of `1` in expression `2`", 
                                        path, expr]]];
      results[[1]]];

    structMatchQ[expr_, path_List] :=
    Module[{result},
           result=MatchQ[expr, Fold[#2[___, #1, ___] &, Last[path], Reverse@Drop[path,-1]]];
          result];

    thorns = Select[interfaces3, structMatchQ[#[[2]], {"intr","entries","HEADER","IMPLEMENTS","name",imp}] &];

    (* Print["thorns = ", Map[First,thorns]]; *)

    If[Length[thorns] > 1,
       ThrowError[ToString@StringForm[
         "Found more than one thorn (`1`) providing implementation `2`.  Kranc does not yet support this.", Map[FileNameSplit[First[#]][[-2]] &, thorns], imp]]];

    If[Length[thorns] === 0,
       ThrowError[ToString@StringForm[
         "Cannot find any thorn providing implementation `1` in `2`.", imp, arrangements]]];

    (* printStruct[thorns[[1,2]]]; *)


    groups = Cases[thorns[[1,2]], "GROUP_VARS"[___], Infinity];
    
    groups = Select[groups, Cases[#,"gtype"["GF"]] =!= {} &];
    
    (* Print["groups = "]; *)
    (* Map[Print, groups]; *)

    groupFromStruct[str_] :=
    {Cases[str,"name"[n_] :> n][[1]], Cases[str,"VARS"[vs___] :> Map[First,{vs}]][[1]]};

    krancGroups = Map[groupFromStruct, groups];

    krancGroups]];



Options[CreateKrancThornTT2] = ThornOptions;

DefFn[CreateKrancThornTT2[thornName_String, opts:OptionsPattern[]] :=
  Module[
    {groups, pderivs, opts2, fdOrder = Global`fdOrder, PDstandard = Global`PDstandard},
    groups = Map[CreateGroupFromTensor, OptionValue[Variables]];

    inheritedGroups = Join@@Map[InheritedGroups, OptionValue[InheritedImplementations]];

    Print["inheritedGroups = ", inheritedGroups];

    pderivs =
    Join[OptionValue[PartialDerivatives],
         {
           PDstandard[i_] ->
           StandardCenteredDifferenceOperator[1,fdOrder/2,i],
           PDstandard[i_, i_] ->
           StandardCenteredDifferenceOperator[2,fdOrder/2,i],
           PDstandard[i_, j_] ->
           StandardCenteredDifferenceOperator[1,fdOrder/2,i] StandardCenteredDifferenceOperator[1,fdOrder/2,j]
         }];

    opts2 = mapReplaceAdd[{opts}, PartialDerivatives, pderivs];

    opts2 = mapReplaceAdd[opts2, IntParameters, Join[lookup[opts2,IntParameters,{}],
                                                     {{Name -> fdOrder, Default -> 2, AllowedValues -> {2, 4}}}]];

    opts2 = opts2 /. PD -> PDstandard;

    CreateKrancThornTT[groups,OptionValue[ParentDirectory],thornName,
                       DeclaredGroups -> Map[groupName, groups],
                       Sequence@@opts2]]];

End[];
EndPackage[];
