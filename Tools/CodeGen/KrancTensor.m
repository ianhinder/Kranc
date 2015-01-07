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

If[!ValueQ[KrancTensor`$KrancTensorPackage],
  KrancTensor`$KrancTensorPackage = "TensorTools";
  KrancTensor`$KrancTensorNeeds = Sequence["TensorToolsKranc`", "TensorTools`"];,
  KrancTensor`$KrancTensorNeeds = "xTensorKranc`";
];

BeginPackage["KrancTensor`", {"Errors`", "KrancThorn`", "MapLookup`", "KrancGroups`",
                              "Kranc`", KrancTensor`$KrancTensorNeeds, "ConservationCalculation`",
                              "KrancGroups`", "Differencing`",
                              "Piraha`", "ScriptOutput`"}];

CreateKrancThornTT::usage = "Construct a Kranc thorn using tensor expressions.";
CreateKrancThornTT2::usage = "Construct a Kranc thorn using tensor expressions.";

(* FIXME: Move CreateGroupFromTensor here *)

printStruct;

Begin["`Private`"];

(* --------------------------------------------------------------------------
   Tensor Tools
   -------------------------------------------------------------------------- *)

Options[CreateKrancThornTT] = ThornOptions;

CreateKrancThornTT[groups_, parentDirectory_, thornName_, opts:OptionsPattern[]] :=
  Module[{calcs, expCalcs, expGroups, options, derivs, expDerivs, reflectionSymmetries, declaredGroups, consCalcs, expConsCalcs, intParams, realParams, pds, nds},
    InfoMessage[Terse, "Creating thorn "<>thornName];
    InfoMessage[Terse, "Processing tensorial arguments"];
    calcs = lookup[{opts}, Calculations];
    consCalcs = lookupDefault[{opts}, ConservationCalculations, {}];
    derivs = lookupDefault[{opts}, PartialDerivatives, {}];

    If[$KrancTensorPackage === "xTensor",
      intParams = lookupDefault[{opts}, IntParameters, {}] /. {___, Name -> name_, ___} :> name;
      realParams = lookupDefault[{opts}, RealParameters, {}] /. {___, Name -> name_, ___} :> name;
      Scan[DefineParameter, intParams];
      Scan[DefineParameter, realParams];
    ];

    nds = Union[derivs[[All,1,0]]];
    Which[
    $KrancTensorPackage === "TensorTools",
      Map[DefineDerivative, nds];,
    $KrancTensorPackage === "xTensor",
      pds = Unique /@ nds;
      MapThread[DefineDerivative, {pds, nds}];
      {calcs, consCalcs} = {calcs, consCalcs} /. Thread[nds -> pds];
    ];

    Map[CheckCalculationTensors, calcs];

    If[OptionValue[GenerateScript], WriteScript[groups, parentDirectory, thornName, opts]];
    (* Print["Exiting (debug)"]; *)
    (* Quit[]; *)

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
  Module[{splice},
  mapValueMapMultiple[calc, 
    {Shorthands -> ExpandComponents,
     CachedVariables -> ExpandComponents,
     SplitBy -> (Map[Function[x,If[ListQ[x], ExpandComponents[x], splice@ExpandComponents[{x}]]], #] /. (splice[{y___}] -> Sequence@@y) &),
     CollectList -> ExpandComponents,
     Equations -> ExpandComponents,
     PrimitiveEquations -> MakeExplicit,
     ConservedEquations -> MakeExplicit,
     Primitives -> MakeExplicit}]];

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

DefFn[
  printStruct[x_String, indent_:0] :=
  Module[
    {},
    Print[StringJoin@ConstantArray[" ",indent],x]]];

(* Structure functions *)

DefFn[
  structGet[expr_, path_List] :=
  Module[
    {results, pat,x},
    pat = Fold[#2[___, #1, ___] &, x:Last[path], Reverse@Drop[path,-1]];
    results = Cases[{expr}, pat :> x];

    If[Length[results] === 0, 
       ThrowError[ToString@StringForm["Cannot find `1` in expression `2`", path, expr]]];

    results[[1]]]];

DefFn[
  structMatchQ[expr_, path_List] :=
  Module[{result},
         result=MatchQ[expr, Fold[#2[___, #1, ___] &, Last[path], Reverse@Drop[path,-1]]];
         result]];

(* Cactus tree functions *)

DefFn[
  getCactusDirectory[] :=
  FileNameJoin[{KrancDirectory,"..",".."}]];

DefFn[
  getAllInterfaceFiles[] :=
  FileNames[FileNameJoin[{getCactusDirectory[],"arrangements","*","*","interface.ccl"}]]];

DefFn[
  getAllThorns[] :=
  Map[FileNameDrop[#,-1] &, getAllInterfaceFiles[]]];

(* Groups and variables functions *)

(*

Get groups provided by an implementation:
  If there is a thorn with the same name as the implementation
  AND the implementation of this thorn is the one we want,
    Return the groups of this thorn
  else
    Scan all the interface files and find the thorn(s) providing this implementation
    Warn if there is not exactly one
    Return the groups of this thorn

*)

DefFn[
thornOfImplementation[imp_String] :=
  Module[
    {impThorns = Select[getAllThorns[], StringMatchQ[FileNameTake[#,-1],imp,IgnoreCase->True] &],
     impThorns2},
    If[impThorns =!= {} && StringMatchQ[implementationOfThorn[impThorns[[1]]], imp, IgnoreCase->True],
       impThorns[[1]],
       (* else *)
       Module[
         {impThorns2 = Select[getAllThorns[], StringMatchQ[implementationOfThorn[#], imp, IgnoreCase -> True] &]},
         Switch[Length[impThorns2],
                0, ThrowError[ToString@StringForm["Cannot find a thorn with implementation `1`", imp]],
                1, impThorns2[[1]],
                _, Print[StringForm[
                  "WARNING: Found more than one thorn (`1`) providing implementation `2`.  Using `3`.",
                  Map[FileNameTake[#,-1] &, impThorns], imp, FileNameTake[impThorns[[1]],-1]]];
                impThorns2[[1]]]]]]];

DefFn[
  implementationOfThorn[thorn_String] :=
  structGet[interfaceTreeOfThorn[thorn],
            {"intr","entries","HEADER","IMPLEMENTS","name",_}]];

DefFn[
  interfaceTreeOfThorn[thorn_String] :=
  parseInterfaceCCL[FileNameJoin[{thorn,"interface.ccl"}]]];

DefFn[
  parseInterfaceCCL[interfaceFile_String] :=
  (ParsePEG["intrfccl.peg", "intr", interfaceFile] //.
   {("startIndex" -> _) :> Sequence[],
    ("endIndex" -> _) :> Sequence[]} )//.
  {(XMLObject["Document"][_,data_,___]) :> data, 
   XMLElement[s_,_,c_] :> s@@c}];

DefFn[
  gfGroupVarsOfInterfaceTree[tree_] :=
  Select[
    Cases[tree, "GROUP_VARS"[___], Infinity],
    Cases[#, "gtype"["GF"]] =!= {} &]];

unquote[s_String] := StringDrop[StringDrop[s,-1],1];

unquote["squote"[s_]] := StringDrop[StringDrop[s,-1],1];
unquote["dquote"[s_]] := StringDrop[StringDrop[s,-1],1];

tensorType[s_String] :=
  Module[
    {symmetric = StringMatchQ[s, __~~"_sym"],
     s1 = StringReplace[s, x__~~"_sym" -> x]},
    If[StringTake[s,1] === "4", ThrowError["Kranc does not yet support 4D tensors"]];
    If[StringMatchQ[s1,"scalar",IgnoreCase->True],
       {"",symmetric},
       {ToLowerCase@s1,symmetric}]];

tagToOptions[s_] :=
  Module[
    {tag,val},
    {tag,val} = StringSplit[s,"="];
    Switch[
      tag,
      "tensorweight", {TensorWeight -> val},
      "tensortypealias", {TensorType -> tensorType@unquote[val]},
      _, {}]];


groupOptionsFromTags[tags_] :=
  If[Length[tags] === 0, {},
     (* Print["tags = ", tags//InputForm]; *)
     (* Print["tags split = ", InputForm@StringSplit[tags[[1]]]]; *)
     Flatten[tagToOptions/@StringSplit[tags[[1]]],1]];

DefFn[
  groupStructureOfGroupVar[groupVar_,imp_String] :=
  {Cases[groupVar,"name"[n_] :> imp<>"::"<>n][[1]], Cases[groupVar,"VARS"[vs___] :> Map[First,{vs}]][[1]], 
   Sequence@@groupOptionsFromTags[Cases[Print[groupVar//InputForm]; groupVar,"tags"[tags_] :> unquote[tags]]]}];

DefFn[
  InheritedGroups[imp_String] :=
  Map[groupStructureOfGroupVar[#,imp] &,
      gfGroupVarsOfInterfaceTree[interfaceTreeOfThorn[thornOfImplementation[imp]]]]];

Options[CreateKrancThornTT2] = ThornOptions;

DefFn[CreateKrancThornTT2[thornName_String, opts:OptionsPattern[]] :=
  Module[
    {groups, pderivs, opts2, fdOrder = Global`fdOrder, PDstandard = Global`PDstandard},
    groups = Map[CreateGroupFromTensor, OptionValue[Variables]];
    groups = Join[groups,OptionValue[DeclaredGroups]];

    Print["Searching for inherited groups..."];
    inheritedGroups = Join@@Map[InheritedGroups, OptionValue[InheritedImplementations]];
    Print["Done."];

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

    opts2 = opts2 /. (DeclaredGroups-> ar__)->DeclaredGroups->Map[groupName,groups];

    CreateKrancThornTT[groups,OptionValue[ParentDirectory],thornName,
                       Sequence@@opts2]]];

End[];
EndPackage[];
