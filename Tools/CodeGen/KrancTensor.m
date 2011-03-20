(* ::Package:: *)

(* $Id$ *)

(*  Copyright 20042-2010
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
(* Generate Cactus Thorns from a high-level interface  *)
(****************************************************************************)

BeginPackage["KrancTensor`", {"Errors`", "KrancThorn`", "MapLookup`", "KrancGroups`", "Kranc`", "TensorTools`"}];

CreateKrancThornTT::usage = "Construct a Kranc thorn using TensorTools";
CreateGroupFromTensor::usage = "";

Begin["`Private`"];

(* --------------------------------------------------------------------------
   Tensor Tools
   -------------------------------------------------------------------------- *)

CreateKrancThornTT[groups_, parentDirectory_, thornName_, opts___] :=
  Module[{calcs, expCalcs, expGroups, options, derivs, expDerivs, reflectionSymmetries, declaredGroups},
    InfoMessage[Terse, "Processing tensorial arguments"];
    calcs = lookup[{opts}, Calculations];
    derivs = lookupDefault[{opts}, PartialDerivatives, {}];
    Map[CheckCalculationTensors, calcs];
    expCalcs = Map[makeCalculationExplicit, calcs];

    InfoMessage[Info, "Group definitions:", groups];
    VerifyGroups[groups];

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
