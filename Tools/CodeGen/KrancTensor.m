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

BeginPackage["KrancTensor`", {"Errors`", "KrancThorn`", "MapLookup`", "KrancGroups`", "Kranc`", $KrancTensorPackage, "ConservationCalculation`", "TensorTools`"}];

CreateKrancThornTT::usage = "Construct a Kranc thorn using tensor expressions.";

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

End[];
EndPackage[];
