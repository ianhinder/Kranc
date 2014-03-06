(* ::Package:: *)

(*  Copyright 2010 Barry Wardell

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
*)

BeginPackage["xTensorKranc`",
  {"Differencing`", "Kranc`", "KrancGroups`"}
];

CreateGroupFromTensor::usage = "CreateGroupFromTensor[T[a, b, ...]] Creates a variable group from the tensor T";
DefineTensor::usage = "DefineTensor[T[a, b, ...]] defines the tensor T with indices a, b, c, ....";
ReflectionSymmetries::usage = "ReflectionSymmetries[T[a, b, ...]] Produces a list of reflection symmetries of the tensor T.";
ExpandComponents::usage = "ExpandComponents[expr] converts an expression x containing abstract indices into one containing components instead."

$KrancIndices = xAct`xTensor`IndexRange[a, l];

Begin["`Private`"];

(*************************************************************)
(* Set up xTensor *)
(*************************************************************)
contexts = $ContextPath;
Block[{Print},
  Needs["xAct`xTensor`"];
  Needs["xAct`xCore`"];
  Needs["xAct`xCoba`"];
];
newContexts = Complement[$ContextPath, contexts];
Protect[$KrancIndices];

(* FIXME: Add support for ManualCartesian attribute *)
TensorCharacterString[t_Symbol?xTensorQ[]] := "Scalar";
TensorCharacterString[t_Symbol?xTensorQ[inds___]] := StringJoin[If[UpIndexQ[#],"U","D"]&/@{inds}];

Options[makeExplicit] = {IncludeCharacter -> False};
SetAttributes[makeExplicit, Listable];
e : makeExplicit[_Plus, opts:OptionsPattern[]] := Distribute[Unevaluated[e]];
makeExplicit[x_Times, opts:OptionsPattern[]] := Map[makeExplicit[#, opts]&, x];
makeExplicit[Power[x_,p_], opts:OptionsPattern[]] := Power[makeExplicit[x, opts],p];
makeExplicit[x_?NumericQ, opts:OptionsPattern[]] := x;
makeExplicit[x_, {}, opts:OptionsPattern[]] := makeExplicit[x];
makeExplicit[t_Symbol?xTensorQ[inds___], opts:OptionsPattern[]] := Module[{indexNumbers,character,indexString},
  indexNumbers=First/@{inds};
  
  If[OptionValue[IncludeCharacter],
    character = TensorCharacterString[t[inds]];
    indexString = StringJoin[character,ToString/@indexNumbers],
    indexString = StringJoin[ToString/@indexNumbers]
  ];
  SymbolJoin[PrintAs[t],Sequence@@indexString]
];

makeExplicit[x_, opts:OptionsPattern[]] := x;

makeExplicit[(cd_?CovDQ)[ind_][expr_], opts:OptionsPattern[]] := Module[{indexNumbers},
  indexNumbers=First/@{ind};

  Global`PDstandard2nd[makeExplicit[expr, opts], Sequence@@indexNumbers]
];

Options[ExpandComponents] = Options[ExpandComponents];

ExpandComponents[x_Rule, opts:OptionsPattern[makeExplicit]] := Thread[ExpandComponents[x[[1]], opts] -> ExpandComponents[x[[2]], opts]];
ExpandComponents[dot[x_], opts:OptionsPattern[makeExplicit]] := dot/@ExpandComponents[x, opts];
ExpandComponents[x_List, opts:OptionsPattern[makeExplicit]] := Flatten[Map[ExpandComponents[#, opts]&, x], 1];
ExpandComponents[x_, opts:OptionsPattern[makeExplicit]] :=
  Module[{eqs, options},

  eqs = ComponentArray[TraceBasisDummy[x]];
  options = Evaluate[FilterRules[{opts}, Options[makeExplicit]]];
  If[Length[options]==0,
    makeExplicit[eqs],
    makeExplicit[eqs, options]
  ]
];


(* Compute the reflection symmetries of a tensor *)
ReflectionSymmetries[t_Symbol?xTensorQ[inds__]] :=
  Module[{b=Global`Euclidean, cnums, components, componentIndices, counts},
    (* Get the compoent indices of the basis *)
    cnums = CNumbersOf[b, VBundleOfBasis[b]];

    (* Get a list of components of the tensor t in the basis b *)
    components = Flatten[ComponentArray[ToBasis[b][t[inds]]]];

    (* Get the indices of each component *)
    componentIndices = Map[IndicesOf[b], components];

    (* Count the number of instances of each basis index. *)
    countInds[expr_, basis_, cinds_] := Map[(Count[expr,{#,basis}]+Count[expr,{#,-basis}])&, cinds];
    counts = Map[countInds[#, b, cnums]&, componentIndices];

    (* For each instance, multiply by -1 *)
    Thread[ExpandComponents[t[inds]] -> (-1)^counts]
];

ReflectionSymmetries[t_Symbol?xTensorQ[]] := t -> {1,1,1};
ReflectionSymmetries[t_] := t -> {1, 1, 1};

(* FIXME: Implement this fully *)
GetTensorAttribute[t_Symbol?xTensorQ, TensorWeight] := WeightOfTensor[t];

CreateGroupFromTensor[t_Symbol?xTensorQ[inds__]] := Module[{tCharString, nInds, tags, vars, group},
  InfoMessage[InfoFull, "Creating group from tensor with kernel " <> SymbolName[t] <> " and indices " <> ToString[{inds}]];

  (* Get a string representing the character of the tensor *)
  tCharString = TensorCharacterString[t[inds]];
  InfoMessage[InfoFull, "Tensor character string: ", tCharString];

  (* Check if the tensor is symmetric *)
  nInds = Length[SlotsOfTensor[t]];
  If[SymmetryGroupOfTensor[t] == StrongGenSet[Range[nInds],GenSet[Cycles[Range[nInds]]]], 
        tCharString = tCharString <> "_sym"];

  (* FIXME: Add tensorspecial, cartesianreflectionparities  and tensorparity *)
  tags = {"tensortypealias" -> tCharString, "tensorweight" -> GetTensorAttribute[t, TensorWeight]};

  vars = If[nInds == 0, {t}, {t[inds]}];
  group = CreateGroup[SymbolName[t] <> "_group", vars, {Tags -> tags}];
  Return[group]
];

ReflectionSymmetries[x___]:= ThrowError["ReflectionSymmetries error: "<>ToString[x]];
CreateGroupFromTensor[x___]:= ThrowError["CreateGroupFromTensor error: "<>ToString[x]];

CheckTensors[expr_] := Validate[expr];

End[];
EndPackage[];

(* Add xAct packages to $ContextPath *)
Do[
  If[!MemberQ[$ContextPath, package], AppendTo[$ContextPath, package]],
  {package, xTensorKranc`Private`newContexts}];
