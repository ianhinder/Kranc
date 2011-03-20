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

BeginPackage["xTensorKranc`", {"xAct`xTensor`", "xAct`xCore`","xAct`xCoba`"}];


CreateGroupFromTensor::usage = "";
ReflectionSymmetriesOfTensor::usage = "Produce a list of reflection symmetries of a tensor.";
MakeExplicit::usage = "xTensorMakeExplicit[expr] converts an expression x containing abstract indices into one containing components instead."
IncludeCharacter::usage = "IncludeCharacter is an option for makeExplicit which specifies whether the character should also be included in the generated variable names."
TensorCharacterString::usage = "TensorCharacterString[tensor[inds]] returns a string consisting of a sequence of U's and D's representing the character of tensor."
Begin["`Private`"];

(* FIXME: Add support for ManualCartesian attribute *)
TensorCharacterString[t_Symbol?xTensorQ[]] := "Scalar";
TensorCharacterString[t_Symbol?xTensorQ[inds___]] := StringJoin[If[UpIndexQ[#],"U","D"]&/@{inds}];

Options[makeExplicit] = {IncludeCharacter -> False};
SetAttributes[makeExplicit, Listable];
e : makeExplicit[_Plus, opts:OptionsPattern[]] := Distribute[Unevaluated[e]];
makeExplicit[x_Times, opts:OptionsPattern[]] :=Map[makeExplicit[#, opts]&, x];
makeExplicit[Power[x_,p_], opts:OptionsPattern[]] := Power[makeExplicit[x, opts],p];
makeExplicit[x_?NumericQ, OptionsPattern[]] := x;
makeExplicit[x_, {}] := makeExplicit[x];
makeExplicit[t_Symbol?xTensorQ[inds___], OptionsPattern[]] := Module[{indexNumbers,character,indexString},
  indexNumbers=First/@{inds};
  
  If[OptionValue[IncludeCharacter],
    character = TensorCharacterString[t[inds]];
    indexString = StringJoin[character,ToString/@indexNumbers],

    indexString = StringJoin[ToString/@indexNumbers]
  ];
  SymbolJoin[PrintAs[t],Sequence@@indexString]
];

makeExplicit[cd_?CovDQ[ind_][expr_], OptionsPattern[]] := Module[{indexNumbers,character,indexString},
  indexNumbers=First/@{ind};

  If[OptionValue[IncludeCharacter],
    character = TensorCharacterString[cd[ind]];
    indexString = StringJoin[character,ToString/@indexNumbers],

    indexString = StringJoin[ToString/@indexNumbers]
  ];
  SymbolJoin["d",indexString,makeExplicit[expr]]
];

Options[MakeExplicit] = Options[makeExplicit];
MakeExplicit[x_, opts:OptionsPattern[makeExplicit]] :=
  Module[{eqs, options},

  eqs = ComponentArray[TraceBasisDummy[x]];
  options = Evaluate[FilterRules[{opts}, Options[makeExplicit]]];
  If[Length[options]==0,
    makeExplicit[eqs],
    makeExplicit[eqs, options]
  ]
];

(* Compute the reflection symmetries of a tensor *)
ReflectionSymmetriesOfTensor[t_Symbol?xTensorQ[inds__], b_] :=
  Module[{cinds, components, componentIndices, counts},
    (* Get the compoent indices of the basis *)
    cinds = CNumbersOf[b, VBundleOfBasis[b]];

    (* Get a list of components of the tensor t in the basis b *)
    components = Flatten[ComponentArray[ToBasis[b][t[inds]]]];

    (* Get the indices of each component *)
    componentIndices = Map[IndicesOf[b], components];


    (* Count the number of instances of each basis index. *)
    countInds[expr_, basis_, cinds_] := Map[(Count[expr,{#,basis}]+Count[expr,{#,-basis}])&, cinds];
    counts = Map[countInds[#, b, cinds]&, componentIndices];

    (* For each instance, multiply by -1 *)
    Thread[components -> (-1)^counts]
];

ReflectionSymmetriesOfTensor[t_Symbol?xTensorQ[],b_] := {};

(* FIXME: Implement this fully *)
GetTensorAttribute[t_Symbol?xTensorQ[inds___], TensorWeight] := WeightOfTensor[t];

CreateGroupFromTensor[t_Symbol?xTensorQ[inds__], b_] := Module[{tCharString, nInds, tags, vars, group},
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

CheckTensors[expr_] := Validate[expr];

End[];
EndPackage[];







