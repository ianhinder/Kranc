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
xTensorReflectionSymmetriesOfTensor::usage = "Produce a list of reflection symmetries of a tensor.";


xTensorMakeExplicit::usage = "xTensorMakeExplicit[expr] converts an expression x containing abstract indices into one containing components instead."
IncludeCharacter::usage = "IncludeCharacter is an option for makeExplicit which specifies whether the character should also be included in the generated variable names."
TensorCharacterString::usage = "TensorCharacterString[tensor[inds]] returns a string consisting of a sequence of U's and D's representing the character of tensor."
Begin["`Private`"];

TensorCharacterString[t_Symbol?xTensorQ[inds___]] := If[UpIndexQ[#],"U","D"]&/@{inds};
TensorCharacterString[cd_Symbol?CovDQ[inds___]] := If[UpIndexQ[#],"U","D"]&/@{inds};

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

Options[xTensorMakeExplicit] = Options[makeExplicit];
xTensorMakeExplicit[x_, opts:OptionsPattern[makeExplicit]] :=
  Module[{eqs, options},

  eqs = ComponentArray[TraceBasisDummy[x]];
  options = Evaluate[FilterRules[{opts}, Options[makeExplicit]]];
  If[Length[options]==0,
    makeExplicit[eqs],
    makeExplicit[eqs, options]
  ]
];

xTensorReflectionSymmetriesOfTensor[t_Symbol?xTensorQ[inds__],b_] :=
  Module[{components, componentIndices, counts},
    components = Flatten[ComponentArray[ToBasis[b][t[inds]]]];
    componentIndices = Map[IndicesOf[b][#]&,components];
    counts = Map[{Count[#,{0,-b}],Count[#,{1,-b}],Count[#,{2,-b}]}&,componentIndices];
    Thread[components -> (-1)^counts]
];

xTensorReflectionSymmetriesOfTensor[t_Symbol?xTensorQ[],b_] := {}

(* FIXME: Implement this properly *)
TensorAttributes[t_Symbol?xTensorQ[inds___]] = {TensorWeight -> 1, Symmetries -> {}};

End[];
EndPackage[];







