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
  {"Differencing`", "Errors`", "Kranc`", "KrancGroups`"}
];

DefineTensor::usage = "DefineTensor[T[a, b, ...]] defines the tensor T with indices a, b, c, ....";
DefineDerivative::usage = "DefineDerivative[pd, nd] registers a symbol pd to be used as a derivative operator, with numerical discretisation nd.";
SetComponents::usage = "SetComponents[T[a, b, ...], v] defines the components of the tensor T to be the values given in the list v."

CreateGroupFromTensor::usage = "CreateGroupFromTensor[T[a, b, ...]] Creates a variable group from the tensor T";
ReflectionSymmetries::usage = "ReflectionSymmetries[T[a, b, ...]] Produces a list of reflection symmetries of the tensor T.";
ExpandComponents::usage = "ExpandComponents[expr] converts an expression x containing abstract indices into one containing components instead."

Euc::usage = "Euc[i, j] represents the Euclidean tensor which is 1 if i=j, and 0 otherwise.";
Eps::usage = "Eps[i, j, k] represents the Levi-Civita alternating tensor";

$KrancIndices = Symbol /@ Complement[CharacterRange["a", "z"], {"h", "r", "x", "y", "z"}];
Do[
  Evaluate[Symbol["l"<>ToString[ind]]] = -ind;
  Evaluate[Symbol["u"<>ToString[ind]]] = ind;
  , {ind, $KrancIndices}];

KrancManifold;
TangentKrancManifold;

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

dimension = 3;
$CVVerbose = False;
Block[{$DefInfoQ = False},
  DefManifold[KrancManifold, dimension, $KrancIndices];
  DefBasis[KrancBasis, TangentKrancManifold, Range[dimension]];
  DefInertHead[dot];
];

(*************************************************************)
(* DefineTensor *)
(*************************************************************)

DefineTensor[t_[inds___], opts___] :=
 Block[{$DefInfoQ = False},
  DefTensor[t[inds], KrancManifold, opts];
  (* Automatically convert abstract and numeric indices to basis indices *)
  t[i___, j_?AbstractIndexQ, k___] := t[i, {j, KrancBasis}, k];
  t[i___, -j_?AbstractIndexQ, k___] := t[i, {-j, -KrancBasis}, k];
  t[i___, j_Integer?Positive, k___] :=
   Module[{slots, basis},
    slots = SlotsOfTensor[t];
    If[Length[{i, j, k}] != Length[slots],
      ThrowError[
        "Tensor " <> ToString[Unevaluated[t[i, j, k]]] <>
        " has an incorrect number of indices"];
    ];
    basis = slots[[Length[{i}] + 1]] /. TangentKrancManifold -> KrancBasis;
    t[i, {j, basis}, k]
  ];
];

(*************************************************************)
(* DefineDerivative *)
(*************************************************************)

(* Define a new derivative operator. This is defined in terms of
   xTensor's partial derivative operator wrapped in an inert head
   to keep track of what the numerical discretisation should be. *)
DefineDerivative[pd_, numderiv_] :=
 Block[{$DefInfoQ = False},
  InfoMessage[InfoFull, "Defining derivative:", pd];
  Module[{nd},
    DefInertHead[nd];
    NumericalDiscretisation[nd] ^= numderiv;

    (* Support both prefix (xTensor) and postfix (Kranc) style derivatives *)
    (* Automatically convert abstract and numeric indices to basis indices *)
    pd[-i_?AbstractIndexQ][t_] := nd[PDKrancBasis[{-i, -KrancBasis}][t]];
    pd[t_, -i_?AbstractIndexQ] := nd[PDKrancBasis[{-i, -KrancBasis}][t]];
    pd[t_, i_Integer?Negative] := nd[PDKrancBasis[{-i, -KrancBasis}][t]];

    (* Distribute the nd wrapper over Plus, e.g. expand
       nd[pd[t[0],0] + pd[t[1],1]] out to nd[pd[t[0],0]] + nd[pd[t[1],1]].
       This is important for xTensor's canonicalizer to work reliably. *)
    e : nd[_Plus] := Distribute[Unevaluated[e]];
  ]
];

SetComponents[t_?xTensorQ[i :(_?BIndexQ ...)], values_] :=
 Module[{},
  AllComponentValues[t[i], values];
  RuleToSet[t];
]

(* FIXME: I'm not sure if we really should be encouraging the use of these *)
Module[{a,b,c},
  {a, b, c} = $KrancIndices[[1;;3]];
  DefineTensor[Eps[-a, -b, -c]];
  SetComponents[Eps[-a, -b, -c], Array[Signature[{##}] &, {3, 3, 3}]];

  DefineTensor[Euc[a, b]];
  SetComponents[Euc[a, b], IdentityMatrix[{3,3}]];
];

(*************************************************************)
(* ExpandComponents *)
(*************************************************************)

krancForm[expr_] := 
  expr //. {
    Scalar[x_] :> NoScalar[x], 
    t_Symbol?xTensorQ[i : (_?CIndexQ ..)] :> 
      SymbolJoin[t, Sequence @@ ToString /@ {i}[[All, 1]]], 
    t_Symbol?xTensorQ[] :> t,
    (* FIXME: Better handling of derivatives *)
    nd_[pd_?CovDQ[i : (_?CIndexQ ..)][t_?xTensorQ[inds__]]] :> NumericalDiscretisation[nd][krancForm[t[inds]], i[[1]]]
  };

SetAttributes[ExpandComponents, Listable];
ExpandComponents[lhs_ -> rhs_] :=
  
  Module[{lhsC, rhsC, inds, rules},
   InfoMessage[InfoFull, "Expanding tensor expression: ", InputForm[lhs] -> InputForm[rhs]];

   (* Check we have a valid tensor equation *)
   (* FIXME: Maybe we should find an alternative to Quiet here *)
   Check[Quiet[Validate[lhs + rhs], Validate::unknown],
     ThrowError["Invalid tensor equation", lhs -> rhs]];

   (* Find the free indices *)
   inds = IndicesOf[Free, BIndex][lhs];

   (* Get a list of components *)
   (* FIXME: Maybe we should find an alternative to Quiet here *)
   Quiet[
     lhsC = ToCanonical[Flatten[{ComponentArray[TraceBasisDummy[lhs], inds]}, 1]];
     rhsC = ToCanonical[Flatten[{ComponentArray[TraceBasisDummy[rhs], inds]}, 1]];
   , ToCanonical::noident];

   (* Pick out the independent components *)
   rules = krancForm[DeleteDuplicates[Thread[lhsC -> rhsC], #1[[1]] == #2[[1]] &]];
   InfoMessage[InfoFull, "Expanded to: ", Map[InputForm, rules, {2}]];
   Sequence @@ rules
];

ExpandComponents[x_] :=
 Module[{},
  (* FIXME: Maybe we should find an alternative to Quiet here *)
  Check[Quiet[Validate[x], Validate::unknown], ThrowError["Invalid tensor expression"]];
  Sequence @@ krancForm[
   DeleteDuplicates[
   (* FIXME: Maybe we should find an alternative to Quiet here *)
    Quiet[ToCanonical[
     Flatten[{ComponentArray[TraceBasisDummy[x]]}, 1]], ToCanonical::noident]]]
];

(*************************************************************)
(* ReflectionSymmetries *)
(*************************************************************)
(* FIXME: Add support for ManualCartesian attribute *)

ReflectionSymmetries[t_Symbol?xTensorQ[inds__]] :=
  Module[{cnums, components, componentIndices, counts},
    (* Get the compoent indices of the basis *)
    InfoMessage[InfoFull, "Getting symmetries of ", InputForm[t[inds]]];
    cnums = CNumbersOf[KrancBasis, VBundleOfBasis[KrancBasis]];

    (* Get a list of components of the tensor t in the basis b *)
    components = DeleteDuplicates[ToCanonical[Flatten[ComponentArray[TraceBasisDummy[t[inds]]]]]];

    (* Get the indices of each component *)
    componentIndices = Map[IndicesOf[CIndex, KrancBasis], components];

    (* Count the number of instances of each basis index. *)
    countInds[expr_, basis_, cinds_] := Map[(Count[expr,{#,basis}]+Count[expr,{#,-basis}])&, cinds];
    counts = Map[countInds[#, KrancBasis, cnums]&, componentIndices];

    (* For each instance, multiply by -1 *)
    Thread[krancForm[components] -> (-1)^counts]
];

ReflectionSymmetries[t_Symbol?xTensorQ[]] := t -> {1,1,1};
ReflectionSymmetries[t_] := t -> {1, 1, 1};
ReflectionSymmetries[x___]:= ThrowError["ReflectionSymmetries error: "<>ToString[x]];

(*************************************************************)
(* CreateGroupFromTensor *)
(*************************************************************)

tensorCharacterString[t_Symbol?xTensorQ[]] := "Scalar";
tensorCharacterString[t_Symbol?xTensorQ[inds___]] := StringJoin[If[UpIndexQ[#],"U","D"]&/@{inds}];

CreateGroupFromTensor[t_Symbol?xTensorQ[inds___]] := Module[{tCharString, nInds, tags, group},
  InfoMessage[InfoFull, "Creating group from tensor " <> ToString[t[inds]]];

  (* Get a string representing the character of the tensor *)
  tCharString = tensorCharacterString[t[inds]];

  (* Check if the tensor is symmetric *)
  nInds = Length[SlotsOfTensor[t]];
  If[SymmetryGroupOfTensor[t] == StrongGenSet[Range[nInds],GenSet[Cycles[Range[nInds]]]], 
        tCharString = tCharString <> "_sym"];

  (* FIXME: Add tensorspecial, cartesianreflectionparities, checkpoint and tensorparity *)
  tags = {"tensortypealias" -> tCharString, "tensorweight" -> WeightOfTensor[t]};

  group = CreateGroup[SymbolName[t] <> "_group", {t[inds]}, {Tags -> tags}];
  group
];

CreateGroupFromTensor[x___]:= ThrowError["Invalid arguments to CreateGroupFromTensor: "<>ToString[x]];

End[];
EndPackage[];

(* Add xAct packages to $ContextPath *)
Do[
  If[!MemberQ[$ContextPath, package], AppendTo[$ContextPath, package]],
  {package, xTensorKranc`Private`newContexts}];
