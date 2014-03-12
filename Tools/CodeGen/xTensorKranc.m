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
DefineConnection::usage = "";
DefineParameter::usage = "DefineParameter[p] registers a symbol p to be used as a constant parameter.";
SetComponents::usage = "SetComponents[T[a, b, ...], v] defines the components of the tensor T to be the values given in the list v."
MatrixInverse::usage = "";
MatrixOfComponents::usage = "";

SetTensorAttribute::usage = "";
HasTensorAttribute::usage = "";
GetTensorAttribute::usage = "";
TensorParity;
TensorWeight;
TensorSpecial;
TensorManualCartesianParities;
Checkpoint;
AntiSymmetrize;
AssertSymmetricDecreasing;
AssertSymmetricIncreasing;
CreateGroupFromTensor::usage = "CreateGroupFromTensor[T[a, b, ...]] Creates a variable group from the tensor T";
ReflectionSymmetries::usage = "ReflectionSymmetries[T[a, b, ...]] Produces a list of reflection symmetries of the tensor T.";
ExpandComponents::usage = "ExpandComponents[expr] converts an expression x containing abstract indices into one containing components instead."

Euc::usage = "Euc[i, j] represents the Euclidean tensor which is 1 if i=j, and 0 otherwise.";
EucUD::usage = "EucUD[i, -j] represents the Euclidean tensor which is 1 if i=j, and 0 otherwise.";
EucDU::usage = "EucDU[-i, j] represents the Euclidean tensor which is 1 if i=j, and 0 otherwise.";
EucDD::usage = "EucDD[-i, -j] represents the Euclidean tensor which is 1 if i=j, and 0 otherwise.";
Eps::usage = "Eps[i, j, k] represents the Levi-Civita alternating tensor";

$KrancIndices = Symbol /@ Complement[CharacterRange["a", "z"], {"h", "r", "t", "x", "y", "z"}];
Do[{Symbol["l"<>ToString[ind]], Symbol["u"<>ToString[ind]]}, {ind, CharacterRange["a", "z"]}];

KrancManifold;
TangentKrancManifold;
KrancBasis;

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
  DefInertHead[KrancSign];
  DefInertHead[KrancAbs];
  DefInertHead[StepFunction];
];

Do[
 Module[{newind},
  newind = DummyIn[TangentKrancManifold];
  Evaluate[Symbol["l"<>ToString[ind]]] = -newind;
  Evaluate[Symbol["u"<>ToString[ind]]] = newind;
 ],
 {ind, CharacterRange["a", "z"]}
];

(*************************************************************)
(* DefineTensor *)
(*************************************************************)

DefineTensor[s_[inds___], opts___] :=
 Block[{$DefInfoQ = False}, Module[{symbolName, t},
  InfoMessage[InfoFull, "Defining tensor: " <> SymbolName[s]];

  symbolName = ToString[s];
  If[AbstractIndexQ[s], UndefAbstractIndex[s]];
  t = Symbol[symbolName];
  s = t;

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

  (* Define components which are related by symmetries *)
  SetComponents[t[inds], ToCanonical[ComponentArray[t[inds]]]];
]];

(* Scalars *)
KrancScalarQ[_] := False;

DefineTensor[s_, opts___] :=
 Block[{$DefInfoQ = False}, Module[{symbolName, t},
  InfoMessage[InfoFull, "Defining scalar: " <> SymbolName[s]];

  symbolName = ToString[s];
  If[AbstractIndexQ[s], UndefAbstractIndex[s]];
  t = Symbol[symbolName];
  s = t;

  DefTensor[t[], KrancManifold, opts];
  t[i__] := ThrowError["Tensor " <> ToString[SymbolName[t][i]] <>
                       " should not have indices as it has been defined as a scalar."];
  KrancScalarQ[t] = True;
]];

(*************************************************************)
(* SetTensorAttribute *)
(*************************************************************)

SetTensorAttribute[t_, attr_, val_] :=
  t /: KrancTensorAttribute[t, attr] = val;

HasTensorAttribute[t_, attr_] :=
  ValueQ[KrancTensorAttribute[t, attr]];

GetTensorAttribute[t_, attr_] :=
 Module[{},
  If[!HasTensorAttribute[t, attr],
    ThrowError["Tensor " <> ToString[t] <> " does not have a " <> ToString[attr] <> " attribute."];
  ];
  KrancTensorAttribute[t, attr]
];

(*************************************************************)
(* AntiSymmetrize *)
(*************************************************************)

AntiSymmetrize[expr_, a_, b_] /; Quiet[Check[(Validate[expr]; True), False, Validate::unknown]] :=
 Antisymmetrize[expr, {a, b}];

(*************************************************************)
(* AssertSymmetricIncreasing / AssertSymmetricDecreasing *)
(*************************************************************)

AssertSymmetricIncreasing[t_?xTensorQ[inds__], syminds__] := 
 Module[{ainds, asymInds, symSlots},
  {ainds, asyminds} = {{inds}, {syminds}} /. {i_, (KrancBasis | -KrancBasis)} :> i;
  symSlots = Position[ainds, #][[1, 1]] & /@ asyminds;
  SymmetryGroupOfTensor[t] ^= Symmetric[symSlots, Cycles];

  (* Define components which are related by symmetries *)
  SetComponents[t[inds], ToCanonical[ComponentArray[t[inds]]]];
];

AssertSymmetricIncreasing[t_?xTensorQ[inds__]] := 
  AssertSymmetricIncreasing[t[inds], inds];

AssertSymmetricDecreasing := 
  ThrowError["AssertSymmetricDecreasing is no longer supported"];

(*************************************************************)
(* DefineParameter *)
(*************************************************************)

DefineParameter[p_] := 
 Block[{$DefInfoQ = False}, Module[{symbolName, s},
  InfoMessage[InfoFull, "Defining parameter: "<> SymbolName[p]];

  symbolName = ToString[p];
  If[AbstractIndexQ[p], UndefAbstractIndex[p]];
  s = Symbol[symbolName];
  p = s;

  If[!ConstantSymbolQ[s], DefConstantSymbol[s]];
]];

(*************************************************************)
(* DefineDerivative *)
(*************************************************************)

(* Define a new derivative operator. This is defined in terms of
   xTensor's partial derivative operator wrapped in an inert head
   to keep track of what the numerical discretisation should be. *)
DefineDerivative[pd_, numderiv_] :=
 Block[{$DefInfoQ = False},
  InfoMessage[InfoFull, "Defining derivative: " <> SymbolName[pd]];
  Module[{nd},
    DefInertHead[nd];
    NumericalDiscretisation[nd] ^= numderiv;

    (* Support both prefix (xTensor) and postfix (Kranc) style derivatives *)
    (* Automatically convert abstract and numeric indices to basis indices *)
    pd[t_, i : (-_?AbstractIndexQ) ..] := nd[Fold[PDKrancBasis[{#2, -KrancBasis}][#1] &, t, {i}]];
    pd[-i_?AbstractIndexQ][t_] := nd[PDKrancBasis[{-i, -KrancBasis}][t]];
    pd[t_, i_Integer?Negative] := nd[PDKrancBasis[{-i, -KrancBasis}][t]];
    pd[t_, i_Integer?Positive] := nd[PDKrancBasis[{i, -KrancBasis}][t]];

    (* Distribute the nd wrapper over Plus, e.g. expand
       nd[pd[t[0],0] + pd[t[1],1]] out to nd[pd[t[0],0]] + nd[pd[t[1],1]].
       This is important for xTensor's canonicalizer to work reliably. *)
    e : nd[_Plus] := Distribute[Unevaluated[e]];
  ]
];

(*************************************************************)
(* DefineConnection *)
(*************************************************************)

DefineConnection[CD_, pd_, G_] :=
 Block[{$DefInfoQ = False}, Module[{basis, christoffel, T, a, b, c},
  InfoMessage[InfoFull, "Defining covariant derivative " <>
              SymbolName[CD] <> " with connection " <> SymbolName[G]];

  (* The connection must be a rank-3 tensor with the first index up *)
  If[SlotsOfTensor[G] =!= {TangentKrancManifold, -TangentKrancManifold, -TangentKrancManifold},
     ThrowError["Cannot use " <> SymbolName[G] <>
                " as a connection as it has an incorrect tensor character."]];

  (* Define the new covariant derivative operator and an associated basis *)
  {a, b, c} = $KrancIndices[[1 ;; 3]];
  basis = SymbolJoin[CD, KrancBasis];
  DefBasis[basis, TangentKrancManifold, Range[DimOfVBundle[TangentKrancManifold]]];
  DefCovD[CD[-a], TangentKrancManifold];

  (* Make sure xTensor actually defines the Christoffel symbol by using CD on a tensor *)
  DefTensor[T[a], TangentKrancManifold];
  ToBasis[KrancBasis][CD[-{a, basis}][T[{a, basis}]]];

  (* Whenever the covariant derivative is encountered, automatically convert it to
     partial derivatives and Christoffel symbols*)
  (* FIXME: Make this work with more than 2 derivatives *)
  CD[t:(_?xTensorQ[___]), i : (-_?AbstractIndexQ) ..] :=
   Module[{exprInBasis},
    (* Convert to xTensor notation with indices in basis *)
    exprInBasis = Fold[CD[{#2, -basis}][#1] &, t /. KrancBasis -> basis, {i}];
    
    (* Do the basis transformation to the Kranc basis, introducing Christoffel symbols *)
    FixedPoint[ToBasis[KrancBasis], exprInBasis] /.
     {CD[{-k_, -KrancBasis}][CD[{-j_, -KrancBasis}][T_[inds___]]] :> pd[T[inds], -j, -k],
      CD[{-j_, -KrancBasis}][T_[inds___]] :> pd[T[inds], -j]}
  ];

  (* Define the components the Christoffel symbol to be given by G *)
  christoffel = SymbolJoin["ChristoffelPD", CD, "KrancBasisPDKrancBasis"];

  SetComponents[
    christoffel[{a, KrancBasis}, -{b, KrancBasis}, -{c, KrancBasis}],
    ComponentArray[G[a, -b, -c]]];
]];

SetComponents[t_?xTensorQ[i :(_?BIndexQ ...)], values_] :=
 Module[{},
  SetToRule[t];
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

  DefineTensor[EucUD[a, -b]];
  SetComponents[EucUD[a, -b], IdentityMatrix[{3,3}]];

  DefineTensor[EucDU[-a, b]];
  SetComponents[EucDU[-a, b], IdentityMatrix[{3,3}]];

  DefineTensor[EucDD[-a, -b]];
  SetComponents[EucDD[-a, -b], IdentityMatrix[{3,3}]];
];

(*************************************************************)
(* MatrixInverse *)
(*************************************************************)

MatrixInverse[t_?xTensorQ] :=
 Module[{inverse, tensorCharacter, rank, a, b, kba, kbb},
  inverse = SymbolJoin["MatrixInverse", t];

  If[xTensorQ[inverse], Return[inverse]];

  tensorCharacter = SlotsOfTensor[t] /. TangentKrancManifold -> 1;
  rank = Length[tensorCharacter];

  If[rank =!= 2,
    ThrowError["MatrixInverse cannot be used with a tensor of rank "<>ToString[rank]];
  ];

  {a, b} = $KrancIndices[[1;;2]] tensorCharacter;
  {kba, kbb} = {KrancBasis, KrancBasis} tensorCharacter;

  DefineTensor[inverse[-b, -a]];
  SetComponents[inverse[-b, -a], Inverse[ComponentArray[t[a, b], IndexList[{b, kbb}, {a, kba}]]]];
  inverse
];

MatrixInverse[t_?xTensorQ[i_, j_]] := MatrixInverse[t][i, j];

MatrixOfComponents = ComponentArray;

(*************************************************************)
(* ExpandComponents *)
(*************************************************************)

krancForm[expr_] := 
  expr //. {
    Scalar[x_] :> NoScalar[x], 
    pd_?CovDQ[i__][pd_?CovDQ[j__][t_]] :> pd[j, i][t],
    nd_[pd_?CovDQ[i : (_?CIndexQ ..)][t_?xTensorQ[inds___]]] :>
     NumericalDiscretisation[nd][krancForm[t[inds]], Sequence @@ ({i}[[All, 1]])],
    nd_[pd_?CovDQ[i : (_?CIndexQ ..)][t_Symbol]] :>
     NumericalDiscretisation[nd][krancForm[t], Sequence @@ ({i}[[All, 1]])],
    t_Symbol?xTensorQ[i : (_?CIndexQ ..)] :> 
     SymbolJoin[t, Sequence @@ ToString /@ {i}[[All, 1]]],
    t_Symbol?xTensorQ[] :> t
  };

(* This is very ugly, but it works *)
toCondition[c1_] := {c1, Not[c1]};
toCondition[c1_, c2_] := Flatten[Outer[And, {c1, Not[c1]}, toCondition[c2]]];
toCondition[c1_List, c2_] := Flatten[Outer[And, c1, {c2, Not[c2]}]];
toCondition[c1_, c2_, c3__] := Fold[toCondition, c1, {c2, c3}];
expandIfThen[lhs_ -> rhs_] :=
 Module[{conditions, allConditions, allEqs, expandedEqs, allIfs, rules},
  If[MemberQ[lhs, IfThen, Infinity, Heads -> True],
   ThrowError["IfThen is not supported in the left hand side of an equation"];
  ];
  InfoMessage[InfoFull, "Expanding branches of IfThen expression"];
  conditions = Sort[DeleteDuplicates[Cases[{rhs}, IfThen[cond_, _, _] :> cond, Infinity]]];
  allConditions = Reverse[toCondition @@ conditions];
  If[Length[conditions] === 1,
    allEqs = Thread[(lhs -> rhs) /. IfThen[_, true_, false_] :> {false, true}];,
    allEqs = (lhs -> rhs) //.
      Table[MapThread[(IfThen[#1, true_, false_] :> If[#2 == True, true, false]) &,
                      {conditions, MapThread[SameQ, {(List @@@ allConditions)[[i]], conditions}]}],
            {i, 2^Length[conditions]}];
  ];
  expandedEqs = {ExpandComponents[#]} & /@ allEqs;
  allIfs = Transpose[{allConditions, #}] & /@ Transpose[expandedEqs[[All, All, 2]]];
  rules = Thread[expandedEqs[[1, All, 1]] -> 
               Map[Function[{ifs}, Fold[IfThen[#2[[1]], #2[[2]], #1] &, ifs[[1, 2]], ifs[[2 ;;]]]], allIfs]];
  InfoMessage[InfoFull, "Expanded IfThen branches to: ", Map[InputForm, rules, {2}]];
  rules
];

ExpandComponents[l_ -> r_] :=
  Module[{lhs, rhs, lhsC, rhsC, inds, rules},
   InfoMessage[InfoFull, "Expanding tensor expression: ", InputForm[l] -> InputForm[r]];

   (* Special treatment for IfThen statements. FIXME: Not sure if this belongs here *)
   If[MemberQ[l -> r, IfThen, Infinity, Heads -> True],
     rules = expandIfThen[l -> r];
   ,
   
   (* Add brackets to scalars if they aren't present *)
   {lhs, rhs} = {l, r} /. {t_?KrancScalarQ[] -> t[], t_?KrancScalarQ -> t[]};

   (* Replace some Mathematica functions with inert head versions *)
   {lhs, rhs} = {lhs, rhs} /. {Sign -> KrancSign, Abs -> KrancAbs, Switch -> KrancSwitch};

   (* Check we have a valid tensor equation *)
   (* FIXME: Maybe we should find an alternative to Quiet here *)
   Check[Quiet[Validate[{lhs, rhs}], Validate::unknown],
     ThrowError["Invalid tensor equation", lhs -> rhs]];

   (* Find the free indices *)
   inds = IndicesOf[Free, BIndex][lhs];
   If[(inds =!= IndicesOf[Free, BIndex][rhs]) && !(NumericQ[rhs] || ConstantSymbolQ[rhs]),
     ThrowError["Free indices of left hand side do not match right hand side in ", lhs -> rhs]];

   (* Get a list of components *)
   lhsC = Flatten[{ComponentArray[TraceBasisDummy[lhs], inds]}];
   If[NumericQ[rhs] || ConstantSymbolQ[rhs],
     rhsC = ConstantArray[rhs, Length[lhsC]];,
     rhsC = Flatten[{ComponentArray[TraceBasisDummy[rhs], inds]}];
   ];

   (* Pick out the independent components *)
   rules = Thread[lhsC -> rhsC] /. {(0 -> _) -> Sequence[], (- _ -> _) -> Sequence[]};
   rules = krancForm[DeleteDuplicates[rules, #1[[1]] == #2[[1]] &]];
   rules = rules /. {KrancSign -> Sign, KrancAbs -> Abs, KrancSwitch -> Switch};
   ];
   InfoMessage[InfoFull, "Expanded to: ", Map[InputForm, rules, {2}]];
   Sequence @@ rules
];

(* FIXME: Figure out a way to avoid duplicating the code above in here *)
ExpandComponents[x_] /; Head[x] =!= List :=
 Module[{expr, inds, exprC},
  InfoMessage[InfoFull, "Expanding tensor expression: ", x];
  If[MemberQ[expr, IfThen, Infinity, Heads -> True],
    ThrowError["IfThen is only supported in the right hand side of equations"];
  ];
  expr = x /. {t_?KrancScalarQ[] -> t[], t_?KrancScalarQ -> t[]};
  expr = expr /. {Sign -> KrancSign, Abs -> KrancAbs, Switch -> KrancSwitch};
  Check[Quiet[Validate[expr], Validate::unknown], ThrowError["Invalid tensor expression", expr]];
  inds = IndicesOf[Free, BIndex][expr];
  exprC = Flatten[{ComponentArray[TraceBasisDummy[expr], inds]}];
  rules = krancForm[DeleteDuplicates[exprC /. {0 -> Sequence[]}]];
  InfoMessage[InfoFull, "Expanded to: ", Map[InputForm, rules]];
  Sequence @@ rules
];

ExpandComponents[l:{}] := {};
ExpandComponents[l:{(_ -> _)..}] := ExpandComponents /@ l;

(* FIXME: This is the form used by Shorthands - I think it should be handled elsewhere *)
ExpandComponents[l:{(_?xTensorQ[___] | _ ? AtomQ | dot[_])..}] :=
  DeleteDuplicates[Flatten[ExpandComponents /@ l] /.  {-s_ :> s, 0 -> Sequence[]}];


(*************************************************************)
(* ReflectionSymmetries *)
(*************************************************************)
(* FIXME: Add support for ManualCartesian attribute *)

ReflectionSymmetries[t_Symbol?xTensorQ[inds___]] /;
 HasTensorAttribute[t, TensorParity] := t -> {1, 1, 1} GetTensorAttribute[t, TensorParity];

ReflectionSymmetries[t_Symbol?xTensorQ[inds___]] /;
 HasTensorAttribute[t, TensorManualCartesianParities] :=
  t -> {1, 1, 1} GetTensorAttribute[t, TensorManualCartesianParities];

ReflectionSymmetries[t_Symbol?xTensorQ[inds__]] :=
  Module[{cnums, components, componentIndices, counts},
    (* Get the compoent indices of the basis *)
    InfoMessage[InfoFull, "Getting symmetries of ", InputForm[t[inds]]];
    cnums = CNumbersOf[KrancBasis, VBundleOfBasis[KrancBasis]];

    (* Get a list of components of the tensor t in the basis b *)
    components = DeleteDuplicates[Flatten[ComponentArray[TraceBasisDummy[t[inds]]]]];

    (* Get the indices of each component *)
    componentIndices = Map[IndicesOf[CIndex, KrancBasis], components];

    (* Count the number of instances of each basis index. *)
    countInds[expr_, basis_, cinds_] := Map[(Count[expr,{#,basis}]+Count[expr,{#,-basis}])&, cinds];
    counts = Map[countInds[#, KrancBasis, cnums]&, componentIndices];

    (* For each instance, multiply by -1 *)
    Thread[krancForm[components] -> (-1)^counts]
];

ReflectionSymmetries[t_Symbol?xTensorQ[]] := t -> {1,1,1};
ReflectionSymmetries[t_Symbol?KrancScalarQ] := ReflectionSymmetries[t[]];
ReflectionSymmetries[t_] := t -> {1, 1, 1};
ReflectionSymmetries[x___]:= ThrowError["ReflectionSymmetries error: "<>ToString[x]];

(*************************************************************)
(* CreateGroupFromTensor *)
(*************************************************************)

tensorCharacterString[t_Symbol?xTensorQ[___]] /;
  HasTensorAttribute[t, TensorManualCartesianParities] := "ManualCartesian";
tensorCharacterString[t_Symbol?xTensorQ[]] := "Scalar";
tensorCharacterString[t_Symbol?xTensorQ[inds___]] := StringJoin[If[UpIndexQ[#],"U","D"]&/@{inds}];

manualCartesianParity[t_] :=
 Module[{p},
  p = GetTensorAttribute[t, TensorManualCartesianParities];
  If[!MatchQ[p, {Repeated[(-1 | 1), {3}]}],
    ThrowError["Expecting a list of three parities for TensorManualCartesianParities, must be 1 or -1"];
  ];
  StringJoin @@ (p /. {-1 -> "-", +1 -> "+"})
];

tensorWeight[t_Symbol?xTensorQ] :=
  If[HasTensorAttribute[t, TensorWeight], GetTensorAttribute[t, TensorWeight], WeightOfTensor[t]];

CreateGroupFromTensor[t_Symbol?xTensorQ[inds___]] := Module[{tCharString, nInds, tags, group},
  InfoMessage[InfoFull, "Creating group from tensor " <> ToString[t[inds]]];

  (* Get a string representing the character of the tensor *)
  tCharString = tensorCharacterString[t[inds]];

  (* Check if the tensor is symmetric *)
  nInds = Length[SlotsOfTensor[t]];
  If[SymmetryGroupOfTensor[t] == StrongGenSet[Range[nInds],GenSet[Cycles[Range[nInds]]]], 
        tCharString = tCharString <> "_sym"];

  tags = {"tensortypealias" -> tCharString, "tensorweight" -> tensorWeight[t]};

  If[HasTensorAttribute[t, TensorSpecial],
    AppendTo[tags, "tensorspecial" -> GetTensorAttribute[t, TensorSpecial]]];

  If[HasTensorAttribute[t, TensorManualCartesianParities],
    AppendTo[tags, "cartesianreflectionparities" -> manualCartesianParity[t]]];

  If[HasTensorAttribute[t, TensorParity],
    AppendTo[tags, "tensorparity" -> GetTensorAttribute[t, TensorParity]]];

  If[HasTensorAttribute[t, Checkpoint],
    AppendTo[tags, "checkpoint" -> GetTensorAttribute[t, Checkpoint]]];

  group = CreateGroup[SymbolName[t] <> "_group", {t[inds]}, {Tags -> tags}];
  group
];

CreateGroupFromTensor[t_Symbol?KrancScalarQ] := CreateGroupFromTensor[t[]];

CreateGroupFromTensor[x___]:= ThrowError["Invalid arguments to CreateGroupFromTensor: "<>ToString[x]];

End[];
EndPackage[];

(* Add xAct packages to $ContextPath *)
Do[
  If[!MemberQ[$ContextPath, package], AppendTo[$ContextPath, package]],
  {package, xTensorKranc`Private`newContexts}];
