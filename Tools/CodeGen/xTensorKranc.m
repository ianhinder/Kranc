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

CreateGroupFromTensor::usage = "CreateGroupFromTensor[T[a, b, ...]] Creates a variable group from the tensor T";
DefineTensor::usage = "DefineTensor[T[a, b, ...]] defines the tensor T with indices a, b, c, ....";
SetComponents::usage = "SetComponents[T[a, b, ...], v] defines the components of the tensor T to be the values given in the list v."

ReflectionSymmetries::usage = "ReflectionSymmetries[T[a, b, ...]] Produces a list of reflection symmetries of the tensor T.";
ExpandComponents::usage = "ExpandComponents[expr] converts an expression x containing abstract indices into one containing components instead."
pd::usage = "pd[t, i] represents the i component of the partial derivative of the tensor t.";
Euc::usage = "Euc[i, j] represents the Euclidean tensor which is 1 if i=j, and 0 otherwise.";
Eps::usage = "Eps[i, j, k] represents the Levi-Civita alternating tensor";

$KrancIndices = Symbol /@ Complement[CharacterRange["a", "z"], {"h", "r", "x", "y", "z"}];
Do[
  Evaluate[Symbol["l"<>ToString[ind]]] = -ind;
  Evaluate[Symbol["u"<>ToString[ind]]] = ind;
  , {ind, $KrancIndices}];

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
pd[t_, i_] := PDKrancBasis[i][t];

(* Add some convenience functions *)
DefineTensor[t_[inds___], opts___] :=
 Block[{$DefInfoQ = False}, DefTensor[t[inds], KrancManifold, opts]];

toBasis[dot[x_]] := dot[toBasis[x]];
toBasis[x_] := FixedPoint[ToBasis[KrancBasis], x];

SetComponents[t_?xTensorQ[i :(_?AIndexQ ...)], values_] :=
 Module[{},
  AllComponentValues[toBasis[t[i]], values];
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
    PDKrancBasis[i_][t_] :> Global`PDstandard2nd[krancForm[t], i[[1]]]};

SetAttributes[ExpandComponents, Listable];
ExpandComponents[lhs_ -> rhs_] :=
  
  Module[{lhsB, rhsB, lhsC, rhsC, lhsCi, inds, rules},
   (* Check we have a valid tensor equation *)

   (* FIXME: Maybe we should find an alternative to Quiet here *)
   Check[Quiet[Validate[lhs + rhs], Validate::unknown],
     ThrowError["Invalid tensor equation", lhs -> rhs]];

   (* Convert abstract index expressions to KrancBasis *)
   lhsB = toBasis[lhs];
   rhsB = toBasis[rhs];

   (* Find the free indices *)
   inds = IndicesOf[Free, BIndex][lhsB];

   (* Get a list of components *)
   (* FIXME: Maybe we should find an alternative to Quiet here *)
   Quiet[
     lhsC = ToCanonical[Flatten[{ComponentArray[TraceBasisDummy[lhsB], inds]}, 1]];
     rhsC = ToCanonical[Flatten[{ComponentArray[TraceBasisDummy[rhsB], inds]}, 1]];
   , ToCanonical::noident];

   (* Pick out the independent components *)
   rules = krancForm[DeleteDuplicates[Thread[lhsC -> rhsC], #1[[1]] == #2[[1]] &]];
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
     Flatten[{ComponentArray[TraceBasisDummy[toBasis[x]]]}, 1]], ToCanonical::noident]]]
];

(*************************************************************)
(* ReflectionSymmetries *)
(*************************************************************)
(* FIXME: Add support for ManualCartesian attribute *)

ReflectionSymmetries[t_Symbol?xTensorQ[inds__]] :=
  Module[{cnums, components, componentIndices, counts},
    (* Get the compoent indices of the basis *)
    InfoMessage[InfoFull, "Getting symmetries of ", InputForm[t[inds]]]
    cnums = CNumbersOf[KrancBasis, VBundleOfBasis[KrancBasis]];

    (* Get a list of components of the tensor t in the basis b *)
    components = DeleteDuplicates[ToCanonical[Flatten[ComponentArray[TraceBasisDummy[toBasis[t[inds]]]]]]];

    (* Get the indices of each component *)
    componentIndices = Map[IndicesOf[CIndex, KrancBasis], components];

    (* Count the number of instances of each basis index. *)
    countInds[expr_, basis_, cinds_] := Map[(Count[expr,{#,basis}]+Count[expr,{#,-basis}])&, cinds];
    counts = Map[countInds[#, b, cnums]&, componentIndices];

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

CreateGroupFromTensor[t_Symbol?xTensorQ[inds__]] := Module[{tCharString, nInds, tags, vars, group},
  InfoMessage[InfoFull, "Creating group from tensor " <> ToString[t[inds]]];

  (* Get a string representing the character of the tensor *)
  tCharString = tensorCharacterString[t[inds]];

  (* Check if the tensor is symmetric *)
  nInds = Length[SlotsOfTensor[t]];
  If[SymmetryGroupOfTensor[t] == StrongGenSet[Range[nInds],GenSet[Cycles[Range[nInds]]]], 
        tCharString = tCharString <> "_sym"];

  (* FIXME: Add tensorspecial, cartesianreflectionparities, checkpoint and tensorparity *)
  tags = {"tensortypealias" -> tCharString, "tensorweight" -> WeightOfTensor[t]};

  vars = If[nInds == 0, {t}, {ExpandComponents[t[inds]]}];
  group = CreateGroup[SymbolName[t] <> "_group", vars, {Tags -> tags}];
  Return[group]
];

CreateGroupFromTensor[x___]:= ThrowError["CreateGroupFromTensor error: "<>ToString[x]];

End[];
EndPackage[];

(* Add xAct packages to $ContextPath *)
Do[
  If[!MemberQ[$ContextPath, package], AppendTo[$ContextPath, package]],
  {package, xTensorKranc`Private`newContexts}];
