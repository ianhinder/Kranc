
(* $Id$ *)

(*  Copyright 2004 Ian Hinder

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

BeginPackage["TensorTools`", {"Errors`", "MapLookup`", "Kranc`"}];

(* Cause the sym context to be added to the context of anyone loading
   this package *)
(*$ContextPath = Join[{"sym`"}, $ContextPath]; *)

(* Define usage messages for these functions.  Mentioning them here
   adds them to the TensorTools context *)

DefineTensor::usage = "DefineTensor[kernel] registers kernel as a \
TensorTools tensor kernel.";

MakeExplicit::usage = "MakeExplicit[x] converts an expression x \
containing abstract indices into one containing components \
instead.";

AssertSymmetricIncreasing::usage = "AssertSymmetricIncreasing[tensor] \
indicates that when the two-component tensor (e.g. T[la,lb]) is \
represented as components, no distinction should be made between the \
two orderings of the indices, and the decreasing order is to be \
preferred.";

AssertSymmetricDecreasing::usage = "AssertSymmetricDecreasing[tensor] \
indicates that \ when the two-component tensor (e.g. T[la,lb]) is \
represented as components, no distinction \ should be made between the \
two orderings of the indices, and the increasing order is to be \
preferred.";

PD::usage = "PD[x,i1, i2, ...] represents the partial derivative of x
with respect to the indices i1, i2, ...";

(*OD::usage = "OD[x,i1, i2, ...] represents the partial derivative of x
with respect to the indices i1, i2, ...  This syntax is accepted for
compatibility with MathTensor.";*)

Lie::usage = "Lie[x,V] is the Lie derivative of expression x with
respect to the TensorTools vector V (specified without an index).";

FD::usage = "FD[x,i1, i2, ...] represents the local partial derivative of x
with respect to the indices i1, i2, ...";

MatrixOfComponents::usage = "MatrixOfComponents[tensor] returns a
matrix of the components of a two-tensor";

Tensor::usage = "Tensor[kernel, index, ...] represents a TensorTools
tensor.";

TensorIndex::usage = "TensorIndex[type, label] represents a
TensorTools tensor index";

TensorProduct::usage = "TensorProduct[x, y, ...] represents a product
of expressions which has already been checked for duplicated dummy
indices.";

MakeLocalSymbol::usage = "MakeLocalSymbol[s] creates a symbol in the
current context consisting of the symbol s suffixed with the letter
L";

MatrixInverse::usage = "MatrixInverse[tensor] converts a tensor
component (eg T[1,3]) into the 1,3 component of T^(-1)";

CDtoPD::usage = "CDtoPD[x] converts all covariant derivatives in x
registered using DefineConnection into partial derivatives.";

LieToPD::usage = "LieToPD[x] converts all Lie derivatives in x ";

PDtoFD::usage = "PDtoFD[x] converts all partial derivatives in x
into local partial derivatives.";

DefineConnection::usage = "DefineConnection[cd, pd, ch] registers a
connection with TensorTools.  The covariant derivative operator will be cd,
the partial derivative operator will be pd, and the Christoffel symbol will
be ch.";

DefineJacobian::usage = "DefineJacobian[pd, fd, J, dJ] registers a
Jacobian with TensorTools.  The partial derivative operator will be pd,
the local partial derivative operator will be fd, and the Jacobian and its
derivative will be J and dJ.";

ResetJacobians::usage = "ResetJacobians unregisters all Jacobians.";

KD::usage = "KD[x,y] is the Kronecker delta symbol.  It can be given
tensorial or numerical indices.";

Eps::usage = "Eps[i,j,...] is the alternating symbol.  It can be given
tensorial or numerical indices.";

Zero3::usage = "Zero3[i,j,k] is zero.";

Euc::usage = "Euc[i,j] is the Euclidian metric.  It can be given
tensorial or numerical indices.";

SetEnhancedTimes::usage = "SetEnhancedTime[boolean] enables or
disabled automatic checking and relabelling of products for duplicated
dummy indices.";

RemoveDuplicates::usage = "RemoveDuplicates[list] removes any
duplicated elements from list.  Useful with MakeExplicit where some of
the tensor symmetries cause duplicates to be created.";

IndexIsLower;
IndexIsUpper;

CheckTensors::usage = "";

SwapIndices::usage = "";
Symmetrize::usage = "";
AntiSymmetrize::usage = "";
calcSymmetryOfComponent;
ReflectionSymmetriesOfTensor;
HasTensorAttribute;
GetTensorAttribute;
SetTensorAttribute;
TensorAttributes;
Symmetries;
TensorParity;
TensorWeight;
TensorSpecial;
TensorManualCartesianParities;
Checkpoint;
IsTensor;
toggleIndex;

(* This is for compatibility with MathTensor notation *)
(*OD = PD;*)

Begin["`Private`"];

listOfTensors = {};
listOfLowerIndices = {};
TensorTools`null=TensorTools`\[Null];
upper = "u";
lower = "l";
delta = \[Delta]

SwapIndices[x_, i1_, i2_] := 
  Module[{temp, unique},
    u = Unique[];
    temp = x /. i1 -> u;
    temp2 = temp /. i2 -> i1;
    temp3 = temp2 /. u -> i2;
    temp3];

Symmetrize[x_, i1_, i2_] := 
  1/2(x + SwapIndices[x, i1, i2]);

AntiSymmetrize[x_, i1_, i2_] := 
  1/2(x - SwapIndices[x, i1, i2]);

(* -------------------------------------------------------------------------- 
   Utility functions
   -------------------------------------------------------------------------- *)

(* Given a list, intersperse a string between its elements*)
separateWithString[l_List, s_] := 
  If[l == {},
    {},
    If[Length[l] == 1,
      l,
      List[l[[1]], s, separateWithString[Rest[l], s]]]];

(* -------------------------------------------------------------------------- 
   Index manipulation functions
   -------------------------------------------------------------------------- *)

(* List all the letters we can use as index labels *)
listLetters[] = Map[ToString, CharacterRange["a", "z"]];

(* List all the numbers we can use as index labels *)
listNumbers[] = Table[ToString[i], {i,0,9}];

(* Create and return a symbol using the strings 'prefix' and 'suffix'
   to be used as in index.  Ensure that this symbol is created in the
   TensorTools context. *)
indexSymbol[(prefix_ ? StringQ) /; (StringLength[prefix] == 1),
            (suffix_ ? StringQ) /; (StringLength[suffix] == 1)] :=
    Symbol["TensorTools`" <> prefix <> suffix];

listIndexLabels[] =
  Join[listLetters[],listNumbers[]];

listUpperIndices[] =
  Map[indexSymbol[upper, #] &, listIndexLabels[]];

listLowerIndices[] =
  Map[indexSymbol[lower, #] &, listIndexLabels[]];

IndexIsUpper[TensorIndex[_, "u"]] := True;
IndexIsUpper[TensorIndex[_, "l"]] := False;

IndexIsLower[TensorIndex[_, "l"]] := True;
IndexIsLower[TensorIndex[_, "u"]] := False;


(* -------------------------------------------------------------------------- 
   TensorIndex
   -------------------------------------------------------------------------- *)

Format[TensorIndex[label_, "u"], OutputForm] :=
  "u"<>ToString[label];

Format[TensorIndex[label_, "l"], OutputForm] :=
  "l"<>ToString[label];

Format[TensorIndex[label_, "u"], StandardForm] :=
  Superscript[null,label];

Format[TensorIndex[label_, "l"], StandardForm] :=
  Subscript[null,label];

(* Cannot get InputForm to work *)

defineIndex[prefix_, label_] :=
  Module[{i},
    i = TensorIndex[label, prefix];
    Evaluate[indexSymbol[prefix, label]] := i];

defineIndices[] :=
  Module[{},
    Map[defineIndex[upper,#] &, listIndexLabels[]];  
    Map[defineIndex[lower,#] &, listIndexLabels[]]];  

defineIndices[];

(* -------------------------------------------------------------------------- 
   Tensor
   -------------------------------------------------------------------------- *)

DefineTensor[T_] :=
  Module[{},

    Format[Tensor[T, is:((TensorIndex[_,_] | _Integer) ..) ], StandardForm] := 
      PrecedenceForm[
        SequenceForm[T,"[",Sequence@@Riffle[{is},","],"]"],
        10000];

    Format[Tensor[T, is:((TensorIndex[_,_] | _Integer) ..) ], OutputForm] := 
      PrecedenceForm[
        SequenceForm[T,"[",Sequence@@Riffle[{is},","],"]"],
        10000];

(* Cannot get InputForm to work *)

(*    Format[Tensor[T, is:((TensorIndex[_,_] | _Integer) ..) ], InputForm] := 
      HoldForm[T[is]];*)

    T[is:((TensorIndex[_,_] | _Integer) ..)] := Tensor[T, is];
    TensorAttributes[T] = {TensorWeight -> 0, Symmetries -> {}};
    T];

(* -------------------------------------------------------------------------- 
   Index manipulation
   -------------------------------------------------------------------------- *)

toggleIndex[TensorIndex[i_, upper]] := TensorIndex[i, lower];
toggleIndex[TensorIndex[i_, lower]] := TensorIndex[i, upper];

lowerIndicesIn[x_] := Cases[x, TensorIndex[_, lower], Infinity];
upperIndicesIn[x_] := Cases[x, TensorIndex[_, upper], Infinity];
indicesIn[x_] := Cases[x, TensorIndex[_, _], Infinity];

(* -------------------------------------------------------------------------- 
   Dummy indices
   -------------------------------------------------------------------------- *)

(* List all the dummy indices in x *)
dummiesIn[x_] :=
  Module[{lowerIndices = lowerIndicesIn[x],
          upperIndices = upperIndicesIn[x]},
    Intersection[lowerIndices, Map[toggleIndex, upperIndices]]];

(* List all the free indices in x *)
freesIn[x_] :=
  Module[{lowers = lowerIndicesIn[x],
          uppers = upperIndicesIn[x]},
    Join[Complement[lowers, Map[toggleIndex, uppers]],
         Complement[uppers, Map[toggleIndex, lowers]]]];

(* Return a list of the (lower) dummy indices used in x that are used as either free
   or dummy indices in y *)
dummiesConflicting[x_, y_] :=
  Module[{dummies = dummiesIn[x],
          inds = Join[lowerIndicesIn[y], Map[toggleIndex, upperIndicesIn[y]]]},
    Intersection[dummies, inds]];

(* Replace those dummy indices in x that conflict with those in y *)    
replaceConflicting[x_, y_] :=
  Module[{dummies = dummiesConflicting[x, y]},
    replaceDummies[x, dummies, Take[remainingLowerIndices[{x,y}], Length[dummies]]]];

(* Replace the dummies in x with the indices given *)
replaceDummies[x_, dummies_, newIndices_] :=
  If[dummies == {},
     x,
     replaceDummies[replaceDummyIndex[x, dummies[[1]], newIndices[[1]]], 
                    Rest[dummies], Rest[newIndices]]];

(* List the lower indices that do not occur in x in either upper or lower form *)
remainingLowerIndices[x_] :=
  Module[{indsInExpr = Join[lowerIndicesIn[x], Map[toggleIndex, upperIndicesIn[x]]]},
    Select[listLowerIndices[], ! MemberQ[indsInExpr, #] &]];

(* Return an index that isn't used in x *)
dummyNotIn[x_] := First[remainingLowerIndices[x]];

(* Given two lower (or upper) indices, replace the first with the
   second wherever it occurs in x *)
replaceDummyIndex[x_, li_, ri_] :=
  (x /. li -> ri) /. (toggleIndex[li] -> toggleIndex[ri]);


(* -------------------------------------------------------------------------- 
   TensorProduct
   -------------------------------------------------------------------------- *)

(* TensorProduct is very similar to Times.  If two tensorial
   expressions are multiplied together, we need to replace any
   conflicting dummy indices.  So we provide a definition for Times on
   tensorial quantities.  But it needs to return something other than
   another Times, otherwise it will be evaluated again.  So a
   TensorProduct represents a Times that has already been checked for
   conflicting dummy indices.  It can have any expressions in it. *)

SetAttributes[TensorProduct, {Flat, OneIdentity}];

(* For some reason this causes infinite loops - might want to check this later *)
(*TensorProduct[t:(Tensor[_,__])] := t;*)

(* Choose the definition of Times - whether or not we want it to check indices *)
SetEnhancedTimes[q_] :=
  Module[{},
    Unprotect[Times];
    If[q,
      (* Needs to replace multiple dummy indices *)
      Times[S_ ? tensorialQ, T_ ? tensorialQ] :=
        Module[{S2,T2,Spair, Tpair, Stensorial, Ttensorial, Snontensorial, Tnontensorial},
        Spair = separateTensorialFactors[S];
        Tpair = separateTensorialFactors[T];
        Stensorial = Spair[[2]];
        Ttensorial = Tpair[[2]];
        Snontensorial = Spair[[1]];
        Tnontensorial = Tpair[[1]];
        S2 = replaceConflicting[Stensorial, Ttensorial];
        T2 = replaceConflicting[Ttensorial, S2];

        Snontensorial * Tnontensorial * TensorProduct[S2,T2]],
    Times[S_ ? tensorialQ, T_ ? tensorialQ] =.];
    Protect[Times]];

SetEnhancedTimes[True];

(* Is x tensorial?  i.e. does it contain any TensorTools indices? *)
tensorialQ[x_] := 
  Cases[{x}, TensorIndex[__], Infinity] != {};

(* Is x nontensorial?  i.e. is it devoid of any TensorTools indices? *)
nontensorialQ[x_] := 
  Cases[{x}, TensorIndex[__], Infinity] == {};

(* Given an expression, return a rule of the form a -> b.  If it's a
   multiplication, a is the nontensorial factors, and b is the
   tensorial factors.  *)
separateTensorialFactors[x_] :=
  If[Head[x] === Times,
    Module[{
      tensorialElements = Select[x, tensorialQ],
      nontensorialElements = Select[x, ! tensorialQ[#] &]},
      nontensorialElements -> tensorialElements],
    1 -> x];

Unprotect[Times];

Times[TensorProduct[], x_] = x;

Format[x_ t:TensorProduct[((Tensor[__] | CD[__] | PD[__] | FD[__]) ..)]] := 
  SequenceForm[x," ",t];

Format[x_ t:Tensor[__]] := 
  SequenceForm[x," ",t];

Protect[Times];

Format[TensorProduct[t:((Tensor[__] | CD[__] | PD[__] | FD[__]) ..)]] := 
  PrecedenceForm[SequenceForm[t],1000];

Format[t:TensorProduct[x__]] := Infix[t,null];

(* -------------------------------------------------------------------------- 
   More index manipulation
   -------------------------------------------------------------------------- *)

(* Takes any expression and returns the position that a given index
   occupies the first time it occurs in the expression *)
slotOfIndex[i_, x_] :=
  Position[tensorOfIndex[i, x], i,1][[1]][[1]];

kernelOfIndex[i_, x_] :=
  Cases[{x}, Tensor[K_, _ ..., i, _ ...] -> K, Infinity][[1]];

tensorOfIndex[i_, x_] :=
  Cases[{x}, t:Tensor[K_, _ ..., i, _ ...], Infinity][[1]];

(* Check that the given index is present in two tensor products with
the same kernel in each *)
checkFreeIndex[i_, p1_, p2_] :=
  kernelOfIndex[i, p1] == kernelOfIndex[i, p2] &&
  slotOfIndex[i, p1] == slotOfIndex[i, p2];

indexAt[kernel_, slot_, x_] :=
  Cases[{x}, Tensor[kernel, is__], Infinity][[1]][[slot]];

checkDummyIndex[i_, p1_, p2_] :=
  Module[{p1Kernel, p1Slot, ic, p1cKernel, p1cSlot, i2, ic2},
    p1Kernel = kernelOfIndex[i, p1];
    p1Slot = slotOfIndex[i, p1];
    ic = toggleIndex[i];
    p1cKernel = kernelOfIndex[ic, p1];
    p1cSlot = slotOfIndex[ic, p1];
    
    i2 = indexAt[p1Kernel, p1Slot, p2];
    ic2 = indexAt[p1cKernel, p1cSlot, p2];

    i2 == toggleIndex[ic2]];

Unprotect[Equal];

Equal[TensorProduct[ts__], TensorProduct[ss__]] :=
  Module[{frees = freesIn[{ts}], dummies = dummiesIn[{ts}]},
    If[! Apply[And, Map[checkFreeIndex[#, {ts}, {ss}] &, frees]],
      False,
      Apply[And, Map[checkDummyIndex[#, {ts}, {ss}] &, dummies]]]];

Equal[T1:(Tensor[_, __]), T2:(Tensor[_, __])] := Equal[TensorProduct[T1], TensorProduct[T2]];

Protect[Equal];

(* -------------------------------------------------------------------------- 
   High level expression manipulation functions
   -------------------------------------------------------------------------- *)

(* Take an expression and return a list of expressions with free
   indices running over the appropriate range, all dummy indices
   expanded, and tensor components and derivatives in single-symbol
   form suitable for code generation *)
MakeExplicit[x_] := 
  ((((makeSplit[makeSum[PDtoFD[LieToPD[CDtoPD[x]]]]] /. componentNameRule) /. KDrule) /. EpsilonRule)
       /. derivativeNameRule) /. TensorProduct -> Times;

MakeExplicit[l:List[Rule[_, _] ..]] :=
  Flatten[Map[removeDuplicatesFromMap, Map[MakeExplicit, l]],1];

MakeExplicit[l:List[(Tensor[__] | _ ? AtomQ | dot[_]) ..]] :=
  explicitVariableList[l];

MakeExplicit[{}] :=
  {};

(* Give the components of an expression wrt its free indices *)
makeSplit[x_] := 
  makeSplitOverFrees[x, freesIn[x]];

(* Give the components of an expression wrt the given indices.  This
   function is misnamed *)
makeSplitOverFrees[x_, is_] := 
  If[is == {},
     {x},
     Apply[Join, Map[makeSplitOverFrees[#, Rest[is]] &, 
                     makeSplitOverFreeIndex[x, is[[1]]]]]];

(* Give the components of an expression wrt a free lower index *)
makeSplitOverFreeIndex[x_, j:TensorIndex[_,lower]] := 
  Module[{i}, Table[x /. j -> i, {i, 3}]];

  (* FIXME: Changing the above is very drastic; does it have any major
  consequences? *)

(* Give the components of an expression wrt a free upper index *)
makeSplitOverFreeIndex[x_, j:TensorIndex[_,upper]] := 
  Module[{i}, Table[x /. j -> i, {i, 3}]];

(* Given an expression, return a list of versions of this expression
   with a particular index replaced with the numbers from 1 to 3.
   This should probably list negative indices for the lowered
   components. *)
listComponents[x_, i:TensorIndex[_,_]] := 
  Module[{j},Table[x /. i -> j, {j, 3}]];

(* Given an expression, return a list of versions of this expression
   with a particular pair of lower and upper indices replaced with the
   numbers from 1 to 3.  This should probably list negative indices
   for the lowered components. *)
listComponents[x_, index1:(TensorIndex[_,lower]), index2:(TensorIndex[_,upper])] := 
  Module[{i},
    Table[(x /. index1 -> i) /. index2 -> i, {i, 3}]];

listComponentsOfDummyIndex[x_, i:(TensorIndex[_,lower])] := 
  listComponents[x, i, toggleIndex[i]];


(* Given an expression consisting of sums, products, symbols, abstract
   tensors and ordinary derivatives, return an expression where all
   the implicit summations have been explicitly carried out. *)

makeSum[x_ y_] := 
  Module[{x2 = makeSum[x],
          y2 = makeSum[y]},
          makeSumOverDummies[x2 y2, dummiesIn[x2 y2]]];

makeSum[TensorProduct[ts__]] :=
  Module[{summed, prodSummed},
    summed = Map[makeSum, {ts}];
    prodSummed = Apply[TensorProduct,summed];
    makeSumOverDummies[prodSummed, dummiesIn[prodSummed]]];

makeSum[t:(Tensor[K_, is__])] :=
    makeSumOverDummies[t, dummiesIn[t]];


(* Given a sum of terms, each one has independent dummy indices, so
   they cannot all be summed together.  Must split off into terms
   before extracting the dummy indices. *)
makeSum[x_ + y_] := 
  makeSum[x] + makeSum[y];

makeSum[x_ == y_] := 
  makeSum[x] == makeSum[y];

makeSum[x_ -> y_] := 
  makeSum[x] -> makeSum[y];

makeSum[x_] := 
  makeSumOverDummies[x, dummiesIn[x]];

sumComponentsOfDummyIndex[x_, i_] := 
  Apply[Plus,listComponents[x, i, toggleIndex[i]]];

(* Given an expression and a list of lower indices which are dummies
   in that expression, expand the dummy indices in the expression into
   components. *)
makeSumOverDummies[x_, is_] := 
  If[is == {}, 
     x, 
     makeSumOverDummies[sumComponentsOfDummyIndex[x, is[[1]]], Rest[is]]];

(* -------------------------------------------------------------------------- 
   Rules for converting expressions into different forms
   -------------------------------------------------------------------------- *)

(* Convert references to components of abstract tensors into
   single-symbol names; i.e.  T[2,3] -> T23.  The tensors must have been
   registered with DefineTensor *)
componentNameRule := 
  Tensor[K_, y__Integer] :> 
    Symbol[ToString[K] <> makeSuffix[List[y]]];


(* Convert references to ordinary derivative components into single-symbol
   names; i.e., PD[x, 1, 2, 3] -> D123[x]. *)

(* This is disabled now (by introducing a dummy pattern), because we
   would like all differencing to be performed by our custom
   operators.  Note that at the moment this means we can't control
   order of differencing by macros etc. *)

(* FIXME *)

derivativeNameRule := 
  wibble[x_, y__] :> 
    Symbol["D" <> StringJoin[Map[ToString[Abs[#]]&,{y}]]] [x];

makeSuffix[is_List] := 
  StringJoin[Map[ToString[Abs[#]]&, is]];


(* -------------------------------------------------------------------------- 
   New symmetry code
   -------------------------------------------------------------------------- *)

(* Compatibility layer; could redesign to use directly *)
positionOfIndex[T_[inds__], i_] :=
  slotOfIndex[i, T[inds]];

needToSwap[Tensor_[K_, inds__], pos1_, pos2_, increasing_] :=
  Module[{val1 = Abs[{inds}[[pos1-1]]], val2 = Abs[{inds}[[pos2-1]]]},
    If[increasing,
      val1 > val2,
      val1 < val2]];

swapComponents[T:Tensor[K_, inds__], pos1_, pos2_] :=
  Module[{S = T},
    S[[pos1]] = T[[pos2]];
    S[[pos2]] = T[[pos1]];
    S];

makePreferenceEquation[Tensor[K_, inds__], pos1_, pos2_] :=
  Module[{eq (*, eqE*)},
    eq = Tensor[K, inds] -> swapComponents[Tensor[K, inds], pos1, pos2];
(*    eqE = eq /. componentNameRule;*)
    Evaluate[eq[[1]]] = eq[[2]];

(* This should not be necessary if the above has worked.  Right? *)
(*    Evaluate[eqE[[1]]] = eqE[[2]] *)

];

AssertSymmetricIncreasing[Tensor[K_, a_, b_]] :=
  AssertSymmetricIncreasing[Tensor[K, a ,b], a, b];

AssertSymmetricDecreasing[Tensor[K_, a_, b_]] :=
  AssertSymmetricDecreasing[Tensor[K, a, b], a, b];

AssertSymmetricIncreasing[Tensor[K_, inds__], i1_, i2_] :=
  assertSymmetric[Tensor[K, inds], i1, i2, True];

AssertSymmetricDecreasing[Tensor[K_, inds__], i1_, i2_] :=
  assertSymmetric[Tensor[K, inds], i1, i2, False];

interchangeNumbers[perm_, i_, j_] :=
  ((perm /. i -> X) /. j -> i) /. X -> j;

assertSymmetric[Tensor[K_, inds__], i1_, i2_, increasing_] :=
  Module[
    {pos1 = positionOfIndex[Tensor[K, inds], i1],
     pos2 = positionOfIndex[Tensor[K, inds], i2],
     oldSymmetries, newPerm, newSymmetries},

     oldSymmetries = GetTensorAttribute[K, Symmetries];
     newPerm = interchangeNumbers[Table[i, {i, 1, Length[{inds}]}], pos1-1, pos2-1];
     newSymmetries = Join[oldSymmetries, {newPerm, 1}];

     SetTensorAttribute[K, Symmetries, newSymmetries];
     InfoMessage[Info, "Setting symmetries of " <> ToString[K] <> " to be " <> ToString[newSymmetries]];

     Map[makePreferenceEquation[#, pos1, pos2] &,
         Select[makeSplit[Tensor[K, inds]],
                needToSwap[#, pos1, pos2, increasing] &]]];

(* -------------------------------------------------------------------------- 
   Miscellaneous code
   -------------------------------------------------------------------------- *)

MakeLocalSymbol[str_] :=
  Symbol[ToString[str] <> "L"];

AssertPartialDerivativesCommute[] :=    
  (D12 = D21; 
   D13 = D31; 
   D23 = D32);

AssertPartialDerivativesCommute[];

MatrixOfComponents[Tensor[K_, i_, j_]] :=
  listComponents[listComponents[Tensor[K,i,j], j], i] /. 
    componentNameRule /. derivativeNameRule;

tensorDeterminant[Tensor[K_, i_, j_]] :=
  Det[MatrixOfComponents[Tensor[K,i,j]]];

MatrixInverse[Tensor[K_, i_Integer, j_Integer]] :=
  Inverse[MatrixOfComponents[Tensor[K,ua,ub]]][[Abs[i],Abs[j]]];

RicciFromChristoffel[i_, j_, gamma_] :=
  Module[{kl, ku, ml, mu},
    kl = remainingLowerIndices[{i,j}];
    ku = toggleIndex[kl];
    ml = remainingLowerIndices[{i,j,kl}];
    mu = toggleIndex[ml];

    PD[gamma[ku, j, i], kl] - PD[gamma[ku, kl, i], j] + 
      gamma[mu, j, i] gamma[ku, kl, ml] - 
      gamma[mu, kl, i] gamma[ku, j, ml]];

(* Replace occurrences of the expression y in x with the expression z,
   and return the result.  This function relabels any dummy indices in
   z to avoid conflicts with existing indices in x.  We should
   probably check that y and z have the same free indices. For the
   moment, y must not have any dummies in it. *)
replaceTensor[x_, Tensor[y_, inds__], z_] :=
  x /. Tensor[y, inds] -> replaceConflicting[z, x];

replaceTensor[x1_ + x2_, y_, z_] :=
  replaceTensor[x1, y, z] + replaceTensor[x2, y, z];

(* -------------------------------------------------------------------------- 
   Covariant Derivatives
   -------------------------------------------------------------------------- *)

(* Note: covariant derivatives, Lie derivatives and Partial
   derivatives all have a lot in common.  It would make sense to
   abstract that out into the concept of a general derivative, then
   add in the specific bits to each one.  This has not been done here.
   *)

connections = {};

(* This should probably be unnecessary *)
setDefaultConnection[k_] :=
  defaultConnection = k;

(* This should probably be unnecessary *)
getDefaultConnection[] :=
  defaultConnection;

(* Given the name of a connection (to be used as the covariant
   derivative) and the kernel of a Christoffel symbol for that
   connection, register these so that the conversion of the covariant
   derivative to the ordinary derivative can use the correct
   Christoffel symbol. *)
DefineConnection[cd_, pd_, gamma_] :=
  Module[{},
    connections = Join[connections, {{cd, pd, gamma}}]];


(* Things we can do with covariant derivatives:

   (1) Liebnitz: CD[x_ y_,i_] -> CD[x,i] y + x CD[y,i]
   (2) Linear: CD[x_ + y_,i_] -> CD[x,i] + CD[y,i]
   (3) Linear: CD[i_Integer x_] -> i CD[x]
   (4) High order derivatives: CD[x_, i_, is__] -> CD[CD[x,i],is]

*)

(* Reduction rules for single covariant derivatives *)

CDLeibnizTimes := 
  CD[x_ y_, i_] :> CD[x,i] y + x CD[y,i];

CDLeibnizTensorProduct := 
  CD[TensorProduct[s_, t__], i_] :> 
    TensorProduct[CD[s,i],t] + TensorProduct[s,CD[TensorProduct[t],i]];

CDLinearity :=
  CD[x_ + y_, i_] :> CD[x,i] + CD[y,i];

(* Take a high order covariant derivative and replace it with repeated
   application of a single one *)
CDUnflatten :=
  CD[x_, i_, is__] :> CD[CD[x,i],is];

(* Take an expression containing covariant derivatives (possibly of
   order greater than one) and reduce it in such a way that the only
   covariant derivates remaining are 1st order, and all products have
   been expanded using the Leibniz rule, and all sums have been
   distributed over using the linearity property. This is the form we
   want it in for replacing covariant derivatives with partial
   derivatives *)
CDReduce :=
  x_ :>  (((((x //. CDUnflatten) //. CDLeibnizTimes) 
            //. CDLeibnizTensorProduct) //. CDLinearity) //. PDReduce);

(* Evaluation rules *)

(* After the CD is converted to a PD, it will contain a dummy index.
   This needs careful attention, so instead of leaving the result in
   its TensorProduct wrapper, we remove it and use a Times instead.
   This will force the dummy indices to be checked and relabelled
   automatically, and the result placed into a TensorProduct when it
   is clean. *)

CDtoPDRuleTensorProduct := 
  TensorProduct[t1___, CD[x_,i_], t2___] :> 
    TensorProduct[t1, t2] * (CD[x,i] //. CDtoPDRule)

(* Convert a CD into an PD plus some extra terms, one for each index
   in the expression.  We do not specify only free indices here; if
   there are dummy indices the result will cancel algebraically. *)

CDtoPDRule :=
  CD[x_, i_] :>
    PD[x,i] + Apply[Plus,Map[CDtoPDTerm[x, #, i] &, indicesIn[x]]];

(* Give the term in the expansion of the CD corresponding to the index
i in x.  Differentiation is with respect to "wrt". *)

CDtoPDTerm[x_, i:TensorIndex[_,lower], wrt_] :=
  Module[{di = dummyNotIn[{x, wrt}], gamma = defaultConnection},
    -TensorProduct[(x /. i -> di), Tensor[gamma, toggleIndex[di], wrt, i]]];

CDtoPDTerm[x_, i:TensorIndex[_,upper], wrt_] :=
  Module[{di = toggleIndex[dummyNotIn[{x, wrt}]], gamma = defaultConnection},
    TensorProduct[(x /. i -> di), Tensor[gamma, i, wrt, toggleIndex[di]]]];

CDtoPDFullRule := x_ :> (((x //. CDReduce) //. CDtoPDRuleTensorProduct) //. CDtoPDRule);

removeSingleTensorProducts := TensorProduct[x_] :> x;

CDtoPDDefaultConnection[x_] :=
  x //. CDtoPDFullRule //. removeSingleTensorProducts;

CDtoPDForConnection[x_, connection_] :=
  Module[{cd,pd,gamma, y, oldGamma, result},
    cd = connection[[1]];
    pd = connection[[2]];
    gamma = connection[[3]];
    oldGamma = getDefaultConnection[];
    setDefaultConnection[gamma];
    y = x /. {cd :> CD, pd :> PD};
    result = CDtoPDDefaultConnection[y];
    setDefaultConnection[oldGamma];
    result];

CDtoPDForConnections[x_, cs_] :=
  If[cs == {},
    x,
    CDtoPDForConnections[CDtoPDForConnection[x, First[cs]], Rest[cs]]];

CDtoPD[x_] :=
  CDtoPDForConnections[x, connections];




(* -------------------------------------------------------------------------- 
   Partial Derivatives
   -------------------------------------------------------------------------- *)

(* This is clearly duplicated (for obvious reasons) from the CD case.
   Do we want to perhaps generalize the concept of a derivative
   operator?  *)

(* Reduction rules *)

PDLeibnizTimes := 
  PD[x_ y_, i_] :> PD[x,i] y + x PD[y,i];

PDLeibnizTensorProduct := 
  PD[TensorProduct[s_, t__], i_] :> 
    TensorProduct[PD[s,i],t] + TensorProduct[s,PD[TensorProduct[t],i]];

PDLinearity :=
  PD[x_ + y_,i_] :> PD[x,i] + PD[y,i];

(* Flattening *)

PDFlatten := 
  PD[PD[x_,is__],js__] :> PD[x,is,js];

PDUnflatten :=
  PD[x_, i_, is__] :> PD[PD[x,i],is];

PDReduce :=
  x_ :>  (((((x //. PDUnflatten) //. PDLeibnizTimes) //. PDLeibnizTensorProduct) //. PDLinearity) //. PDFlatten);

PD[x:(_Real | _Integer), is__] := 0;

(*
Format[PD[x_ ? AtomQ, is:((TensorIndex[_,_] | _Integer) ..)]] :=
  SequenceForm[x, ",", is];

Format[PD[Tensor[k_,inds__], is:((TensorIndex[_,_] | _Integer) ..)]] :=
  SequenceForm[k, inds, ",", is];

Format[PD[x_, is:((TensorIndex[_,_] | _Integer) ..)]] :=
  SequenceForm["(",x,")", ",", is];
*)


(* -------------------------------------------------------------------------- 
   Lie Derivatives
   -------------------------------------------------------------------------- *)

(* We would like to unify all three types of derivative, since they
   have a lot in common with each other.  This will have to wait! *)

(* Reduction rules for single Lie derivatives *)

LieLeibnizTimes := 
  Lie[x_ y_, v_] :> Lie[x,v] y + x Lie[y,v];

LieLeibnizTensorProduct := 
  Lie[TensorProduct[s_, t__], v_] :> 
    TensorProduct[Lie[s,v],t] + TensorProduct[s,Lie[TensorProduct[t],v]];

LieLinearity :=
  Lie[x_ + y_, v_] :> Lie[x,v] + Lie[y,v];


(* Take an expression containing Lie derivatives and reduce it in such
   a way that all products have been expanded using the Leibniz rule,
   and all sums have been distributed over using the linearity
   property. This is the form we want it in for replacing Lie
   derivatives with ordinary derivatives *)

LieReduce :=
  x_ :>  ((((x //. LieLeibnizTimes) 
            //. LieLeibnizTensorProduct) //. LieLinearity) //. PDReduce);

(* Evaluation rules *)

(* After the Lie derivative is converted to an PD, it will contain a
   dummy index.  This needs careful attention, so instead of leaving
   the result in its TensorProduct wrapper, we remove it and use a
   Times instead.  This will force the dummy indices to be checked and
   relabelled automatically, and the result placed into a
   TensorProduct when it is clean. *)

LieToPDRuleTensorProduct := 
  TensorProduct[t1___, Lie[x_,v_], t2___] :> 
    TensorProduct[t1, t2] * (Lie[x,v] //. LieToPDRule)

(* Convert a Lie derivative into an PD plus some extra terms, one for
   each index in the expression.  We do not specify only free indices
   here; if there are dummy indices the result will cancel
   algebraically. *)

LieToPDRule :=
  Lie[x_, v_] :>
    Module[{di, diLower, diUpper},
      diLower = dummyNotIn[x];
      diUpper = toggleIndex[diLower];
      v[diUpper] PD[x,diLower] + Apply[Plus,Map[LietoPDTerm[x, #, v] &, indicesIn[x]]]];

(* Give the term in the expansion of the Lie derivative corresponding
   to the index i in x.  Differentiation is with respect to v. *)

LietoPDTerm[x_, i:TensorIndex[_,lower], v_] :=
  Module[{di = dummyNotIn[{x}]},
    TensorProduct[(x /. i -> di), PD[v[toggleIndex[di]], i]]];

LietoPDTerm[x_, i:TensorIndex[_,upper], v_] :=
  Module[{di = dummyNotIn[{x}]},
    -TensorProduct[(x /. i -> toggleIndex[di]), PD[v[i], di]]];

LieToPDFullRule := x_ :> (((x //. LieReduce) //. LieToPDRuleTensorProduct) //. LieToPDRule);

LieToPD[x_] :=
  x //. LieToPDFullRule //. removeSingleTensorProducts;

(* -------------------------------------------------------------------------- 
   Local Partial Derivatives
   -------------------------------------------------------------------------- *)

jacobians = {};

DefineJacobian[pd_, fd_, J_, dJ_] :=
  Module[{},
    jacobians = Join[jacobians, {{pd, fd, J, dJ}}]];

ResetJacobians := Module[{}, jacobians = {}];



(* This is duplicated (and extended!) from the PD case.  *)

(* Reduction rules *)

FDLeibnizTimes := 
  FD[x_ y_, i_] :> FD[x,i] y + x FD[y,i];

FDLeibnizTensorProduct := 
  FD[TensorProduct[s_, t__], i_] :> 
    TensorProduct[FD[s,i],t] + TensorProduct[s,FD[TensorProduct[t],i]];

FDLinearity :=
  FD[x_ + y_,i_] :> FD[x,i] + FD[y,i];

FDFlatten := 
  FD[FD[x_,is__],js__] :> FD[x,is,js];

FDUnflatten :=
  FD[x_, i_, is__] :> FD[FD[x,i],is];

FDJacobi :=
  FD[J[i_,j_], k_] :> dJ[i,j,k];

FDConst :=
  FD[x:(_Real | _Integer), is__] -> 0;

FDReduce :=
  x_ :> (((((((x //. FDUnflatten) //. FDJacobi) //. FDLeibnizTimes) //. FDLeibnizTensorProduct) //. FDLinearity) //. FDFlatten) //. FDConst);

Format[FD[x_ ? AtomQ, is:((TensorIndex[_,_] | _Integer) ..)]] :=
  SequenceForm[x, ",", is];

Format[FD[Tensor[k_,inds__], is:((TensorIndex[_,_] | _Integer) ..)]] :=
  SequenceForm[k, inds, ",", is];

Format[FD[x_, is:((TensorIndex[_,_] | _Integer) ..)]] :=
  SequenceForm["(",x,")", ",", is];



(* After the PD is converted to a FD, it will contain a dummy index.
   This needs careful attention, so instead of leaving the result in
   its TensorProduct wrapper, we remove it and use a Times instead.
   This will force the dummy indices to be checked and relabelled
   automatically, and the result placed into a TensorProduct when it
   is clean. *)

PDtoFDRuleTensorProduct := 
  TensorProduct[t1___, PD[x_,i___], t2___] :> 
    (TensorProduct[t1, t2] /. PDtoFDRuleTensorProduct) PD[x,i]

(* Convert a PD into an FD. *)

PFdummies := {};
setDummies[x_] := Module[{dums},
                         dums=remainingLowerIndices[x];
                         (* Print["Setting PFdummies to",dums]; *)
                         PFdummies:=dums];
newDummy := Module[{fst,rem},
                   fst=First[PFdummies]; rem=Rest[PFdummies];
                   (* Print["new dummy",fst]; *)
                   PFdummies:=rem; fst];

PDtoFDRule1[t_] :=
  PD[x_, i_] :>
    Module[{la, ua},
      (* la = lz; *)
      (* la = dummyNotIn[{t,x,i}]; *)
      la = newDummy;
      ua = toggleIndex[la];
      J[ua,i] FD[x,la]];
PDtoFDRule2[t_] :=
  PD[x_, i_, j_] :>
    Module[{la, ua, lb, ub},
      (* la = lz; *)
      (* la = dummyNotIn[{t,x,i,j}]; *)
      la = newDummy;
      ua = toggleIndex[la];
      (* lb = ly; *)
      (* lb = dummyNotIn[{t,x,i,j,la}]; *)
      lb = newDummy;
      ub = toggleIndex[lb];
      dJ[ua,i,j] FD[x,la] + J[ua,i] J[ub,j] FD[x,la,lb]];
PDtoFDRule[t_] :=
  x_ :> (((x /. PDtoFDRule2[t]) /. PDtoFDRule1[t]) //. FDReduce);



PDtoFDDefaultJacobian[x_] := Module[{y,z},
  y = x /. PDtoFDRuleTensorProduct;
  setDummies[y];
  z = y /. PDtoFDRule[y];
  setDummies[0];
  z /. removeSingleTensorProducts];

PDtoFDForJacobian[x_, jacobian_] :=
  Module[{pd,fd,j,dj, y, result},
    pd = jacobian[[1]];
    fd = jacobian[[2]];
    j  = jacobian[[3]];
    dj = jacobian[[4]];
    (* The above rules use the global function names PD, FD, J, and
       dJ.  We therefore have to swap these names in and out.  *)
    y = x /. If[pd=!=PD, {PD -> quotedPD}, {}]
          /. If[pd=!=PD, {pd -> PD      }, {}]
          /. If[fd=!=FD, {FD -> quotedFD}, {}]
          /. If[fd=!=FD, {fd -> FD      }, {}]
          /. If[j =!=J , {J  -> quotedJ }, {}]
          /. If[j =!=J , {j  -> J       }, {}]
          /. If[dj=!=dJ, {dJ -> quoteddJ}, {}]
          /. If[dj=!=dJ, {dj -> dJ      }, {}];
    result = PDtoFDDefaultJacobian[y];
    result /. If[dj=!=dJ, {dJ       -> dj}, {}]
           /. If[dj=!=dJ, {quoteddJ -> dJ}, {}]
           /. If[j =!=J , {J        -> j }, {}]
           /. If[j =!=J , {quotedJ  -> J }, {}]
           /. If[fd=!=FD, {FD       -> fd}, {}]
           /. If[fd=!=FD, {quotedFD -> FD}, {}]
           /. If[pd=!=PD, {PD       -> pd}, {}]
           /. If[pd=!=PD, {quotedPD -> PD}, {}]];

PDtoFDForJacobians[x_, js_] :=
  If[js == {},
    x,
    PDtoFDForJacobians[PDtoFDForJacobian[x, First[js]], Rest[js]]];

PDtoFD[x_] :=
  PDtoFDForJacobians[x, jacobians];



(* -------------------------------------------------------------------------- 
   Kronecker delta
   -------------------------------------------------------------------------- *)

KDrule := KD[i_Integer, j_Integer] :> KroneckerDelta[Abs[i],Abs[j]];

Format[KD[is:((TensorIndex[_,_] | _Integer) ..)]] :=   
  SequenceForm[delta,is];

(* -------------------------------------------------------------------------- 
   Alternating symbol
   -------------------------------------------------------------------------- *)

EpsilonRule := Eps[x__] :> Signature[Map[Abs, {x}]];

(* -------------------------------------------------------------------------- 
   Euclidean metric
   -------------------------------------------------------------------------- *)

Euc[a_,b_] := If[a == b, 1, 0];

(* -------------------------------------------------------------------------- 
   Zero3
   -------------------------------------------------------------------------- *)

Zero3[a_,b_,c_] := 0;

(* -------------------------------------------------------------------------- 
   Miscellaneous
   -------------------------------------------------------------------------- *)

appendNonduplicates[l_, a_] :=
  If[l == {},
     a,
     If[MemberQ[a, l[[1]]],
        appendNonduplicates[Rest[l], a],
        appendNonduplicates[Rest[l], Flatten[{l[[1]], a}]]]];

RemoveDuplicates[l_] :=
  Reverse[appendNonduplicates[l,{}]];

extractMapDomain[m_] :=
  RemoveDuplicates[Map[#[[1]] &, m]];

removeDuplicatesFromMap[m_] :=
  Map[# -> (# /. m) &, extractMapDomain[m]];


abstractToExplicitMap[m_] :=
  Module[{explicitMap1, domain},
    explicitMap1 = Flatten[Map[MakeExplicit, m], 1];
    domain = extractMapDomain[explicitMap1];
    Map[# -> (# /. explicitMap1) &, domain]];

explicitVariableList[l_] :=
  RemoveDuplicates[Flatten[Map[MakeExplicit, l], 1]];

(* -------------------------------------------------------------------------- 
   Checking tensor expressions for consistency
   -------------------------------------------------------------------------- *)

(* These functions all throw an exception if an expression is invalid *)

CheckTensors[l_List] :=
  Module[{},
(*    Print["CheckTensors: list: ", l];*)
    Map[CheckTensors, l]];

(*CheckTensors[x:Tensor[k_, is__] -> y_] :=
  CheckTensors[x,y];*)

CheckTensors[x_ -> y_] :=
  Module[{},
(*    Print["Checking rule ", x -> y];*)
    CheckTensors[x];
    CheckTensors[y];

(*    Print["tensorialQ[x] == ", tensorialQ[x]];
    Print["tensorialQ[y] == ", tensorialQ[y]];*)

    If[tensorialQ[x] || tensorialQ[y],
      CheckTensors[x,y]];
    True];

CheckTensors[f t:Tensor[k_,is__]] :=
  Module[{},
(*  Print["CheckTensors: f t:"];*)
  CheckTensor[t]];

CheckTensors[a_ t:TensorProduct[x_,y_]] :=
  Module[{},
(*  Print["CheckTensors: a tp:"];*)
  CheckTensors[t]];

CheckTensors[x_ y_] :=
  Module[{},
(*  Print["CheckTensors: x y:"];*)
  CheckTensors[TensorProduct[x,y]]];

CheckTensors[TensorProduct[x_,y_]] :=
  Module[{xs,ys},
(*  Print["CheckTensors: TenPr:"];*)
    CheckTensors[x];
    CheckTensors[y];
    xs = freesIn[x];
    ys = freesIn[y];

    If[!(Intersection[xs,ys] === {}),
       ThrowError["Tensor expressions have conflicting indices: ", x, y, xs, ys]];
    True];

CheckTensors[x_ + y_] :=
  Module[{},
(*  Print["CheckTensors: x + y:"];*)
  CheckTensors[x,y]];

CheckTensors[x_, y_] :=
  Module[{xs,ys},
(*  Print["CheckTensors: x,y:"];*)
    CheckTensors[x];
    CheckTensors[y];
    xs = freesIn[x];
    ys = freesIn[y];

(*    Print["CheckTensors[", x, ",", y,"]"];
    Print["xs == ", xs];
    Print["ys == ", ys];*)

    If[ x === 0 || y === 0, Return[True]];

    (* Allow scalar functions to be added to tensorial expressions *)
    If[ Length[xs] === 0 || Length[ys] === 0, Return[True]];



    If[ Length[ys] == 0, Return[True]];

    If[!(xs === ys),
(*      Print["Throwing..."];*)
      ThrowError["Tensor expressions have mismatched indices: ", x, y, xs, ys]];
    True;
      ];

CheckTensors[t:Tensor[k_, is__]] :=
  Module[{is2},
(*    Print["CheckTensors: Tensor: ", t];*)
    is2 = Select[{is}, !NumericQ[#]&];
    If[!(Union[is2] === Sort[is2]),
       ThrowError["Tensor has repeated indices: ", t, is2]];
    True];

CheckTensors[t:f_[TensorIndex[__]..]] :=
  Module[{},
(*    Print["CheckTensors: Head is ", f];*)
    If[!(f === Tensor || f === Eps || f === KroneckerDelta),
       ThrowError["Tensor index in an object that is not a declared tensor.", t]];
       ];

CheckTensors[x_] := 
  Module[{},
(*    Print["Default tensor check: ", x];*)
    True];

(* Reflection symmetries *)

calcSymmetryOfComponent[comp_, inds_] := 
  Module[{sym, q, string},
    sym = {1, 1, 1};  (* default *)
    string = ToString[comp];

    If[inds > StringLength[string],
      ThrowError["calcSymmetryOfComponent: Component " 
      <> ToString[comp] <> " has been described as having " 
      <> ToString[inds] <> " indices, but it does not have enough characters."]];

    Do[
      (* Get the index at the ith position as a number *)
      q = ToExpression[StringTake[string, {i}]];
      If[!IntegerQ[q], ThrowError["calcSymmetryOfComponent: Expecting a numeric index at position " 
        <> ToString[i] <> " in \"" <> string <> "\": \"" <> ToString[q] <> "\"."]];

      sym[[q]] = -sym[[q]],
      
      {i, StringLength[string] - inds + 1, StringLength[string]}];
      
      sym];


ReflectionSymmetriesOfTensor[Tensor[k_, inds__]] :=
  Module[{is = {inds},
         sym = {1,1,1}},

    indexCount = Length[is];
    components = MakeExplicit[k[inds]];

    If[HasTensorAttribute[k, TensorParity],
      sym = sym * GetTensorAttribute[k, TensorParity]];
  
    Map[# -> calcSymmetryOfComponent[#, indexCount] &, components]
  ];

ReflectionSymmetriesOfTensor[f_] :=
  f -> {1,1,1};

ReflectionSymmetriesOfTensor[f_ ? IsTensor] :=
  Module[{sym = {1,1,1}},

    If[HasTensorAttribute[f, TensorParity],
      sym = sym * GetTensorAttribute[f, TensorParity]];
    If[HasTensorAttribute[f, TensorManualCartesianParities],
      sym = GetTensorAttribute[f, TensorManualCartesianParities]];

    f -> sym
  ];
  
(* -------------------------------------------------------------------------- 
   TensorAttributes
   -------------------------------------------------------------------------- *)


SetTensorAttribute[k_, attr_, value_] :=
  Module[{oldmap, newmap},
    oldMap = TensorAttributes[k];
    If[!ListQ[oldMap],
      ThrowError["SetTensorAttribute: " <> ToString[k] <> " has not been defined as a tensor"]];

    If[mapContains[oldMap, attr],
      newMap = mapReplace[oldMap, attr, value],
      newMap = mapAdd[oldMap, attr, value]];
    TensorAttributes[k] = newMap;
    ];

GetTensorAttribute[k_, attr_] :=
  Module[{oldMap},
    oldMap = TensorAttributes[k];
    If[!ListQ[oldMap],
      ThrowError["GetTensorAttribute: " <> ToString[k] <> " has not been defined as a tensor"]];
    If[mapContains[oldMap, attr],
      Return[lookup[oldMap, attr]],
      ThrowError["Tensor " <> ToString[k] <> " does not have a " <> ToString[attr] <> " attribute."]]
  ];

HasTensorAttribute[k_, attr_] :=
  Module[{oldMap},
    oldMap = TensorAttributes[k];
    If[!ListQ[oldMap],
      ThrowError["HasTensorAttribute: " <> ToString[k] <> " has not been defined as a tensor"]];
    mapContains[oldMap, attr]];




IsTensor[k_] :=
  ListQ[TensorAttributes[k]];






End[];

EndPackage[];
