
(* 

Differencing in Kranc
=====================

The user works in terms of "partial derivatives".  For example,
expressions of the form PD[phi,1,2] for the mixed partial derivative
of the function phi in the 1 and 2 directions.

Each of these partial derivatives (i.e. "PD") is defined in terms of a
"difference operator", e.g. dzero[i_], where i is the direction.  So
whenever the user enters PD[phi,1,2], what gets calculated is dzero[1]
dzero[2] phi.

These difference operators are defined in terms of the elementary
"shift" operators: dplus[i_] := (shift[i] - 1)/spacing[i], and can be
composed with each other as a result: dzero[i_] := (dplus[i] +
dminus[i]) / 2.

The definition of a partial derivative in terms of difference
operators is local to a particular calculation.  The definition is
given in the form:

PD[u_, i_] -> dzero[i],
PD[u_, i_] -> dzero[i],
PD[u_, i_, i_] -> dplus[i] dminus[i],
PD[u_, i_, j_] -> dzero[i] dzero[j]

The source code generated contains the definitions of these difference
operators as macros.  E.g. 

#define PD_1(u,i,j,k) (u[i+1,j,k] - u[i-1,j,k]) hdxi

Then, at the start of each loop, all necessary derivatives are
precomputed:

PD_1_phi = PD_1(phi,i,j,k);

Then, in the calculation itself, the variable PD_1_phi is used.

In order to maintain compatibility with the existing method of
operation, we allow a partial derivative definition to be conditional
on a particular preprocessor macro.  So the full definition of a
PartialDerivative is: *)

(*
partialDerivative = 
{Name -> PD,
 Macro -> FD_C2,
 Rules ->
 {PD[i_] -> dzero[i],
  PD[i_, i_] -> dplus[i] dminus[i],
  PD[i_, j_] -> dzero[i] dzero[j]}};
*)

(* 

If such a conditional is present, then the definition of the PD_1 etc
macros will use it.  This allows several different differencing
schemes to be implemented, and the behaviour to be chosen at Cactus
configuration time.

*)

BeginPackage["sym`"];

{Name, Definitions, dxi, dyi, dzi, i, j, k, shift, spacing};

EndPackage[];

BeginPackage["Differencing`", {"CodeGen`", "sym`", "MapLookup`"}];

ConvertPartialDerivativeToMacros::usage = "";
GFDsInExpression::usage = "";
DeclareDerivative::usage = "";
AllGFDRules::usage = "";
PrecomputeDerivative::usage = "";
DPlus::usage = "";
DMinus::usage = "";
DZero::usage = "";


Begin["`Private`"];


DPlus[n_] := (shift[n] - 1)/spacing[n];
DMinus[n_] := (1 - 1/shift[n])/spacing[n];
DZero[n_] := (DPlus[n] + DMinus[n])/2;


GFDerivativeName[pd_[gf_, i_]] := 
Symbol["Global`" <> ToString[pd] <> ToString[i] <> ToString[gf]];

GFDerivativeName[pd_[gf_, i_, j_]] := 
Symbol["Global`" <> ToString[pd] <> ToString[i] <> ToString[j] <> ToString[gf]];

DerivativeName[pd_[i_]] := Symbol["Global`" <> ToString[pd] <> ToString[i]];
DerivativeName[pd_[i_, j_]] := Symbol["Global`" <> ToString[pd] <> ToString[i] <> ToString[j]];

PrecomputeDerivative[d:pd_[gf_, inds__]] :=
  AssignVariable[GFDerivativeName[d], {DerivativeName[pd[inds]], "(", gf,", i, j, k)"}];

DeclareDerivative[d:pd_[gf_, inds__]] :=
  DeclareVariable[GFDerivativeName[d], "CCTK_REAL"];

PDsFromDefinition[def_] :=
  AllDerivatives[lookup[def, Name]];

ListAllPDs[defs_] :=
  Apply[Join, Map[PDsFromDefinition, defs]];


GFDsInExpression[x_, pddefs_] :=
  Module[{},
    pds = Flatten[Map[PDsFromDefinition, pddefs],1];
    Print["pds == ", pds];
    gfds = Flatten[Map[GFDsInExpressionForPD[x,#] &, pds], 1];
    gfds];

GFDsInExpressionForPD[x_, pd_[inds__]] :=
  Union[Cases[x, pd[gf_, inds], Infinity]];

PDToReplacementRule[pd_[inds__]] :=
  pd[x_, inds] :> GFDerivativeName[pd[x,inds]];


AllGFDRules[pddefs_] :=
  Map[PDToReplacementRule, ListAllPDs[pddefs]];



(*

A "partial derivative" or "PD" is an expression of the form
pd_[inds__].  A "grid function derivative" or "GFD" is an expression
of the form pd_[gf_, inds__].  A "partial derivative definition" or
"pdd" is the structure that defines the differencing for a partial
derivative.

Get a list of all the PDs that could exist.

Find all the GFDs in whatever expressions we want to precompute for.

Output the code to precompute these derivatives.

Convert the GFDs into rules for replacing each GFD with its variable
name.

Apply these rules to the expression.

*)






ConvertPartialDerivativeToMacros[pd_] :=
  Module[{name, all, rules, spacings, rhss, names, pairs, macros},
    name = lookup[pd, Name];
    all = AllDerivatives[name];
    rules = lookup[pd, Definitions];
    spacings = {spacing[1] -> 1/dxi, spacing[2] -> 1/dyi, spacing[3] -> 1/dzi};
    rhss = all /. rules /. spacings;
    names = all /. name[inds__] :> DerivativeName[name[inds]];
    pairs = Thread[Rule[names, rhss]];


    macros = Map[ConstructDifferenceMacro[#[[1]], #[[2]]] &, pairs];

    macros];

AllDerivatives[name_] :=
  Join[Table[name[i],{i,1,3}], Flatten[Table[name[i,j], {i,1,3},{j,1,3}],1]];

ConstructDifferenceMacro[name_, op_] :=
  Module[{rhs, rhs2, b},

  rhs = DifferenceGF[op, i, j, k];

  rhs2 = CFormHideStrings[ReplacePowers[rhs]];
(*
  rhs2 = CFormReplace[rhs, IndexFunction[i_,j_,k_] -> "u[CCTK_GFINDEX3D(cctkGH, " 
<> ToString[CForm[i]] <> 
    ", " <> ToString[CForm[j]] <> ", " <> ToString[CForm[k]] <> ")]"];*)

  b = FlattenBlock[{"#define ", name, "(u,i,j,k) ", rhs2}];
  b];



(*
  hide = Map[# -> Unique[] &, Cases[rhs, IndexFunction[__], Infinity]];

  renderIndexFunction[IndexFunction[u_, i_, j_, k_]] :=
    "u[CCTK_GFINDEX3D(cctkGH, " <> ToString[CForm[i]] <> 
    ", " <> ToString[CForm[j]] <> ", " <> ToString[CForm[k]] <> ")]";

  restore = Map[#[[2]] -> #[[1]] &, hide];

  restore2 = Map[ToString[#[[1]]] -> renderIndexFunction[#[[2]]] & , restore];

  rhs2 = ToString[CForm[rhs /. hide]];

  rhs3 = StringReplace[rhs2, restore2];

  FlattenBlock[
  {"#define ", name, "(u,i,j,k) ", rhs3}]];
*)


CFormReplace[x_, a_ -> b_] :=
  Module[{hide, restore, restore2, x2, x3},
    hide = Map[# -> Unique[] &, Cases[x, a, Infinity]];
    restore = Map[#[[2]] -> #[[1]] &, hide];
    Print["a -> b == ", a -> b];
    restore2 = Map[ToString[#[[1]]] -> (#[[2]] /. a :> b) & , restore];
    x2 = ToString[CForm[x /. hide]];
    x3 = StringReplace[x2, restore2]];

CFormHideStrings[x_] := StringReplace[ToString[CForm[x]], "\"" -> ""];

DifferenceGF[op_, i_, j_, k_] :=
  Module[{expanded},
    expanded = Expand[op];

    If[Head[expanded] != Plus,
    Throw["Expecting Plus as head of " <> op]];


    Apply[Plus, Map[DifferenceGFTerm[#, i, j, k] &, expanded]]];

DifferenceGFTerm[op_, i_, j_, k_] :=
  Module[{nx, ny, nz, remaining},
    If[Head[op] != Times,
      Throw["Expecting Times in " <> FullForm[op]]];
    nx = Exponent[op, shift[1]];
    ny = Exponent[op, shift[2]];
    nz = Exponent[op, shift[3]];

    remaining = op / (shift[1]^nx) / (shift[2]^ny) / (shift[3]^nz);

    remaining "u[CCTK_GFINDEX3D(cctkGH," <> ToString[i+nx] <> "," <> 
      ToString[j+ny] <> "," <> ToString[k+nz] <> ")]"];

End[];

EndPackage[];
