
(* 

Differencing in Kranc
=====================

The user works in terms of "partial derivatives".  For example,
expressions of the form PD[phi,1,2] for the mixed partial derivative
of the function phi in the 1 and 2 directions.

Each of these partial derivatives (i.e. "PD") is defined in terms of
an expression involving "difference operators", e.g. dzero[i_], where
i is the direction.  So whenever the user enters PD[phi,1,2], what
gets calculated is dzero[1] dzero[2] phi.

These difference operators are defined in terms of the elementary
"shift" operators: dplus[i_] := (shift[i] - 1)/spacing[i], and can be
composed with each other as a result: dzero[i_] := (dplus[i] +
dminus[i]) / 2.

The definition of a partial derivative in terms of difference
operators is given in the form:

partialDerivative = 
{Name -> PD,
 Macro -> FD_C2,
 Rules ->
 {PD[i_] -> dzero[i],
  PD[i_, i_] -> dplus[i] dminus[i],
  PD[i_, j_] -> dzero[i] dzero[j]}};

The source code generated contains the definitions of these difference
operators as macros.  E.g. 

#define PD1(u,i,j,k) (u[i+1,j,k] - u[i-1,j,k]) hdxi

Then, at the start of each loop, all necessary derivatives are
precomputed:

PD1phi = PD1(phi,i,j,k);

Then, in the calculation itself, the variable PD1phi is used.

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

(* Given a grid function derivative, return the C variable name that
   we will use to precompute its value. *)
GFDerivativeName[pd_[gf_, i_]] := 
Symbol["Global`" <> ToString[pd] <> ToString[i] <> ToString[gf]];

(* Given a grid function derivative, return the C variable name that
   we will use to precompute its value. *)
GFDerivativeName[pd_[gf_, i_, j_]] := 
Symbol["Global`" <> ToString[pd] <> ToString[i] <> ToString[j] <> ToString[gf]];


(* Given a derivative, return the macro name used for it *)
DerivativeName[pd_[i_]] := Symbol["Global`" <> ToString[pd] <> ToString[i]];
DerivativeName[pd_[i_, j_]] := Symbol["Global`" <> ToString[pd] <> ToString[i] <> ToString[j]];

(* Return a codegen block to precompute the given grid function
   derivative *)
PrecomputeDerivative[d:pd_[gf_, inds__]] :=
  AssignVariable[GFDerivativeName[d], {DerivativeName[pd[inds]], "(", gf,", i, j, k)"}];

(* Return a codegen block to declare a grid function derivative
   precompute variable *)
DeclareDerivative[d:pd_[gf_, inds__]] :=
  DeclareVariable[GFDerivativeName[d], "CCTK_REAL"];

(* Given a derivative definition, return all the derivatives that
   could be defined using the name of the definition *)
PDsFromDefinition[def_] :=
  AllDerivatives[lookup[def, Name]];

(* Given a list of derivative definitions, return all the possible
   derivatives that could be defined in it *)
ListAllPDs[defs_] :=
  Apply[Join, Map[PDsFromDefinition, defs]];

(* List all the grid function derivatives in x that are defined in
   pddefs *)
GFDsInExpression[x_, pddefs_] :=
  Module[{},
    pds = Flatten[Map[PDsFromDefinition, pddefs],1];
    gfds = Flatten[Map[GFDsInExpressionForPD[x,#] &, pds], 1];
    gfds];

(* List all the grid function derivatives in x that are generated from
   pd *)
GFDsInExpressionForPD[x_, pd_[inds__]] :=
  Union[Cases[x, pd[gf_, inds], Infinity]];

(* Given a derivative, return a rule that can be applied to an
   expression to convert that derivative into its corresponding macro
   call *)
PDToReplacementRule[pd_[inds__]] :=
  pd[x_, inds] :> GFDerivativeName[pd[x,inds]];

(* Return all the rules for converting derivatives into macros that
   are defined in pddefs *)
AllGFDRules[pddefs_] :=
  Map[PDToReplacementRule, ListAllPDs[pddefs]];

(* Given a single derivative, return the macro that defines it *)
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

(* List all the derivatives with a particular name  *)
AllDerivatives[name_] :=
  Join[Table[name[i],{i,1,3}], Flatten[Table[name[i,j], {i,1,3},{j,1,3}],1]];

(* Given the name of a derivative, and its expression in terms of
   shift operators, return a codegen block defining the macro *)
ConstructDifferenceMacro[name_, op_] :=
  Module[{rhs, rhs2, b},
    rhs = DifferenceGF[op, i, j, k];
    rhs2 = CFormHideStrings[ReplacePowers[rhs]];
    FlattenBlock[{"#define ", name, "(u,i,j,k) ", rhs2}]];

(* Convert an expression to CForm, but remove the quotes from any
   strings present *)
CFormHideStrings[x_] := StringReplace[ToString[CForm[x]], "\"" -> ""];

(* Farm out each term of a difference operator *)
DifferenceGF[op_, i_, j_, k_] :=
  Module[{expanded},
    expanded = Expand[op];

    If[Head[expanded] != Plus,
    Throw["Expecting Plus as head of " <> op]];

    Apply[Plus, Map[DifferenceGFTerm[#, i, j, k] &, expanded]]];

(* Return the fragment of a macro definition for defining a derivative
   operator *)
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
