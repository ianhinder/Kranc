
(*

Second generation of custom differencing
========================================

The user supplies a list of "derivative operators".  These are
expressions in shift operators shift[1], shift[2] and shift[3].  The
derivative operators can take an arbitrary number of numeric
arguments.  For example, the standard differencing would be performed
by a list of operators of the form:

 PD[i_] -> dzero[i],
 PD[i_] -> dzero[i],
 PD[i_, i_] -> dplus[i] dminus[i],
 PD[i_, j_] -> dzero[i] dzero[j]

You can include derivative operators which have no arguments.  If you
wanted the Laplacian, you would use

 Lap[] -> Sum[dplus[i] dminus[i], {i,1,3}]

We would like these definitions to be optionally conditional on the
definition of some preprocessor macro, so we can support the old
behaviour by default. We can add this behaviour later.  This means we
need to supply examples for the fourth order differencing operators
for people to use easily.

In a calculation, the user uses expressions like PD[phi,1,2].  Kranc
generates macro definitions for each derivative; i.e. in this case it
would create a macro definition for PD12(u,i,j,k).  At the start of a
calculation loop, variables are created to store the results of
precomputing each of the derivatives needed in that loop.
e.g. PD12phi = PD12(phi,i,j,k).  Kranc then replaces PD[phi,1,2] with
PD12phi in the calculation.

*)

(*

Types and data structures
=========================

DerivativeOperator
~~~~~~~~~~~~~~~~~~

A DerivativeOperator (derivOp) is an expression of the form

  name_[patterns__] -> expr_

where expr is a sum of products of shift operators, spacings, and
numerical factors.  It represents how an arbitrary grid function
should be differenced as a result of this derivative operator.  Note
that the grid function itself is omitted from the definition.  For
example,

  PD[i_] -> 1/2(shift[i_] + 1/shift[i])

ComponentDerivativeOperator
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A DerivativeOperator (derivOp) is an expression of the form

  name_[indices__] -> expr_

which is the same as a DerivativeOperator but with the indices
numerical.

  PD[2] -> 1/2(shift[2] + 1/shift[2])

GridFunctionDerivative
~~~~~~~~~~~~~~~~~~~~~~

A GridFunctionDerivative (GFD) is an expression of the form

  name_[gf_,index___]

for example

  PD[phi,1,2]

It is in the form of an expression the user would enter in a
calculation.

User API
========

ConstructDifferencingHeader[derivOps_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a list of DerivativeOperators, return a CodeGen block consisting
of the header file which needs to be included before any calculations.

PrecomputeDerivatives[derivOps_, expr_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Return a CodeGen block which precomputes all the derivatives needed in
expr.

DeclareDerivatives[derivOps_, expr_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Return a CodeGen block which precomputes all the derivatives needed in
expr.

ReplaceDerivatives[derivOps_, expr_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Replace all the GridFunctionDerivatives in expr with their variable
names.

*)

BeginPackage["sym`"];

{Name, Definitions, shift, spacing};

EndPackage[];

BeginPackage["Differencing`", {"CodeGen`", "sym`", "MapLookup`"}];

CreateDifferencingHeader::usage = "";
PrecomputeDerivatives::usage = "";
DeclareDerivatives::usage = "";
ReplaceDerivatives::usage = "";
DPlus::usage = "";
DMinus::usage = "";
DZero::usage = "";
shift::usage = "";
spacing::usage = "";

Begin["`Private`"];

DPlus[n_] := (shift[n] - 1)/spacing[n];
DMinus[n_] := (1 - 1/shift[n])/spacing[n];
DZero[n_] := (DPlus[n] + DMinus[n])/2;

(*************************************************************)
(* User API *)
(*************************************************************)

CreateDifferencingHeader[derivOps_] :=
  Module[{componentDerivOps, dupsRemoved, expressions},
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents, derivOps]];
    dupsRemoved = RemoveDuplicateRules[componentDerivOps];
    expressions = Flatten[Map[ComponentDerivativeOperatorMacroDefinition, dupsRemoved]];
    Map[{#, "\n"} &, expressions]];


PrecomputeDerivatives[derivOps_, expr_] :=
  Module[{componentDerivOps, gfds},
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents, derivOps]];
    gfds = GridFunctionDerivativesInExpression[derivOps, expr];
    Map[PrecomputeDerivative, gfds]];

DeclareDerivatives[derivOps_, expr_] :=
  Module[{componentDerivOps, gfds},
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents, derivOps]];
    gfds = GridFunctionDerivativesInExpression[derivOps, expr];
    Map[DeclareDerivative, gfds]];

ReplaceDerivatives[derivOps_, expr_] :=
  Module[{componentDerivOps, gfds},
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents, derivOps]];
    gfds = GridFunctionDerivativesInExpression[derivOps, expr];
    rules = Map[# :> GridFunctionDerivativeName[#] &, gfds];
    expr /. rules];


(*************************************************************)
(* Misc *)
(*************************************************************)

PrecomputeDerivative[d:pd_[gf_, inds___]] :=
  Module[{},
    macroName = ComponentDerivativeOperatorMacroName[pd[inds] -> expr];
    AssignVariable[GridFunctionDerivativeName[d], {macroName, "(", gf,", i, j, k)"}]];

DeclareDerivative[d:pd_[gf_, inds___]] :=
  DeclareVariable[GridFunctionDerivativeName[d], "CCTK_REAL"];


(*************************************************************)
(* GridFunctionDerivative *)
(*************************************************************)

GridFunctionDerivativeName[pd_[gf_, inds___]] :=
  Module[{},
    stringName = StringJoin[Map[ToString, Join[{pd}, {inds}, {gf}]]];
    Symbol["Global`" <> stringName]];


GridFunctionDerivativesInExpression[derivOps_, expr_] := 
  Module[{componentDerivOps, derivs},
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents, derivOps]];
    derivs = Map[First, componentDerivOps];
    patterns = Map[# /. x_[inds___] -> x[y_, inds] &, derivs];
    Flatten[Map[Union[Cases[{expr}, #, Infinity]] &, patterns]]];


(*************************************************************)
(* DerivativeOperator *)
(*************************************************************)


ComponentDerivativeOperatorMacroDefinition[componentDerivOp:(name_[inds___] -> expr_)] :=
  Module[{macroName, rhs, rhs2, i = "i", j = "j", k = "k", spacings},
    macroName = ComponentDerivativeOperatorMacroName[componentDerivOp];
    rhs = DifferenceGF[expr, i, j, k];
    spacings = {spacing[1] -> 1/"dxi", spacing[2] -> 1/"dyi", spacing[3] -> 1/"dzi"};
    rhs2 = CFormHideStrings[ReplacePowers[rhs /. spacings]];
    FlattenBlock[{"#define ", macroName, "(u,i,j,k) ", rhs2}]];

ComponentDerivativeOperatorMacroName[componentDerivOp:(name_[inds___] -> expr_)] :=
  Module[{stringName},
    stringName = StringJoin[Map[ToString, Join[{name}, {inds}]]];
    stringName];

(* Farm out each term of a difference operator *)
DifferenceGF[op_, i_, j_, k_] :=
  Module[{expanded},
    expanded = Expand[op];

    If[Head[expanded] === Plus,
      Apply[Plus, Map[DifferenceGFTerm[#, i, j, k] &, expanded]],
      DifferenceGFTerm[expanded, i, j, k]]];


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


DerivativeOperatorGFDs[gf_];

DerivativeOperatorToComponents[name_[indPatterns___] -> expr_] :=
  Module[{ips, symbols, symbolRanges, symbolLHS, table},
    ips = {indPatterns};
    symbols = Map[First, ips];
    symbolRanges = Map[{#, 1, 3} &, Union[symbols]];
    symbolLHS = name[Apply[Sequence, symbols]];
    table = Apply[Table, Join[{symbolLHS -> expr}, symbolRanges]];
    Flatten[table]];


RemoveDuplicates[l_] :=
  Module[{this,next,rest,positions},
    If[l === {},
      Return[{}]];
    this = First[l];
    rest = Rest[l];
    If[FreeQ[rest, this],
       Prepend[RemoveDuplicates[rest],this],

       positions = Position[rest, this];
       next = Delete[rest, positions];
       Prepend[RemoveDuplicates[next], this]]];

RemoveDuplicateRules[l_] :=
  Module[{lhs,lhs2,rhs2,result},

    Print["Removing duplicates in ", l];

    lhs = Map[First, l];
    lhs2 = RemoveDuplicates[lhs];
    rhs2 = lhs2 /. l;

    result = Thread[Rule[lhs2,rhs2]];
    Print["Result is: ", result];
    result];



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



End[];

EndPackage[];
