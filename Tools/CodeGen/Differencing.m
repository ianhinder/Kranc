
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

GridFunctionDerivativesInExpression[derivOps_, expr_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Return a list of GF derivatives that are used in expr.


ReplaceDerivatives[derivOps_, expr_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Replace all the GridFunctionDerivatives in expr with their variable
names.

StandardCenteredDifferenceOperator[p_, m_, i_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Return a difference operator approximating a derivative of order p
using m grid points before and m grid points after the centre
point. Should be checked by someone competent!


*)

BeginPackage["sym`"];

{Name, Definitions, shift, spacing, SBPDerivative};

EndPackage[];

BeginPackage["Differencing`", {"CodeGen`", "sym`", "MapLookup`", 
             "LinearAlgebra`MatrixManipulation`", "Errors`"}];

CreateDifferencingHeader::usage = "";
PrecomputeDerivatives::usage = "";
DeclareDerivatives::usage = "";
ReplaceDerivatives::usage = "";
StandardCenteredDifferenceOperator::usage = "";
GridFunctionDerivativesInExpression::usage = "";
DPlus::usage = "";
DMinus::usage = "";
DZero::usage = "";
shift::usage = "";
spacing::usage = "";
ComponentDerivativeOperatorStencilWidth::usage = "";

Begin["`Private`"];

DPlus[n_] := (shift[n] - 1)/spacing[n];
DMinus[n_] := (1 - 1/shift[n])/spacing[n];
DZero[n_] := (DPlus[n] + DMinus[n])/2;

(*************************************************************)
(* User API *)
(*************************************************************)

CreateDifferencingHeader[derivOps_, zeroDims_] :=
  Module[{componentDerivOps, dupsRemoved, expressions, componentDerivOps2, zeroDimRules, derivOps2, pDefs},
    Map[DerivativeOperatorVerify, derivOps];

    zeroDimRules = Map[shift[#] -> 1 &, zeroDims];

    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents, derivOps]];

    componentDerivOps2 = componentDerivOps /. zeroDimRules;

    dupsRemoved = RemoveDuplicateRules[componentDerivOps2];

    mDefPairs = Map[ComponentDerivativeOperatorMacroDefinition, dupsRemoved];

    pDefs = Union[Flatten[Map[First, mDefPairs]]];
    expressions = Flatten[Map[#[[2]]&, mDefPairs]];

(*    expressions = Flatten[Map[ComponentDerivativeOperatorInlineDefinition, dupsRemoved]];*)

    {pDefs,Map[{#, "\n"} &, expressions]}];

ordergfds[_[v1_,___], _[v2_,___]] := 
  Order[v1,v2] != -1;

PrecomputeDerivatives[derivOps_, expr_] :=
  Module[{componentDerivOps, gfds, dupsRemoved,sortedgfds},
    Map[DerivativeOperatorVerify, derivOps];
    gfds = GridFunctionDerivativesInExpression[derivOps, expr];
    sortedgfds = Sort[gfds, ordergfds];
    Map[PrecomputeDerivative, sortedgfds]];

DeclareDerivatives[derivOps_, expr_] :=
  Module[{componentDerivOps, gfds, dupsRemoved,sortedgfds},
    Map[DerivativeOperatorVerify, derivOps];
    gfds = GridFunctionDerivativesInExpression[derivOps, expr];
    sortedgfds = Sort[gfds, ordergfds];
    Map[DeclareDerivative, sortedgfds]];

ReplaceDerivatives[derivOps_, expr_, precompute_] :=
  Module[{componentDerivOps, gfds},
    Map[DerivativeOperatorVerify, derivOps];
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents, derivOps]];
    gfds = GridFunctionDerivativesInExpression[derivOps, expr];

    If[precompute,
      rules = Map[# :> GridFunctionDerivativeName[#] &, gfds],
      rules = Map[# :> evaluateDerivative[#] &, gfds]];
    expr /. rules];


(*************************************************************)
(* Misc *)
(*************************************************************)

PrecomputeDerivative[d:pd_[gf_, inds___]] :=
  Module[{},
    AssignVariable[GridFunctionDerivativeName[d], evaluateDerivative[d]]];

evaluateDerivative[d:pd_[gf_, inds___]] :=
  Module[{macroname},
    macroName = ComponentDerivativeOperatorMacroName[pd[inds] -> expr];
    Return[ToString[macroName] <> "(" <> ToString[gf] <> ", i, j, k)"]];

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
  Module[{componentDerivOps, derivs, patterns, dupsRemoved},
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents, derivOps]];
    dupsRemoved = RemoveDuplicateRules[componentDerivOps];
    derivs = Map[First, dupsRemoved];
    patterns = Map[# /. x_[inds___] -> x[y_, inds] &, derivs];
    Flatten[Map[Union[Cases[{expr}, #, Infinity]] &, patterns]]];

(*************************************************************)
(* DerivativeOperator *)
(*************************************************************)

sbpMacroDefinition[macroName_, d_] :=
  Module[{ds = Switch[d, 1, "x", 2, "y", 3, "z"],
          l = Switch[d, 1, "i", 2, "j", 3, "k"]},
    FlattenBlock[{"#define ", macroName, "(u,i,j,k) (sbp_deriv_" <> ds
    <> "(i,j,k,sbp_" <> l <> "min,sbp_" <> l <> "max,d" <> ds <> ",u,q" <> ds <> ",cctkGH))"}]    ];

ComponentDerivativeOperatorMacroDefinition[componentDerivOp:(name_[inds___] -> expr_)] :=
  Module[{macroName, rhs, rhs2, i = "i", j = "j", k = "k", spacings, spacings2, pat, ss, num, den, newnum, signModifier, quotient, liName, rhs3, rhs4},
  
    macroName = ComponentDerivativeOperatorMacroName[componentDerivOp];

    If[expr === SBPDerivative[1],
      Return[sbpMacroDefinition[macroName, 1]]];

    If[expr === SBPDerivative[2],
      Return[sbpMacroDefinition[macroName, 2]]];

    If[expr === SBPDerivative[3],
      Return[sbpMacroDefinition[macroName, 3]]];

    rhs = DifferenceGF[expr, i, j, k];
    spacings = {spacing[1] -> 1/"dxi", spacing[2] -> 1/"dyi", spacing[3] -> 1/"dzi"};
    spacings2 = {spacing[1] -> "dx", spacing[2] -> "dy", spacing[3] -> "dz"};

    rhs2 = FullSimplify[rhs];

(*    Print["rhs2 == ", FullForm[rhs2]];*)

    pat = Times[spInExpr:(Power[spacing[_],_]..), (Rational[x_,y_])..., rest__];
(*    Print["pat == ", pat//FullForm];*)

    If[MatchQ[rhs2, pat],
(*       Print["matches!"];*)
       ss = Times[rhs2 /. pat -> spInExpr];
(*       Print["ss == ", ss];*)
       num = rhs2 /. pat -> x;
       den = rhs2 /. pat -> y;
       If[num < 0,
          newnum = - num;
          signModifier = "m",
          newnum = num;
          signModifier = ""];

       quotient = 
         If[newnum/den == 1,
            "1o",
            ToString[newnum] <> "o" <> ToString[den]];
          
(*
       Print["num == ", num];
       Print["den == ", den];
       Print["quotient == ", quotient];
       Print["signModifier == ", signModifier];
       Print["spacings2 == ", spacings2];
       Print["ss == ", ss//FullForm];
       Print["Inverse spacings: ", Simplify[1/(ss /. spacings2)]];
*)
       liName = "p" <> signModifier <> quotient <> ToString[Apply[SequenceForm,Simplify[1/(ss /. spacings2)]]];
(*       Print["liName == ", liName];*)

       rhs3 = rhs2 /. pat -> Times[liName, rest],
(*       Print["!!!!!!!!DOES NOT MATCH!!!!!!!!!"];*)
       rhs3 = rhs2];

(*    Print["rhs3 == ", rhs3];*)

    pDefs = {{liName -> CFormHideStrings[ReplacePowers[num / den ss /. spacings2]]}};

(*    rhs4 = Factor[rhs3];*)

    rhs4 = rhs3 //. (x_ a_ + x_ b_) -> x(a+b);
    rhs5 = rhs4 //. (x_ a_ - y_ b_) -> x(a-b);
    
(*    Print[componentDerivOp, ": "];
    Print[FullForm[rhs5]];
    Print[""];*)

(*    rhs4 = rhs4 //. (x_ a_ - x_ b_) -> x(a-b);*)

    rhs6 = CFormHideStrings[ReplacePowers[rhs5 /. spacings]];
    {pDefs, FlattenBlock[{"#define ", macroName, "(u,i,j,k) ", "(", rhs6, ")"}]}];

ComponentDerivativeOperatorInlineDefinition[componentDerivOp:(name_[inds___] -> expr_)] :=
  Module[{inlineName, rhs, rhs2, i = "i", j = "j", k = "k", spacings},
  
    inlineName = ComponentDerivativeOperatorMacroName[componentDerivOp];

    rhs = DifferenceGF[expr, i, j, k];
(*    rhs = DifferenceGFInline[expr, i, j, k];*)
    spacings = {spacing[1] -> 1/"dxi", spacing[2] -> 1/"dyi", spacing[3] -> 1/"dzi"};
    rhs2 = CFormHideStrings[FullSimplify[ReplacePowers[rhs /. spacings]]];

    DefineFunction[inlineName, "static inline CCTK_REAL", 
      "CCTK_REAL *u, int i, int j, int k",
      {"return ", rhs2, ";\n"}]];

ComponentDerivativeOperatorMacroName[componentDerivOp:(name_[inds___] -> expr_)] :=
  Module[{stringName},
    stringName = StringJoin[Map[ToString, Join[{name}, {inds}]]];
    stringName];




ComponentDerivativeOperatorStencilWidth[componentDerivOp:(name_[inds___] -> expr_)] :=
  Module[{cases, nx, ny, nz},
    cases = Union[Flatten[Cases[{expr}, shift[_] | Power[shift[_],_], Infinity]]];
    Print[cases];

    nx = Exponent[op, shift[1]];
    ny = Exponent[op, shift[2]];
    nz = Exponent[op, shift[3]];

  ];




(* Farm out each term of a difference operator *)
DifferenceGF[op_, i_, j_, k_] :=
  Module[{expanded},
    expanded = Expand[op];
    
    If[Head[expanded] === Plus,
      Apply[Plus, Map[DifferenceGFTerm[#, i, j, k] &, expanded]],
      DifferenceGFTerm[expanded, i, j, k]]];

DifferenceGFInline[op_, i_, j_, k_] :=
  Module[{expanded},
    expanded = Expand[op];
    
    If[Head[expanded] === Plus,
      Apply[Plus, Map[DifferenceGFTermInline[#, i, j, k] &, expanded]],
      DifferenceGFTerm[expanded, i, j, k]]];


(* Return the fragment of a macro definition for defining a derivative
   operator *)
DifferenceGFTerm[op_, i_, j_, k_] :=
  Module[{nx, ny, nz, remaining},

    If[op === 0,
      Return[0]];

(*    If[!(Head[op] === Times) && !(Head[op] === Power) && !AtomQ[op],
      ThrowError["Finite difference operator not recognized: ", op, "Full form is: ", FullForm[op]]];*)

    nx = Exponent[op, shift[1]];
    ny = Exponent[op, shift[2]];
    nz = Exponent[op, shift[3]];

    remaining = op / (shift[1]^nx) / (shift[2]^ny) / (shift[3]^nz);

    If[Cases[{remaining}, shift[_], Infinity] != {},
      ThrowError["Could not parse difference operator:", op]];
    
    If[CodeGen`SOURCELANGUAGE == "C",
    remaining "u[CCTK_GFINDEX3D(cctkGH," <> ToString[CFormHideStrings[i+nx]] <> "," <>
      ToString[CFormHideStrings[j+ny]] <> "," <> ToString[CFormHideStrings[k+nz]] <> ")]",
    remaining "u(" <> ToString[FortranForm[i+nx]] <> "," <> 
      ToString[FortranForm[j+ny]] <> "," <> ToString[FortranForm[k+nz]] <> ")"] ];

(* Return the fragment of a function definition for defining a derivative
   operator *)
DifferenceGFTermInline[op_, i_, j_, k_] :=
  Module[{nx, ny, nz, remaining},

    If[op === 0,
      Return[0]];

    nx = Exponent[op, shift[1]];
    ny = Exponent[op, shift[2]];
    nz = Exponent[op, shift[3]];

    remaining = op / (shift[1]^nx) / (shift[2]^ny) / (shift[3]^nz);

    If[Cases[{remaining}, shift[_], Infinity] != {},
      ThrowError["Could not parse difference operator:", op]];
    
    remaining "u[CCTK_GFINDEX3D(cctkGH," <> ToString[CFormHideStrings[i+nx]] <> "," <>
      ToString[CFormHideStrings[j+ny]] <> "," <> ToString[CFormHideStrings[k+nz]] <> ")]"
     ];


DerivativeOperatorGFDs[gf_];

DerivativeOperatorToComponents[name_[indPatterns___] -> expr_] :=
  Module[{ips, symbols, symbolRanges, symbolLHS, table},
    ips = {indPatterns};

    If[MatchQ[ips, List[ (_Pattern) ...]],

      symbols = Map[First, ips];
      symbolRanges = Map[{#, 1, 3} &, Union[symbols]];
      symbolLHS = name[Apply[Sequence, symbols]];
      table = Apply[Table, Join[{symbolLHS -> expr}, symbolRanges]];
      Return[Flatten[table]]];


    If[MatchQ[ips, List[ (_ ? NumberQ) ...]],
      Return[{name[indPatterns] -> expr}]];

    Throw["DerivativeOperatorToComponents: Expecting indices which are symbolic patterns or numbers"];
];

DerivativeOperatorVerify[derivOp_] :=
  If[!MatchQ[derivOp, pd_[_Pattern ...] -> expr_?DerivativeOperatorRHSVerify] && 
     !MatchQ[derivOp, pd_[_ ? NumberQ ...] -> expr_?DerivativeOperatorRHSVerify],
     Throw["Derivative operator definition failed verification: ", ToString[derivOp]]];

DerivativeOperatorRHSVerify[expr_] :=
  Module[{allAtoms, symbols},
    allAtoms = Union[Level[expr, {-1}]];
    symbols = Cases[allAtoms, x_Symbol];
    True];


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

    lhs = Map[First, l];
    lhs2 = RemoveDuplicates[lhs];
    rhs2 = lhs2 /. l;

    result = Thread[Rule[lhs2,rhs2]];

    result];

(* Return a difference operator approximating a derivative of order p
   using m grid points before and m grid points after the centre
   point. Return an error if this is not possible. *)

StandardCenteredDifferenceOperator[p_, m_, i_] :=
  Module[{f, h, coeffs, expansion, e1, e2, eqs, mat, vec, result, deriv},
    coeffs = Table[Symbol["c" <> ToString[n]], {n, 1, 2m + 1}];
    expansion = Apply[Plus, Thread[coeffs Table[f[n h], {n, -m, +m}]]];
    e1 = expansion /. f[n_ h] -> Series[f[n h], {h, 0, 2m + 1}];
    e2 = Table[Coefficient[e1, Derivative[n][f][0]], {n, 0, 2m + 1}];
    eqs = Table[e2[[n]] == If[n - 1 == p, 1, 0], {n, 1, 2m + 1}];
    {mat, vec} = LinearEquationsToMatrices[eqs, coeffs];
    result = Inverse[mat].vec;
    deriv = expansion /. Thread[coeffs -> result];
    deriv /. {f[n_ h] -> shift[i]^n, f[h]->shift[i], f[0] -> 1, h -> spacing[i]}];

End[];

EndPackage[];
