
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
generates macro definitions for each derivative; i.e., in this case it
would create a macro definition for PD12(u,i,j,k).  At the start of a
calculation loop, variables are created to store the results of
precomputing each of the derivatives needed in that loop.
E.g. PD12phi = PD12(phi,i,j,k).  Kranc then replaces PD[phi,1,2] with
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

  PD[i_] -> 1/2(shift[i] + 1/shift[i])

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
point. TODO: Should be checked by someone competent!


StandardUpwindDifferenceOperator[p_, m1_, m2_, i_]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Return an upwind operator approximating a derivative of order p
using m1 grid points before and m2 grid points after the centre
point. TODO: Should be checked by someone competent!


*)

BeginPackage["Differencing`", {"CodeGen`", "CodeGenC`", "CodeGenCactus`", "CodeGenKranc`",
                               "Kranc`", "MapLookup`", 
             (* "LinearAlgebra`MatrixManipulation`", *) "Errors`", "Code`", "Object`", "OperationCount`"}];

CreateDifferencingHeader::usage = "";
PrecomputeDerivatives::usage = "";
PrecomputeFDTiledDerivatives::usage = "";
PrecomputeDGTiledDerivatives::usage = "";
ReplaceDerivatives::usage = "";
StandardCenteredDifferenceOperator::usage = "";
StandardUpwindDifferenceOperator::usage = "";
GridFunctionDerivativesInExpression::usage = "";
DPlus::usage = "";
DMinus::usage = "";
DiffPlusOp::usage = "";
DiffMinusOp::usage = "";
DZero::usage = "";
shift::usage = "";
spacing::usage = "";
ComponentDerivativeOperatorStencilWidth::usage = "";
CheckStencil::usage = "";
StencilSize::usage = "";
DifferencingProcessCode;

GridFunctionDerivativeToDef;
DifferencingOperatorExpansion;
$DifferencingMacroExpansions;
CountDerivativeOperations;

Begin["`Private`"];

DPlus[n_] := (shift[n] - 1)/spacing[n];
DMinus[n_] := (1 - 1/shift[n])/spacing[n];
DiffPlusOp[n_] := (shift[n] - 1);
DiffMinusOp[n_] := (1 - 1/shift[n]);
DZero[n_] := (DPlus[n] + DMinus[n])/2;

(*************************************************************)
(* User API *)
(*************************************************************)

DefFn[
  CreateDifferencingHeader[derivOps1_, zeroDims_, vectorise_, intParams_] :=
  Module[{componentDerivOps, dupsRemoved, expressions, componentDerivOps2, zeroDimRules,
          pDefs, derivOps},
    derivOps = Flatten[Map[expandDerivOpOverParameters[#, intParams] &, derivOps1],1];

    Map[DerivativeOperatorVerify, derivOps];

    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents[#,zeroDims] &,
                                    derivOps]];

    componentDerivOps2 = componentDerivOps;

    dupsRemoved = Block[{$RecursionLimit = Infinity}, RemoveDuplicateRules[componentDerivOps2]];

    mDefPairs = Map[ComponentDerivativeOperatorMacroDefinition[#, vectorise] &, dupsRemoved];

    pDefs = Union[Flatten[Map[First, mDefPairs]]];

    expressions = Flatten[Map[#[[2]]&, mDefPairs]];

    {pDefs, Map[{#, "\n"} &, expressions]}]];

ordergfds[_[v1_,___], _[v2_,___]] := 
  Order[v1,v2] != -1;

getParamName[p_List] := lookup[p,Name];
getParamName[p_] := p;

DefFn[CountDerivativeOperations[derivOps_, expr_, zeroDims_] :=
  Module[{gfds, gfdEvaluations},
    gfds = GridFunctionDerivativesInExpression[derivOps, expr, zeroDims];
    gfdEvaluations = gfds /. Flatten[$DifferencingMacroExpansions];
    Scan[CountOperations, gfdEvaluations]]];

DefFn[
  PrecomputeDerivatives[derivOps_, expr_, intParams_, zeroDims_, macroPointer_,
                        fdTile_, dgTile_] :=
  Module[{componentDerivOps, gfds, sortedgfds, opNames, intParamNames, paramsInOps,
          paramName, opsWithParam, opNamesWithParam, replace, param, pd, u},

    CountDerivativeOperations[derivOps, expr, zeroDims];

    gfds = GridFunctionDerivativesInExpression[derivOps, expr, zeroDims];
    sortedgfds = Sort[gfds, ordergfds];

    opNames = Union[Map[Head, sortedgfds]];
    intParamNames = getParamName /@ intParams;
    paramsInOps = Union@Flatten@Map[Cases[derivOps, #, Infinity] &, intParamNames];

    If[Length[paramsInOps] > 1,
      ThrowError["Cannot have more than one integer parameter in the list of partial derivative definitions"]];

    If[paramsInOps === {},
      Map[DerivativeOperatorVerify, derivOps];
      Map[PrecomputeDerivative[#,Automatic,macroPointer,fdTile,dgTile]&, sortedgfds],
      (* else *)
      paramName = First[paramsInOps];
      opsWithParam = Select[derivOps, Cases[#, paramName, Infinity] =!= {} &];
      opNamesWithParam = Map[#[[1,0]] &, opsWithParam];

      replace[value_] :=
        Map[(# -> combineOpNameWithParameter[#, paramName, value]) &, opNamesWithParam];

      param = Select[intParams, getParamName[#] === paramName &][[1]];
      {Map[DeclareVariable[GridFunctionDerivativeName[#],DataType[]] &, sortedgfds],
       "\n",
       SwitchStatement[paramName,
         Sequence@@Table[{value, Map[PrecomputeDerivative[# /. replace[value],#,macroPointer,fdTile,dgTile] &, sortedgfds]}, 
                         {value, lookup[param, AllowedValues]}]]}]]];

DefFn[
  PrecomputeFDTiledDerivatives[derivOps_, expr_, zeroDims_] :=
  Module[
    {gfds, sortedgfds, noindexgfds, highestgfds},
    CountDerivativeOperations[derivOps, expr, zeroDims];
    gfds = GridFunctionDerivativesInExpression[derivOps, expr, zeroDims];
    sortedgfds = Sort[gfds, ordergfds];
    
    (* Replaces all derivative indices with 1 *)
    noindexgfds = DeleteDuplicates[
      Map[# //. {pd_[gf_, ind1_] :> pd[gf, 1],
                 pd_[gf_, ind1_, ind2_] :> pd[gf, 1, 1]} &, sortedgfds]];
    (* Remove all first derivatives for variables that have second derivatives
       taken *)
    highestgfds = Reverse[
      DeleteDuplicates[
        Reverse[noindexgfds],
        MatchQ[{#1, #2}, {pd_[gf_, inds1__], pd_[gf_, inds2__]}]&]];

    Map[PrecomputeFDTiledDerivative, highestgfds]]];

DefFn[
  PrecomputeDGTiledDerivatives[derivOps_, expr_, zeroDims_] :=
  Module[
    {gfds, sortedgfds},
    CountDerivativeOperations[derivOps, expr, zeroDims];
    gfds = GridFunctionDerivativesInExpression[derivOps, expr, zeroDims];
    sortedgfds = Sort[gfds, ordergfds];
    Map[PrecomputeDGTiledDerivative, sortedgfds]]];

DefFn[
  ReplaceDerivatives[derivOps_, expr_, precompute_, zeroDims_, macroPointer_] :=
  Module[{componentDerivOps, gfds},
    Map[DerivativeOperatorVerify, derivOps];
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents[#,zeroDims] &, derivOps]];
    gfds = GridFunctionDerivativesInExpression[derivOps, expr, zeroDims];

    If[precompute,
      rules = Map[# :> GridFunctionDerivativeName[#] &, gfds],
      rules = Map[# :> evaluateDerivative[#,macroPointer] &, gfds]];
    expr /. rules]];

(* Generate code to ensure that there are sufficient ghost and
   boundary points for the passed derivative operators used in eqs *)
DefFn[
  CheckStencil[derivOps_, eqs_, name_, zeroDims_, fdTile_, dgTile_] :=
  Module[{gfds, rgzList, rgz},
    gfds = Map[GridFunctionDerivativesInExpression[{#}, eqs, zeroDims] &, derivOps];
    rgzList = MapThread[If[Length[#2] > 0, DerivativeOperatorStencilWidth[#1,zeroDims], {0,0,0}] &, {derivOps, gfds}];
    If[Length[rgzList] === 0, Return[{}]];
    rgz = Map[Max, Transpose[rgzList]];
    If[fdTile || dgTile, rgz = Map[1&, rgz]];
    If[Max[rgz] == 0, {},
    {"EnsureStencilFits(cctkGH, ", Quote@name, ", ", Riffle[rgz,", "], ");\n"}]]];

parametersUsedInOps[derivOps_, intParams_] :=
  Union@Flatten[Map[Cases[derivOps, getParamName[#] -> #, Infinity] &,
                    intParams], 1];

DefFn[
  CheckStencil[derivOps_, eqs_, name_, zeroDims_, fdTile_, dgTile_,
               intParams_] :=
  Module[{psUsed, p},
    psUsed = parametersUsedInOps[derivOps, intParams];
    If[Length[psUsed] > 1, ThrowError["Too many parameters in partial derivatives"]];
    If[psUsed === {},
      CheckStencil[derivOps,eqs,name,zeroDims,fdTile,dgTile],
      p = psUsed[[1]];
      SwitchStatement[getParamName[p],
        Sequence@@Table[{value,
                         CheckStencil[derivOps/.getParamName[p]->value, eqs,
                                      name, zeroDims, fdTile, dgTile]},
                        {value, lookup[p, AllowedValues]}]]]]];

(* It would be good to refactor StencilSize and CheckStencil as much
   of the logic is repeated *)
DefFn[
  StencilSize[derivOps_, eqs_, name_, zeroDims_] :=
  Module[
    {gfds, gfops, allCompOps, usedCompOps, widths, stencilWidth},

    (* {PD[alpha,1,2], ...} *)
    gfds = Flatten[Map[GridFunctionDerivativesInExpression[{#}, eqs, zeroDims] &, derivOps],1];

    (* Drop the variable name from the gfd *)
    (* {PD[1,2], ...} *)
    gfops = Map[Drop[#,1] &, gfds];

    allCompOps = Flatten[(DerivativeOperatorToComponents[#,zeroDims] &) /@ derivOps,1];
    usedCompOps = Select[allCompOps, MemberQ[gfops, First[#]] &];
    If[usedCompOps === {}, Return[{0,0,0}]];
    widths = ComponentDerivativeOperatorStencilWidth/@usedCompOps;
    stencilWidth = Map[Max,Transpose[widths]];
    stencilWidth]];

DefFn[
  StencilSize[derivOps_, eqs_, name_, zeroDims_, intParams_] :=
  Module[{psUsed, p},
    psUsed = parametersUsedInOps[derivOps, intParams];
    If[Length[psUsed] > 1, ThrowError["Too many parameters in partial derivatives"]];
    If[psUsed === {},
      StencilSize[derivOps,eqs,name,zeroDims],
      p = psUsed[[1]];
      {getParamName[p],
        Table[{value,
               StencilSize[derivOps/.getParamName[p]->value, eqs,
                           name, zeroDims]},
              {value, lookup[p, AllowedValues]}]}]]];

(*************************************************************)
(* Misc *)
(*************************************************************)

DefFn[
  PrecomputeDerivative[d:pd_[gf_, inds___], vargfd_, macroPointer_,
                       fdTile_, dgTile_] :=
  Module[{},
    If[vargfd === Automatic,
      DefineConstant[GridFunctionDerivativeName[d], DataType[], evaluateDerivative[d,macroPointer,fdTile,dgTile]],
      AssignVariable[GridFunctionDerivativeName[vargfd], evaluateDerivative[d,macroPointer,fdTile,dgTile]]]]];

DefFn[
  PrecomputeFDTiledDerivative[d:pd_[gf_, inds___]] :=
  Module[
    {
      pdn = ToString[pd],
      gfn = ToString[gf],
      gfdn = ToString[GridFunctionDerivativeName[d]],
      indstr = Apply[StringJoin, Map[ToString[#-1]&, {inds}]],
      dxistrs = Map[{"dxi","dyi","dzi"}[[#]]&, {inds}],
      dxistr
    },
    If[False,
    If[Length[{inds}]==2 && {inds}[[1]]=={inds}[[2]],
       (* If both indices are the same, then we need to apply a single
          stencil, which has only a single direction. Cut off the
          second (identical) direction, and append "2" to the stencil
          name. *)
       indstr = StringDrop[indstr, -1];
       pdn = pdn<>"2"
      ];
    dxistr = Fold[("kmul("<>#1<>", "<>#2<>")")&, dxistrs];
    {
      "unsigned char "<>gfdn<>"T[tsz] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
      (* "static thread_local aligned_vector<unsigned char, CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL)> "<>gfdn<>"T_base(tsz);\n",
      "unsigned char *restrict const "<>gfdn<>"T = "<>gfdn<>"T_base.data();\n", *)
      "stencil_fd_dim3_dir"<>indstr<>"<fdop_"<>pdn<>", npoints_i, npoints_j, npoints_k>(&((const unsigned char *)"<>gfn<>")[off1], "<>gfdn<>"T, "<>dxistr<>", dj, dk);\n"
    }];
    Which[{inds} == {1},
          {
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,1]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,2]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,3]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "stencil_fd_dim3<fdop_"<>ToString[pd]<>", npoints_i, npoints_j, npoints_k>(\n",
            "  &((const unsigned char *)"<>ToString[gf]<>")[off1],\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,1]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,2]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,3]]]<>"T,\n",
            "  dxi, dyi, dzi, dj, dk);\n"
          },
          {inds} == {1, 1},
          {
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,1]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,2]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,3]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,1,1]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,1,2]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,1,3]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,2,2]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,2,3]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "unsigned char "<>ToString[GridFunctionDerivativeName[pd[gf,3,3]]]<>"T[tsz]\n",
            "  CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
            "stencil_fd_dim3<fdop_"<>ToString[pd]<>", fdop_"<>ToString[pd]<>"2, npoints_i, npoints_j, npoints_k>(\n",
            "  &((const unsigned char *)"<>ToString[gf]<>")[off1],\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,1]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,2]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,3]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,1,1]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,1,2]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,1,3]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,2,2]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,2,3]]]<>"T,\n",
            "  "<>ToString[GridFunctionDerivativeName[pd[gf,3,3]]]<>"T,\n",
            "  dxi, dyi, dzi, dxxi, dxyi, dxzi, dyyi, dyzi, dzzi, dj, dk);\n"
          },
          True,
          {}]]];

DefFn[
  PrecomputeDGTiledDerivative[d:pd_[gf_, inds___]] :=
  Module[
    {
      pdn = ToString[pd],
      gfn = ToString[gf],
      gfdn = ToString[GridFunctionDerivativeName[d]],
      indstr = Apply[StringJoin, Map[ToString[#-1]&, {inds}]],
      dxistr = Apply[StringJoin,
                     Map[("kmul(" <> {"dxi","dyi","dzi"}[[#]] <> ", " <>
                          "ToReal(2.0/(order+1))), ")&,
                         {inds}]]
    },
    {
      "unsigned char "<>gfdn<>"T[tsz] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
      (* "static thread_local aligned_vector<unsigned char, CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL)> "<>gfdn<>"T_base(tsz);\n",
      "unsigned char *restrict const "<>gfdn<>"T = "<>gfdn<>"T_base.data();\n", *)
      "stencil_dg_dim3_dir"<>indstr<>"<dgop_"<>pdn<>">(&((const unsigned char *)"<>gfn<>")[off1], "<>gfdn<>"T, "<>dxistr<>"dj, dk);\n"
    }]];

DefFn[
  evaluateDerivative[d:pd_[gf_, inds___], macroPointer_, fdTile_, dgTile_] :=
  Module[{macroname},
    macroName = ComponentDerivativeOperatorMacroName[pd[inds] -> expr];
    (* Return[ToString[macroName] <> "(" <> ToString[gf] <> ", i, j, k)"] *)
    Which[
      fdTile || dgTile,
      Return["vec_load(getelt(" <>
             ToString[macroName] <> ToString[gf] <> "T, off))"],
      macroPointer,
      Return[ToString[macroName] <> "(&" <> ToString[gf] <> "[index])"],
      True,
      Return[ToString[macroName] <> "(" <> ToString[gf] <> ")"]]
  ]];

(*************************************************************)
(* GridFunctionDerivative *)
(*************************************************************)

DefFn[
  GridFunctionDerivativeName[pd_[gf_, inds___]] :=
  Module[{},
    stringName = StringJoin[Map[ToString, Join[{pd}, {inds}, {gf}]]];
    Symbol["Global`" <> stringName]]];


DefFn[
  GridFunctionDerivativesInExpression[derivOps_, expr_, zeroDims_] := 
  Module[{componentDerivOps, derivs, patterns, dupsRemoved},
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents[#,zeroDims]&, derivOps]];
    dupsRemoved = RemoveDuplicateRules[componentDerivOps];
    derivs = Map[First, dupsRemoved];
    patterns = Map[# /. x_[inds___] -> x[y_, inds] &, derivs];
    Flatten[Map[Union[Cases[{expr}, #, Infinity]] &, patterns]]]];

(* Return the definition associated with a grid function derivative *)
GridFunctionDerivativeToDef[pd_[gf_, inds___], derivOps_, zeroDims] :=
  Module[{componentDerivOps},
    componentDerivOps = Flatten[Map[DerivativeOperatorToComponents[#,zeroDims]&, derivOps]];
    pd[inds] /. componentDerivOps];

(*************************************************************)
(* DerivativeOperator *)
(*************************************************************)

DefFn[
  sbpMacroDefinition[macroName_, d_] :=
  Module[{ds = Switch[d, 1, "x", 2, "y", 3, "z"],
          l = Switch[d, 1, "i", 2, "j", 3, "k"]},
    FlattenBlock[{"#define ", macroName, "(u,i,j,k) (sbp_deriv_" <> ds
    <> "(i,j,k,sbp_" <> l <> "min,sbp_" <> l <> "max,d" <> ds <> ",u,q" <> ds <> ",cctkGH))"}]    ]];

DefFn[
  ComponentDerivativeOperatorMacroDefinition[componentDerivOp:(name_[inds___] -> expr_), vectorise_] :=
  Module[{macroName, rhs, fnrhs, dirargs, i = "i", j = "j", k = "k", spacings, spacings2, pat, ss, num, den, newnum, signModifier, quotient, liName, finalDef},
  
    macroName = ComponentDerivativeOperatorMacroName[componentDerivOp];

    If[expr === SBPDerivative[1],
      Return[sbpMacroDefinition[macroName, 1]]];

    If[expr === SBPDerivative[2],
      Return[sbpMacroDefinition[macroName, 2]]];

    If[expr === SBPDerivative[3],
      Return[sbpMacroDefinition[macroName, 3]]];

    rhs = DifferenceGF[expr, i, j, k, vectorise];
(*    Print["rhs1 == ", FullForm[rhs]];*)
    spacings = {spacing[1] -> 1/"dxi", spacing[2] -> 1/"dyi", spacing[3] -> 1/"dzi"};
    spacings2 = {spacing[1] -> "dx", spacing[2] -> "dy", spacing[3] -> "dz"};

    rhs = FullSimplify[rhs];

(*    Print["rhs2 == ", FullForm[rhs]];*)

    pat = (Times[spInExpr:(Power[spacing[_],_]...), (Rational[x_,y_])..., rest__]) | (rest__);
    (* Print["pat == ", pat//FullForm]; *)

    If[MatchQ[rhs, pat],
(*       Print["matches!"];*)
       ss = Times[rhs /. pat -> spInExpr];
(*       Print["ss == ", ss];*)
       num = rhs /. pat -> x;
       den = rhs /. pat -> y;
(*       Print["num == ", num];
       Print["den == ", den];*)
       If[{num, 1, 2} === {1, 2},(* Print["SEQ!"]; *) newnum = 1; den=1; signModifier = "",
         If[num < 0,
            newnum = - num;
            signModifier = "m",
            newnum = num;
            signModifier = ""]];

       quotient = 
         If[newnum/den == 1,
            "1o",
            ToString[newnum] <> "o" <> ToString[den]];
          

(*       Print["quotient == ", quotient];
       Print["signModifier == ", signModifier];
       Print["spacings2 == ", spacings2];
       Print["ss == ", ss//FullForm];
       Print["Inverse spacings: ", Simplify[1/(ss /. spacings2)]];
       Print["Sequenced: ", Apply[SequenceForm,Simplify[1/(ss /. spacings2)],{0,Infinity}]];*)

       liName = "p" <> signModifier <> quotient <> ToString[Apply[SequenceForm,Simplify[1/(ss /. spacings2)],{0,Infinity}]];
       pDefs = {{liName -> CFormHideStrings[ProcessExpression[num / den ss /. spacings2, vectorise, False]]}};

       rhs = rhs /. pat -> Times[liName, rest],
(*       Print["!!!!!!!!DOES NOT MATCH!!!!!!!!!"];*)
       rhs = rhs;
       pDefs = {};
       liName = rhs];

(*    Print["rhs3 == ", FullForm[rhs]];*)


(*    rhs = Factor[rhs];*)
    rhs = rhs //. (x_ a_ + x_ b_) -> x (a+b);
    rhs = rhs //. (x_ a_ - x_ b_) -> x (a-b);
    
(*    Print[componentDerivOp, ": "];
    Print[FullForm[rhs]];
    Print[""];*)

    Sow[name[u_, inds] -> rhs, DifferencingOperatorExpansion];

    rhs = CFormHideStrings[ProcessExpression[rhs /. spacings, vectorise, False]];
    (* Print["rhs=",FullForm[rhs]]; *)

    dirargs = If[StringMatchQ[rhs, RegularExpression[".*\\bdir\\d\\b.*"]],
                 Map[StringJoin["dir", ToString[#]] &, {1, 2, 3}],
                 {}];

    (* Call another FD operator if we can swap or exchange array indices;
       this will reduce code size.
       We perform two kinds of changes here:
       (1) op[j,i] -> op[i,j]  if j>i
           Commute partial derivatives
       (2) op[j]   -> op[i]    if j>2
           Transpose array indices, but not the first which has unit stride *)
    fnrhs =
    Switch[componentDerivOp[[1]],
           _[3],
           Module[{otherOp, otherOpName},
                  otherOp = componentDerivOp[[1]][[0]][2] -> componentDerivOp[[2]];
                  otherOpName = ComponentDerivativeOperatorMacroName[otherOp];
                  otherOpName<>"_impl("<>Riffle[Join[{"u", liName, "cdk", "cdj"}, dirargs], ", "]<>")"],
           _[1,3],
           Module[{otherOp, otherOpName},
                  otherOp = componentDerivOp[[1]][[0]][1,2] -> componentDerivOp[[2]];
                  otherOpName = ComponentDerivativeOperatorMacroName[otherOp];
                  otherOpName<>"_impl("<>Riffle[Join[{"u", liName, "cdk", "cdj"}, dirargs], ", "]<>")"],
           _[2,1],
           Module[{otherOp, otherOpName},
                  otherOp = componentDerivOp[[1]][[0]][1,2] -> componentDerivOp[[2]];
                  otherOpName = ComponentDerivativeOperatorMacroName[otherOp];
                  otherOpName<>"_impl("<>Riffle[Join[{"u", liName, "cdj", "cdk"}, dirargs], ", "]<>")"],
           _[3,1],
           Module[{otherOp, otherOpName},
                  otherOp = componentDerivOp[[1]][[0]][1,2] -> componentDerivOp[[2]];
                  otherOpName = ComponentDerivativeOperatorMacroName[otherOp];
                  otherOpName<>"_impl("<>Riffle[Join[{"u", liName, "cdk", "cdj"}, dirargs], ", "]<>")"],
           _[3,2],
           Module[{otherOp, otherOpName},
                  otherOp = componentDerivOp[[1]][[0]][2,3] -> componentDerivOp[[2]];
                  otherOpName = ComponentDerivativeOperatorMacroName[otherOp];
                  otherOpName<>"_impl("<>Riffle[Join[{"u", liName, "cdj", "cdk"}, dirargs], ", "]<>")"],
           _[3,3],
           Module[{otherOp, otherOpName},
                  otherOp = componentDerivOp[[1]][[0]][2,2] -> componentDerivOp[[2]];
                  otherOpName = ComponentDerivativeOperatorMacroName[otherOp];
                  otherOpName<>"_impl("<>Riffle[Join[{"u", liName, "cdk", "cdj"}, dirargs], ", "]<>")"],
           _,
           rhs];
    
    finalDef =
      If[vectorise,
    {pDefs, FlattenBlock[{
      "#ifndef KRANC_DIFF_FUNCTIONS\n",
       (* default, differencing operators are macros *)
      "#  define ", macroName, "(u) ", "(", rhs, ")\n",
      "#else\n",
       (* new, differencing operators are static functions *)
      "#  define ", macroName, "(u) ", "(", macroName, "_impl(u,", liName, ",cdj,cdk))\n",
      "static CCTK_REAL_VEC ", macroName, "_impl(const CCTK_REAL* restrict const u, const CCTK_REAL_VEC ", liName, ", const ptrdiff_t cdj, const ptrdiff_t cdk) CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;\n",
      "static CCTK_REAL_VEC ", macroName, "_impl(const CCTK_REAL* restrict const u, const CCTK_REAL_VEC ", liName, ", const ptrdiff_t cdj, const ptrdiff_t cdk)\n",
      (* We cannot handle dirN,
         so we punt on all expressions that contain dirN *)
      If[StringMatchQ[rhs, RegularExpression[".*\\bdir\\d\\b.*"]],
         { "{ assert(0); return ToReal(1e30); /* ERROR */ }\n" },
         { "{\n",
           "  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);\n",
           "  return ", fnrhs, ";\n",
           "}\n" }],
      "#endif\n"
    }]},

    {pDefs, FlattenBlock[{
      "#ifndef KRANC_DIFF_FUNCTIONS\n",
       (* default, differencing operators are macros *)
      "#  define ", macroName, "(u) ", "(", rhs, ")\n",
      "#else\n",
       (* new, differencing operators are static functions *)
       (* this handles both dirN and non-dirN expressions *)
      "#  define ", macroName, "(u) ", "(", macroName, "_impl(u,", liName, ",cdj,cdk", Riffle[dirargs, ",", {1, -2, 2}], "))\n",
      "static CCTK_REAL ", macroName, "_impl(const CCTK_REAL* restrict const u, const CCTK_REAL ", liName, ", ", Riffle[Map["const ptrdiff_t "<># &, Join[{"cdj", "cdk"}, dirargs]], ", "], ") CCTK_ATTRIBUTE_NOINLINE CCTK_ATTRIBUTE_UNUSED;\n",
      "static CCTK_REAL ", macroName, "_impl(const CCTK_REAL* restrict const u, const CCTK_REAL ", liName, ", ", Riffle[Map["const ptrdiff_t "<># &, Join[{"cdj", "cdk"}, dirargs]], ", "], ")\n",
      "{\n",
      "  const ptrdiff_t cdi CCTK_ATTRIBUTE_UNUSED = sizeof(CCTK_REAL);\n",
      "  return ", fnrhs, ";\n",
      "}\n",
      "#endif\n"
    }]}];

    finalDef
]];

ComponentDerivativeOperatorMacroName[componentDerivOp:(name_[inds___] -> expr_)] :=
  Module[{stringName},
    stringName = StringJoin[Map[ToString, Join[{name}, {inds}]]];
    stringName];

DerivativeOperatorStencilWidth[derivOp_,zeroDims_] :=
  Map[Max, Transpose[Map[ComponentDerivativeOperatorStencilWidth, 
                         DerivativeOperatorToComponents[derivOp,zeroDims]]]];

DefFn[
  ComponentDerivativeOperatorStencilWidth[componentDerivOp:(name_[inds___] -> expr_)] :=
  Module[{cases, nx, ny, nz, result},
    result = Table[
    cases = Union[Flatten[Cases[{expr}, shift[d] | Power[shift[d],_], Infinity]]];
    ns = Map[Exponent[#, shift[d]] &, cases];
    If[Length[ns] == 0, 0, Max[Abs[ns]]], {d, 1, 3}];

    (* We do not know the run-time value of any shorthands used in
       operator definitions.  In all the current known cases, this
       will be a "direction" which is +/- 1.  In future, the
       differencing mechanism will support shorthand arguments to
       operators and this hack can be removed. *)
    result = Replace[result, _Symbol -> 1, {-1}];

    If[!And@@Map[NumericQ, result],
      ThrowError["Stencil width is not numeric in "<>ToString[componentDerivOp]]];
    result]];

(* Farm out each term of a difference operator *)
DifferenceGF[op_, i_, j_, k_, vectorise_] :=
  Module[{expanded},
    expanded = Expand[op];
    
    If[Head[expanded] === Plus,
      Apply[Plus, Map[DifferenceGFTerm[#, i, j, k, vectorise] &, expanded]],
      DifferenceGFTerm[expanded, i, j, k, vectorise]]];


(* Return the fragment of a macro definition for defining a derivative
   operator *)
DifferenceGFTerm[op_, i_, j_, k_, vectorise_] :=
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
      ThrowError["Could not parse difference operator", op]];
    
    If[CodeGenC`SOURCELANGUAGE == "C",

  If[vectorise,
    remaining "KRANC_GFOFFSET3D(u," <>
      ToString[CFormHideStrings[nx /. {dir1->1, dir2->1, dir3->1}]] <> "," <>
      ToString[CFormHideStrings[ny /. {dir1->1, dir2->1, dir3->1}]] <> "," <>
      ToString[CFormHideStrings[nz /. {dir1->1, dir2->1, dir3->1}]] <> ")",

    remaining "KRANC_GFOFFSET3D(u," <>
      ToString[CFormHideStrings[nx]] <> "," <>
      ToString[CFormHideStrings[ny]] <> "," <>
      ToString[CFormHideStrings[nz]] <> ")"],

    remaining "u(" <> ToString[FortranForm[i+nx]] <> "," <> 
      ToString[FortranForm[j+ny]] <> "," <> ToString[FortranForm[k+nz]] <> ")"] ];


DerivativeOperatorGFDs[gf_];

DefFn[
  DerivativeOperatorToComponents[name_[indPatterns___] -> expr_, zeroDims_] :=
  Module[{ips, symbols, symbolRanges, symbolLHS, table, zeroDimRules},
    ips = {indPatterns};

    zeroDimRules = Map[shift[#] -> 1 &, zeroDims];

    If[MatchQ[ips, List[ (_Pattern) ...]],

      symbols = Map[First, ips];
      symbolRanges = Map[{#, 1, 3} &, Union[symbols]];
      symbolLHS = name[Apply[Sequence, symbols]];
      table = Apply[Table, Join[{symbolLHS -> expr}, symbolRanges]];
      Return[Flatten[table/.zeroDimRules]]];


    If[MatchQ[ips, List[ (_ ? NumberQ) ...]],
      Return[{name[indPatterns] -> expr/.zeroDimRules}]];

    ThrowError["DerivativeOperatorToComponents: Expecting indices which are symbolic patterns or numbers"];
]];

DerivativeOperatorVerify[derivOp_] :=
  If[!MatchQ[derivOp, pd_[_Pattern ...] -> expr_?DerivativeOperatorRHSVerify] && 
     !MatchQ[derivOp, pd_[_ ? NumberQ ...] -> expr_?DerivativeOperatorRHSVerify],
     ThrowError["Derivative operator definition failed verification: ", ToString[derivOp]]];

DerivativeOperatorRHSVerify[expr_] :=
  Module[{allAtoms, symbols},
    allAtoms = Union[Level[expr, {-1}]];
    symbols = Cases[allAtoms, x_Symbol];
    True];

If[DeleteDuplicates[{1,1}] === {1},
  RemoveDuplicates = DeleteDuplicates,
(* else *)
DefFn[
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
       Prepend[RemoveDuplicates[next], this]]]]];

RemoveDuplicateRules[l_] :=
  Module[{lhs,lhs2,rhs2,result},

    lhs = Map[First, l];
    lhs2 = RemoveDuplicates[lhs];
    rhs2 = lhs2 /. l;

    result = Thread[Rule[lhs2,rhs2]];

    result];

(* Return a difference operator approximating a derivative of order p
   (i.e. a p-th derivative with a certain order of accuracy)
   using m grid points before and m grid points after the centre
   point. Return an error if this is not possible. *)

StandardCenteredDifferenceOperator[p_, m_Integer, i_] :=
  Module[{f, h, coeffs, expansion, e1, e2, eqs, mat, vec, result, 
    deriv, mat2, vec2, coefArrs}, 
    coeffs = Table[Symbol["c" <> ToString[n]], {n, 1, 2 m + 1}];
    expansion = Apply[Plus, Thread[coeffs Table[f[n h], {n, -m, +m}]]];
    e1 = expansion /. f[n_ h] -> Series[f[n h], {h, 0, 2 m + 1}];
    e2 = Table[Coefficient[e1, Derivative[n][f][0]], {n, 0, 2 m + 1}];
    eqs = Table[e2[[n]] == If[n - 1 == p, 1, 0], {n, 1, 2 m + 1}];
    coefArrs = Normal@CoefficientArrays[eqs, coeffs];
    mat = coefArrs[[2]];
    vec = Map[Last, eqs];
    result = Inverse[mat].vec;
    deriv = expansion /. Thread[coeffs -> result];
    deriv /. {f[n_ h] -> shift[i]^n, f[h] -> shift[i], f[0] -> 1, 
    h -> spacing[i]}];


(* Return a difference operator approximating a derivative of order p
   (i.e. a p-th derivative with a certain order of accuracy)
   using m1 grid points before and m2 grid points after the centre
   point. Return an error if this is not possible. *)

StandardUpwindDifferenceOperator[p_, m1_Integer, m2_Integer, i_] :=
  Module[{f, h, coeffs, expansion, e1, e2, eqs, mat, vec, result, deriv, coefArrs},
    coeffs = Table[Symbol["c" <> ToString[n]], {n, 1, m1 + m2 + 1}];
    expansion = Apply[Plus, Thread[coeffs Table[f[n h], {n, -m1, +m2}]]];
    e1 = expansion /. f[n_ h] -> Series[f[n h], {h, 0, m1 + m2 + 1}];
    e2 = Table[Coefficient[e1, Derivative[n][f][0]], {n, 0, m1 + m2 + 1}];
    eqs = Table[e2[[n]] == If[n - 1 == p, 1, 0], {n, 1, m1 + m2 + 1}];
    coefArrs = Normal@CoefficientArrays[eqs, coeffs];
    mat = coefArrs[[2]];
    vec = Map[Last, eqs];
    result = Inverse[mat].vec;
    deriv = expansion /. Thread[coeffs -> result];
    deriv /. {f[n_ h] -> shift[i]^n, f[h]->shift[i], f[0] -> 1, h -> spacing[i]} ]; 


(* The function LinearEquationsToMatrices is deprecated.  These
functions test that the replacement using CoefficientArray gives the
same answer. *)

(*
StandardCenteredDifferenceOperatorOld[p_, m_, i_] :=
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

testNewOps[] :=
  Table[Print[{p, m, i}]; 
    StandardCenteredDifferenceOperatorOld[p, m, i] === 
    StandardCenteredDifferenceOperator[p, m, i], 
    {p, 1, 3}, {m, 1, 6}, {i, 1, 3}];

StandardUpwindDifferenceOperatorOld[p_, m1_, m2_, i_] := 
  Module[{f, h, coeffs, expansion, e1, e2, eqs, mat, vec, result, deriv},
    coeffs = Table[Symbol["c" <> ToString[n]], {n, 1, m1 + m2 + 1}];
    expansion = Apply[Plus, Thread[coeffs Table[f[n h], {n, -m1, +m2}]]];
    e1 = expansion /. f[n_ h] -> Series[f[n h], {h, 0, m1 + m2 + 1}];
    e2 = Table[Coefficient[e1, Derivative[n][f][0]], {n, 0, m1 + m2 + 1}];
    eqs = Table[e2[[n]] == If[n - 1 == p, 1, 0], {n, 1, m1 + m2 + 1}];
    {mat, vec} = LinearEquationsToMatrices[eqs, coeffs];
    result = Inverse[mat].vec;
    deriv = expansion /. Thread[coeffs -> result];
    deriv /. {f[n_ h] -> shift[i]^n, f[h]->shift[i], f[0] -> 1, h -> spacing[i]} ]; 

testNewUpwindOps[] :=
  Table[Print[{p, m1, m2, i}]; 
    StandardUpwindDifferenceOperatorOld[p, m1, m2, i] === 
    StandardUpwindDifferenceOperator[p, m1, m2, i], 
    {p, 1, 3}, {m1, 1, 6}, {m2, 1, 6}, {i, 1, 3}];

*)

combineOpNameWithParameter[opName_, paramName_,value_] :=
  Symbol["Global`"<> ToString@opName <>
                     ToString@paramName <>
                     ToString@value];

expandDerivOpOverParameter[op_, intParam_] :=
  Module[{paramName, values, ops},
    (* Some historical duplication here *)
    If[!ListQ[intParam], Return[{op}]];
    paramName = lookup[intParam, Name];
    If[Cases[op, paramName, Infinity] =!= {},
      values = lookup[intParam, AllowedValues];
      ops = Table[combineOpNameWithParameter[op[[1,0]],paramName,value]@@op[[1]] ->
                   (op[[2]] /. (paramName -> value)),
        {value, values}],
      (* else *)
      ops = {op}];
    ops
  ];

expandDerivOpOverParameters[op_, intParams_] :=
  Module[{usedParams},
    If[Head[op] =!= Rule, ThrowError["Invalid partial derivative",op]];
    usedParams = Select[intParams, Cases[op, getParamName[#], Infinity] =!= {} &];
    If[Length[usedParams] > 1,
      ThrowError["Partial derivatives can only depend on a single parameter"]];
    If[usedParams === {},
      {op},
      expandDerivOpOverParameter[op, usedParams[[1]]]]];


Options[DifferencingProcessCode] = ThornOptions;

DefFn[
  DifferencingProcessCode[cIn_Code, opts:OptionsPattern[]] :=
  Block[{$CodeGenTarget = NewObject[TargetC, {"UseVectors" -> OptionValue[UseVectors]}]},
  Module[
    {diffHeader, pDefs, c = cIn},

    InfoMessage[Terse, "Creating differencing header file"];

    (* This is the easiest way to propagate this information; the
       differencing code will be rewritten soon, so this should go
       away *)
    {{pDefs, diffHeader}, $DifferencingMacroExpansions} = Reap[CreateDifferencingHeader[
      GetObjectField[c, "PartialDerivatives"], OptionValue[ZeroDimensions],
      OptionValue[UseVectors], OptionValue[IntParameters]], DifferencingOperatorExpansion];
    c = SetObjectField[c, "Calculations", Map[Join[#, {PreDefinitions -> pDefs}] &, GetObjectField[c, "Calculations"]]];
    diffHeader = Join[
      If[OptionValue[UseVectors] && ! OptionValue[UseOpenCL],
         {"#include <assert.h>\n",
          "#include \"vectors.h\"\n",
          "\n"},
         {}],
      diffHeader];
    (* TODO: fix circular dependency which stops us from importing OpenCL in this package *)
    If[OptionValue[UseOpenCL], diffHeader = OpenCL`OpenCLProcessDifferencingHeader[diffHeader]];
    AppendObjectField[
      c, "Sources",
      {Filename -> "Differencing.h", Contents -> diffHeader}]]]];

End[];

EndPackage[];
