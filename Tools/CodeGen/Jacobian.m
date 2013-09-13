
(*  Copyright 2011 Ian Hinder

    This file is part of Kranc.

    Kranc is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Kranc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Kranc; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

BeginPackage["Jacobian`", {"Errors`", "Helpers`", "Kranc`", "Differencing`", "MapLookup`",
                           "CodeGen`", "CodeGenC`", "KrancGroups`", "Code`", "Object`"}];

JacobianQ;
InsertJacobian;
CreateJacobianVariables;
JacobianGenericFDParameters;
JacobianSymbols;
JacobianGroups;
JacobianCheckGroups;
JacobianConditionalGridFunctions;
JacobianProcessCode;

Begin["`Private`"];

Options[JacobianQ] = ThornOptions;
JacobianQ[opts:OptionsPattern[]] :=
  Length[OptionValue[Jacobian]] > 0;

(* Assign a shorthand containing the Jacobian multiplied by the passed
   1st derivative *)
jacobianShorthand[d:(deriv_[var_, i_])] :=
  Module[{},
     derivToJacDeriv[d] ->
      IfThen["use_jacobian", Sum[Symbol["J"<>ToString[j]<>ToString[i]] deriv[var, j], {j, 1 3}], deriv[var, i]]
  ];

(* Assign a shorthand containing the Jacobian multiplied by the passed
   2nd derivative *)
jacobianShorthand[d:(deriv_[var_, i_,j_])] :=
  Module[{ip,jp},
     {ip,jp} = Sort[{i,j}]; (* dJ is symmetric in the last two indices *)
     derivToJacDeriv[d] ->
      IfThen["use_jacobian", Sum[Symbol["dJ"<>ToString[a]<>ToString[ip]<>ToString[jp]] deriv[var, a], {a, 1 3}] + 
      Sum[Symbol["J"<>ToString[a]<>ToString[i]] Symbol["J"<>ToString[b]<>ToString[j]] deriv[var, a, b], {a, 1 3}, {b, 1, 3}],
      deriv[var, i, j]]
  ];

(* Convert a 1st derivative to a Jacobian-multiplied derivative *)
derivToJacDeriv[deriv_[var_, i_]] :=
  Symbol["Global`Jac"<>ToString[deriv]<>ToString[i]<>ToString[var]];

(* Convert a 2nd derivative to a Jacobian-multiplied derivative *)
derivToJacDeriv[deriv_[var_, i_, j_]] :=
  Symbol["Global`Jac"<>ToString[deriv]<>ToString[i]<>ToString[j]<>ToString[var]];

shorthandsInDerivDef[def_, shorthands_] :=
  Module[{allAtoms},
    allAtoms = Union[Level[def, {-1}]];
    Intersection[shorthands, allAtoms]];

(* Insert a Jacobian shorthand definition into a list of equations
   after all the shorthands that it uses *)
insertDerivInEqs[deriv_, defs_, eqs_, shorthands_, zeroDims_] :=
  Module[{shortsUsed, lhss, positions, positions2, position, newShort, derivsInShort, defsUsed},
    newShort = jacobianShorthand[deriv]; (* Definition expression for new shorthand *)
    derivsInShort = GridFunctionDerivativesInExpression[defs, newShort, zeroDims]; (* Derivatives needed *)
    defsUsed = Map[GridFunctionDerivativeToDef[#, defs, zeroDims] &, derivsInShort]; (* Definitions of those derivatives *)
    shortsUsed = Union[Flatten[Map[shorthandsInDerivDef[#, shorthands] &, defsUsed],1]];
    lhss = Map[First, eqs];
    positions = Map[Position[lhss, #, {1}, 1] &, shortsUsed];
    MapThread[If[Length[#2] === 0,
      ThrowError["Shorthand " <> ToString[#1] <> " used in derivative " <> ToString[deriv] <>
        " but not defined in calculation"]] &, {shortsUsed, positions}];
    positions2 = Map[#[[1,1]] &, positions];
    position = If[Length[positions2] === 0, 1, Max[positions2]+1];
    Insert[eqs, newShort, position]
  ];

(* Given a calculation containing partial derivatives, return a
   version of the calculation with all the partial derivatives multiplied
   by the Jacobian *)
Options[InsertJacobian] = ThornOptions;
InsertJacobian[calc_List, opts:OptionsPattern[]] :=
  Module[{pdDefs, derivs, newShortDefs, newShorts, combinedShorts, combinedEqs, combinedCalc, eqs, newEqs,
          shorts},
    shorts = lookupDefault[calc, Shorthands, {}];
    pdDefs = OptionValue[PartialDerivatives];
    derivs = GridFunctionDerivativesInExpression[pdDefs, lookup[calc, Equations],
                                                 OptionValue[ZeroDimensions]];
    If[Length[derivs] === 0, Return[calc]];
    newShortDefs = Map[jacobianShorthand, derivs];
    newShorts = Map[First, newShortDefs];
    combinedShorts = Join[shorts, newShorts];
    eqs = lookup[calc, Equations];
    newEqs = eqs /. (x_?(MemberQ[derivs, #] &) :> derivToJacDeriv[x]);
    combinedEqs = newEqs;

    Do[
      combinedEqs = insertDerivInEqs[derivs[[i]], pdDefs, combinedEqs, shorts,
                                     OptionValue[ZeroDimensions]],
      {i, Length[derivs], 1, -1}];

    combinedCalc = mapReplace[mapReplace[mapEnsureKey[calc, Shorthands, {}], Shorthands, combinedShorts], Equations, combinedEqs];
    combinedCalc];

(* Define local pointers to the members of the Jacobian and Jacobian
   derivatives groups *)
CreateJacobianVariables[] :=
CommentedBlock["Jacobian variable pointers",
  {"const bool use_jacobian1 = (!CCTK_IsFunctionAliased(\"MultiPatch_GetMap\") || MultiPatch_GetMap(cctkGH) != jacobian_identity_map)\n",
   "                      && strlen(jacobian_group) > 0;\n",
   "const bool use_jacobian = assume_use_jacobian>=0 ? assume_use_jacobian : use_jacobian1;\n",
   "const bool usejacobian CCTK_ATTRIBUTE_UNUSED = use_jacobian;\n",
   "if (use_jacobian && (" (*, "strlen(jacobian_determinant_group) == 0) || strlen(jacobian_inverse_group) == 0 || " *), "strlen(jacobian_derivative_group) == 0))\n",
   "{\n",
   "  CCTK_WARN(1, \"GenericFD::jacobian_group " (*<> ", GenericFD::jacobian_determinant_group, GenericFD::jacobian_inverse_group, " *) , "and GenericFD::jacobian_derivative_group must both be set to valid group names\");\n",
   "}\n\n",
   "const CCTK_REAL* restrict jacobian_ptrs[9];\n",
   "if (use_jacobian) GenericFD_GroupDataPointers(cctkGH, jacobian_group,\n",
   "                                              9, jacobian_ptrs);\n",
    "\n",
    Table[{"const CCTK_REAL* restrict const J",i,j," CCTK_ATTRIBUTE_UNUSED = use_jacobian ? jacobian_ptrs[",(i-1)*3+j-1,"] : 0;\n"},{i,1,3},{j,1,3}],
    "\n",
   (* "const CCTK_REAL* restrict jacobian_determinant_ptrs[1] CCTK_ATTRIBUTE_UNUSED;\n", *)
   (* "if (use_jacobian) GenericFD_GroupDataPointers(cctkGH, jacobian_determinant_group,\n", *)
   (* "                                              1, jacobian_determinant_ptrs);\n", *)
   (*  "\n", *)
   (*  {"const CCTK_REAL* restrict const detJ CCTK_ATTRIBUTE_UNUSED = use_jacobian ? jacobian_ptrs[0] : 0;\n"}, *)
   (*  "\n", *)
   (* "const CCTK_REAL* restrict jacobian_inverse_ptrs[9] CCTK_ATTRIBUTE_UNUSED;\n", *)
   (* "if (use_jacobian) GenericFD_GroupDataPointers(cctkGH, jacobian_inverse_group,\n", *)
   (* "                                              9, jacobian_inverse_ptrs);\n", *)
   (*  "\n", *)
   (*  Table[{"const CCTK_REAL* restrict const iJ",i,j," CCTK_ATTRIBUTE_UNUSED = use_jacobian ? jacobian_inverse_ptrs[",(i-1)*3+j-1,"] : 0;\n"},{i,1,3},{j,1,3}], *)
   (* "\n", *)
   "const CCTK_REAL* restrict jacobian_derivative_ptrs[18] CCTK_ATTRIBUTE_UNUSED;\n",
   "if (use_jacobian) GenericFD_GroupDataPointers(cctkGH, jacobian_derivative_group,\n",
   "                                              18, jacobian_derivative_ptrs);\n",
    "\n",
    Module[{syms = Flatten[Table[{"dJ",i,j,k},{i,1,3},{j,1,3},{k,j,3}],2]},
      MapIndexed[{"const CCTK_REAL* restrict const ", #1, " CCTK_ATTRIBUTE_UNUSED = use_jacobian ? jacobian_derivative_ptrs[", #2-1, "] : 0;\n"} &, syms]]}];

(* List of symbols which should be allowed in a calculation *)
JacobianSymbols[] :=
  Map[Symbol, Join[
    Flatten[Table[FlattenBlock[{"J",i,j}],{i,1,3},{j,1,3}],1],
    {FlattenBlock[{"detJ"}]},
    Flatten[Table[FlattenBlock[{"iJ",i,j}],{i,1,3},{j,1,3}],1],
    Flatten[Table[FlattenBlock[{"dJ",i,j,k}],{i,1,3},{j,1,3},{k,j,3}],2]]];

(* Parameters to inherit from GenericFD *)
JacobianGenericFDParameters[] :=
  {{Name -> "assume_use_jacobian",        Type -> "CCTK_INT"},
   {Name -> "jacobian_group",             Type -> "CCTK_STRING"},
   {Name -> "jacobian_determinant_group", Type -> "CCTK_STRING"},
   {Name -> "jacobian_inverse_group",     Type -> "CCTK_STRING"},
   {Name -> "jacobian_derivative_group",  Type -> "CCTK_STRING"},
   {Name -> "jacobian_identity_map",      Type -> "CCTK_INT"}};

(* The symbols which are used for the Jacobian variables in the
   generated source code.  These do not have to coincide with the
   actual variable names, as the variable pointers are read using
   CCTK_VarDataPtr. *)
JacobianGroups[] :=
  {{"unknown::unknown", {Global`J11, Global`J12, Global`J13, Global`J21, Global`J22, Global`J23, Global`J31, Global`J32, Global`J33}},
   {"unknown::unknown", {Global`detJ}},
   {"unknown::unknown", {Global`iJ11, Global`iJ12, Global`iJ13, Global`iJ21, Global`iJ22, Global`iJ23, Global`iJ31, Global`iJ32, Global`iJ33}},
   {"unknown::unknown", {Global`dJ111, Global`dJ112, Global`dJ113, Global`dJ122, Global`dJ123, Global`dJ133,
                         Global`dJ211, Global`dJ212, Global`dJ213, Global`dJ222, Global`dJ223, Global`dJ233,
                         Global`dJ311, Global`dJ312, Global`dJ313, Global`dJ322, Global`dJ323, Global`dJ333}}};

JacobianCheckGroups[groups_] :=
  Module[{int},
    int = Intersection[allGroupVariables[groups], allGroupVariables[JacobianGroups[]]];
    If[int =!= {},
      ThrowError["Error: Some group variables conflict with reserved Jacobian variable names: " <> ToString[int]]]];

(* These gridfunctions are only given local variable copies if the use_jacobian variable is true *)
JacobianConditionalGridFunctions[] :=
  {("J" ~~ DigitCharacter ~~ DigitCharacter) |
   ("detJ") |
   ("iJ" ~~ DigitCharacter ~~ DigitCharacter) |
   ("dJ" ~~ DigitCharacter ~~ DigitCharacter ~~ DigitCharacter),
   "use_jacobian",
   None};

Options[JacobianProcessCode] = ThornOptions;

DefFn[
  JacobianProcessCode[c_Code, opts:OptionsPattern[]] :=
  Module[
    {},
    If[OptionValue[UseJacobian],
       JacobianCheckGroups[GetObjectField[c, "Groups"]]];
    c]];

End[];

EndPackage[];
