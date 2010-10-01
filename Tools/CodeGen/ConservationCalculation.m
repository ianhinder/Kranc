
(*  Copyright 2010 Ian Hinder

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

BeginPackage["ConservationCalculation`", {"Errors`", "Helpers`", "Kranc`",
  "MapLookup`", "KrancGroups`", "CalculationFunction`", "Differencing`"}];

ProcessConservationCalculation;
ConservationDifferencingOperators;
DiffPlus;
DiffMinus;
ShiftMinus;
PDplus;
ConservationCalculationDeclaredGroups;
ConservationDifferencingRealParameters;
hlleAlpha;
PrimitiveEquations;
ConservedEquations;

Begin["`Private`"];

ConservationDifferencingOperators[] :=
{
  DiffPlus[i_] -> DiffPlusOp[i],
  DiffMinus[i_] -> DiffMinusOp[i],
  ShiftMinus[i_] -> 1/shift[i],
  PDplus[i_] -> DPlus[i]
};

ConservationDifferencingRealParameters[] :=
{
  hlleAlpha
};

zeroRHSCalc[calc_] :=
{
  Name -> lookup[calc,Name] <> "_zero_rhs",
  Schedule -> {"in MoL_CalcRHS"},
  Equations ->
    (Map[First, lookup[calc, Equations]] /. {flux[v_, rest___] :> (dot[v] -> 0)})
};

minmodVar[v_, i_, vLeft_, vRight_] :=
{
  slopeL -> DiffMinus[v, i],
  slopeR -> DiffPlus[v, i],
  slope -> MinMod[slopeL, slopeR],
  vLeft -> v - 0.5 slope,
  vRight -> v + 0.5 slope
}

vanLeerVar[v_, i_, vLeft_, vRight_] :=
{
  slopeL -> DiffMinus[v, i],
  slopeR -> DiffPlus[v, i],
  slope -> VanLeer[slopeL, slopeR],
  vLeft -> v - 0.5 slope,
  vRight -> v + 0.5 slope
}

leftSymbol[v_] :=
  Symbol["Global`" <> ToString[v] <> "Left"];

rightSymbol[v_] :=
  Symbol["Global`" <> ToString[v] <> "Right"];

fluxSymbol[v_] :=
  Symbol["Global`" <> ToString[v] <> "Flux"];

(* Return the list of conserved variables in a calculation *)
consVars[calc_] :=
  (Map[First, lookup[calc, Equations]] /. {flux[v_, rest___] :> v})

(* Return the list of variables to reconstruct in a calculation *)
primitiveVars[calc_] :=
  Module[{allGFs, calcSyms, gfsUsed, conserved, primitive},
    allGFs = allGroupVariables[lookup[calc, Groups]];
    calcSyms = calculationSymbols[calc];
    gfsUsed = Intersection[allGFs, calcSyms];
    conserved = consVars[calc];
    primitive = Complement[gfsUsed, conserved];
    primitive];

(* Return the variables for which Left and Right GFs need to be created *)
lrGFs[calc_] :=
  Module[{allGFs, calcSyms, gfsUsed, conserved, primitive},
    allGFs = allGroupVariables[lookup[calc, Groups]];
    calcSyms = calculationSymbols[calc];
    gfsUsed = Intersection[allGFs, calcSyms];
    conserved = consVars[calc];
    primitive = Complement[gfsUsed, conserved];
    Join[primitive, conserved]];

reconstructCalc[calc_, i_] :=
{
  Name -> lookup[calc,Name] <> "_reconstruct_" <> ToString[i],
  Where -> Interior,
  Schedule -> {"in MoL_CalcRHS after " <>
    If[i == 1, lookup[calc,Name] <> "_zero_rhs",
               lookup[calc,Name] <> "_rhs_" <> ToString[i-1]]},
  Shorthands -> {slopeL, slopeR, slope},
  ApplyBCs -> True,
  Equations ->
    Flatten[Table[minmodVar[v,i, leftSymbol[v], rightSymbol[v]],
                  {v, primitiveVars[calc]}], 1]
};

replaceVars[x_, vars_, f_] :=
  Module[{},
    x /. Map[(# -> f[#])&, vars]];

hlle[flux[q_, j_] -> frhs_, vars_] :=
  Module[{},
  {
    leftSymbol[fluxSymbol[q]] -> replaceVars[frhs, vars, leftSymbol],
    rightSymbol[fluxSymbol[q]] -> replaceVars[frhs, vars, rightSymbol],
    fluxSymbol[q] ->
      1/2(leftSymbol[fluxSymbol[q]] + rightSymbol[fluxSymbol[q]] +
          hlleAlpha(ShiftMinus[rightSymbol[q],j] - leftSymbol[q]))
  }];

fluxCalc[calc_, i_] :=
  Module[{fluxes = Select[lookup[calc, Equations], MatchQ[#, flux[_,i]->_] &]},
  {
    Name -> lookup[calc,Name] <> "_flux_" <> ToString[i],
    ApplyBCs -> True,
    Where -> Interior,
    Schedule -> {"in MoL_CalcRHS after " <> lookup[calc,Name] <>
      "_conserved_flux_" <> ToString[i]},
    Shorthands -> Join[Map[leftSymbol[fluxSymbol[#]]&, consVars[calc]],
      Map[rightSymbol[fluxSymbol[#]]&, consVars[calc]]],
    Equations ->
      Flatten[Map[hlle[#,GridFunctionsInExpression[#[[2]], lookup[calc, Groups]]] &, fluxes],1]
  }];

rhs[calc_, i_] :=
{
  Name -> lookup[calc,Name] <> "_rhs_" <> ToString[i],
  Schedule -> {"in MoL_CalcRHS after " <> lookup[calc,Name] <> "_flux_" <> ToString[i]},
  Where -> Interior,
  Equations ->
    Table[dot[v] -> dot[v] - PDplus[fluxSymbol[v], i], {v, consVars[calc]}]
};

primitivesCalc[calc_, thornName_] :=
{
  Name -> lookup[calc, Name] <> "_primitives",
  Schedule -> {"in MoL_PostStep after " <> thornName <>"_ApplyBCs"},
  Equations -> lookup[calc, PrimitiveEquations]
};

conservedCalc[calc_] :=
{
  Name -> lookup[calc, Name] <> "_conserved",
  Schedule -> {"at POSTINITIAL"},
  Equations -> lookup[calc, ConservedEquations]
};

(* Given a ConservationCalculation structure, return a list of
   calculations which should be scheduled to solve that conservation
   law. *)
ProcessConservationCalculation[calc_, thornName_] :=
  Module[{},
  {
    zeroRHSCalc[calc],
    Sequence@@Flatten[
      Table[
        {conservedCalc[calc],
         reconstructCalc[calc, i],
(*         conservedFluxCalc[i], *)
         fluxCalc[calc, i],
         rhs[calc, i],
         primitivesCalc[calc, thornName]}, {i, 1, 1}], 1]
  }];

(* Return all the new groups which need to be created for this
   conservation calculation *)
ConservationCalculationDeclaredGroups[calc_] :=
  Module[{},
    Map[CreateGroup[
      ToString[#]<>"_lr_group",
      {leftSymbol[#], rightSymbol[#]}, {}] &, lrGFs[calc]] ~Join~
    Map[CreateGroup[
      ToString[#]<>"_flux_group",
      {fluxSymbol[#]}, {}] &, consVars[calc]]];

End[];

EndPackage[];
