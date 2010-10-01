
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

Begin["`Private`"];

ConservationDifferencingOperators[] :=
{
  DiffPlus[i_] -> DiffPlusOp[i],
  DiffMinus[i_] -> DiffMinusOp[i],
  ShiftMinus[i_] -> 1/shift[i],
  PDplus[i_] -> DPlus[i]
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
reconsVars[calc_] :=
  Module[{allGFs, calcSyms, gfsUsed, conserved, primitive},
    allGFs = allGroupVariables[lookup[calc, Groups]];
    Print["allGFs = ", allGFs];
    calcSyms = calculationSymbols[calc];
    Print["calcSyms = ", calcSyms];
    gfsUsed = Intersection[allGFs, calcSyms];
    Print["gfsUsed = ", gfsUsed];
    conserved = consVars[calc];
    Print["conserved = ", conserved];
    primitive = Complement[gfsUsed, conserved];
    Print["primitive = ", primitive];
    primitive];

(* Return the variables for which Left and Right GFs need to be created *)
lrGFs[calc_] :=
  Module[{allGFs, calcSyms, gfsUsed, conserved, primitive},
    Print["1"];
    Print[calc];
    allGFs = allGroupVariables[lookup[calc, Groups]];
    Print["2"];
    calcSyms = calculationSymbols[calc];
    Print["3"];
    gfsUsed = Intersection[allGFs, calcSyms];
    Print["4"];
    conserved = consVars[calc];
    Print["5"];
    primitive = Complement[gfsUsed, conserved];
    Print["6"];
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
                  {v, reconsVars[calc]}], 1]
}

fluxCalc[i_] :=
{
  Name -> "eulerauto_flux_" <> ToString[i],
  ApplyBCs -> True,
  Where -> Interior,
  Schedule -> {"in MoL_CalcRHS after eulerauto_conserved_flux_" <> ToString[i]},
  Shorthands -> {vRightTemp[ui]},
  Equations -> 
  {
    vRightTemp[ui] -> ShiftMinus[vRight[ui],i],

    DenF -> 1/2 (eulerDenFlux[rhoLeft, vLeft, pLeft, i] + 
                 eulerDenFlux[ShiftMinus[rhoRight,i], 
                              vRightTemp,
                              ShiftMinus[pRight,i], i] + 
                 alpha (ShiftMinus[DenRight,i] - DenLeft)),

    SF[uj] -> 1/2 (eulerSFlux[rhoLeft, vLeft, pLeft, {i, uj}] + 
                   eulerSFlux[ShiftMinus[rhoRight, i], 
                              vRightTemp,
                              ShiftMinus[pRight, i], {i, uj}] + 
                   alpha (ShiftMinus[SRight[uj],i] - SLeft[uj])),

    EnF -> 1/2 (eulerEnFlux[rhoLeft, vLeft, pLeft, EnLeft, i] + 
               eulerEnFlux[ShiftMinus[rhoRight,i], 
                           vRightTemp,
                           ShiftMinus[pRight,i], ShiftMinus[EnRight,i], i] + 
               alpha (ShiftMinus[EnRight,i] - EnLeft))
  }
};

rhs[calc_, i_] :=
{
  Name -> lookup[calc,Name] <> "_rhs_" <> ToString[i],
  Schedule -> {"in MoL_CalcRHS after " <> lookup[calc,Name] <> "_flux_" <> ToString[i]},
  Where -> Interior,
  Equations -> 
    Table[dot[v] -> dot[v] - PDplus[fluxSymbol[v], i], {v, consVars[calc]}]
};

(* Given a ConservationCalculation structure, return a list of
   calculations which should be scheduled to solve that conservation
   law. *)
ProcessConservationCalculation[calc_] :=
  Module[{},
  {
    zeroRHSCalc[calc], 
    Sequence@@Flatten[
      Table[
        {reconstructCalc[calc, i],
(*         conservedFluxCalc[i], *)
(*         fluxCalc[i],  *)
         rhs[calc, i]}, {i, 1, 1}], 1]
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
