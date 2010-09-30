
Get["KrancThorn`"];

SetEnhancedTimes[False];

(**************************************************************************************)
(* Derivatives *)
(**************************************************************************************)

derivatives =
{
  PDstandard2nd[i_] -> StandardCenteredDifferenceOperator[1,1,i],
  PDstandard2nd[i_, i_] -> StandardCenteredDifferenceOperator[2,1,i],
  PDstandard2nd[i_, j_] -> StandardCenteredDifferenceOperator[1,1,i] *
    StandardCenteredDifferenceOperator[1,1,j],

  PDstandard4th[i_] -> StandardCenteredDifferenceOperator[1,2,i],
  PDstandard4th[i_, i_] -> StandardCenteredDifferenceOperator[2,2,i],
  PDstandard4th[i_, j_] -> StandardCenteredDifferenceOperator[1,2,i] *
    StandardCenteredDifferenceOperator[1,2,j],

  PDonesided2nd[1] -> dir[1] (-shift[1]^(2 dir[1]) + 4 shift[1]^dir[1] - 3 )/(2 spacing[1]),
  PDonesided2nd[2] -> dir[2] (-shift[2]^(2 dir[2]) + 4 shift[2]^dir[2] - 3 )/(2 spacing[2]),
  PDonesided2nd[3] -> dir[3] (-shift[3]^(2 dir[3]) + 4 shift[3]^dir[3] - 3 )/(2 spacing[3]),

  PDplus[i_] -> DPlus[i],
  PDminus[i_] -> DMinus[i],
  DiffPlus[i_] -> DiffPlusOp[i],
  DiffMinus[i_] -> DiffMinusOp[i],
  ShiftMinus[i_] -> 1/shift[i]
};

(* PD = PDstandard2nd; *)
(* PD = PDminus; *)

(**************************************************************************************)
(* Tensors *)
(**************************************************************************************)

(* Register the tensor quantities with the TensorTools package *)
Map[DefineTensor, {Den, S, En, rho, v, p, DenF, DenRight, DenLeft, SF, SRight, SLeft,
  EnF, EnRight, EnLeft, vLeft, vRight, rhoLeft, rhoRight, pLeft, pRight, vRightTemp}];

(**************************************************************************************)
(* Groups *)
(**************************************************************************************)

evolvedGroups = Map[CreateGroupFromTensor, {Den, S[uj], En}];
nonevolvedGroups = Map[CreateGroupFromTensor, 
{
  rho, v[uj], p,
  DenF, DenRight, DenLeft,
  SF[uj], SRight[uj], SLeft[uj],
  EnF, EnRight, EnLeft,
  rhoLeft, rhoRight,
  vLeft[uj], vRight[uj],
  pLeft, pRight
}];

declaredGroups = Join[evolvedGroups, nonevolvedGroups];
declaredGroupNames = Map[First, declaredGroups];

groups = Join[declaredGroups];

(**************************************************************************************)
(* Initial data *)
(**************************************************************************************)

(* initialSineCalc = *)
(* { *)
(*   Name -> "euler_initial_sine", *)
(*   Schedule -> {"at CCTK_INITIAL as euler_initial"}, *)
(*   ConditionalOnKeyword -> {"initial_data", "sine"}, *)
(*   Equations -> *)
(*   { *)
(*     rho -> 1 + amp Sin[2 Pi x], *)
(*     v1 -> v0, *)
(*     v2 -> 0, *)
(*     v3 -> 0, *)
(*     p -> 1 (\* Rewrite this so that we get something compatible with En = 1 *\) *)
(*   } *)
(* }; *)

initialShockCalc =
{
  Name -> "euler_initial_shock",
  Schedule -> {"at CCTK_INITIAL as euler_initial"},
  ConditionalOnKeyword -> {"initial_data", "shock"},
  Equations ->
  {
    rho -> rhoR0 UnitStep[x-0.5] + rhoL0 (1-UnitStep[x-0.5]),
    v[1] -> vR0 UnitStep[x-0.5] + vL0 (1-UnitStep[x-0.5]),
    v[2] -> 0,
    v[3] -> 0,
    p -> pR0 UnitStep[x-0.5] + pL0 (1-UnitStep[x-0.5])
  }
};

conservedCalc =
{
  Name -> "euler_conserved",
  Schedule -> {"at INITIAL after euler_initial"},
  Equations -> 
  {
    Den -> rho,
    S[ui] -> rho v[ui],
    En -> p/(gamma-1) + 1/2 rho v[ui] v[uj] Euc[li,lj]
  }
};

conservedFluxCalc[i_] :=
{
  Name -> "euler_conserved_flux_" <> ToString[i],
  Schedule -> {"in MoL_CalcRHS after euler_reconstruct_" <> ToString[i]},
  Equations -> 
  {
    DenLeft -> rhoLeft,
    DenRight -> rhoRight,
    SLeft[ui] -> rhoLeft vLeft[ui],
    SRight[ui] -> rhoRight vRight[ui],
    EnLeft -> pLeft/(gamma-1) + 1/2 rhoLeft vLeft[ui] vLeft[uj] Euc[li,lj],
    EnRight -> pRight/(gamma-1) + 1/2 rhoRight vRight[ui] vRight[uj] Euc[li,lj]
  }
};

primitivesCalc =
{
  Name -> "euler_primitives",
  Schedule -> {"in MoL_PostStep after Euler_ApplyBCs"}, (* Need BCs *)
  Equations -> 
  {
    rho -> Den,
    v[ui] -> S[ui] / Den,
    p -> (gamma-1)(En - 1/2 Euc[li,lj] S[ui] S[uj]/Den)
  }
};


(**************************************************************************************)
(* Evolution equations *)
(**************************************************************************************)

(* Euler's equation is dot[u] + PD[F[ui],li] = 0

   with

     u = {D, S, E}

   and

     DF[ui] = rho v[ui]
     SF[ui,uj] = rho v[ui] v[uj] + p Euc[ui,uj]
     EnF[ui] = v[ui](En + p)

*)

eulerDenFlux[rho_, v_, p_, i_] :=
  rho v[i];

eulerSFlux[rho_, v_, p_, {i_, j_}] :=
  rho v[i] v[j] + p Euc[i,j];

eulerEnFlux[rho_, v_, p_, En_, i_] :=
  v[i](En + p);

zeroRHSCalc[] :=
{
  Name -> "euler_zero_rhs",
  Schedule -> {"in MoL_CalcRHS"},
  Equations -> 
  {
    dot[Den] -> 0,
    dot[S[ui]] -> 0,
    dot[En] -> 0
  }
};

minmodVar[v_, i_, vLeft_, vRight_] :=
{
  slopeL -> DiffMinus[v, i],
  slopeR -> DiffPlus[v, i],
  slope -> MinMod[slopeL, slopeR],
  vLeft -> v - 0.5 slope,
  vRight -> v + 0.5 slope
}

reconstructCalc[i_] :=
{
  Name -> "euler_reconstruct_" <> ToString[i],
  Where -> Interior,
  Schedule -> {"in MoL_CalcRHS after " <> 
    If[i == 1, "euler_zero_rhs", "euler_rhs_" <> ToString[i-1]]},
  Shorthands -> {slopeL, slopeR, slope},
  ApplyBCs -> True,
  Equations -> 
    Flatten[{
      minmodVar[rho,i, rhoLeft, rhoRight],
      Flatten[Table[
        minmodVar[v[j], i, Symbol["vLeft"<>ToString[j]],
                           Symbol["vRight"<>ToString[j]]], {j, 1, 3}],1],
      minmodVar[p,i, pLeft, pRight]
  },1]
};

fluxCalc[i_] :=
{
  Name -> "euler_flux_" <> ToString[i],
  ApplyBCs -> True,
  Where -> Interior,
  Schedule -> {"in MoL_CalcRHS after euler_conserved_flux_" <> ToString[i]},
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

rhs[i_] :=
{
  Name -> "euler_rhs_" <> ToString[i],
  Schedule -> {"in MoL_CalcRHS after euler_flux_" <> ToString[i]},
  Where -> Interior,
  Equations -> 
  {
    dot[Den] -> dot[Den] - PDplus[DenF, i],
    dot[S[uj]] -> dot[S[uj]] - PDplus[SF[uj], i],
    dot[En] -> dot[En] - PDplus[EnF, i]
  }
};

makeConservationCalcs[] :=
({ zeroRHSCalc[]} ~Join~ Flatten[Table[
  {reconstructCalc[i],
   conservedFluxCalc[i],
   fluxCalc[i],
   rhs[i]}, {i, 1, 1}], 1]);

realParameters = {sigma, v0, amp, rhoR0, rhoL0, vR0, vL0, pR0, pL0, gamma, alpha};

keywordParameters = {
  {
    Name -> "initial_data",
    Default -> "shock",
    AllowedValues -> {"shock"}
  }
};

(**************************************************************************************)
(* Construct the thorn *)
(**************************************************************************************)

calculations = 
{
  initialShockCalc,
  primitivesCalc,
  conservedCalc
} ~Join~ makeConservationCalcs[];


CreateKrancThornTT[groups, ".", "Euler", 
  Calculations -> calculations,
  DeclaredGroups -> declaredGroupNames,
  PartialDerivatives -> derivatives,
  RealParameters -> realParameters,
  KeywordParameters -> keywordParameters];
