
Get["KrancThorn`"];

SetEnhancedTimes[False];

(**************************************************************************************)
(* Derivatives *)
(**************************************************************************************)

derivatives =
{
(*
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
  PDonesided2nd[3] -> dir[3] (-shift[3]^(2 dir[3]) + 4 shift[3]^dir[3] - 3 )/(2 spacing[3])
*)
  PDplus[i_] -> DPlus[i],

  DiffPlus[i_] -> DiffPlusOp[i],
  DiffMinus[i_] -> DiffMinusOp[i]
};

(* PD = PDstandard2nd; *)
PD = PDplus;

(**************************************************************************************)
(* Tensors *)
(**************************************************************************************)

(* Register the tensor quantities with the TensorTools package *)
Map[DefineTensor, {u, uF, uR, uLeft}];

(**************************************************************************************)
(* Groups *)
(**************************************************************************************)

evolvedGroups = Map[CreateGroupFromTensor, {u}];
nonevolvedGroups = Map[CreateGroupFromTensor, {uF, uLeft, uR}];

declaredGroups = Join[evolvedGroups, nonevolvedGroups];
declaredGroupNames = Map[First, declaredGroups];

groups = Join[declaredGroups];

(**************************************************************************************)
(* Initial data *)
(**************************************************************************************)

initialSineCalc =
{
  Name -> "burgers_initial_sine",
  Schedule -> {"at CCTK_INITIAL as burgers_initial"},
  ConditionalOnKeyword -> {"initial_data", "sine"},
  Equations ->
  {
    u -> 1 + amp Sin[2 Pi x]
  }
};

initialShockCalc =
{
  Name -> "burgers_initial_shock",
  Schedule -> {"at CCTK_INITIAL as burgers_initial"},
  ConditionalOnKeyword -> {"initial_data", "shock"},
  Equations ->
  {
    u -> uR0 UnitStep[x-0.5] + uL0 (1-UnitStep[x-0.5])
  }
};

(**************************************************************************************)
(* Evolution equations *)
(**************************************************************************************)

burgersFlux[u_] := 1/2 u^2;

zeroRHSCalc[] :=
{
  Name -> "burgers_zero_rhs",
  Schedule -> {"in MoL_CalcRHS"},
  Equations -> 
  {
    dot[u] -> 0
  }
};

reconstructCalc[i_] :=
{
  Name -> "burgers_reconstruct_" <> ToString[i],
  Where -> Interior,
  Schedule -> {"in MoL_CalcRHS after " <> 
    If[i == 1, "burgers_zero_rhs", "burgers_rhs_" <> ToString[i-1]]},
  Shorthands -> {slopeL, slopeR, slope},
  ApplyBCs -> True,
  Equations -> 
  {
    slopeL -> DiffMinus[u, i],
    slopeR -> DiffPlus[u, i],
    slope -> IfThen[Abs[slopeL] < Abs[slopeR], slopeL, slopeR],
    uLeft -> u - 0.5 slope,
    uR -> u + 0.5 slope
  }
};

fluxCalc[f_, i_] :=
{
  Name -> "burgers_flux_" <> ToString[i],
  Schedule -> {"in MoL_CalcRHS after burgers_reconstruct_" <> ToString[i]},
  Equations -> 
  {
    uF -> 1/2 (f[uLeft] + f[uR] + alpha (uLeft - uR))
  }
};

rhs[i_] :=
{
  Name -> "burgers_rhs_" <> ToString[i],
  Schedule -> {"in MoL_CalcRHS after burgers_flux_" <> ToString[i]},
  Where -> Interior,
  Equations -> 
  {
    dot[u] -> dot[u] + PDplus[uF, i]
  }
};

makeConservationCalcs[f_] :=
({ zeroRHSCalc[]} ~Join~ Flatten[Table[
  {reconstructCalc[i],
   fluxCalc[f, i],
   rhs[i]}, {i, 1, 1}], 1]);


realParameters = {sigma, v0, amp, uL0, uR0, alpha};

keywordParameters = {
  {
    Name -> "initial_data",
    Default -> "sine",
    AllowedValues -> {"sine", "shock"}
  }
};

(**************************************************************************************)
(* Construct the thorn *)
(**************************************************************************************)

calculations = 
{
  initialSineCalc,
  initialShockCalc
} ~Join~ makeConservationCalcs[burgersFlux];

Print[calculations];

CreateKrancThornTT[groups, ".", "Burgers", 
  Calculations -> calculations,
  DeclaredGroups -> declaredGroupNames,
  PartialDerivatives -> derivatives,
  RealParameters -> realParameters,
  KeywordParameters -> keywordParameters];
