
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
  PDonesided2nd[3] -> dir[3] (-shift[3]^(2 dir[3]) + 4 shift[3]^dir[3] - 3 )/(2 spacing[3])

  (* PDplus[i_] -> DPlus[i], *)
  (* PDminus[i_] -> DMinus[i], *)
  (* ShiftPlus[i_] -> EPlus, *)
  (* ShiftMinus[i_] -> EMinus *)
};

(* PD = PDstandard2nd; *)
PD = PDplus;

(**************************************************************************************)
(* Tensors *)
(**************************************************************************************)

(* Register the tensor quantities with the TensorTools package *)
Map[DefineTensor, {Frho, F2rho, rho, v, dir}];

(**************************************************************************************)
(* Groups *)
(**************************************************************************************)

evolvedGroups = Map[CreateGroupFromTensor, {rho}];
nonevolvedGroups = Map[CreateGroupFromTensor, {Frho[ui], F2rho[ui], v[ui]}];

declaredGroups = Join[evolvedGroups, nonevolvedGroups];
declaredGroupNames = Map[First, declaredGroups];

groups = Join[declaredGroups];

(**************************************************************************************)
(* Initial data *)
(**************************************************************************************)

initialSineCalc =
{
  Name -> "advect_initial_sine",
  Schedule -> {"at CCTK_INITIAL as advect_initial"},
  ConditionalOnKeyword -> {"initial_data", "sine"},
  Equations ->
  {
    v1 -> v0,
    v2 -> 0,
    v3 -> 0,
    rho -> 1 + amp Sin[2 Pi x]
  }
};

initialShockCalc =
{
  Name -> "advect_initial_shock",
  Schedule -> {"at CCTK_INITIAL as advect_initial"},
  ConditionalOnKeyword -> {"initial_data", "shock"},
  Equations ->
  {
    v1 -> v0,
    v2 -> 0,
    v3 -> 0,
    rho -> amp UnitStep[x-0.5]
  }
};

(**************************************************************************************)
(* Evolution equations *)
(**************************************************************************************)

evolCalc =
{
  Name -> "advect_evol",
  Schedule -> {"in MoL_CalcRHS"},
  Shorthands -> {},
  Where -> Interior,
  Equations -> 
  {
    (* dot[rho]   -> PDplus[F2rho[ui], li] *)
    dot[rho]   -> PDstandard2nd[Frho[ui],li]
                  (* alpha PDstandard2nd[rho,li,lj] Euc[ui,uj] *)
  }
};

fluxCalc =
{
  Name -> "advect_flux",
  Schedule -> {"in MoL_PostStep after Advect_ApplyBCs"},
  Equations -> 
  {
    Frho[ui] -> rho v[ui]
  }
};

(* flux2Calc = *)
(* { *)
(*   Name -> "advect_flux2", *)
(*   Schedule -> {"in MoL_PostStep after advect_flux"}, *)
(*   Equations ->  *)
(*   { *)
(*     F2rho[ui] -> 1/2(ShiftMinus[Frho[ui], lj] Euc[uj,ui] + Frho[ui] +  *)
(*                      alpha(ShiftMinus[rho, lj] Euc[uj,ui] - rho)) *)
(*   } *)
(* }; *)

realParameters = {sigma, v0, amp};

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
  initialShockCalc,
  evolCalc,
  fluxCalc
  (* flux2Calc *)
};

CreateKrancThornTT[groups, ".", "Advect", 
  Calculations -> calculations,
  DeclaredGroups -> declaredGroupNames,
  PartialDerivatives -> derivatives,
  RealParameters -> realParameters,
  KeywordParameters -> keywordParameters];
