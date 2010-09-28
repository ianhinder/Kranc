
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
  PDminus[i_] -> DMinus[i]
};

PD = PDstandard2nd;
(* PD = PDminus; *)

(**************************************************************************************)
(* Tensors *)
(**************************************************************************************)

(* Register the tensor quantities with the TensorTools package *)
Map[DefineTensor, {w, Frho, Fw, FEn, rho, En, p, dir, v}];

(**************************************************************************************)
(* Groups *)
(**************************************************************************************)

evolvedGroups = Map[CreateGroupFromTensor, {rho, w[uj], En, v[uj] (* This should not be here *) }];
nonevolvedGroups = Map[CreateGroupFromTensor, {Frho[ui], Fw[ui,uj], FEn[ui], p}];

declaredGroups = Join[evolvedGroups, nonevolvedGroups];
declaredGroupNames = Map[First, declaredGroups];

groups = Join[declaredGroups];

(**************************************************************************************)
(* Initial data *)
(**************************************************************************************)

initialSineCalc =
{
  Name -> "euler_initial_sine",
  Schedule -> {"at CCTK_INITIAL as euler_initial"},
  ConditionalOnKeyword -> {"initial_data", "sine"},
  Equations ->
  {
    v1 -> v0,
    v2 -> 0,
    v3 -> 0,
    rho -> 1 + amp Sin[2 Pi x],
    En -> 1
  }
};

initialShockCalc =
{
  Name -> "euler_initial_shock",
  Schedule -> {"at CCTK_INITIAL as euler_initial"},
  ConditionalOnKeyword -> {"initial_data", "shock"},
  Equations ->
  {
    v1 -> 0.5 + 0.5 UnitStep[x-0.5],
    v2 -> 0,
    v3 -> 0,
    rho -> 1 + amp UnitStep[x-0.5],
    En -> 1
  }
};

conservedCalc =
{
  Name -> "euler_conserved",
  Schedule -> {"at INITIAL after euler_initial"},
  Equations -> 
  {
    w[ui] -> rho v[ui]
  }
};

(**************************************************************************************)
(* Evolution equations *)
(**************************************************************************************)

evolCalc =
{
  Name -> "euler_evol",
  Schedule -> {"in MoL_CalcRHS"},
  Shorthands -> {dir[ui]},
  Where -> Interior,
  Equations -> 
  {
    dot[rho]   -> PD[Frho[ui],  li],
    dot[w[uj]] -> PD[Fw[uj,ui], li],
    dot[En]    -> PD[FEn[ui],   li]
  }
};

primitivesCalc =
{
  Name -> "euler_primitives",
  Schedule -> {"in MoL_PostStep after Euler_ApplyBCs"}, (* Need BCs *)
  Equations -> 
  {
    v[ui] -> w[ui] / rho,
    p -> 2/3 (En - 1/2 Euc[li,lj] v[ui] v[uj])
  }
};

fluxCalc =
{
  Name -> "euler_flux",
  Schedule -> {"in MoL_PostStep after euler_primitives"},
  Equations -> 
  {
    Frho[ui] -> rho v[ui],
    Fw[uj,ui] -> rho v[ui] v[uj] + p Euc[ui,uj],
    FEn[ui] -> v[ui] * (En + p)
  }
};

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
  fluxCalc,
  (* pressureCalc, *)
  primitivesCalc,
  conservedCalc
};

CreateKrancThornTT[groups, ".", "Euler", 
  Calculations -> calculations,
  DeclaredGroups -> declaredGroupNames,
  PartialDerivatives -> derivatives,
  RealParameters -> realParameters,
  KeywordParameters -> keywordParameters];
