
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
  rho, v[uj], p
}];

declaredGroups = Join[evolvedGroups, nonevolvedGroups];
declaredGroupNames = Map[First, declaredGroups];

groups = Join[declaredGroups];

(**************************************************************************************)
(* Initial data *)
(**************************************************************************************)

(* initialSineCalc = *)
(* { *)
(*   Name -> "eulerauto_initial_sine", *)
(*   Schedule -> {"at CCTK_INITIAL as eulerauto_initial"}, *)
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
  Name -> "eulerauto_initial_shock",
  Schedule -> {"at CCTK_INITIAL as eulerauto_initial"},
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

realParameters = {sigma, v0, amp, rhoR0, rhoL0, vR0, vL0, pR0, pL0, gamma, alpha};

keywordParameters = {
  {
    Name -> "initial_data",
    Default -> "shock",
    AllowedValues -> {"shock"}
  }
};

(**************************************************************************************)
(* Conservation calculation *)
(**************************************************************************************)

eulerCons =
{
  Name -> "eulerauto_cons_calc",
  Equations ->
  {
    flux[Den,ui] -> rho v[ui],
    flux[S[uj],ui] -> rho v[ui] v[uj] + p Euc[ui,uj],
    flux[En,ui] -> v[ui](En + p)
  }
}

(**************************************************************************************)
(* Construct the thorn *)
(**************************************************************************************)

calculations = 
{
  initialShockCalc
};

consCalculations = {eulerCons};

CreateKrancThornTT[groups, ".", "EulerAuto", 
  Calculations -> calculations,
  ConservationCalculations -> consCalculations,
  DeclaredGroups -> declaredGroupNames,
  PartialDerivatives -> derivatives,
  RealParameters -> realParameters,
  KeywordParameters -> keywordParameters];
