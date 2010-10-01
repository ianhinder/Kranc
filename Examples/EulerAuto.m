
Get["KrancThorn`"];

SetEnhancedTimes[False];

(**************************************************************************************)
(* Tensors *)
(**************************************************************************************)

(* Register the tensor quantities with the TensorTools package *)
Map[DefineTensor, {Den, S, En, rho, v, p}];

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

groups = declaredGroups;

(**************************************************************************************)
(* Initial data *)
(**************************************************************************************)

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

eulerCons =
{
  Name -> "eulerauto_cons_calc",

  Equations ->
  {
    flux[Den,ui] -> rho v[ui],
    flux[S[uj],ui] -> rho v[ui] v[uj] + p Euc[ui,uj],
    flux[En,ui] -> v[ui](En + p)
  },

  ConservedEquations ->
  {
    Den -> rho,
    S[ui] -> rho v[ui],
    En -> p/(gamma-1) + 1/2 rho v[ui] v[uj] Euc[li,lj]
  },

  PrimitiveEquations ->
  {
    rho -> Den,
    v[ui] -> S[ui] / Den,
    p -> (gamma-1)(En - 1/2 Euc[li,lj] S[ui] S[uj]/Den)
  }
}

(**************************************************************************************)
(* Parameters *)
(**************************************************************************************)

realParameters = {sigma, v0, amp, rhoR0, rhoL0, vR0, vL0, pR0, pL0, gamma};

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
  initialShockCalc
};

consCalculations = {eulerCons};

CreateKrancThornTT[groups, ".", "EulerAuto", 
  Calculations -> calculations,
  ConservationCalculations -> consCalculations,
  DeclaredGroups -> declaredGroupNames,
  RealParameters -> realParameters,
  KeywordParameters -> keywordParameters];
