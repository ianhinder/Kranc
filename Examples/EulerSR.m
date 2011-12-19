
Get["KrancThorn`"];

SetEnhancedTimes[False];

(**************************************************************************************)
(* Tensors *)
(**************************************************************************************)

(* Register the tensor quantities with the TensorTools package *)
Map[DefineTensor, {Den, S, tau, rho, v, epsi, W, h, p}];

(**************************************************************************************)
(* Groups *)
(**************************************************************************************)

evolvedGroups = Map[CreateGroupFromTensor, {Den, S[lj], tau}];
nonevolvedGroups = Map[CreateGroupFromTensor, 
{
  rho, v[uj], epsi, W, h, p
}];

declaredGroups = Join[evolvedGroups, nonevolvedGroups];
declaredGroupNames = Map[First, declaredGroups];

groups = declaredGroups;

(**************************************************************************************)
(* Initial data *)
(**************************************************************************************)

initialShockCalc =
{
  Name -> "eulersr_initial_shock",
  Schedule -> {"at CCTK_INITIAL as eulersr_initial"},
  ConditionalOnKeyword -> {"initial_data", "shock"},
  Shorthands -> {X},
  Equations ->
  {
    X -> x,
    rho -> rhoR0 StepFunction[X] + rhoL0 (1-StepFunction[X]),
    v[1] -> vR0 StepFunction[X] + vL0 (1-StepFunction[X]),
    v[2] -> vR0 StepFunction[X] + vL0 (1-StepFunction[X]),
    v[3] -> vR0 StepFunction[X] + vL0 (1-StepFunction[X]),
    epsi -> epsiR0 StepFunction[X] + epsiL0 (1-StepFunction[X])
  }
};

(**************************************************************************************)
(* Evolution equations *)
(**************************************************************************************)

(* Euler's equation is dot[u] + PD[F[ui],li] = 0

   with

     u = {D, S, tau}

   and

     DF[ui] = D v[ui]
     SF[ui,lj] = S[lj] v[ui] + p Euc[lj,ui]
     tauF[ui] = v[ui](tau + p)

*)

eulerCons =
{
  Name -> "eulersr_cons_calc",
  Shorthands -> {pBar, Z, Ssq, vsq, pEOS, f, cs, df, Wx},

  Primitives -> {rho, v[ui], epsi},

  Equations ->
  {
    flux[Den,ui] -> Den v[ui],
    flux[S[lj],ui] -> S[lj] v[ui] + ((gamma-1) rho epsi (* This term is p *)) Euc[ui,lj],
    flux[tau,ui] -> v[ui](tau + (gamma-1) rho epsi (* This term is p *))
  },

  ConservedEquations ->
  {
    Wx -> 1 - v[ui] v[uj] Euc[li,lj],
    W -> Wx^(-1/2),
    p -> (gamma-1) rho epsi,
    h -> 1 + epsi + p/rho,

    Den -> rho W,
    S[li] -> rho h W^2 v[uj] Euc[li,lj],
    tau -> rho h W^2 - p - Den
  },

  PrimitiveEquations ->
  {
    (* To compute p, given Den, S[ui], tau and a guess for p (pBar),
         Z = tau + Den + pBar
         S2 = S[li] S[lj] Euc[ui,uj]
         v2 = S2/Z^2
         W = (1-v2)^(-1/2)
         rho = Den/W
         h = Z/(rho W^2)
         epsi = h-1-pBar/rho
         pNew = (gamma - 1) rho epsi
         f = pNew - pBar
         cs = Sqrt[gamma (gamma-1) epsi/h]
         df = v2 cs^2 - 1
         
         -> p (until f is sufficiently small) (also get rho, epsi)
    *)

    pBar -> p, (* from previous timestep *)
    (* Start loop *)

    f -> 10,

    (* This should be some sort of while loop so you run until f <
       1e-12.  A naive implementation in terms of IfThen has subtle
       problems. Ideally, Kranc would support the iterative solution
       of equations directly. Instead, we always run 5 iterations and
       hope that this is enough. *)

    Sequence@@Join@@Table[
    {Z -> tau + Den + pBar,
    Ssq -> S[li] S[lj] Euc[ui,uj],
    vsq -> Ssq/Z^2,
    W -> (1-vsq)^(-1/2),
    rho -> Den/W,
    h -> Z/(rho W^2),
    epsi -> h-1-pBar/rho,
    pEOS -> (gamma - 1) rho epsi,
    f -> pEOS - pBar,
    cs -> Sqrt[gamma (gamma-1) epsi/h],
    df -> vsq cs^2 - 1,
    pBar -> pBar - f/df},
      {i, 1, 5}],

    (* end of loop *)

    p -> pBar,

    v[ui] -> S[lj] Euc[ui,uj] / (rho h W^2)
  }
}

(**************************************************************************************)
(* Parameters *)
(**************************************************************************************)

realParameters = {sigma, v0, amp, rhoR0, rhoL0, vR0, vL0, epsiR0, epsiL0, gamma};

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

CreateKrancThornTT[groups, ".", "EulerSR", 
  Calculations -> calculations,
  ConservationCalculations -> consCalculations,
  DeclaredGroups -> declaredGroupNames,
  RealParameters -> realParameters,
  KeywordParameters -> keywordParameters];
