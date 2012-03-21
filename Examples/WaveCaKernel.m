<< "KrancThorn.m";

SetEnhancedTimes[False];

Do[

groups = {{"phi_g", {phi}}, {"pi_g", {pi}}, {"xCopy_g", {xCopy}}};

PDstandard[fdOrder_] :=
  Symbol["PDstandard"<>ToString[fdOrder]<>"th"];

fdOrders = {2,4};

derivatives =
{
Sequence@@Flatten[Table[
  {PDstandard[fdOrder][i_] -> StandardCenteredDifferenceOperator[1,fdOrder/2,i],
   PDstandard[fdOrder][i_,i_] -> StandardCenteredDifferenceOperator[2,fdOrder/2,i],
   PDstandard[fdOrder][i_,j_] -> StandardCenteredDifferenceOperator[1,fdOrder/2,i] *
                                 StandardCenteredDifferenceOperator[1,fdOrder/2,j]},
  {fdOrder, fdOrders}],1]
};

PD = PDstandard;

f[x_] := Exp[-(x/0.1)^2];

initialGaussianCalc = 
{
  Name -> "initial_gaussian",
  Schedule -> {"AT INITIAL"},
  ExecuteOn -> Host,
  Equations -> 
  {
    phi -> f[t+x],
    pi -> D[f[t+x],t],
    xCopy -> x
  }
};

evolveCalc[fdOrderp_] := 
{
  Name -> "calc_rhs_"<>ToString[fdOrderp],
  Schedule -> {"in MoL_CalcRHS", "at ANALYSIS"},
  (* ConditionalOnTextuals -> {"fdOrder == "<>ToString[fdOrder]}, *)
  Conditional -> fdOrder == fdOrderp,
  Where -> Interior,
  Equations ->
  {
    dot[phi] -> pi,
    dot[pi]  -> Euc[ui,uj] PD[fdOrderp][phi,li,lj]
  }
};

boundCalc = 
{
  Name -> "calc_bound_rhs",
  Schedule -> {"in MoL_RHSBoundaries", "at ANALYSIS"},
  Where -> Boundary,
  Equations ->
  {
    dot[phi] -> D[f[t+xCopy],t],
    dot[pi]  -> D[f[t+xCopy],t,t]
  }
};


copyCalc = 
{
  Name -> "copy_to_device",
  Schedule -> {"at INITIAL after initial_gaussian"},
  Where -> Everywhere,
  ExecuteOn -> Device,
  Equations ->
  {
    phi -> phi,
    pi -> pi
  }
};

intParameters = {{Name -> fdOrder, Default -> 2, AllowedValues -> fdOrders}};

CreateKrancThornTT[groups, ".", 
  If[version === CaKernel, "WaveCaKernel", "WaveHost"],
  Calculations -> {initialGaussianCalc, Sequence@@(evolveCalc/@fdOrders), boundCalc} ~Join~ If[version === CaKernel, {copyCalc}, {}],
  PartialDerivatives -> derivatives,
  UseCaKernel -> If[version === CaKernel, True, False],
  EvolutionTimelevels -> 2,
  DeclaredGroups -> {"phi_g","pi_g","xCopy_g"},
  IntParameters -> intParameters];

, {version, {CaKernel, Host}}];
