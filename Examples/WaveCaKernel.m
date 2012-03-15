<< "KrancThorn.m";

groups = {{"phi_g", {phi}}, {"pi_g", {pi}}, {"xCopy_g", {xCopy}}};

derivatives =
{
  PDstandard2nd[i_] -> StandardCenteredDifferenceOperator[1,1,i],
  PDstandard2nd[i_, i_] -> StandardCenteredDifferenceOperator[2,1,i]
};

PD = PDstandard2nd;

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

evolveCalc = 
{
  Name -> "calc_rhs",
  Schedule -> {"in MoL_CalcRHS"},
  Where -> Interior,
  Equations ->
  {
    dot[phi] -> pi,
    dot[pi]  -> Euc[ui,uj] PD[phi,li,lj]
  }
};

boundCalc = 
{
  Name -> "calc_bound_rhs",
  Schedule -> {"in MoL_RHSBoundaries"},
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

CreateKrancThornTT[groups, ".", 
  "SimpleWaveCaKernel", 
  Calculations -> {initialGaussianCalc, evolveCalc, copyCalc, boundCalc},
  PartialDerivatives -> derivatives,
  UseCaKernel -> True,
  EvolutionTimelevels -> 2,
  DeclaredGroups -> {"phi_g","pi_g","xCopy_g"}];
