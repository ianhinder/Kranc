<< "KrancThorn.m";

groups = {{"phi_g", {phi}}, {"pi_g", {pi}}};

derivatives =
{
  PDstandard2nd[i_] -> StandardCenteredDifferenceOperator[1,1,i],
  PDstandard2nd[i_, i_] -> StandardCenteredDifferenceOperator[2,1,i]
};

PD = PDstandard2nd;

initialSineCalc = 
{
  Name -> "initial_sine",
  Schedule -> {"AT INITIAL"},
  Equations -> 
  {
    phi -> Sin[2 Pi (x - t)],
    pi -> -2 Pi Cos[2 Pi (x - t)]
  }
};

f[x_] := Exp[-(x/0.1)^2];

initialGaussianCalc = 
{
  Name -> "initial_gaussian",
  Schedule -> {"AT INITIAL"},
  Equations -> 
  {
    phi -> f[t+x],
    pi -> D[f[t+x],t]
  }
};

evolveCalc = 
{
  Name -> "calc_rhs",
  Schedule -> {"at EVOL"},
  Where -> Interior,
  Equations ->
  {
    dot[phi] -> pi,
    dot[pi]  -> Euc[ui,uj] PD[phi,li,lj]
  }
};


integrateCalc = 
{
  Name -> "rk1",
  Schedule -> {"at EVOL after calc_rhs"},
  Where -> Interior,
  Equations ->
  {
    phi -> phi + dt dot[phi],
    pi -> pi + dt dot[pi]
  }
};

CreateKrancThornTT[groups, ".", 
  "SimpleWaveCaKernel", 
  Calculations -> {initialGaussianCalc, evolveCalc, integrateCalc},
  PartialDerivatives -> derivatives,
  UseCaKernel -> True,
  EvolutionTimelevels -> 1,
  DeclaredGroups -> {"phi_g","pi_g"}];
