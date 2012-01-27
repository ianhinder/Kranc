<< "KrancThorn.m";

groups = {{"phi_g", {phi}}};

derivatives =
{
  PDstandard[1] -> DPlus[1]
};

PD = PDstandard;

f[x_] := Exp[-(x/0.1)^2];

initialGaussianCalc = 
{
  Name -> "initial_gaussian",
  Schedule -> {"AT INITIAL"},
  Equations -> 
  {
    phi -> f[t+x]
  }
};

evolveCalc = 
{
  Name -> "calc_rhs",
  Schedule -> {"at EVOL"},
  Where -> Interior,
  Equations ->
  {
    dot[phi] -> PD[phi,1]
  }
};


integrateCalc = 
{
  Name -> "rk1",
  Schedule -> {"at EVOL after calc_rhs"},
  Where -> Interior,
  Equations ->
  {
    phi -> phi + dt dot[phi]
  }
};

CreateKrancThornTT[groups, ".", 
  "AdvectCaKernel", 
  Calculations -> {initialGaussianCalc(* , evolveCalc, integrateCalc *)},
  PartialDerivatives -> derivatives,
  UseCaKernel -> True,
  EvolutionTimelevels -> 1,
  DeclaredGroups -> {"phi_g"}];
