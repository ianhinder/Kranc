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
    phi -> phi + dt dot[phi]
    pi -> pi + dt dot[pi]
  }
};

CreateKrancThornTT[groups, ".", 
  "SimpleWaveCaKernel", 
  Calculations -> {initialSineCalc, evolveCalc, integrateCalc},
  PartialDerivatives -> derivatives,
  UseCaKernel -> True,
  DeclaredGroups -> {"phi_g","pi_g"}];
