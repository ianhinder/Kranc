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
    phi -> 0, (*Sin[2 Pi (x - t)],*)
    pi -> 0 (*-2 Pi Cos[2 Pi (x - t)] *)
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

CreateKrancThornTT[groups, ".", 
  "SimpleWaveCaKernel", 
  Calculations -> {initialSineCalc},
  PartialDerivatives -> derivatives,
  UseCaKernel -> True,
  DeclaredGroups -> {"phi_g","pi_g"}];
