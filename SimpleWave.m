
<< "KrancThorn.m";

groups = {{"evolved_group", {phi, pi}}};

derivatives = {
  PDstandard2nd[i_] -> StandardCenteredDifferenceOperator[1,1,i],
  PDstandard2nd[i_, i_] -> StandardCenteredDifferenceOperator[2,1,i]
};

PD = PDstandard2nd;

initialSineCalc = {
  Name -> "initial_sine",
  Schedule -> {"AT INITIAL"},
  Equations -> 
  {
    phi -> Sin[2 Pi (x - t)],
    pi -> -2 Pi Cos[2 Pi (x - t)]
  }
};

evolveCalc = {
  Name -> "calc_rhs",
  Schedule -> {"in MoL_CalcRHS"},
  Equations ->
  {
    dot[phi] -> pi,
    dot[pi]  -> Euc[ui,uj] PD[phi,li,lj]
  }
};

CreateKrancThornTT[groups, ".", 
  "SimpleWave", 
  Calculations -> {initialSineCalc, evolveCalc},
  PartialDerivatives -> derivatives,
  DeclaredGroups -> {"evolved_group"}];
