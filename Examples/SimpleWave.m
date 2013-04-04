<< "KrancThorn.m";

derivatives = {
  PDstandard2nd[i_]     -> StandardCenteredDifferenceOperator[1,1,i],
  PDstandard2nd[i_, i_] -> StandardCenteredDifferenceOperator[2,1,i]};

PD = PDstandard2nd;

groups = {{"evolved_group", {phi, pi}}};

initialSineCalc = {
  Name      -> "initial_sine",
  Schedule  -> {"AT initial"},
  Equations ->
  {
    phi -> Sin[2 Pi (x - t)],
    pi  -> -2 Pi Cos[2 Pi (x - t)]
  }};

evolveCalc = {
  Name      -> "calc_rhs",
  Schedule  -> {"IN MoL_CalcRHS"},
  Where     -> Interior,
  Equations ->
  {
    dot[phi] -> pi,
    dot[pi]  -> Euc[ui,uj] PD[phi,li,lj]
  }};

CreateKrancThornTT[
  groups, ".", 
  "SimpleWave", 
  PartialDerivatives -> derivatives,
  DeclaredGroups     -> {"evolved_group"},
  Calculations       -> {initialSineCalc, evolveCalc}];
