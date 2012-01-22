<< "KrancThorn.m";

groups = {{"ode_group", {a, b}}, {"evolved_group", {phi, pi}}};

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
    pi -> -2 Pi Cos[2 Pi (x - t)],
    a -> 0, b -> 1
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
    dot[pi]  -> a Euc[ui,uj] PD[phi,li,lj],
    dot[a] -> b,
    dot[b] -> -a
  }
};

CreateKrancThornTT[groups, ".", 
  "SimpleWaveODE", 
  Calculations -> {initialSineCalc, evolveCalc},
  PartialDerivatives -> derivatives,
  DeclaredGroups -> {"evolved_group"},
  ODEGroups -> {"ode_group"}
];

