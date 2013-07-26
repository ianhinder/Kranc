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
    phi -> IfThen[x > 0, Sin[2 Pi (x - t)], 0],
    pi  -> IfThen[x > 0, -2 Pi Cos[2 Pi (x - t)], 0]
  }};

evolveCalc = {
  Name      -> "calc_rhs",
  Schedule  -> {"IN MoL_CalcRHS"},
  Where     -> Interior,
  Equations ->
  {
    dot[phi] -> IfThen[alpha>0, pi, 2 pi],
    dot[pi]  -> IfThen[alpha>0, Euc[ui,uj] PD[phi,li,lj], 2 Euc[ui,uj] PD[phi,li,lj]]
  }};

CreateKrancThornTT[
  groups, ".", 
  "IfThenTest", 
  PartialDerivatives -> derivatives,
  RealParameters     -> {alpha},
  DeclaredGroups     -> {"evolved_group"},
  Calculations       -> {initialSineCalc, evolveCalc}];

failures = 0;

If[!StringMatchQ[Import["IfThenTest/src/calc_rhs.cc","Text"],
                 __~~"if (alpha > 0.)"~~__],
   failures++;
   Print["Test failed"]];

If[!StringMatchQ[Import["IfThenTest/src/initial_sine.cc","Text"],
                 __~~"IfThen(xL"~~Whitespace~~">"~~Whitespace~~"0."~~__],
   failures++;
   Print["Test failed"]];

Quit[failures]
