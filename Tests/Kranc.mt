
(* Mathematica Test File *)

$derivatives = {
  PDstandard2nd[i_]     -> StandardCenteredDifferenceOperator[1,1,i],
  PDstandard2nd[i_, i_] -> StandardCenteredDifferenceOperator[2,1,i]};

PD = PDstandard2nd;

$groups = {{"evolved_group", {phi, pi}}};

$initialSineCalc = {
  Name      -> "initial_sine",
  Schedule  -> {"AT initial"},
  Equations ->
  {
    phi -> Sin[2 Pi (x - t)],
    pi  -> -2 Pi Cos[2 Pi (x - t)]
  }};

$evolveCalc = {
  Name      -> "calc_rhs",
  Schedule  -> {"IN MoL_CalcRHS"},
  Where     -> Interior,
  Equations ->
  {
    dot[phi] -> pi,
    dot[pi]  -> Euc[ui,uj] PD[phi,li,lj]
  }};




(****************************************************************)
(* CreateThorn *)
(****************************************************************)

Test[
	CreateKrancThornTT[{}, "TestThorns", "CreateThorn", Calculations -> {}]
	,
	Null
	,
	TestID->"CreateThorn"
]

(****************************************************************)
(* IfThen *)
(****************************************************************)

(* Test that IfThen statements are only factored out into if
   statements if they don't contain grid functions, as this doesn't
   work with vectorisation. *)

(* The PD symbol is treated in a nonoptimal way by Kranc, and this
   causes problems for the tests.  This should be cleaned up. *)

Test[
  Module[
    {derivatives = {
      PDstandard2nd[i_]     -> StandardCenteredDifferenceOperator[1,1,i],
      PDstandard2nd[i_, i_] -> StandardCenteredDifferenceOperator[2,1,i]},
     groups = {{"evolved_group", {phi, pi}}},
     initialSineCalc = {
       Name      -> "initial_sine",
       Schedule  -> {"AT initial"},
       Equations ->
       {
         phi -> IfThen[x > 0, Sin[2 Pi (x - t)], 0],
         pi  -> IfThen[x > 0, -2 Pi Cos[2 Pi (x - t)], 0]
       }},
     evolveCalc = {
       Name      -> "calc_rhs",
       Schedule  -> {"IN MoL_CalcRHS"},
       Where     -> Interior,
       Equations ->
       {
         dot[phi] -> IfThen[alpha>0, pi, 2 pi],
         dot[pi]  -> IfThen[alpha>0, Euc[ui,uj] PD[phi,li,lj], 2 Euc[ui,uj] PD[phi,li,lj]]
       }}},

    (* PD = PDstandard2nd *)
    
    CreateKrancThornTT[
      groups, "TestThorns", 
      "IfThen", 
      PartialDerivatives -> derivatives,
      RealParameters     -> {alpha},
      DeclaredGroups     -> {"evolved_group"},
      Calculations       -> {initialSineCalc, evolveCalc}]];

  {StringMatchQ[Import["TestThorns/IfThen/src/calc_rhs.cc","Text"],
                 __~~"if (alpha > 0)"~~__],
   StringMatchQ[Import["TestThorns/IfThen/src/initial_sine.cc","Text"],
                __~~"IfThen(xL"~~Whitespace~~">"~~Whitespace~~"0"~~__]}
  ,
  {True, True}
  ,
  TestID -> "IfThen"
    ]

(****************************************************************)
(* SimpleWave *)
(****************************************************************)

Test[
  CreateKrancThornTT[
    $groups, "TestThorns", "SimpleWave",
    PartialDerivatives -> $derivatives,
    DeclaredGroups     -> {"evolved_group"},
    Calculations       -> {$initialSineCalc, $evolveCalc}]
  ,
  Null
  ,
  TestID->"SimpleWave"
]
