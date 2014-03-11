#!/usr/bin/env MathematicaScript -script

(* If MathematicaScript is not available on your path, you can run
   this script with

     MathKernel -run 'Get["TestTensorTools.m"]; Quit[]'

*)

(****************************************************************)
(* Initialise *)
(****************************************************************)

messageHandler = If[Last[#], Print["Messages generated"]; Quit[1]] &;
Internal`AddHandler["Message", messageHandler]

$Path = Join[{"../Tools/CodeGen", "../Tools/MathematicaMisc"}, $Path];
SetOptions["stdout", PageWidth -> Infinity];
SetOptions[$Output, FormatType -> OutputForm];

Print["Loading Kranc"];
<< xTensorKranc`;

Print["Arguments: ", $ScriptCommandLine];

enhancedTimes = False;

SetEnhancedTimes[enhancedTimes];

(****************************************************************)
(* Definitions *)
(****************************************************************)

SetAttributes[test, HoldFirst]

test[t_, r1_] :=
  Module[
    {r},
    Print["Input: ", HoldForm[InputForm@t]];
    r = If[enhancedTimes, r1 /. Times -> TensorProduct, r1];
    Print["Expected: ", Expand@r//InputForm];
    If[Expand[Evaluate[t]] === Expand[r],
       testsPassed++; 
       Print["Result: ", Expand[r]//InputForm];
       Print["Pass"],
       (* else *)
       testsFailed++;
       Print["Result: ", Expand@Evaluate[t]//InputForm];
       Print["Fail"]];
    Print[]];

count[num_Integer, noun_String] :=
  ToString[num] <> " " <> noun <> If[num === 1, "", "s"];

reportResults[] :=
  Module[
    {},
    Print[count[testsPassed, "test"], " passed"];
    Print[count[testsFailed, "test"], " failed"]];

(****************************************************************)
(* Tests *)
(****************************************************************)

testsPassed = 0; testsFailed = 0;

DefineTensor /@ {A, B, Su[ua], Sd[la], Tu[ua], Td[la], Uu[ua], Ud[la],
   Tuu[ua,ub], Tud[ua,lb], Tdu[la,ub], Tdd[la,lb]};

DefInertHead /@ {F, G};

DefineDerivative[pd, nd];

(* We currently only test with correct input as it is CheckTensor's
   responsibility to check that the input is correct.  This should be
   changed. *)

Print["Running tests"];
Print[];

(****************************************************************)
(* makeSum *)
(****************************************************************)

makeSum = TraceBasisDummy;

test[makeSum[1], 1];

test[makeSum[x], x];

test[makeSum[x^2], x^2];

test[makeSum[x^2 + 1], 1 + x^2];

test[makeSum[Su[ua]], Su[ua]];


test[makeSum[Sd[la]], Sd[la]];

test[makeSum[x Su[ua]], x Su[ua]];

test[makeSum[x Su[ua] + Tu[ua]], x Su[ua] + Tu[ua]];

test[makeSum[Su[ua] Td[la]], Su[1] Td[1] + Su[2] Td[2] + Su[3] Td[3]];

test[makeSum[Uu[ua] Ud[la]], Uu[1] Ud[1] + Uu[2] Ud[2] + Uu[3] Ud[3]];

test[makeSum[a Su[ua] Td[la] + 1], a * (Su[1] Td[1] + Su[2] Td[2] + Su[3] Td[3]) + 1];

test[makeSum[Su[ua] Td[la] + Uu[ua] Ud[la]],
 Su[1] Td[1] + Su[2] Td[2] + Su[3] Td[3] + Uu[1] Ud[1] + Uu[2] Ud[2] + Uu[3] Ud[3]];

test[makeSum[A * (Su[ua] Td[la] + x) + B * (Uu[ua] Ud[la] + y)],
 A * (Su[1] Td[1] + Su[2] Td[2] + Su[3] Td[3] + x) +
  B * (Uu[1] Ud[1] + Uu[2] Ud[2] + Uu[3] Ud[3] + y)];

test[makeSum[
  Su[ua] Td[la] Uu[ub] Ud[lb]], (Su[1] Td[1] + Su[2] Td[2] + Su[3] Td[3]) (Uu[1] Ud[1] +
    Uu[2] Ud[2] + Uu[3] Ud[3])];

test[makeSum[F[Sd[la]]], F[Sd[la]]];

test[makeSum[F[Sd[la] Tu[ua]]], F[Sd[1] Tu[1] + Sd[2] Tu[2] + Sd[3] Tu[3]]];

test[makeSum[F[G[Sd[la] Tu[ua]]]], F[G[Sd[1] Tu[1] + Sd[2] Tu[2] + Sd[3] Tu[3]]]];

test[makeSum[Tu[ua] F[Sd[la]]], Tu[1] F[Sd[1]] + Tu[2] F[Sd[2]] + Tu[3] F[Sd[3]]];

test[makeSum[Sqrt[Td[la] Su[ua]]], Sqrt[Su[1] Td[1] + Su[2] Td[2] + Su[3] Td[3]]];


(****************************************************************)
(* makeSplit *)
(****************************************************************)

makeSplit[x_] := Flatten[ComponentArray[x]];

test[makeSplit[Sd[la]], {Sd[1], Sd[2], Sd[3]}];

test[makeSplit[Tdd[la, lb]], {Tdd[1, 1], Tdd[1, 2], Tdd[1, 3], Tdd[2, 1], Tdd[2, 2],
  Tdd[2, 3], Tdd[3, 1], Tdd[3, 2], Tdd[3, 3]}];

test[makeSplit[Sd[la] Td[lb]], {Sd[1] Td[1], Sd[1] Td[2], Sd[1] Td[3],
  Sd[2] Td[1],
  Sd[2] Td[2], Sd[2] Td[3], Sd[3] Td[1], Sd[3] Td[2], Sd[3] Td[3]}];

test[makeSplit[Sd[la] -> Td[la]], {Sd[1] -> Td[1], Sd[2] -> Td[2],
  Sd[3] -> Td[3]}];

(****************************************************************)
(* MakeExplicit *)
(****************************************************************)

MakeExplicit[x_] := {ExpandComponents[x]};

test[MakeExplicit[Sd[la] Tu[ua] Ud[lb]], (Sd1 Tu1 + Sd2 Tu2 + Sd3 Tu3) {Ud1, Ud2, Ud3}];

test[MakeExplicit[Sd[la] -> Ud[la]], {Sd1 -> Ud1, Sd2 -> Ud2, Sd3 -> Ud3}];

test[MakeExplicit[
  A -> Sd[la] IfThen[1, Tu[ua], Uu[ua]]],
  {A -> IfThen[1, Sd1 Tu1 + Sd2 Tu2 + Sd3 Tu3, Sd1 Uu1 + Sd2 Uu2 + Sd3 Uu3]}];

test[MakeExplicit[Sqrt[Su[ua] Td[la]]], {Sqrt[Su1 Td1 + Su2 Td2 + Su3 Td3]}];

test[MakeExplicit[Sqrt[Uu[ua] Ud[la]]], {Sqrt[Uu1 Ud1 + Uu2  Ud2 + Uu3 Ud3]}];

test[FullSimplify[MakeExplicit[MatrixInverse[Tuu[la, lb]] Tuu[ub, uc]]],
 {1, 0, 0, 0, 1, 0, 0, 0, 1}];

test[FullSimplify[MakeExplicit[MatrixInverse[Tuu[lb, la]] Tuu[uc, ub]]],
 {1, 0, 0, 0, 1, 0, 0, 0, 1}];

test[FullSimplify[MakeExplicit[MatrixInverse[Tud[ua, lb]] Tud[ub, lc]]],
 {1, 0, 0, 0, 1, 0, 0, 0, 1}];

test[FullSimplify[MakeExplicit[MatrixInverse[Tud[ub, la]] Tud[uc, lb]]],
 {1, 0, 0, 0, 1, 0, 0, 0, 1}];

test[FullSimplify[MakeExplicit[MatrixInverse[Tdu[la, ub]] Tdu[lb, uc]]],
 {1, 0, 0, 0, 1, 0, 0, 0, 1}];

test[FullSimplify[MakeExplicit[MatrixInverse[Tdu[lb, ua]] Tdu[lc, ub]]],
 {1, 0, 0, 0, 1, 0, 0, 0, 1}];

test[FullSimplify[MakeExplicit[MatrixInverse[Tdd[ua, ub]] Tdd[lb, lc]]],
 {1, 0, 0, 0, 1, 0, 0, 0, 1}];

test[FullSimplify[MakeExplicit[MatrixInverse[Tdd[ub, ua]] Tdd[lc, lb]]],
 {1, 0, 0, 0, 1, 0, 0, 0, 1}];

(****************************************************************)
(* Partial derivatives *)
(****************************************************************)

test[MakeExplicit[pd[Ud[la], lb]], {nd[Ud1, 1], nd[Ud1, 2], nd[Ud1, 3], nd[Ud2, 1],
   nd[Ud2, 2], nd[Ud2, 3], nd[Ud3, 1], nd[Ud3, 2], nd[Ud3, 3]}];

test[MakeExplicit[pd[Uu[ua], la]], {nd[Uu1, 1] + nd[Uu2, 2] + nd[Uu3, 3]}];

(* test[Simplify[MakeExplicit[pd[MatrixInverse[Tdd[ua, ub]] Tdd[lb, lc], ld]]], {0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0}];

test[FullSimplify[MakeExplicit[pd[MatrixInverse[Tdd[ua, ub]], ld] Tdd[lb, lc]] +
   MakeExplicit[MatrixInverse[Tdd[ua, ub]] pd[Tdd[lb, lc], ld]]],
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0}]; *)

reportResults[];

Quit[If[testsFailed > 0,1,0]]
