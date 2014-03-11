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

DefineTensor /@ {S[ua], SS[la], T[ua], TT[la], u[ua], v[la], A, B, S2[ua,ub], SS2[la,lb],
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

test[makeSum[S[ua]], S[ua]];


test[makeSum[SS[la]], SS[la]];

test[makeSum[x S[ua]], x S[ua]];

test[makeSum[x S[ua] + T[ua]], x S[ua] + T[ua]];

test[makeSum[S[ua] TT[la]], S[1] TT[1] + S[2] TT[2] + S[3] TT[3]];

test[makeSum[u[ua] v[la]], u[1] v[1] + u[2] v[2] + u[3] v[3]];

test[makeSum[a S[ua] TT[la] + 1], a * (S[1] TT[1] + S[2] TT[2] + S[3] TT[3]) + 1];

test[makeSum[S[ua] TT[la] + u[ua] v[la]],
 S[1] TT[1] + S[2] TT[2] + S[3] TT[3] + u[1] v[1] + u[2] v[2] + u[3] v[3]];

test[makeSum[A * (S[ua] TT[la] + x) + B * (u[ua] v[la] + y)],
 A * (S[1] TT[1] + S[2] TT[2] + S[3] TT[3] + x) +
  B * (u[1] v[1] + u[2] v[2] + u[3] v[3] + y)];

test[makeSum[
  S[ua] TT[la] u[ub] v[lb]], (S[1] TT[1] + S[2] TT[2] + S[3] TT[3]) (u[1] v[1] +
    u[2] v[2] + u[3] v[3])];

test[makeSum[F[SS[la]]], F[SS[la]]];

test[makeSum[F[SS[la] T[ua]]], F[SS[1] T[1] + SS[2] T[2] + SS[3] T[3]]];

test[makeSum[F[G[SS[la] T[ua]]]], F[G[SS[1] T[1] + SS[2] T[2] + SS[3] T[3]]]];

test[makeSum[T[ua] F[SS[la]]], T[1] F[SS[1]] + T[2] F[SS[2]] + T[3] F[SS[3]]];

test[makeSum[Sqrt[TT[la] S[ua]]], Sqrt[S[1] TT[1] + S[2] TT[2] + S[3] TT[3]]];


(****************************************************************)
(* makeSplit *)
(****************************************************************)

makeSplit[x_] := Flatten[ComponentArray[x]];

test[makeSplit[SS[la]], {SS[1], SS[2], SS[3]}];

test[makeSplit[SS2[la, lb]], {SS2[1, 1], SS2[1, 2], SS2[1, 3], SS2[2, 1], SS2[2, 2],
  SS2[2, 3], SS2[3, 1], SS2[3, 2], SS2[3, 3]}];

test[makeSplit[SS[la] TT[lb]], {SS[1] TT[1], SS[1] TT[2], SS[1] TT[3],
  SS[2] TT[1],
  SS[2] TT[2], SS[2] TT[3], SS[3] TT[1], SS[3] TT[2], SS[3] TT[3]}];

test[makeSplit[SS[la] -> TT[la]], {SS[1] -> TT[1], SS[2] -> TT[2],
  SS[3] -> TT[3]}];

(****************************************************************)
(* MakeExplicit *)
(****************************************************************)

MakeExplicit[x_] := {ExpandComponents[x]};

test[MakeExplicit[SS[la] T[ua] v[lb]], (SS1 T1 + SS2 T2 + SS3 T3) {v1, v2, v3}];

test[MakeExplicit[SS[la] -> v[la]], {SS1 -> v1, SS2 -> v2, SS3 -> v3}];

test[MakeExplicit[
  A -> SS[la] IfThen[1, T[ua], v[ua]]],
  {A -> IfThen[1, SS1 T1 + SS2 T2 + SS3 T3, SS1 v1 + SS2 v2 + SS3 v3]}];

test[MakeExplicit[Sqrt[S[ua] TT[la]]], {Sqrt[S1 TT1 + S2 TT2 + S3 TT3]}];

test[MakeExplicit[Sqrt[u[ua] v[la]]], {Sqrt[u1 v1 + u2  v2 + u3 v3]}];

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

test[MakeExplicit[pd[u[la], lb]], {nd[u1, 1], nd[u1, 2], nd[u1, 3], nd[u2, 1],
   nd[u2, 2], nd[u2, 3], nd[u3, 1], nd[u3, 2], nd[u3, 3]}];

test[MakeExplicit[pd[u[ua], la]], {nd[u1, 1] + nd[u2, 2] + nd[u3, 3]}];

(* test[Simplify[MakeExplicit[pd[MatrixInverse[SS2[ua, ub]] SS2[lb, lc], ld]]], {0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0}];

test[FullSimplify[MakeExplicit[pd[MatrixInverse[SS2[ua, ub]], ld] SS2[lb, lc]] +
   MakeExplicit[MatrixInverse[SS2[ua, ub]] pd[SS2[lb, lc], ld]]],
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0}]; *)

reportResults[];

Quit[If[testsFailed > 0,1,0]]
