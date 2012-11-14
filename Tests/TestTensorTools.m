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

Print["Loading tensortools"];
<< TensorTools`;

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

DefineTensor /@ {S, SS, T, TT, u, v, w, a, b};

(* We currently only test with correct input as it is CheckTensor's
   responsibility to check that the input is correct.  This should be
   changed. *)

Print["Running tests"];
Print[];

(****************************************************************)
(* makeSum *)
(****************************************************************)

makeSum = TensorTools`Private`makeSum

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

test[makeSum[a * (S[ua] TT[la] + x) + b * (u[ua] v[la] + y)],
 a * (S[1] TT[1] + S[2] TT[2] + S[3] TT[3] + x) +
  b * (u[1] v[1] + u[2] v[2] + u[3] v[3] + y)];

test[makeSum[
  S[ua] TT[la] u[ub] v[lb]], (S[1] TT[1] + S[2] TT[2] + S[3] TT[3]) (u[1] v[1] +
    u[2] v[2] + u[3] v[3])];

test[makeSum[f[SS[la]]], f[SS[la]]];

test[makeSum[f[SS[la] T[ua]]], f[SS[1] T[1] + SS[2] T[2] + SS[3] T[3]]];

test[makeSum[f[g[SS[la] T[ua]]]], f[g[SS[1] T[1] + SS[2] T[2] + SS[3] T[3]]]];

test[makeSum[T[ua] f[SS[la]]], T[1] f[SS[1]] + T[2] f[SS[2]] + T[3] f[SS[3]]];

test[makeSum[Sqrt[TT[la] S[ua]]], Sqrt[S[1] TT[1] + S[2] TT[2] + S[3] TT[3]]];

test[makeSum[IfThen[cond, TT[la] S[ua], 0]],
 IfThen[cond, S[1] TT[1] + S[2] TT[2] + S[3] TT[3], 0]];

test[makeSum[IfThen[cond, TT[la] S[ua], u[la] v[ua]]],
 IfThen[cond, S[1] TT[1] + S[2] TT[2] + S[3] TT[3],
  u[1] v[1] + u[2] v[2] + u[3] v[3]]];


(****************************************************************)
(* makeSplit *)
(****************************************************************)

makeSplit = TensorTools`Private`makeSplit;

test[makeSplit[SS[la]], {SS[1], SS[2], SS[3]}];

test[makeSplit[a[la, lb]], {a[1, 1], a[1, 2], a[1, 3], a[2, 1], a[2, 2],
  a[2, 3], a[3, 1], a[3, 2], a[3, 3]}];

test[makeSplit[SS[la] TT[lb]], {SS[1] TT[1], SS[1] TT[2], SS[1] TT[3],
  SS[2] TT[1],
  SS[2] TT[2], SS[2] TT[3], SS[3] TT[1], SS[3] TT[2], SS[3] TT[3]}];

test[makeSplit[SS[la] -> TT[la]], {SS[1] -> TT[1], SS[2] -> TT[2],
  SS[3] -> TT[3]}];

(****************************************************************)
(* MakeExplicit *)
(****************************************************************)

test[MakeExplicit[SS[la] T[ua] v[lb]], (SS1 T1 + SS2 T2 + SS3 T3) {v1, v2, v3}];

test[MakeExplicit[SS[la] -> v[la]], {SS1 -> v1, SS2 -> v2, SS3 -> v3}];

test[MakeExplicit[
  SS[la] IfThen[1, T[ua], v[ua]]], {SS1 IfThen[1, T1, v1] +
   SS2 IfThen[1, T2, v2] + SS3 IfThen[1, T3, v3]}];

test[MakeExplicit[Sqrt[S[ua] TT[la]]], {Sqrt[S1 TT1 + S2 TT2 + S3 TT3]}];

test[MakeExplicit[Sqrt[u[ua] v[la]]], {Sqrt[u1 v1 + u2  v2 + u3 v3]}];

(* This fails with 

   StringJoin::string: String expected at position 1 in StringJoin[1].

   which is probably an error in the error-detection or formatting
   code, since the input is not valid according to TensorTools (the
   tensor u has not been declared with two lower indices).

 *)

(* test[FullSimplify[MakeExplicit[MatrixInverse[u[ua, ub]] u[lb, lc]]], *)
(*  {1, 0, 0, 0, 1, 0, 0, 0, 1}]; *)

test[FullSimplify[MakeExplicit[MatrixInverse[a[ua, ub]] a[lb, lc]]],
 {1, 0, 0, 0, 1, 0, 0, 0, 1}];

(****************************************************************)
(* Partial derivatives *)
(****************************************************************)

test[MakeExplicit[PD[u[la], lb]], {PD[u1, 1], PD[u1, 2], PD[u1, 3], PD[u2, 1],
   PD[u2, 2], PD[u2, 3], PD[u3, 1], PD[u3, 2], PD[u3, 3]}];

test[MakeExplicit[PD[u[ua], la]], {PD[u1, 1], PD[u2, 2], PD[u3, 3]}];

test[Simplify[MakeExplicit[PD[MatrixInverse[a[ua, ub]] a[lb, lc], ld]]], {0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0}];

test[FullSimplify[MakeExplicit[PD[MatrixInverse[a[ua, ub]], ld] a[lb, lc]] +
   MakeExplicit[MatrixInverse[a[ua, ub]] PD[a[lb, lc], ld]]],
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 0}];

reportResults[];

Quit[If[testsFailed > 0,1,0]]
