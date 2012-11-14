
(* Run this script with:

     math -run 'Get["TestTensorTools.m"]; Quit[]'

*)


(* Initialise *)

$Path = Join[{"../Tools/CodeGen", "../Tools/MathematicaMisc"}, $Path];

Print["Loading tensortools"];
<< TensorTools`;

enhancedTimes = False;

SetEnhancedTimes[enhancedTimes];

(* Definitions *)

SetAttributes[test, HoldFirst]

test[t_, r1_] :=
 Module[{r},
  r = If[enhancedTimes, r1 /. Times -> TensorProduct, r1];
  If[Expand[Evaluate[t]] === Expand[r], testsPassed++; 
   Print["Input: ", HoldForm[InputForm@t]];
   Print["Result: ", Expand[r]//InputForm];
   Print["Pass"],
   (* else *)
   testsFailed++;
    Print["Input:", HoldForm@t];
    Print["Expected:", Expand[r], FullForm[Expand@r]];
    Print["Result:", Expand@Evaluate[t], FullForm[Expand@Evaluate[t]]];
    Print["Fail"]];
    Print[]];

reportResults[] :=
 If[testsFailed > 0, 
  Print[ToString[testsFailed] <> " test" <> 
    If[testsFailed > 1, "s", ""] <> " failed"],
  Print["All " <> ToString[testsPassed] <> " test" <> 
    If[testsPassed > 1, "s", ""] <> " passed"]];

(* Tests *)

testsPassed = 0; testsFailed = 0;

DefineTensor /@ {S, T, u, v, w};

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

(* test[makeSum[S[ua] T[la]], S[1] T[1] + S[2] T[2] + S[3] T[3]]; *)

(* test[makeSum[u[ua] v[la]], u[1] v[1] + u[2] v[2] + u[3] v[3]]; *)

(* test[makeSum[a S[ua] T[la] + 1], a (S[1] T[1] + S[2] T[2] + S[3] T[3]) + 1]; *)

(* test[makeSum[S[ua] T[la] + u[ua] v[la]],  *)
(*  S[1] T[1] + S[2] T[2] + S[3] T[3] + u[1] v[1] + u[2] v[2] + u[3] v[3]]; *)

(* test[makeSum[a (S[ua] T[la] + x) + b (u[ua] v[la] + y)],  *)
(*  a (S[1] T[1] + S[2] T[2] + S[3] T[3] + x) +  *)
(*   b (u[1] v[1] + u[2] v[2] + u[3] v[3] + y)]; *)

(* test[makeSum[ *)
(*   S[ua] T[la] u[ub] v[lb]], (S[1] T[1] + S[2] T[2] + S[3] T[3]) (u[1] v[1] +  *)
(*     u[2] v[2] + u[3] v[3])]; *)

(* test[makeSum[f[S[la]]], f[S[la]]]; *)

(* test[makeSum[f[S[la] T[ua]]], f[S[1] T[1] + S[2] T[2] + S[3] T[3]]]; *)

(* test[makeSum[f[g[S[la] T[ua]]]], f[g[S[1] T[1] + S[2] T[2] + S[3] T[3]]]]; *)

(* test[makeSum[T[ua] f[S[la]]], T[1] f[S[1]] + T[2] f[S[2]] + T[3] f[S[3]]]; *)

(* test[makeSum[Sqrt[T[la] S[ua]]], Sqrt[S[1] T[1] + S[2] T[2] + S[3] T[3]]]; *)

(* test[makeSum[IfThen[cond, T[la] S[ua], 0]],  *)
(*  IfThen[cond, S[1] T[1] + S[2] T[2] + S[3] T[3], 0]]; *)

(* test[makeSum[IfThen[cond, T[la] S[ua], u[la] v[ua]]],  *)
(*  IfThen[cond, S[1] T[1] + S[2] T[2] + S[3] T[3],  *)
(*   u[1] v[1] + u[2] v[2] + u[3] v[3]]]; *)


(* (\****************************************************************\) *)
(* (\* makeSplit *\) *)
(* (\****************************************************************\) *)

(* makeSplit = TensorTools`Private`makeSplit; *)

(* test[makeSplit[S[la]], {S[1], S[2], S[3]}]; *)

(* test[makeSplit[S[la, lb]], {S[1, 1], S[1, 2], S[1, 3], S[2, 1], S[2, 2],  *)
(*   S[2, 3], S[3, 1], S[3, 2], S[3, 3]}]; *)

(* test[makeSplit[S[la] T[lb]], {S[1] T[1], S[1] T[2], S[1] T[3], S[2] T[1],  *)
(*   S[2] T[2], S[2] T[3], S[3] T[1], S[3] T[2], S[3] T[3]}]; *)

(* test[makeSplit[S[la] -> T[la]], {S[1] -> T[1], S[2] -> T[2], S[3] -> T[3]}]; *)

(* (\****************************************************************\) *)
(* (\* MakeExplicit *\) *)
(* (\****************************************************************\) *)

(* test[MakeExplicit[S[la] T[ua] v[lb]], (S1 T1 + S2 T2 + S3 T3) {v1, v2, v3}]; *)

(* test[MakeExplicit[S[la] -> v[la]], {S1 -> v1, S2 -> v2, S3 -> v3}]; *)

(* test[MakeExplicit[ *)
(*   S[la] IfThen[1, T[ua], v[ua]]], {S1 IfThen[1, T1, v1] +  *)
(*    S2 IfThen[1, T2, v2] + S3 IfThen[1, T3, v3]}]; *)

(* test[MakeExplicit[Sqrt[S[ua] T[la]]], {Sqrt[S1 T1 + S2 T2 + S3 T3]}]; *)

(* test[MakeExplicit[Sqrt[u[ua] v[la]]], {Sqrt[u1 v1 + u2  v2 + u3 v3]}]; *)

(* test[FullSimplify[MakeExplicit[MatrixInverse[u[ua, ub]] u[lb, lc]]], *)
(*  {1, 0, 0, 0, 1, 0, 0, 0, 1}]; *)

(* (\****************************************************************\) *)
(* (\* Partial derivatives *\) *)
(* (\****************************************************************\) *)

(* test[MakeExplicit[PD[u[la], lb]], {PD[u1, 1], PD[u1, 2], PD[u1, 3], PD[u2, 1], *)
(*    PD[u2, 2], PD[u2, 3], PD[u3, 1], PD[u3, 2], PD[u3, 3]}]; *)

(* test[MakeExplicit[PD[u[ua], la]], {PD[u1, 1], PD[u2, 2], PD[u3, 3]}]; *)

(* test[Simplify[MakeExplicit[PD[MatrixInverse[u[ua, ub]] u[lb, lc], ld]]], {0,  *)
(*   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  *)
(*   0}]; *)

(* test[FullSimplify[MakeExplicit[PD[MatrixInverse[u[ua, ub]], ld] u[lb, lc]] + *)
(*    MakeExplicit[MatrixInverse[u[ua, ub]] PD[u[lb, lc], ld]]], *)
(*  {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  *)
(*   0, 0}]; *)

reportResults[];
