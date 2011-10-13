
script = $CommandLine[[-1]];
krancDir = Environment["KRANCDIR"];

$Path = Join[$Path,
  {krancDir <> "/Tools/CodeGen",
   krancDir <> "/Tools/MathematicaMisc",
   krancDir <> "/Tools/External"}];
Needs["Errors`"];
Needs["KrancThorn`"];
Needs["Profile`"];
If[Environment["KRANCVERBOSE"] == "yes",
  SetDebugLevel[InfoFull]];

SetOptions["stdout", PageWidth -> Infinity];

(* I have not found a good way to abort on the first generated
   message.  All the attempts below are triggered on messages which
   have been Quieted and I don't know a way to silently skip these.
   Note that some built-in Mathematica functions seem to Quiet
   messages internally, so even though no Quieting is done in Kranc,
   such messages still cause an abort with the below methods.
   Instead, we let the computation finish after messages have been
   generated, and then use Check to throw an exception. *)

(* ThrowMessage[text_, id1_, pat:_[_[id_, args___]]] := *)
(*   Module[{}, *)
(*     Print["text = ", text]; *)
(*     Print[StringForm[text,args], "Aborting due to message"]]; *)

(* ThrowMessage2[args___] := *)
(*   Module[{}, *)
(*     Print["args = ", {args}]; *)
(*     Quit[]]; *)

(* exception = Catch[Catch[ *)
(* Internal`HandlerBlock[ *)
(*  (\* See http://groups.google.com/group/comp.soft-sys.math.mathematica/browse_thread/thread/6314f05952ff5028 *\) *)
(*  {"MessageTextFilter", ThrowMessage[#1,#2,#3] &}, *)
(*   Get[script];None]], _]; *)

(* exception = Catch[Catch[ *)
(* Internal`HandlerBlock[ *)
(*  (\* See http://groups.google.com/group/comp.soft-sys.math.mathematica/browse_thread/thread/6314f05952ff5028 *\) *)
(*  {"Message", Replace[#, _[_, True] :> ThrowMessage2[#]] &},  *)
(*   Get[script];None]], _]; *)

(* Unprotect[Message]; *)

(* $AbortMessage = True; *)

(* Message[args___] := *)
(*   Block[{$AbortMessage = False}, *)
(*      If[{args}[[1]] =!= $Off[],Print["Message: ", args]; *)
(*      Message[args]]] /; $AbortMessage *)
    
(* Protect[Message]; *)

(* Quiet[Message[InverseFunction::ifun]]; *)

exception = Catch[Catch[
  Check[
    Block[
      {$RecursionLimit = Infinity},
      (*{result,timers} =  GetTimers[ *) Get[script](*]*)];

    (* Put[timers, "timer-output-1.m"]; *)

    (* timers = CoalesceTimers[timers]; *)
    (* Put[timers, "timer-output-2.m"]; *)

    (* timers = ThresholdTimers[timers,0.1]; *)
    (* Put[timers, "timer-output-3.m"]; *)

    (* (\* Put[timers2, "timer-output-2.m"]; *\) *)

    (* PrintTimerTree[timers]; *)

    None,
    ThrowError["Messages were generated - aborted"]]], _];

If[exception =!= None,
  Print["Exception:"];
  PrintError[exception];
  Quit[1],
  Quit[]];
