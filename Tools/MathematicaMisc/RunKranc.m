
script = If[ValueQ[$KrancScript],$KrancScript,$CommandLine[[-1]]];
Kranc`KrancDirectory = If[ValueQ[$KrancDirectory], $KrancDirectory, Environment["KRANCDIR"]];

(* Uncomment the following to use xTensor in place of TensorTools *)
(* KrancTensor`$KrancTensorPackage="xTensor"; *)

$Path = Join[$Path,
  {Kranc`KrancDirectory <> "/Tools/CodeGen",
   Kranc`KrancDirectory <> "/Tools/MathematicaMisc",
   Kranc`KrancDirectory <> "/Tools/PirahaPeg"}];
Needs["Errors`"];
Needs["KrancThorn`"];
Needs["Profile`"];
Needs["KrancScript`"];
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

Kranc`Private`get[package_] :=
 Module[{contents},
  contents = Import[package, "HeldExpressions"];

  (* Warn about and replace PD *)
  If[KrancTensor`$KrancTensorPackage === "xTensor" && MemberQ[contents, PD, Infinity, Heads -> True],
    InfoMessage[DebugQuiet, "PD is a reserved symbol and its use in a calculation is deprecated"];
    contents = contents /. PD -> KrancPD;
  ];
  ReleaseHold[contents]
]

exception = Catch[CatchKrancError@Catch[
  Check[
    Block[
      {(*$RecursionLimit = Infinity*)},

      Switch[
        FileExtension[script],
        "m",      Kranc`Private`get[script],
        "kranc",  CreateThornFromKrancScript[script],
        _,        ThrowError["Unknown file extension for "<>script<>".  Recognised extensions are .m and .kranc."]]];

    None,
    ThrowError["Messages were generated - aborted"]]], _];

(* Catch non-Kranc exceptions *)
(* TODO: probably this should be removed *)
If[exception =!= None && exception =!= $Failed,
  Print["Exception:"];
  PrintError[exception];
  Quit[1],
  Quit[]];
