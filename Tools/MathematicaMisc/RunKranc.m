
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
 Module[{contents, tensors, tensorsWithIndices, replacements},
  contents = Import[package, "HeldExpressions"];

  (* Warn about and replace PD *)
  If[MemberQ[contents, PD, Infinity, Heads -> True],
    InfoMessage[Warnings, "PD is a reserved symbol and its use in a calculation is deprecated"];
    contents = contents /. PD -> KrancPD;
  ];

  (* Replace DefineTensor *)
  
  (* Find all cases where DefineTensor is used. Ignore cases where a second argument is given
     as the second argument would include indices. *)
  tensors = Cases[contents, HoldPattern[DefineTensor[t_]] :> t, Infinity, Heads -> True];
  AppendTo[tensors, Cases[contents, HoldPattern[Map[DefineTensor, l_]] :> l, Infinity, Heads -> True]];
  (* AppendTo[tensors, Cases[contents, HoldPattern[Map[DefineTensor[#, __] &, l_]] :> l, Infinity, Heads -> True]]; *)
  tensors = Flatten[tensors];
  
  (* We only care about cases where indices were not explicitly included *)
  tensors = Map[(# /. t_[___] :> Sequence[] )&, tensors];
  
  (* Build up a list of replacement rules to replace a tensor without indices with one with
     the appropriate indices. We look at the rest of the script to determine the appropriate
     indices *)
  tensorsWithIndices =
    Map[Function[{t}, Module[{res},
          res = Cases[contents /. MatrixInverse[t[i_, j_]] :> t[-j,-i], t[i__] :> {i}, Infinity];
          res = res /. {- _?AbstractIndexQ -> - TangentKrancManifold, _?AbstractIndexQ -> TangentKrancManifold};
          res = DeleteDuplicates[res];
          t @@@ res]],
        tensors];
  tensorsWithIndices = Map[
    Function[{l}, Module[{ret},
      Which[
        Length[l] === 0, ret = {};,
        Length[l] > 1,
          ThrowError["Unable to determine indices of tensor defined without indices, " <>
                     "possible candidates are: ", l];,
        True, ret = First[l];
      ];
      ret
    ]], tensorsWithIndices];

  replacements = MapThread[Rule, {tensors, tensorsWithIndices}] /. (_ -> {}) -> Sequence[];
  replacements = replacements /. {-TangentKrancManifold :> DummyIn[-TangentKrancManifold], TangentKrancManifold :> DummyIn[TangentKrancManifold]};

  If[replacements =!= {},
    InfoMessage[Warnings, "Found tensors in DefineTensor without their indices specified explicitly."];
    InfoMessage[Info, "Assuming they are given by: ", replacements];
  ];
  ReleaseHold[contents /.
    {
     HoldPattern[DefineTensor[t_]] :> DefineTensor[t /. replacements],
     HoldPattern[Map[DefineTensor, l_]] :> Map[DefineTensor, l /. replacements]
     (* HoldPattern[Map[DefineTensor[#, x__]&, l_]] :> Map[DefineTensor[#, x]&, l /. replacements] *)
    }
  ]
]

Kranc`Private`loadKrancInput[script_] :=
  Switch[
    FileExtension[script],
    "m",      If[KrancTensor`$KrancTensorPackage === "xTensor",
                     Kranc`Private`get[script],
                     Get[script]],
    "kranc",  CreateThornFromKrancScript[script],
    _,        ThrowError["Unknown file extension for "<>script<>
      ".  Recognised extensions are .m and .kranc."]];

exception =
  CatchKrancError[
    Check[
      Kranc`Private`loadKrancInput[script]; None,
      ThrowError["Messages were generated"]]];

If[exception =!= None,
  Quit[1],
  Quit[0]];
