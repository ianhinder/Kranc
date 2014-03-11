#!/usr/bin/env MathematicaScript -script

SetOptions[ $Output, FormatType -> OutputForm ];

<< MUnitRunner`;

(****************************************************************)
(* Initialise *)
(****************************************************************)

Kranc`KrancDirectory = FileNameJoin[{Directory[],".."}];

$TestThornDirectory = "TestThorns/thorns";

$Path = Join[$Path,
  {Kranc`KrancDirectory <> "/Tools/CodeGen",
   Kranc`KrancDirectory <> "/Tools/MathematicaMisc",
   Kranc`KrancDirectory <> "/Tools/PirahaPeg"}];
Needs["Errors`"];
Needs["KrancThorn`"];
(* (\* Needs["Profile`"]; *\) *)

(* SetDebugLevel[DebugQuiet]; *)

alltests = {
  "CodeGen",
  "Kranc",
  "McLachlan",
  "Tiling"
};

args = Drop[$ScriptCommandLine, 1];

If[Length[args] > 0,
   If[StringMatchQ[args[[1]], "*.mt"],
      tests = {StringReplace[args[[1]], ".mt" -> ""]}],
   tests = alltests];

(Print["\n"]; TestRun[#<>".mt", Loggers -> {VerbosePrintLogger[]}, TestRunTitle -> #]) & /@ tests;
Print[];

(* ReportFunctionCounts[]; *)
