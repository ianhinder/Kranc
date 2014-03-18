#!/usr/bin/env MathematicaScript -script

SetOptions[ $Output, FormatType -> OutputForm ];
SetOptions["stdout", PageWidth -> Infinity];

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

alltests = FileBaseName/@FileNames["*.mt"];

args = Drop[$ScriptCommandLine, 1];

If[Length[args] > 0,
  tests = FileBaseName/@Select[args, StringMatchQ[#, "*.mt"] &],
  tests = alltests];

results = (Print["\n"]; TestRun[#<>".mt", Loggers -> {VerbosePrintLogger[]}, TestRunTitle -> #]) & /@ tests;
Print[];
Print[];
MapThread[Print[If[#2,"P","F"]," ",#1] &, {tests, results}];
Print[];

If[And@@results, Print["All tests passed"],
  Print["Tests failed in ", StringJoin@Riffle[Pick[tests, Not/@results],", "]]];

(* ReportFunctionCounts[]; *)
