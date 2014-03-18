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

runTest[filename_String] :=
  Module[{globalBefore = Select[Names["Global`*"],StringFreeQ[#,"$"]&], globalAfter,
    globalNew, result, contextBefore, contextAfter},
    Print["\n"];
    contextBefore = $Context;
    result = TestRun[filename<>".mt", Loggers -> {VerbosePrintLogger[]}, TestRunTitle -> filename];
    contextAfter = $Context;
    (* Test failures cause the final End[] in a test file to not be evaluated *)
    If[contextAfter =!= contextBefore, $Context = contextBefore];
    globalAfter = Select[Names["Global`*"],StringFreeQ[#,"$"]&];
    globalNew = Complement[globalAfter, globalBefore];
    (* If[globalNew =!= {}, Print[]; Print[]; Print[filename, ".mt: Symbols leaked into Global` context: ", globalNew//InputForm]]; *)
    result];

results = runTest /@ tests;
Print[];
Print[];
Print["Test files run:"];
MapThread[Print[If[#2," ","F"]," ",#1] &, {tests, results}];
Print[];

If[And@@results, Print["All tests passed"],
  Print["Tests failed in ", StringJoin@Riffle[Pick[tests, Not/@results],", "]]];

(* ReportFunctionCounts[]; *)
