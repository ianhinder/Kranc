#!/usr/bin/env math -script

SetOptions[ $Output, FormatType -> OutputForm ];

<< MUnitRunner`;

(****************************************************************)
(* Initialise *)
(****************************************************************)

Kranc`KrancDirectory = FileNameJoin[{Directory[],".."}];

$Path = Join[$Path,
  {Kranc`KrancDirectory <> "/Tools/CodeGen",
   Kranc`KrancDirectory <> "/Tools/MathematicaMisc",
   Kranc`KrancDirectory <> "/Tools/PirahaPeg"}];
Needs["Errors`"];
Needs["KrancThorn`"];
(* (\* Needs["Profile`"]; *\) *)

tests = {
		"Kranc"
};

(Print["\n"]; TestRun[#<>".mt", Loggers -> {VerbosePrintLogger[]}, TestRunTitle -> #]) & /@ tests;
Print[];
