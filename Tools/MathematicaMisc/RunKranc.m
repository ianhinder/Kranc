
script = $CommandLine[[-1]];
krancDir = Environment["KRANCDIR"];

$Path = Join[$Path,
  {krancDir <> "/Tools/CodeGen",
   krancDir <> "/Tools/MathematicaMisc",
   krancDir <> "/Tools/External"}];
Needs["Errors`"];
Needs["KrancThorn`"];
If[Environment["KRANCVERBOSE"] == "yes",
  SetDebugLevel[InfoFull]];

exception = Catch[Get[script], KrancError];
PrintError[exception];

Quit[];
