
BeginPackage["Errors`", {"Profile`", "Stack`"}];

PrintError::usage = "";
ThrowError::usage = "";
KrancError::usage = "";
VerifyString;
VerifyStringList;
VerifyList;
InfoMessage;
SetDebugLevel;
ErrorDefinition::usage = "ErrorDefinition[f] creates a default definition of a function f which throws an exception.  This can be used to catch programming errors where f is called with incorrect arguments.";
PrintError;
PrintStructure;

DebugQuiet = 0;
Warnings = 1
Terse = 2;
Info = 3;
InfoFull = 4;
DefFn;
CatchKrancError;

Begin["`Private`"];

debugLevel = Terse;

removeBits[l_] := 
  Module[{s, t},

    t = Select[l, ! (MatchQ[#, HoldForm[Module[___]]] || 
                           MatchQ[#, HoldForm[CompoundExpression[___]]]) &];
    s = If[Length[t] != 0, Drop[t, -1], t];

    Map[# /. HoldForm[h_[args___]] :> h &, s];
    s];

PrintStructure[x_]:=
  PrintStructure[x, "", ""];

PrintStructure[l_List, prefix_, suffix_] :=
  Module[{},
    If[StringLength[ToString[l,InputForm] <> prefix] > 50,
      Print[prefix, "{"];
      Map[PrintStructure[#, "  " <> prefix, ","] &, l];
      Print[prefix, "}"],

      Print[prefix, If[Head[l]===FullForm,ToString[l],ToString[l,InputForm]]]]];

PrintStructure[s_, prefix_, suffix_] :=
  Print[prefix, If[Head[s]===FullForm,ToString[s],ToString[s,InputForm]], suffix];

PrintError[err_] :=
  Module[{},
      If[Head[err] === KrancError,
        Module[{},
          objs = err[[1]];

          Map[PrintStructure, objs];
(*          Print["Error stack:"];
          PrintStructure[stack]*)
],
        err]];

(* Raise an error exception.  Typically, this should be called with
one argument, the string containing the error message.  For backward
compatibility, it can be called with several arguments.  These will be
converted to strings in InputForm and displayed on separate lines. *)
ThrowError[objects__] :=
  Module[{stack = CurrentStack[]},
    Throw[KrancError[{objects},stack], KrancError]];

VerifyString[s_] := 
  If[! StringQ[s],
   ThrowError["Not a string:", s]];

VerifyStringList[l_, err_:None] := 
  If[! MatchQ[l, {___String}],
   ThrowError[If[err===None,"",ToString[err]<>" - "]<>"Not a list of strings:", l]];


VerifyList[l_] := 
  If[!Head[l] === List,
   ThrowError["Not a list:", l]];


InfoMessage[level_, message__] :=
  Module[{args = {message}},
    If[level <= debugLevel,
      Map[Print, args]];
  ];

SetDebugLevel[level_] :=
  debugLevel = level;

ErrorDefinition[x_] :=
  x[args___] :=
    ThrowError["Invalid arguments: ",
      ToString[x] <> "[" <> StringJoin[Riffle[(ToString[FullForm[#]]) & /@ {args},","]] <> "]"];

SetAttributes[DefFn, HoldAll];

DefFn[def:(fn_[args___] := body_)] :=
  Module[
    {},
    ErrorDefinition[fn];
    fn[args] :=
    WithStackFrame[fn, body]];

reportError[k:KrancError[objects_,stack_], KrancError] :=
  Module[{},
    If[MatchQ[objects, {_String}],
      Print["Error: ", objects[[1]]],
      Scan[Print, InputForm/@objects]];
    ShowStack[stack];
    $Failed];

SetAttributes[CatchKrancError, HoldAll];
CatchKrancError[x_] :=
  Catch[x, KrancError, reportError];

End[];

EndPackage[];

