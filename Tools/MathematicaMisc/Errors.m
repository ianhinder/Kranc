
BeginPackage["Errors`"];

PrintError::usage = "";
ThrowError::usage = "";
KrancError::usage = "";
VerifyString;
VerifyStringList;
VerifyList;
InfoMessage;
SetDebugLevel;
ErrorDefinition::usage = "ErrorDefinition[f] creates a default definition of a function f which throws an exception.  This can be used to catch programming errors where f is called with incorrect arguments.";

DebugQuiet = 0;
Warnings = 1
Terse = 2;
Info = 3;
InfoFull = 4;
DefFn;

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
    If[StringLength[ToString[l] <> prefix] > 50,
      Print[prefix, "{"];
      Map[PrintStructure[#, "  " <> prefix, ","] &, l];
      Print[prefix, "}"],

      Print[prefix, ToString[l,InputForm]]]];

PrintStructure[s_, prefix_, suffix_] :=
  Print[prefix, s, suffix];

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


ThrowError[objects__] :=
  Module[{s = Stack[_], s2},
    
    s2 = removeBits[s];
    Throw[KrancError[{objects}(*,s2*)], KrancError]];


VerifyString[s_] := 
  If[! StringQ[s],
   ThrowError["Not a string:", s]];

VerifyStringList[l_] := 
  If[! MatchQ[l, {___String}],
   ThrowError["Not a list of strings:", l]];


VerifyList[l_] := 
  If[!Head[l] === List,
   ThrowError["Not a list:", l]];


InfoMessage[level_, message__] :=
  Module[{args = {message}},
    If[level <= debugLevel,
      Map[PrintStructure, args]];
  ];

SetDebugLevel[level_] :=
  debugLevel = level;

ErrorDefinition[x_] :=
  x[args___] :=
    ThrowError["Invalid arguments to "<>ToString[x], {args}//FullForm];

SetAttributes[DefFn, HoldAll];

DefFn[def:(fn_[args___] := body_)] :=
  Module[
    {},
    ErrorDefinition[fn];
    def];   

End[];

EndPackage[];

