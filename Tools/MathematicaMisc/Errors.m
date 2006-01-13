
BeginPackage["Errors`"];

PrintError::usage = "";
ThrowError::usage = "";
KrancError::usage = "";

Begin["`Private`"];

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
    Print[prefix, "{"];
    Map[PrintStructure[#, "  " <> prefix, ","] &, l];
    Print[prefix, "}"]];

PrintStructure[s_, prefix_, suffix_] :=
  Print[prefix, s, suffix];

PrintError[err_] :=
  Module[{},
      If[Head[err] === KrancError,
        Module[{},
          objs = err[[1]];
          stack = err[[2]];

          Map[PrintStructure, objs];
(*          Print["Error stack:"];
          PrintStructure[stack]*)
],
        err]];


ThrowError[objects__] :=
  Module[{s = Stack[_], s2},
    
    s2 = removeBits[s];
    Throw[KrancError[{objects}, s2], KrancError]];

End[];

EndPackage[];

