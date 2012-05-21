
(*  Copyright 2012 Ian Hinder

    This file is part of Kranc.

    Kranc is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Kranc is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Kranc; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

BeginPackage["ParamCheck`", {"Errors`", "Helpers`", "Kranc`", "CodeGenCactus`",
                             "CodeGenC`", "CodeGen`"}];

ParameterCheckSource;
ParameterCheckSchedule;

Begin["`Private`"];

DefFn[
  ParameterCheckSource[thornName_String, conditions_List] :=
  {Map[IncludeFile, 
       {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],

   DefineCCTKFunction[
     thornName, "void", 
     ConditionalOnParameterTextual[
       "CCTK_MyProc(cctkGH) == 0",
       Map[checkCondition, conditions]]]}];

DefFn[
  ParameterCheckSchedule[thornName_String] :=
  {Name          -> thornName<>"_ParamCheck",
   SchedulePoint -> "at CCTK_PARAMCHECK",
   Options       -> "global",
   Language      -> "C", 
   Comment       -> "Check parameter consistency"}];

DefFn[checkCondition[{cond_, error_String}] :=
  Module[
    {render, renderbool, paramPattern},

    paramPattern = Except[True | False, _Symbol | _Parameter];

    renderbool[Equal[a:paramPattern,b_String]] := {"CCTK_EQUALS(", rendervalue[a], ",\"", b,"\")"};
    renderbool[Unequal[a:paramPattern,b_String]] := {"!CCTK_EQUALS(", rendervalue[a], ",\"", b,"\")"};
    renderbool[Equal[a:paramPattern,b_?NumberQ]] := {rendervalue[a], " == ", rendervalue[b]};
    renderbool[Unequal[a:paramPattern,b_?NumberQ]] := {rendervalue[a], " != ", rendervalue[b]};

    renderbool[Or[a_,b_]] := {"(",renderbool[a]," || ", renderbool[b],")"};
    renderbool[And[a_,b_]] := {"(",renderbool[a]," && ", renderbool[b],")"};
    renderbool[Not[a_]] := {"(!", renderbool[a],")"};
    renderbool[a:paramPattern] := ToString[a/.(Parameter[x_]->x)]; (* Boolean parameter *)

    (* rendervalue[a_String] := a; -- Allow literal pass-through *)
    rendervalue[a_?NumberQ] := ToString[a];
    rendervalue[Parameter[a_String]] := a;
    rendervalue[a_ /; MemberQ[params,a]] := ToString[a];
    renderbool[x_] := ThrowError["Unexpected value in run-time conditional expression (boolean):", x, "in", cond];
    render[x_] := ThrowError["Unexpected value in run-time conditional expression (value):", x, "in", cond];

    unparen[s_] := 
    Module[
      {s2 = FlattenBlock[s],result},
      result = StringReplace[FlattenBlock[s2],StartOfString ~~ "(" ~~ any__ ~~ ")" ~~ EndOfString :> any];
      If[result === s2, result, unparen[result]]];

    ConditionalOnParameterTextual[
      unparen@renderbool[cond],
      {"CCTK_WARN(0, ", StringDrop[Stringify[error],-1], ");\n"}]]];

End[];

EndPackage[];
