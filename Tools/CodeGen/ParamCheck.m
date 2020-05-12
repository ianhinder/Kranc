
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
                             "CodeGenC`", "CodeGen`", "CodeGenKranc`", "Code`", "Object`"}];

ParameterCheckSource;
ParameterCheckSchedule;
ParamCheckProcessCode;

Begin["`Private`"];

DefFn[
  ParameterCheckSource[thornName_String, conditions_List] :=
  {Map[IncludeFile, 
       {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],

   DefineCCTKFunction[
     thornName<>"_ParamCheck", "void", 
     ConditionalOnParameterTextual[
       "CCTK_MyProc(cctkGH) == 0",
       Map[checkCondition, conditions]]]}];

DefFn[
  ParameterCheckSchedule[thornName_String] :=
  {Name          -> thornName<>"_ParamCheck",
   SchedulePoint -> "at PARAMCHECK",
   Options       -> "global",
   Language      -> "C", 
   Comment       -> "Check parameter consistency"}];

DefFn[checkCondition[{cond_, error_String}] :=
  Module[
    {paramPattern, paren, renderbool, rendervalue},

    paramPattern = Except[True | False, _Symbol | _Parameter];

    paren[x_] := {"(", x, ")"};

    renderbool[Equal[a:paramPattern,b_String]] :=
        {"CCTK_EQUALS(", rendervalue[a], ",\"", b, "\")"};
    (* Note: We don't have postfix operators, so we don't need
       parentheses for prefix operators *)
    renderbool[Unequal[a:paramPattern,b_String]] := {"!", renderbool[a==b]};
    (* Note: == and != don't nest, so we don't need parentheses here *)
    renderbool[Equal[a:paramPattern,b_?NumberQ]] := {rendervalue[a], " == ", rendervalue[b]};
    renderbool[Unequal[a:paramPattern,b_?NumberQ]] := {rendervalue[a], " != ", rendervalue[b]};
    (* Note: if == or != are inside && or ||, we could omit the parentheses *)
    renderbool[HoldPattern[And[as__]]] :=
        Riffle[Map[paren[renderbool[#]]&, List[as]], " && "];
    renderbool[HoldPattern[Or[as__]]] :=
        Riffle[Map[paren[renderbool[#]]&, List[as]], " || "];
    renderbool[Not[a_]] := {"!", paren[renderbool[a]]};
    renderbool[a:paramPattern] := ToString[a/.(Parameter[x_]->x)]; (* Boolean parameter *)
    renderbool[x_] := ThrowError["Unexpected value in run-time conditional expression (boolean):", x, "in", cond];

    (* rendervalue[a_String] := a; -- Allow literal pass-through *)
    rendervalue[a_?NumberQ] := ToString[a];
    rendervalue[Parameter[a_String]] := a;
    (* TODO: pass in valid parameters *)
    rendervalue[a_ (*/; MemberQ[params,a] *) ] := ToString[a];
    (* rendervalue[x_] := ThrowError["Unexpected value in run-time conditional expression (value):", x, "in", cond]; *)

    ConditionalOnParameterTextual[
      renderbool[cond],
      {"CCTK_ERROR(", StringDrop[Stringify[error],-1], ");\n"}]]];

Options[ParamCheckProcessCode] = ThornOptions;

DefFn[
  ParamCheckProcessCode[cIn_Code, opts:OptionsPattern[]] :=
  Module[
    {c = cIn},
    If[Length[OptionValue[ParameterConditions]] > 0,
       c = AppendObjectField[
         c, "Sources",
         {Filename -> "ParamCheck.cc",
          Contents -> ParameterCheckSource[GetObjectField[c, "Name"], 
                                           OptionValue[ParameterConditions]]}]];
    c]];

End[];

EndPackage[];
