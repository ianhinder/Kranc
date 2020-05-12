
(* $Id$ *)

(*  Copyright 2004 Sascha Husa, Ian Hinder, Christiane Lechner

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

BeginPackage["CodeGenCactus`", {"Errors`", "Kranc`", "CodeGenC`", "CodeGen`"}];

DefineCCTKFunction::usage = "DefineCCTKFunction[name, type, block] returns a block " <>
  "of code that defines a CCTK function of name 'name' returning type 'type' with " <>
  "body 'block'.";
DefineCCTKSubroutine::usage = "DefineCCTKSubroutine[name, block] returns a block " <>
  "of code that defines a CCTK Fortran subroutine of name 'name' with body 'block'.";
DeclareGridLoopVariables::usage = "DeclareGridLoopVariables[] returns a block " <>
  "that defines the variables needed during a grid loop.";
InitialiseGridLoopVariablesWithStencil::usage = "InitialiseGridLoopVariables[] returns a block " <>
  "that initialises variables needed by a grid loop using the evolution stencils.";
ConditionalOnParameter::usage = "ConditionalOnParameter[name, value, block] returns " <>
  "a block that introduces a conditional expression whereby 'block' is only executed " <>
  "if the Cactus parameter 'name' has value 'value'.";
(*
GridLoop::usage = "GridLoop[block] returns a block that is looped over for every " <>
  "grid point.  Must have previously set up the grid loop variables (see " <>
  "DeclareGridLoopVariables and InitialiseGridLoopVariables.";
*)
GridLoop::usage = "GridLoop[block] returns a block that is looped over for every " <>
  "grid point.  Must have previously set up the grid loop variables (see " <>
  "InitialiseGridLoopVariables.";
ConditionalOnParameterTextual::usage = "";
NameRoot::usage = "";
CCLBlock;
CCLBlockCompact;

Begin["`Private`"];

(* This is a Cactus-callable function *)
DefFn[
  DefineCCTKFunction[name_String, type_String, contents:CodeGenBlock] :=
  {"extern \"C\" ", DefineFunction[
    name, type, "CCTK_ARGUMENTS", 
    {
      (* Some of these functions aren't scheduled
         so the macro doesn't get generated. *)
      "#ifdef DECLARE_CCTK_ARGUMENTS_"<>name<>"\n"<>
      "DECLARE_CCTK_ARGUMENTS_CHECKED("<>name<>");\n"<>
      "#else\n"<>
      "DECLARE_CCTK_ARGUMENTS;\n"<>
      "#endif\n",
      "DECLARE_CCTK_PARAMETERS;\n\n",
      contents
    }]}];

(* This is a Cactus-callable subroutine *)
DefFn[
  DefineCCTKSubroutine[name_String, contents:CodeGenBlock] :=
  {"extern \"C\" ", DefineSubroutine[
    name, "CCTK_ARGUMENTS", 
    {
      "DECLARE_CCTK_ARGUMENTS_CHECKED("<>name<>");\n"<>
      "DECLARE_CCTK_PARAMETERS;\n\n",
      contents
    }]}];

(* Access an element of an array; syntax is different between C and
   Fortran.  Always give this function a C-style array index. *)
DefFn[
  arrayElement[var_String, i_String] :=
  If[SOURCELANGUAGE == "C",
     {var, "[", arrayIndex[i], "]"},
     {var, "(", arrayIndex[i], ")"}]];

(* Given a C-style variable index, return the corresponding index for
   the language currently in use.  The idea is that the caller does not
   need to know what language is being used. *)
DefFn[
  arrayIndex[i:(_Integer|_String|_Symbol)] :=
  If[SOURCELANGUAGE == "C",
     i,
     If[NumberQ[i], i+1, {i, " + 1"}]]];

DefFn[
  max[]:= If[SOURCELANGUAGE == "C", "IMAX", "max"]];

DefFn[
  ConditionalOnParameter[name_String, value_String, block:CodeGenBlock] :=
    {"if (CCTK_EQUALS(", name, ", \"", value, "\"))\n",
     "{\n",
     IndentBlock[block],
     "}\n"}];

DefFn[
  ConditionalOnParameterTextual[text:CodeGenBlock, block:CodeGenBlock] :=
    {"if (", text, ")\n",
     "{\n",
     IndentBlock[block],
     "}\n"}];

(* Code generation for Cactus .par files *)

DefFn[
  activeThorns[list:{_String...}] :=
  {"ActiveThorns = \"", SpaceSeparated[list], "\"\n"}];

DefFn[
  setParameter[thorn_String, par_String, value_] :=
  {thorn, " = ", If[NumberQ[value], ToString[value], "\"" <> value <> "\""], "\n"}];

DefFn[
  setParametersForThorn[thorn_String, map_List] :=
  Map[setParameter[thorn, #[[1]], #[[2]]] &, map]];

DefFn[
  CCLBlock[type_String, name_String, attrs:{(_String -> CodeGenBlock)...},
           contents:CodeGenBlock,comment_String:""] :=
  {type, " ", name,
   Map[" "<>#[[1]]<>"="<>#[[2]] &, attrs], "\n",
   CBlock[contents],
   If[comment === "", "", Quote[comment]],"\n"}];

DefFn[
  CCLBlockCompact[type_String, name_String, attrs:{(_String -> CodeGenBlock)...},
           contents:CodeGenBlock,comment_String:""] :=
  {type, " ", name,
   Map[" "<>#[[1]]<>"="<>#[[2]] &, attrs],
   " {", contents, "}",
   If[comment === "", "", {" ",Quote[comment]}],"\n"}];

End[];

EndPackage[];
