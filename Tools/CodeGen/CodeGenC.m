
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

BeginPackage["CodeGenC`", {"Errors`", "Kranc`", "CodeGen`"}];

SetSourceLanguage::usage = "DEPRECATED: set the source language to  \"C\" or \"Fortran\"";
CommentedBlock::usage = "CommentedBlock[comment, block] returns a block consisting " <>
  "of 'comment' followed by 'block'.";
CBlock::usage = "";
SuffixedCBlock::usage = "";
IncludeFile::usage = "IncludeFile[name] returns a block of code" <>
  "that includes a header file (i.e '#include \"name\"').";
IncludeSystemFile::usage = "IncludeFile[name] returns a block of code" <>
  "that includes a system header file (i.e '#include <name>').";
DeclareVariable::usage = "DeclareVariable[name, type] returns a block of code " <>
  "that declares a variable of given name and type without initialising it.  'name' and 'type' should be " <>
  "strings.";
DeclareVariables::usage = "DeclareVariables[names, type] returns a block of code " <>
  "that declares a list of variables of given name and type.  'names' should be a list" <>
  " of strings and 'type' should be a string string.";
DeclarePointer::usage = "DeclarePointer[name, type] returns a block of code " <>
  "that declares a pointer of given name and type.  'name' and 'type' should be " <>
  "strings.";
DeclarePointers::usage = "DeclarePointers[names, type] returns a block of code " <>
  "that declares a list of pointers of given name and type.  'names' should be a list" <>
  " of strings and 'type' should be a string string.";
DefineVariable::usage = "DefineVariable[name, type, value] returns a block of " <>
  "code that declares and initialises a variable 'name' of type 'type' to value 'value'.";
DefineConstant::usage = "DefineConstant[name, type, value] returns a block of " <>
  "code that declares and initialises a constant 'name' of type 'type' to value 'value'.";
AssignVariable::usage = "AssignVariable[dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
DeclareArray::usage = "";
DefineFunction::usage = "";
DefineSubroutine::usage = "";
Conditional::usage = "";
SwitchStatement::usage = "";
CFormHideStrings::usage = "";
InsertComment::usage = "";
WithNamespace;

Begin["`Private`"];

SetSourceLanguage[lang_] :=
  Print["Warning: SetSourceLanguage is no longer necessary"];

DefFn[
  IncludeFile[filename_String] :=
  {"#include \"", filename, "\"\n"}];

DefFn[
  IncludeSystemFile[filename_String] :=
  {"#include <", filename, ">\n"}];

DefFn[
  DeclareVariable[name:(_String|_Symbol), type_String] :=
  {type, " ",    name, " CCTK_ATTRIBUTE_UNUSED;\n"}];

DefFn[
  DeclareVariables[names_?ListQ, type_String] := 
  {type, " ",    CommaSeparated@names, " CCTK_ATTRIBUTE_UNUSED ;\n"}];

DefFn[
  DeclarePointer[name:(_String|_Symbol), type_String] :=
  {type, " *",    name, ";\n"}];

DefFn[
  DeclarePointers[names_?ListQ, type_String] :=
  {type, " *",           CommaSeparated@names, ";\n"}];

DefFn[
  DeclareArray[name:(_String|_Symbol), dim_Integer, type_String] :=
  {type, " ", name, "[", dim, "];","\n"}];

DefFn[
  DefineVariable[name:(_String|_Symbol), type_String, value:CodeGenBlock] :=
  {type, " ", name, " CCTK_ATTRIBUTE_UNUSED", " = ", value, ";\n"}];

DefFn[
  DefineConstant[name:(_String|_Symbol), type_String, value:CodeGenBlock] :=
  {"const ", type, " ", name, " CCTK_ATTRIBUTE_UNUSED", " = ", value, ";\n"}];

DefFn[
  AssignVariable[dest:(_String|_Symbol), src:CodeGenBlock] :=
  {dest, " = ", src, ";\n"}];

DefFn[
  InsertComment[text:CodeGenBlock] := {"/* ", text, " */\n"}];

DefFn[
  CBlock[block:CodeGenBlock] :=
  {"{\n",
   IndentBlock[block],
   "}\n"}];

DefFn[
  SuffixedCBlock[block:CodeGenBlock, suffix_] :=
  {"{\n",
   IndentBlock[block],
   "} ", suffix, "\n"}];

DefFn[
  CommentedBlock[comment:CodeGenBlock, block:CodeGenBlock] :=
  {InsertComment[comment],
   block,
   "\n"}];

(* FUNCTIONS *)

DefFn[
  DefineFunction[name_String, type_String, args:CodeGenBlock, contents:CodeGenBlock] :=
  {type, " ", name, "(", args, ")\n",
    CBlock[contents]}];

(* SUBROUTINES *)

DefFn[
  DefineSubroutine[name_String, args:CodeGenBlock, contents:CodeGenBlock] :=
  DefineFunction[name, "void", args, contents]];

DefFn[
  switchOption[{value:(_String|_Symbol|_?NumberQ), block:CodeGenBlock}] :=
  {"case ", value, ":\n", CBlock[{block,"break;\n"}]}];

DefFn[
  SwitchStatement[var:(_String|_Symbol), pairs__] :=
  {"switch (", var, ")\n",
   CBlock[{Riffle[Map[switchOption, {pairs}],"\n"],
           "default:\n", IndentBlock[{"CCTK_BUILTIN_UNREACHABLE();\n"}]}]}];

DefFn[
  Conditional[condition:CodeGenBlock, block:CodeGenBlock] :=
  {"if (", condition, ")\n",
   CBlock[block]}];

DefFn[
  Conditional[condition:CodeGenBlock, block1:CodeGenBlock, block2:CodeGenBlock] :=
  {"if (", condition, ")\n",
   CBlock[block1], "else\n", CBlock[block2]}];

(* Convert an expression to CForm, but remove the quotes from any
   strings present *)
DefFn[
  CFormHideStrings[x_, opts___] :=
  StringReplace[ToString[CForm[x,opts]], "\"" -> ""]];

DefFn[
  WithNamespace[ns_String, block:CodeGenBlock] :=
  MakeCodeBlock[
    {"namespace ", ns, " {\n\n",
      block,
      "\n} // namespace ", ns, "\n"}]];

End[];

EndPackage[];
