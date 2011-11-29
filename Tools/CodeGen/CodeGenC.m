
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

SOURCELANGUAGE::usage = "global variable == \"C\" or \"Fortran\" determines language
                        for code generation";
SOURCESUFFIX::usage = "file suffix for source files";
SetSourceLanguage::usage = "set the source language to  \"C\" or \"Fortran\"";

EOL::usage = "the end of line termination string";

CommentedBlock::usage = "CommentedBlock[comment, block] returns a block consisting " <>
  "of 'comment' followed by 'block'.";
CBlock::usage = "";
SuffixedCBlock::usage = "";
IncludeFile::usage = "IncludeFile[name] returns a block of code" <>
  "that includes a header file (i.e '#include \"name\"').";
IncludeSystemFile::usage = "IncludeFile[name] returns a block of code" <>
  "that includes a system header file (i.e '#include <name>').";
DeclareVariable::usage = "DeclareVariable[name, type] returns a block of code " <>
  "that declares a variable of given name and type.  'name' and 'type' should be " <>
  "strings.";
DeclareVariableNoInit::usage = "DeclareVariableNoInit[name, type] returns a block of code " <>
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
  "code that declares and initialised a variable 'name' of type 'type' to value 'value'.";
AssignVariable::usage = "AssignVariable[dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
DeclareAssignVariable::usage = "DeclareAssignVariable[type_, dest_, src_] returns a block of code " <>
  "that declares and sets a constant variable of given name and type.";
DeclareArray::usage = "";
DefineFunction::usage = "";
DefineSubroutine::usage = "";
Conditional::usage = "";
SwitchStatement::usage = "";
CFormHideStrings::usage = "";
InsertComment::usage = "";

Begin["`Private`"];

SOURCELANGUAGE =  "C";
SOURCESUFFIX   = ".cc";

setSourceSuffix[lang_] :=
  If[lang == "C", SOURCESUFFIX = ".cc", SOURCESUFFIX = ".F90"];

SetSourceLanguage[lang_] :=
  If[lang == "C" || lang == "Fortran",
     SOURCELANGUAGE = lang;
     setSourceSuffix[lang];
     If[lang =!= "C", InfoMessage[Terse, "User set source language to " <> lang]],
     (* else *)
     SOURCELANGUAGE = "C";
     setSourceSuffix[".cc"]];

EOL[dummy___] :=
  If[SOURCELANGUAGE == "C" || SOURCELANGUAGE == "C++", ";\n", "\n"];

DefFn[
  IncludeFile[filename_String] :=
  {"#include \"", filename, "\"\n"}];

DefFn[
  IncludeSystemFile[filename_String] :=
  {"#include <", filename, ">\n"}];

DefFn[
  DeclareVariable[name:(_String|_Symbol), type_String] :=
  If[SOURCELANGUAGE == "C",
     {type, " ",    name, " = INITVALUE" <> EOL[]},
     {type, " :: ", name, EOL[]} (* no value init here to avoid implicit SAVE attribute *)]];

DefFn[
  DeclareVariableNoInit[name:(_String|_Symbol), type_String] :=
  If[SOURCELANGUAGE == "C",
     {type, " ",    name, EOL[]},
     {type, " :: ", name, EOL[]} (* no value init here to avoid implicit SAVE attribute *)]];

DefFn[
  DeclareVariables[names_?ListQ, type_String] := 
  If[SOURCELANGUAGE == "C",
     {type, " ",    CommaSeparated@names, EOL[]},
     {type, " :: ", CommaSeparated@names,     EOL[]} (* no value init avoids implicit SAVE attribute *)]];

DefFn[
  DeclarePointer[name:(_String|_Symbol), type_String] :=
  If[SOURCELANGUAGE == "C",
     {type, " *",    name, EOL[]},
     {type, ", target :: ", name, EOL[]}]];

DefFn[
  DeclarePointers[names_?ListQ, type_String] :=
  If[SOURCELANGUAGE == "C",
     {type, " *",           CommaInitSeparated@names, EOL[]},
     {type, ", target :: ", CommaSeparated@names,     EOL[]}]];

DefFn[
  DeclareArray[name:(_String|_Symbol), dim_Integer, type_String] :=
  If[SOURCELANGUAGE == "C",
     DeclareArrayC[name, dim, type],
     DeclareArrayFortran[name, dim, type]]];

DefFn[
  DeclareArrayC[name:(_String|_Symbol), dim_Integer, type_String] :=
  {type, " ", name, "[", dim, "];","\n"}];

DefFn[
  DeclareArrayFortran[name:(_String|_Symbol), dim_Integer, type_String] :=
  {type, " :: ", name, "(", dim, ")","\n"}];

DefFn[
  DefineVariable[name:(_String|_Symbol), type_String, value:CodeGenBlock] :=
  {type, " ", name, " = ", value, EOL[]}];

DefFn[
  AssignVariable[dest:(_String|_Symbol), src:CodeGenBlock] :=
  {dest, " = ", src, EOL[]}];

DefFn[
  DeclareAssignVariable[type_String, dest:(_String|_Symbol), src:CodeGenBlock] :=
  {type, " const ", dest, " = ", src, EOL[]}];

(* comments are always done C-style because they are killed by cpp anyway *) 
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
  SeparatedBlock[{InsertComment[comment],
                  block}]];

(* FUNCTIONS *)

DefFn[
  defineFunctionC[name_String, type_String, args:CodeGenBlock, contents:CodeGenBlock] :=
  SeparatedBlock[
    {type, " ", name, "(", args, ")\n",
     CBlock[contents]}]];
     
DefFn[
  defineFunctionF[name_String, args:CodeGenBlock, contents:CodeGenBlock] :=
  SeparatedBlock[
    {"FUNCTION", " ", name, "(", args, ")\n",
     IndentBlock[contents]}]];

DefFn[
  DefineFunction[name_String, type_String, args:CodeGenBlock, contents:CodeGenBlock] :=
  If[SOURCELANGUAGE == "C",
     defineFunctionC[name, type, args, contents],
     defineFunctionF[name, args, contents]]];

(* SUBROUTINES *)

DefFn[
  DefineSubroutine[name_String, args:CodeGenBlock, contents:CodeGenBlock] :=
  If[SOURCELANGUAGE == "C",
     DefineSubroutineC[name, args, contents],
     DefineSubroutineF[name, args, contents]]];

DefFn[
  DefineSubroutineC[name_String, args:CodeGenBlock, contents:CodeGenBlock] :=
  SeparatedBlock[
    {"extern \"C\" void ", name, "(", args, ")", "\n",
     CBlock[contents]}]];

DefFn[
  DefineSubroutineF[name_String, args:CodeGenBlock, contents:CodeGenBlock] :=
  SeparatedBlock[
    {"subroutine ", name, "(", args, ")", "\n",
     "\nimplicit none\n\n",
     contents,
     "end subroutine\n"}]];

DefFn[
  switchOption[{value:(_String|_Symbol|_?NumberQ), block:CodeGenBlock}] :=
  {"case ", value, ":\n", IndentBlock[{block,"break;\n"}]}];
(* Outer list unnecessary? *)

DefFn[
  SwitchStatement[var:(_String|_Symbol), pairs__] :=
  {"switch(", var, ")\n",
   CBlock[{Riffle[Map[switchOption, {pairs}],"\n"]}]}];

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

End[];

EndPackage[];
