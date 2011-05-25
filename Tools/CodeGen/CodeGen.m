
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

BeginPackage["CodeGen`", {"Errors`", "Kranc`"}];

SOURCELANGUAGE::usage = "global variable == \"C\" or \"Fortran\" determines language
                        for code generation";
SOURCESUFFIX::usage = "file suffix for source files";

EOL::usage = "the end of line termination string";
SetSourceLanguage::usage = "set the source language to  \"C\" or \"Fortran\"";

FlattenBlock::usage = "FlattenBlock[block] converts 'block' to a string.";
SeparatedBlock::usage = "SeparatedBlock[block] returns a version of 'block' with " <>
  "a newline before it.";
GenerateFile::usage = "GenerateFile[name, block] writes 'block' to a file of the " <>
  "specified 'name'.";
AddToFile::usage = "AddToFile[name, block] appends 'block' to a file of the " <>
    "specified 'name'.";
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
AssignVariableInLoop::usage = "AssignVariableInLoop[dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
StoreVariableInLoop::usage = "StoreVariableInLoop[dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
StoreLowPartialVariableInLoop::usage = "StoreLowPartialVariableInLoop[dest_, src_, count_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
StoreHighPartialVariableInLoop::usage = "StoreHighPartialVariableInLoop[dest_, src_, count_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
DeclareAssignVariableInLoop::usage = "DeclareAssignVariableInLoop[type_, dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
MaybeAssignVariableInLoop::usage = "MaybeAssignVariableInLoop[dest_, src_, cond_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
DeclareMaybeAssignVariableInLoop::usage = "DeclareMaybeAssignVariableInLoop[type_, dest_, src_, cond_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
TestForNaN::usage = "TestForNaN[expr_] returns a block of code " <>
  "that tests 'expr' for nan.";
CommentedBlock::usage = "CommentedBlock[comment, block] returns a block consisting " <>
  "of 'comment' followed by 'block'.";
DefineCCTKFunction::usage = "DefineCCTKFunction[name, type, block] returns a block " <>
  "of code that defines a CCTK function of name 'name' returning type 'type' with " <>
  "body 'block'.";
DefineCCTKSubroutine::usage = "DefineCCTKSubroutine[name, block] returns a block " <>
  "of code that defines a CCTK Fortran subroutine of name 'name' with body 'block'.";
GridName::usage = "GridName[variable] returns the name needed to access variable " <>
  "assuming it is a grid variable when inside a grid loop.";
DeclareGridLoopVariables::usage = "DeclareGridLoopVariables[] returns a block " <>
  "that defines the variables needed during a grid loop.";
InitialiseGridLoopVariables::usage = "InitialiseGridLoopVariables[] returns a block " <>
  "that initialises variables needed by a grid loop.";
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

DeclareArray::usage = "";

DefineFunction::usage = "";
ConditionalOnParameterTextual::usage = "";

SpaceSeparated::usage = "";   (* This should not really be in CodeGen *)
NewlineSeparated::usage = ""; (* This should not really be in CodeGen *)

CBlock::usage = "";
SuffixedCBlock::usage = "";
InfoVariable::usage = "";

DeclareFDVariables::usage = "";

InitialiseFDVariables::usage = "";

CommaNewlineSeparated::usage = ""; (* This should not really be in CodeGen *)
CommaSeparated::usage = "";
ReplacePowers::usage = "";
CFormHideStrings::usage = "";
BoundaryLoop::usage = "";
BoundaryWithGhostsLoop::usage = "";
GenericGridLoop::usage = "";

NameRoot::usage = "";
PartitionVarList::usage = "";
Quote::usage = "Quote[x] returns x surrounded by quotes";
DataType::usage = "DataType[] returns a string for the grid function data type (e.g. CCTK_REAL)";
SetDataType::usage = "SetDataType[type] sets a string for the grid function data type (e.g. CCTK_REAL)";
Conditional;

Begin["`Private`"];

SOURCELANGUAGE =  "C";
SOURCESUFFIX   = ".cc";

setSourceSuffix[lang_] :=
If[ (lang == "C"),
  SOURCESUFFIX = ".cc";
,
  SOURCESUFFIX = ".F90";
];


SetSourceLanguage[lang_] :=
If[ (lang == "C" || lang == "Fortran"),
  SOURCELANGUAGE = lang;
  setSourceSuffix[lang];
  InfoMessage[Terse, "User set source language to " <> lang],

  SOURCELANGUAGE = "C";
  setSourceSuffix[".cc"];
  InfoMessage[Terse, "Setting Source Language to C"];
];

SetDataType[type_String] :=
  dataType = type;

DataType[] :=
  If[dataType === Symbol["datatype"],
    Throw["DataType: Have not set a data type"],
    dataType];

(* Code generation utilities; not specific to any language *)

FlattenBlock[b_] := Apply[StringJoin,Map[ToString,If[! AtomQ[b], Flatten[b, Infinity], b]]];

indentBlock[block_] :=
  StringDrop["  " <> StringReplace[FlattenBlock[block], {"\n" -> "\n  "}],-2];

SeparatedBlock[block_] := {"\n", block};

GenerateFile[filename_, contents_] :=
  Module[{fp = OpenWrite[filename]},
    WriteString[fp, FlattenBlock[contents]];
    Close[fp]];

AddToFile[filename_, contents_] :=
  Module[{fp = OpenAppend[filename]},
      WriteString[fp, FlattenBlock[contents]];
          Close[fp]];

intersperse[l_, x_] :=
  If[l == {},
    {},
    If[Rest[l] == {},
      {l[[1]]},
      Join[{l[[1]]}, {x}, intersperse[Rest[l],x]]]];

CommaNewlineSeparated[l_] := intersperse[l, ",\n"];

SpaceSeparated[l_] := 
  Module[{},
  If[!ListQ[l],
    ThrowError["SpaceSeparated: Expecting a list, but was given", l]];
  intersperse[l, " "]];

CommaSeparated[l_] := 
  intersperse[l, ", "];

NewlineSeparated[l_] := 
  intersperse[l, "\n"];

CommaInitSeparated[l_] :=
  intersperse[Map[{#," = INITVALUE"} &, l], ", "];
(*  intersperse[l, " = INITVALUE, "];*)



NameRoot[name_] := Module[{dropNumberRule, root},

      dropNumberRule = {"1" -> "", "2" -> "", "3" -> "", "4" -> "", "5" -> "",
                        "6" -> "", "7" -> "", "8" -> "", "9" -> "", "0" -> "", "rhs" -> ""};

      root = StringReplace[ToString@name, dropNumberRule]
      ];

PartitionVarList[list_]:= Module[{partition, split},

partition[locallist_] := Module[{cutoff},
  cutoff = 6;
  If[Length@locallist > cutoff, Partition[locallist, cutoff, cutoff, {1,1}, {}], {locallist}]
];

split = Split[list, NameRoot[#1] == NameRoot[#2] &];
split = Flatten[Map[partition, split], 1];

split
];


(* Code generation for generic C and C-preprocessed Fortran *)

EOL[dummy___] := If[SOURCELANGUAGE == "C" || SOURCELANGUAGE == "C++", ";\n", "\n"];

IncludeFile[filename_] :=
  {"#include \"", filename, "\"\n"};

IncludeSystemFile[filename_] :=
  {"#include <", filename, ">\n"};

DeclareVariable[name_, type_] :=
If[SOURCELANGUAGE == "C",
  {type, " ",    name, " = INITVALUE" <> EOL[]},
  {type, " :: ", name, EOL[]} (* no value init here to avoid implicit SAVE attribute *)
  ];

DeclareVariableNoInit[name_, type_] :=
If[SOURCELANGUAGE == "C",
  {type, " ",    name, EOL[]},
  {type, " :: ", name, EOL[]} (* no value init here to avoid implicit SAVE attribute *)
  ];


DeclareVariables[names_?ListQ, type_] := 
If[SOURCELANGUAGE == "C",
   {type, " ",    CommaSeparated@names, EOL[]},
   {type, " :: ", CommaSeparated@names,     EOL[]} (* no value init avoids implicit SAVE attribute *)
   ];

DeclarePointer[name_, type_] :=
If[SOURCELANGUAGE == "C",
  {type, " *",    name, EOL[]},
  {type, ", target :: ", name, EOL[]}
  ];

DeclarePointers[names_?ListQ, type_] :=
If[SOURCELANGUAGE == "C",
   {type, " *",           CommaInitSeparated@names, EOL[]},
   {type, ", target :: ", CommaSeparated@names,     EOL[]} 
   ];

DeclareArray[name_, dim_, type_] :=
  If[SOURCELANGUAGE == "C",
     DeclareArrayC[name, dim, type],
     DeclareArrayFortran[name, dim, type]];

DeclareArrayC[name_, dim_, type_] :=
  {type, " ", name, "[", dim, "];","\n"};

DeclareArrayFortran[name_, dim_, type_] :=
  {type, " :: ", name, "(", dim, ")","\n"};

DefineVariable[name_, type_, value_] :=
  {type, " ", name, " = ", value, EOL[]};

AssignVariable[dest_, src_] :=
  {dest, " = ", src, EOL[]};

DeclareAssignVariable[type_, dest_, src_] :=
  {type, " const ", dest, " = ", src, EOL[]};

AssignVariableInLoop[dest_, src_, vectorise_:False] :=
  Module[{loader},
    loader[x_] := If[vectorise, {"vec_load(", x, ")"}, x];
    {dest, " = ", loader[src], EOL[]}];
(*
  {dest, " = ", src, EOL[],
   TestForNaN[dest]};
*)

StoreVariableInLoop[dest_, src_] :=
  {"vec_store_nta(", dest, ",", src, ")", EOL[]};

StoreLowPartialVariableInLoop[dest_, src_, count_] :=
  {"vec_store_nta_partial_lo(", dest, ",", src, ",", count, ")", EOL[]};

StoreHighPartialVariableInLoop[dest_, src_, count_] :=
  {"vec_store_nta_partial_hi(", dest, ",", src, ",", count, ")", EOL[]};

DeclareAssignVariableInLoop[type_, dest_, src_] :=
  {type, " const ", dest, " = vec_load(", src, ")", EOL[]};

MaybeAssignVariableInLoop[dest_, src_, cond_] :=
  If [cond,
      {dest, " = useMatter ? vec_load(", src, ") : ToReal(0.0)", EOL[]},
      {dest, " = vec_load(", src, ")", EOL[]}];

DeclareMaybeAssignVariableInLoop[type_, dest_, src_, mmaCond_, codeCond_, vectorise_:False] :=
  Module[{loader},
    loader[x_] := If[vectorise, {"vec_load(", x, ")"}, x];
    If [mmaCond,
        {type, " ", dest, " = (", codeCond, ") ? ", loader[src], " : ToReal(0.0)", EOL[]},
        {type, " ", dest, " = ", loader[src], EOL[]}]];

TestForNaN[expr_] :=
  {"if (isnan(", expr, ")) {\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"NaN found\");\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"ipos: %d %d %d\", i, j, k);\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"lbnd: %d %d %d\", cctk_lbnd[0], cctk_lbnd[1], cctk_lbnd[2]);\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"lsh: %d %d %d\", cctk_lsh[0], cctk_lsh[1], cctk_lsh[2]);\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"lssh: %d %d %d\", cctk_lssh[CCTK_LSSH_IDX(0,0)], cctk_lssh[CCTK_LSSH_IDX(0,1)], cctk_lssh[CCTK_LSSH_IDX(0,2)]);\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"", expr, ": %.17g\", (double)", expr, ");\n",
   "}\n"};

(* comments are always done C-style because they are killed by cpp anyway *) 
insertComment[text_] := {"/* ", text, " */\n"};

CBlock[block_] :=
 {"{\n",
  indentBlock[block],
  "}\n"};

SuffixedCBlock[block_, suffix_] :=
 {"{\n",
  indentBlock[block],
  "} ", suffix, "\n"};


loopOverInteger[name_, start_, endplusone_, block_] :=
If[SOURCELANGUAGE == "C" || SOURCELANGUAGE == "C++",

  {"for (", name, " = ", start, "; ", name, " < ", endplusone, "; ", name, "++)\n",
   "{\n",
   indentBlock[block],
   "}\n"},

  {"Do ", name, " = ", start, ", ", endplusone, "\n",
   "\n",
   indentBlock[block],
   "End Do\n"}
];


CommentedBlock[comment_, block_] :=
  SeparatedBlock[{insertComment[comment],
                  block}];

(* FUNCTIONS *)

defineFunctionC[name_, type_, args_, contents_] :=
  SeparatedBlock[
    {type, " ", name, "(", args, ")\n",
      CBlock[contents]}];
     
defineFunctionF[name_, args_, contents_] :=
  SeparatedBlock[
    {"FUNCTION", " ", name, "(", args, ")\n",
      indentBlock[contents]}];

DefineFunction[name_, type_, args_, contents_] :=
  If[SOURCELANGUAGE == "C",
    defineFunctionC[name, type, args, contents],
    defineFunctionF[name, args, contents]];

(* SUBROUTINES *)

defineSubroutine[name_, args_, contents_] :=
  If[SOURCELANGUAGE == "C",
    defineSubroutineC[name, args, contents],
    defineSubroutineF[name, args, contents]];

defineSubroutineC[name_, args_, contents_] :=
  SeparatedBlock[
    {"extern \"C\" void ", name, "(", args, ")", "\n",
     CBlock[contents]}];

defineSubroutineF[name_, args_, contents_] :=
  SeparatedBlock[
    {"subroutine ", name, "(", args, ")", "\n",
     "\nimplicit none\n\n",
     contents,
     "end subroutine\n"}];




(********* Code generation for Cactus C or Fortran code **********)




(* This is a Cactus-callable function *)
DefineCCTKFunction[name_, type_, contents_] :=
  DefineFunction[name, "extern \"C\" " <> type, "CCTK_ARGUMENTS", 
    {
      "DECLARE_CCTK_ARGUMENTS;\n",
      "DECLARE_CCTK_PARAMETERS;\n\n",
      contents
    }];

(* This is a Cactus-callable subroutine *)
DefineCCTKSubroutine[name_, contents_] :=
  defineSubroutine[
    name, "CCTK_ARGUMENTS", 
    {
      "DECLARE_CCTK_ARGUMENTS;\n",
      "DECLARE_CCTK_PARAMETERS;\n\n",
      contents
    }];

DeclareFDVariables[] := 
(*
  CommentedBlock["Declare finite differencing variables",
    {Map[DeclareVariables[#, "CCTK_REAL"] &, {{"dx", "dy", "dz"}, 
                                              {"dxi", "dyi", "dzi"},
                                              {khalf,kthird,ktwothird,kfourthird,keightthird},
                                              {"hdxi", "hdyi", "hdzi"}}],
     "\n"},
    {Map[DeclareVariables[#, "ptrdiff_t"] &, {{"di", "dj", "dk"}}],
     "\n"}];
*)
  CommentedBlock["Declare finite differencing variables", {}];

InitialiseFDSpacingVariablesC[] := 
  {
    (* DeclareAssignVariable["ptrdiff_t", "di", "CCTK_GFINDEX3D(cctkGH,1,0,0) - CCTK_GFINDEX3D(cctkGH,0,0,0)"], *)
    DeclareAssignVariable["ptrdiff_t", "di", "1"],
    DeclareAssignVariable["ptrdiff_t", "dj", "CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0)"],
    DeclareAssignVariable["ptrdiff_t", "dk", "CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0)"],
    DeclareAssignVariable[DataType[], "dx", "ToReal(CCTK_DELTA_SPACE(0))"],
    DeclareAssignVariable[DataType[], "dy", "ToReal(CCTK_DELTA_SPACE(1))"],
    DeclareAssignVariable[DataType[], "dz", "ToReal(CCTK_DELTA_SPACE(2))"]
  };

InitialiseFDSpacingVariablesFortran[] := 
  {
    AssignVariable["dx", "CCTK_DELTA_SPACE(1)"],
    AssignVariable["dy", "CCTK_DELTA_SPACE(2)"],
    AssignVariable["dz", "CCTK_DELTA_SPACE(3)"]
  }


InitialiseFDVariables[vectorise_] :=
  CommentedBlock["Initialise finite differencing variables",
  { If[SOURCELANGUAGE == "Fortran",
       InitialiseFDSpacingVariablesFortran[],
       InitialiseFDSpacingVariablesC[]],
    
    DeclareAssignVariable[DataType[], "dxi", "INV(dx)"],
    DeclareAssignVariable[DataType[], "dyi", "INV(dy)"],
    DeclareAssignVariable[DataType[], "dzi", "INV(dz)"],
    If[vectorise,
     {DeclareAssignVariable[DataType[], "khalf", "ToReal(0.5)"],
      DeclareAssignVariable[DataType[], "kthird", "ToReal(1.0/3.0)"],
      DeclareAssignVariable[DataType[], "ktwothird", "ToReal(2.0/3.0)"],
      DeclareAssignVariable[DataType[], "kfourthird", "ToReal(4.0/3.0)"],
      DeclareAssignVariable[DataType[], "keightthird", "ToReal(8.0/3.0)"],
      DeclareAssignVariable[DataType[], "hdxi", "kmul(ToReal(0.5), dxi)"],
      DeclareAssignVariable[DataType[], "hdyi", "kmul(ToReal(0.5), dyi)"],
      DeclareAssignVariable[DataType[], "hdzi", "kmul(ToReal(0.5), dzi)"]},
     {DeclareAssignVariable[DataType[], "khalf", "0.5"],
      DeclareAssignVariable[DataType[], "kthird", "1/3.0"],
      DeclareAssignVariable[DataType[], "ktwothird", "2.0/3.0"],
      DeclareAssignVariable[DataType[], "kfourthird", "4.0/3.0"],
      DeclareAssignVariable[DataType[], "keightthird", "8.0/3.0"],
      DeclareAssignVariable[DataType[], "hdxi", "0.5 * dxi"],
      DeclareAssignVariable[DataType[], "hdyi", "0.5 * dyi"],
      DeclareAssignVariable[DataType[], "hdzi", "0.5 * dzi"]}]}];

GridName[x_] := If[SOURCELANGUAGE == "C",
                   ToExpression[ToString[x] <> "[index]"],
                   ToString[x] <> "(i,j,k)"
                ];

DeclareGridLoopVariables[] :=
  SeparatedBlock[
    {insertComment["Declare the variables used for looping over grid points"],
     Map[DeclareVariables[#, "CCTK_INT"] &, 
         {{"i", "j", "k"}(*, {"istart", "jstart", "kstart"}, 
          {"iend", "jend", "kend"},
          {"index_offset_x", "index_offset_y", "index_offset_z", "dir", "face"} *)}] (*,
     Map[DeclareArray[#, 6, "CCTK_INT"] &, {"is_symbnd", "is_physbnd", "is_ipbnd"}],
     Map[DeclareArray[#, 3, "CCTK_INT"] &, {"imin", "imax", "bmin", "bmax"}] *), 

     If[SOURCELANGUAGE == "C", DeclareVariable["index", "// CCTK_INT"], "\n"]
  }];

(* Access an element of an array; syntax is different between C and
   Fortran.  Always give this function a C-style array index. *)
arrayElement[var_, i_] :=
  If[SOURCELANGUAGE == "C",
     {var, "[", arrayIndex[i], "]"},
     {var, "(", arrayIndex[i], ")"}];

(* Given a C-style variable index, return the corresponding index for
   the language currently in use.  The idea is that the caller does not
   need to know what language is being used. *)
arrayIndex[i_] :=
  If[SOURCELANGUAGE == "C",
     i,
     If[NumberQ[i], i+1, {i, " + 1"}]];

max[]:= If[SOURCELANGUAGE == "C", "IMAX", "max"];

InitialiseGridLoopVariables[derivativesUsedSwitch_, addToStencilWidth_] :=
  CommentedBlock["Set up variables used in the grid loop for the physical grid points",

  If[ (derivativesUsedSwitch),
  {
  AssignVariable["index_offset_x", max[] <>"(stencil_width, stencil_width_x) + " <> ToString[addToStencilWidth]],
  AssignVariable["index_offset_y", max[] <>"(stencil_width, stencil_width_y) + " <> ToString[addToStencilWidth]],
  AssignVariable["index_offset_z", max[] <>"(stencil_width, stencil_width_z) + " <> ToString[addToStencilWidth]],

  "\n",
  AssignVariable["istart", arrayIndex["index_offset_x"]],
  AssignVariable["jstart", arrayIndex["index_offset_y"]],
  AssignVariable["kstart", arrayIndex["index_offset_z"]],

  "\n",
  AssignVariable["iend", {arrayElement["cctk_lssh", "CCTK_LSSH_IDX(0,0)"], " - index_offset_x"}],
  AssignVariable["jend", {arrayElement["cctk_lssh", "CCTK_LSSH_IDX(0,1)"], " - index_offset_y"}],
  AssignVariable["kend", {arrayElement["cctk_lssh", "CCTK_LSSH_IDX(0,2)"], " - index_offset_z"}]
  },

  {
  AssignVariable["istart", arrayIndex[0]],
  AssignVariable["jstart", arrayIndex[0]],
  AssignVariable["kstart", arrayIndex[0]],

  "\n",
  AssignVariable["iend", arrayElement["cctk_lssh", "CCTK_LSSH_IDX(0,0)"]],
  AssignVariable["jend", arrayElement["cctk_lssh", "CCTK_LSSH_IDX(0,1)"]],
  AssignVariable["kend", arrayElement["cctk_lssh", "CCTK_LSSH_IDX(0,2)"]]
  }]
];


ConditionalOnParameter[name_, value_, block_] :=
  SeparatedBlock[
  {"if (CCTK_EQUALS(", name, ", \"", value, "\"))\n",
   "{\n",
   indentBlock[block],
   "}\n"}];

ConditionalOnParameterTextual[text_, block_] :=
  SeparatedBlock[
  {"if (", text, ")\n",
   "{\n",
   indentBlock[block],
   "}\n"}];

(*
GridLoop[block_] :=
  CommentedBlock["Loop over the grid points",
   loopOverInteger["k", "kstart", "kend",
     loopOverInteger["j", "jstart", "jend",
       loopOverInteger["i", "istart", "iend",

       { If[SOURCELANGUAGE == "C",  
            AssignVariable["index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"],
            ""],
	 block
       }
        ]]]];

*)

(*
GridLoop[block_] :=
  If[SOURCELANGUAGE == "C",  
    CommentedBlock["Loop over the grid points",
      {
        "#pragma omp parallel\n",
        "LC_LOOP3 (unnamed,\n",
        "          i,j,k, istart,jstart,kstart, iend,jend,kend,\n",
        "          cctk_lsh[0],cctk_lsh[1],cctk_lsh[2])\n",
        "{\n",
        indentBlock[
          {
             DeclareVariable["index", "int"],
             AssignVariable["index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"],
             block
          }
        ],
        "}\n",
        "LC_ENDLOOP3 (unnamed);\n"
      }
    ],
    CommentedBlock["Loop over the grid points",
      {
        "#pragma omp parallel\n",
        "LC_LOOP3 (unnamed,\n",
        "          i,j,k, istart,jstart,kstart, iend,jend,kend,\n",
        "          cctk_lsh(1),cctk_lsh(2),cctk_lsh(3))\n",
        indentBlock[block],
        "LC_ENDLOOP3 (unnamed)\n"
      }
    ]
  ];
*)

Options[GenericGridLoop] = ThornOptions;

GenericGridLoop[functionName_, block_, opts:OptionsPattern[]] :=
  If[OptionValue[UseLoopControl],
    GenericGridLoopUsingLoopControl[functionName, block, OptionValue[UseVectors]],
    GenericGridLoopTraditional[block]];

GenericGridLoopTraditional[block_] :=
  CommentedBlock["Loop over the grid points",
   loopOverInteger["k", "min[2]", "max[2]",
     loopOverInteger["j", "min[1]", "max[1]",
       loopOverInteger["i", "min[0]", "max[0]",

       { If[SOURCELANGUAGE == "C",  
            {
              DeclareAssignVariable["int", "index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"]
            }
            ""],
	 block
       }
        ]]]];

GenericGridLoopUsingLoopControl[functionName_, block_, vectorise_] :=
  If[SOURCELANGUAGE == "C",  
    CommentedBlock["Loop over the grid points",
      {
        "#pragma omp parallel\n",
        If[vectorise, "LC_LOOP3VEC", "LC_LOOP3"] <> " (", functionName, ",\n",
        "  i,j,k, min[0],min[1],min[2], max[0],max[1],max[2],\n",
        "  cctk_lsh[0],cctk_lsh[1],cctk_lsh[2]", If[vectorise, {",\n",
        "  CCTK_REAL_VEC_SIZE"},""] <> ")\n",
        "{\n",
        indentBlock[
          {
             (* DeclareVariable["index", "// int"], *)
             (* DeclareAssignVariable["int", "index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"], *)
             DeclareAssignVariable["ptrdiff_t", "index", "di*i + dj*j + dk*k"],
             block
          }
        ],
        "}\n",
        If[vectorise, "LC_ENDLOOP3VEC", "LC_ENDLOOP3"] <> " (", functionName, ");\n"
      }
    ],
    ""
  ];

switchOptions[{value_, block_}] :=
{
  "case ", value, ":\n", block, "break;\n"
}

SwitchStatement[var_, pairs__] :=
{
  "switch(", var, ")\n",
  CBlock[{Map[switchOptions, {pairs}]}]
}



BoundaryLoop[block_] :=
{
  "\nGenericFD_GetBoundaryInfo(cctkGH, cctk_lsh, cctk_lssh, cctk_bbox, cctk_nghostzones, imin, imax, is_symbnd, is_physbnd, is_ipbnd);\n",

  CommentedBlock["Start by looping over the whole grid, minus the NON-PHYSICAL boundary points, which are set by synchronization.  ", {
  AssignVariable[arrayElement["bmin", 0], "is_physbnd[0*2+0] ? 0 : imin[0]"],
  AssignVariable[arrayElement["bmin", 1], "is_physbnd[1*2+0] ? 0 : imin[1]"],
  AssignVariable[arrayElement["bmin", 2], "is_physbnd[2*2+0] ? 0 : imin[2]"],
  AssignVariable[arrayElement["bmax", 0], "is_physbnd[0*2+1] ? cctk_from[CCTK_LSSH_IDX(0,0)] : imax[0]"],
  AssignVariable[arrayElement["bmax", 1], "is_physbnd[1*2+1] ? cctk_from[CCTK_LSSH_IDX(0,1)] : imax[1]"],
  AssignVariable[arrayElement["bmax", 2], "is_physbnd[2*2+1] ? cctk_from[CCTK_LSSH_IDX(0,2)] : imax[2]"]}], 

  CommentedBlock["Loop over all faces",
   loopOverInteger["dir", "0", "3",
     loopOverInteger["face", "0", "2",
     {
      CommentedBlock["Now restrict to only the boundary points on the current face",
       SwitchStatement["face", 
        {0,  {AssignVariable[arrayElement["bmax", "dir"], {arrayElement["imin", "dir"], ""}], 
              AssignVariable[arrayElement["bmin", "dir"], {0, ""}]}},
        {1,  {AssignVariable[arrayElement["bmin", "dir"], {arrayElement["imax", "dir"], "" }],
              AssignVariable[arrayElement["bmax", "dir"], {"cctk_lssh[CCTK_LSSH_IDX(0,dir)]", ""}]}}]],
       conditional[arrayElement["is_physbnd", "dir * 2 + face"],
         loopOverInteger["k", arrayElement["bmin",2], arrayElement["bmax",2],
           loopOverInteger["j", arrayElement["bmin",1], arrayElement["bmax",1],
             loopOverInteger["i", arrayElement["bmin",0], arrayElement["bmax",0],

         { If[SOURCELANGUAGE == "C",  
              AssignVariable["index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"],
              ""],
	   block
         }
      
      ]]]
      ]}
     ]]]};

BoundaryWithGhostsLoop[block_] :=
{
  "\nGenericFD_GetBoundaryInfo(cctkGH, cctk_lsh, cctk_lssh, cctk_bbox, cctk_nghostzones, imin, imax, is_symbnd, is_physbnd, is_ipbnd);\n",

  CommentedBlock["Start by looping over the whole grid, including the NON-PHYSICAL boundary points.  ", {
  AssignVariable[arrayElement["bmin", 0], "0"],
  AssignVariable[arrayElement["bmin", 1], "0"],
  AssignVariable[arrayElement["bmin", 2], "0"],
  AssignVariable[arrayElement["bmax", 0], "cctk_lssh[CCTK_LSSH_IDX(0,0)]"],
  AssignVariable[arrayElement["bmax", 1], "cctk_lssh[CCTK_LSSH_IDX(0,1)]"],
  AssignVariable[arrayElement["bmax", 2], "cctk_lssh[CCTK_LSSH_IDX(0,2)]"]}], 

  CommentedBlock["Loop over all faces",
   loopOverInteger["dir", "0", "3",
     loopOverInteger["face", "0", "2",
     {
      CommentedBlock["Now restrict to only the boundary points on the current face",
       SwitchStatement["face", 
        {0,  {AssignVariable[arrayElement["bmax", "dir"], {arrayElement["imin", "dir"], ""}], 
              AssignVariable[arrayElement["bmin", "dir"], {0, ""}]}},
        {1,  {AssignVariable[arrayElement["bmin", "dir"], {arrayElement["imax", "dir"], "" }],
              AssignVariable[arrayElement["bmax", "dir"], {"cctk_lssh[CCTK_LSSH_IDX(0,dir)]", ""}]}}]],
       conditional[arrayElement["is_physbnd", "dir * 2 + face"],
         loopOverInteger["k", arrayElement["bmin",2], arrayElement["bmax",2],
           loopOverInteger["j", arrayElement["bmin",1], arrayElement["bmax",1],
             loopOverInteger["i", arrayElement["bmin",0], arrayElement["bmax",0],

         { If[SOURCELANGUAGE == "C",  
              AssignVariable["index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"],
              ""],
	   block
         }
      
      ]]]
      ]}
     ]]]};

Conditional[condition_, block_] :=
 {"if (", condition, ")\n",
  CBlock[block]};

Conditional[condition_, block1_, block2_] :=
 {"if (", condition, ")\n",
  CBlock[block1], "else\n", CBlock[block2]};

onceInGridLoop[block_] :=
  Conditional["i == 5 && j == 5 && k == 5",
              block];

InfoVariable[name_] :=
 onceInGridLoop[
  { "char buffer[255];\n",
     "sprintf(buffer,\"" , name , " == %f\", " , name , ");\n",
     "CCTK_INFO(buffer);\n"}];

(* Code generation for Cactus .par files *)

activeThorns[list_] :=
  {"ActiveThorns = \"", SpaceSeparated[list], "\"\n"};

setParameter[thorn_, par_, value_] :=
  {thorn, " = ", If[NumberQ[value], ToString[value], "\"" <> value <> "\""], "\n"};

setParametersForThorn[thorn_, map_] :=
  Map[setParameter[thorn, #[[1]], #[[2]]] &, map];

insertFile[name_] := 
  Module[{istream_, contents_},
    istream = OpenRead[name];
    contents = ReadList[istream, String];
    Close[istream];
    contents];

vectoriseExpression[exprp_] :=
  Module[{isNotMinusOneQ, isNotTimesMinusOneQ, fmaRules, isNotKneg, arithRules, undoRules, expr},
    expr = exprp;

    (* FMA (fused multiply-add) instructions *)
    (* Note that -x is represented as Times[-1, x] *)
    isNotMinusOneQ[n_] := ! (IntegerQ[n] && n == -1);
    isNotTimesMinusOneQ[n_] := ! MatchQ[n,- _];
    fmaRules = {
      + (xx_? isNotMinusOneQ) (yy_? isNotMinusOneQ) + (zz_? isNotTimesMinusOneQ) :> kmadd [xx,yy,zz],
      + (xx_? isNotMinusOneQ) (yy_? isNotMinusOneQ) - (zz_? isNotTimesMinusOneQ) :> kmsub [xx,yy,zz],
      - (xx_? isNotMinusOneQ) (yy_? isNotMinusOneQ) + (zz_? isNotTimesMinusOneQ) :> knmadd[xx,yy,zz],
      - (xx_? isNotMinusOneQ) (yy_? isNotMinusOneQ) - (zz_? isNotTimesMinusOneQ) :> knmsub[xx,yy,zz],
      + (xx_? isNotMinusOneQ) (yy_ + 1) -> kmadd [xx, yy, xx],
      + (xx_? isNotMinusOneQ) (yy_ - 1) -> kmsub [xx, yy, xx],
      - (xx_? isNotMinusOneQ) (yy_ + 1) -> knmadd[xx, yy, xx],
      - (xx_? isNotMinusOneQ) (yy_ - 1) -> knmsub[xx, yy, xx],
      kmadd[xx_, - yy_, zz_] -> knmsub[xx,yy,zz],
      kmsub[xx_, - yy_, zz_] -> knmadd[xx,yy,zz]
    };
    expr = expr //. fmaRules;

    (* Constants *)
    expr = expr /. xx_Integer/; xx!=-1 :> ToReal[xx];
    expr = expr /. xx_Real -> ToReal[xx];
    expr = expr /. - ToReal[xx_] -> ToReal[- xx];
    expr = expr /. ToReal[xx_] + ToReal[yy_] -> ToReal[xx + yy];
    expr = expr /. ToReal[xx_] * ToReal[yy_] -> ToReal[xx * yy];
    expr = expr /. pow[xx_, ToReal[power_]] -> pow[xx, power];
    expr = expr /. ToReal[xx_] == ToReal[yy_] -> ToReal[xx == yy];
    expr = expr /. ToReal[xx_] != ToReal[yy_] -> ToReal[xx != yy];
    (* keep the conditional expression a scalar *)
    expr = expr /. IfThen[ToReal[xx_], yy_, zz_] -> IfThen[xx, yy, zz];

    (* Replace all operators and functions *)
    (* kneg, kadd, ksub, kmul, kdiv *)
    (* TODO: optimise fabs etc. with regard to fmadd etc. as well *)
    isNotKneg[n_] := ! MatchQ[n,kneg[_]];
    arithRules = {
      - xx_ -> kneg[xx],
      xx_ * yy_ -> kmul[xx,yy],
      xx_ / yy_ -> kdiv[xx,yy],
      xx_ + yy_ -> kadd[xx,yy],
      xx_ - yy_ -> ksub[xx,yy],
      kmul[-1,xx_]                     -> kneg[xx],
      kadd[xx_,kneg[yy_]]              -> ksub[xx,yy],
      kadd[kneg[xx_],(yy_? isNotKneg)] :> ksub[yy,xx],
      Abs[xx_]      -> kfabs[xx],
      Log[xx_]      -> klog[xx],
      fabs[xx_]     -> kfabs[xx],
      fmax[xx_,yy_] -> kfmax[xx,yy],
      fmin[xx_,yy_] -> kfmin[xx,yy],
      sqrt[xx_]     -> ksqrt[xx],
      exp[xx_]      -> kexp[xx],
      log[xx_]      -> klog[xx],
      pow[xx_,yy_]  -> kpow[xx,yy],
      kfabs[kneg[xx_]]  -> kfabs[xx],
      kfnabs[kneg[xx_]] -> kfnabs[xx],
      kneg[kfabs[xx_]]  -> kfnabs[xx]
    };
    expr = expr //. arithRules;

    (* Undo some transformations *)
    undoRules = {
      IfThen[kmul[xx_, yy_], aa_, bb_] -> IfThen[xx*yy, aa, bb],
      IfThen[kmul[xx_, yy_] != zz_, aa_, bb_] -> IfThen[xx*yy!=zz, aa, bb],
      ToReal[kneg[xx_]] -> ToReal[-xx],
      ToReal[kmul[xx_, yy_]] -> ToReal[xx*yy],
      kpow[xx_, kneg[power_]] -> kpow[xx, -power]
    };
    expr = expr //. undoRules;
    Return[expr]];

(* Take an expression x and replace occurrences of Powers with the C
macros SQR, CUB, QAD *)
ReplacePowers[expr_, vectorise_] :=
  Module[{rhs},
    rhs = expr /. Power[xx_, -1] -> INV[xx];

    If[SOURCELANGUAGE == "C",
           Module[{},
	     rhs = rhs /. Power[xx_,  2  ] -> SQR[xx];
	     rhs = rhs /. Power[xx_,  3  ] -> CUB[xx];
	     rhs = rhs /. Power[xx_,  4  ] -> QAD[xx];
	     rhs = rhs /. Power[xx_, -2  ] -> INV[SQR[xx]];
             rhs = rhs /. Power[xx_,  1/2] -> sqrt[xx];
             rhs = rhs /. Power[xx_, -1/2] -> INV[sqrt[xx]];
             rhs = rhs /. Power[xx_,  0.5] -> sqrt[xx];
             rhs = rhs /. Power[xx_, -0.5] -> INV[sqrt[xx]];

             (*
	     rhs = rhs /.  1/2 ->  khalf
	     rhs = rhs /. -1/2 -> -khalf;

	     rhs = rhs /.  1/3 ->  kthird;
	     rhs = rhs /. -1/3 -> -kthird;

	     rhs = rhs /.  2/3 ->  ktwothird;
	     rhs = rhs /. -2/3 -> -ktwothird;

	     rhs = rhs /.  4/3 ->  kfourthird;
	     rhs = rhs /. -4/3 -> -kfourthird;

	     rhs = rhs /.  8/3 ->  keightthird;
	     rhs = rhs /. -8/3 -> -keightthird;
             *)

             (* Avoid rational numbers *)
	     rhs = rhs /. Rational[xx_,yy_] :> N[xx/yy, 30];

             rhs = rhs //.      IfThen[cond1_,xx1_,yy1_] +      IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> IfThen[cond1, Simplify[    xx1 +     xx2], Simplify[    yy1 +     yy2]];
             rhs = rhs //. ff1_ IfThen[cond1_,xx1_,yy1_] +      IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> IfThen[cond1, Simplify[ff1 xx1 +     xx2], Simplify[ff1 yy1 +     yy2]];
             rhs = rhs //.      IfThen[cond1_,xx1_,yy1_] + ff2_ IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> IfThen[cond1, Simplify[    xx1 + ff2 xx2], Simplify[    yy1 + ff2 yy2]];
             rhs = rhs //. ff1_ IfThen[cond1_,xx1_,yy1_] + ff2_ IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> IfThen[cond1, Simplify[ff1 xx1 + ff2 xx2], Simplify[ff1 yy1 + ff2 yy2]];

             (* Is this still a good idea when FMA instructions are used? *)
	     rhs = rhs //. xx_ yy_ + xx_ zz_ -> xx (yy+zz);
	     rhs = rhs //. xx_ yy_ - xx_ zz_ -> xx (yy-zz);

             rhs = rhs /. Power[E, power_] -> exp[power];

             (* there have been some problems doing the Max/Min
                replacement via the preprocessor for C, so we do it
                here *)
             rhs = rhs /. Max[xx_, yy_] -> fmax[xx, yy];
             rhs = rhs /. Min[xx_, yy_] -> fmin[xx, yy];

             rhs = rhs /. Power[xx_, power_] -> pow[xx, power];

             If[vectorise === True,
              rhs = vectoriseExpression[rhs]];
           ],

           rhs = rhs /. Power[xx_, power_] -> xx^power
       ];
(*       Print[rhs//FullForm];*)
    rhs
    ];

(* Convert an expression to CForm, but remove the quotes from any
   strings present *)
CFormHideStrings[x_, opts___] := StringReplace[ToString[CForm[x,opts]], "\"" -> ""];



Quote[x_] := {"\"", x, "\""};

End[];

EndPackage[];
