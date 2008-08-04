
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
    along with Foobar; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)
BeginPackage["sym`"];
{INV, SQR, CUB, QAD, exp, pow, fmax, fmin, dx, dy, dz, khalf, kthird, ktwothird, kfourthird, keightthird};

EndPackage[];

BeginPackage["CodeGen`", {"sym`", "Errors`"}];

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
AssignVariableInLoop::usage = "AssignVariable[dest_, src_] returns a block of code " <>
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
SubblockGridName::usage = ""

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

Begin["`Private`"];

SOURCELANGUAGE =  "C";
SOURCESUFFIX   = ".c";

setSourceSuffix[lang_] :=
If[ (lang == "C"),
  SOURCESUFFIX = ".c";
,
  SOURCESUFFIX = ".F90";
];


SetSourceLanguage[lang_] :=
If[ (lang == "C" || lang == "Fortran"),
  SOURCELANGUAGE = lang;
  setSourceSuffix[lang];
  InfoMessage[Terse, "User set source language to " <> lang],

  SOURCELANGUAGE = "C";
  setSourceSuffix[".c"];
  InfoMessage[Terse, "Setting Source Language to C"];
];

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


DeclareVariables[names_?ListQ, type_] := 
If[SOURCELANGUAGE == "C",
   {type, " ",    CommaInitSeparated@names, EOL[]},
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

AssignVariableInLoop[dest_, src_] :=
  {dest, " = ", src, EOL[]};
(*
  {dest, " = ", src, EOL[],
   TestForNaN[dest]};
*)

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
    {"void ", name, "(", args, ")", "\n",
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
  DefineFunction[name, type, "CCTK_ARGUMENTS", 
    {
      "DECLARE_CCTK_ARGUMENTS\n",
      "DECLARE_CCTK_PARAMETERS\n\n",
      contents
    }];

(* This is a Cactus-callable subroutine *)
DefineCCTKSubroutine[name_, contents_] :=
  defineSubroutine[
    name, "CCTK_ARGUMENTS", 
    {
      "DECLARE_CCTK_ARGUMENTS\n",
      "DECLARE_CCTK_PARAMETERS\n\n",
      contents
    }];

DeclareFDVariables[] := 
  CommentedBlock["Declare finite differencing variables",
    {Map[DeclareVariables[#, "CCTK_REAL"] &, {{"dx", "dy", "dz"}, 
                                              {"dxi", "dyi", "dzi"},
                                              {khalf,kthird,ktwothird,kfourthird,keightthird},
                                              {"hdxi", "hdyi", "hdzi"}}],
     "\n"}];

InitialiseFDSpacingVariablesC[] := 
  {
    AssignVariable["dx", "CCTK_DELTA_SPACE(0)"],
    AssignVariable["dy", "CCTK_DELTA_SPACE(1)"],
    AssignVariable["dz", "CCTK_DELTA_SPACE(2)"]
  };

InitialiseFDSpacingVariablesFortran[] := 
  {
    AssignVariable["dx", "CCTK_DELTA_SPACE(1)"],
    AssignVariable["dy", "CCTK_DELTA_SPACE(2)"],
    AssignVariable["dz", "CCTK_DELTA_SPACE(3)"]
  }


InitialiseFDVariables[] :=
  CommentedBlock["Initialise finite differencing variables",
  { If[SOURCELANGUAGE == "Fortran",
       InitialiseFDSpacingVariablesFortran[],
       InitialiseFDSpacingVariablesC[]],
    
    AssignVariable["dxi", "1.0 / dx"],
    AssignVariable["dyi", "1.0 / dy"],
    AssignVariable["dzi", "1.0 / dz"],
    AssignVariable["khalf", "0.5"],
    AssignVariable["kthird", "1/3.0"],
    AssignVariable["ktwothird", "2.0/3.0"],
    AssignVariable["kfourthird", "4.0/3.0"],
    AssignVariable["keightthird", "8.0/3.0"],
    AssignVariable["hdxi", "0.5 * dxi"],
    AssignVariable["hdyi", "0.5 * dyi"],
    AssignVariable["hdzi", "0.5 * dzi"]}];

GridName[x_] := If[SOURCELANGUAGE == "C",
                   ToExpression[ToString[x] <> "[index]"],
                   ToString[x] <> "(i,j,k)"
                ];

SubblockGridName[x_] := If[SOURCELANGUAGE == "C",
                   ToExpression[ToString[x] <> "[subblock_index]"],
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

     If[SOURCELANGUAGE == "C", DeclareVariable["index", "CCTK_INT"], "\n"],
     If[SOURCELANGUAGE == "C", DeclareVariable["subblock_index", "CCTK_INT"], "\n"]
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
        "          cctk_lssh[CCTK_LSSH_IDX(0,0)],cctk_lssh[CCTK_LSSH_IDX(0,1)],cctk_lssh[CCTK_LSSH_IDX(0,2)])\n",
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
        "          cctk_lssh(CCTK_LSSH_IDX(0,1)),cctk_lssh(CCTK_LSSH_IDX(0,2)),cctk_lssh(CCTK_LSSH_IDX(0,3)))\n",
        indentBlock[block],
        "LC_ENDLOOP3 (unnamed)\n"
      }
    ]
  ];
*)

GenericGridLoop[functionName_, useLoopControl_, block_] :=
  If[useLoopControl,
    GenericGridLoopUsingLoopControl[functionName, block],
    GenericGridLoopTraditional[block]];

GenericGridLoopTraditional[block_] :=
  CommentedBlock["Loop over the grid points",
   loopOverInteger["k", "min[2]", "max[2]",
     loopOverInteger["j", "min[1]", "max[1]",
       loopOverInteger["i", "min[0]", "max[0]",

       { If[SOURCELANGUAGE == "C",  
            {
              AssignVariable["index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"],
              AssignVariable["subblock_index", "i - min[0] + (max[0] - min[0]) * (j - min[1] + (max[1]-min[1]) * (k - min[2]))"]
            }
            ""],
	 block
       }
        ]]]];

GenericGridLoopUsingLoopControl[functionName_, block_] :=
  If[SOURCELANGUAGE == "C",  
    CommentedBlock["Loop over the grid points",
      {
        "#pragma omp parallel\n",
        "LC_LOOP3 (", functionName, ",\n",
        "          i,j,k, min[0],min[1],min[2], max[0],max[1],max[2],\n",
        "          cctk_lssh[CCTK_LSSH_IDX(0,0)],cctk_lssh[CCTK_LSSH_IDX(0,1)],cctk_lssh[CCTK_LSSH_IDX(0,2)])\n",
        "{\n",
        indentBlock[
          {
             DeclareVariable["index", "int"],
             DeclareVariable["subblock_index", "int"],
             AssignVariable["index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"],
             AssignVariable["subblock_index", "i - min[0] + (max[0] - min[0]) * (j - min[1] + (max[1]-min[1]) * (k - min[2]))"],
             block
          }
        ],
        "}\n",
        "LC_ENDLOOP3 (", functionName, ");\n"
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
  AssignVariable[arrayElement["bmax", 0], "is_physbnd[0*2+1] ? cctk_lssh[CCTK_LSSH_IDX(0,0)] : imax[0]"],
  AssignVariable[arrayElement["bmax", 1], "is_physbnd[1*2+1] ? cctk_lssh[CCTK_LSSH_IDX(0,1)] : imax[1]"],
  AssignVariable[arrayElement["bmax", 2], "is_physbnd[2*2+1] ? cctk_lssh[CCTK_LSSH_IDX(0,2)] : imax[2]"]}], 

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

conditional[condition_, block_] :=
 {"if (", condition, ")\n",
  CBlock[block]};

onceInGridLoop[block_] :=
  conditional["i == 5 && j == 5 && k == 5",
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

(* Take an expression x and replace occurrences of Powers with the C
macros SQR, CUB, QAD *)
ReplacePowers[x_] :=
  Module[{rhs},
    rhs = x /. Power[xx_, -1] -> INV[xx];

    If[SOURCELANGUAGE == "C",
           Module[{},
	     rhs = rhs //. Power[xx_, 2] -> SQR[xx];
	     rhs = rhs //. Power[xx_, 3] -> CUB[xx];
	     rhs = rhs //. Power[xx_, 4] -> QAD[xx];

	     rhs = rhs //. xx_/2 -> khalf xx;
	     rhs = rhs //. (-1/2) -> -khalf;

	     rhs = rhs //. xx_/3 -> kthird xx;
	     rhs = rhs //. (-1/3) -> -kthird;

	     rhs = rhs //. 2/3 -> ktwothird;
	     rhs = rhs //. (-2/3) -> -ktwothird;

	     rhs = rhs //. 4/3 -> kfourthird;
	     rhs = rhs //. (-4/3) -> -kfourthird;

	     rhs = rhs //. 8/3 -> keightthird;
	     rhs = rhs //. (-8/3) -> -keightthird;

	     rhs = rhs //. xx_ y_ + xx_ z_ -> xx(y+z); 

             rhs = rhs //. Power[E, power_] -> exp[power];
             rhs = rhs //. Power[xx_, 0.5] -> sqrt[xx];

             (* there have been some problems doing the Max/Min
                replacement via the preprocessor for C, so we do it
                here *)
             rhs = rhs //. Max[xx_, yy_] -> fmax[xx, yy];
             rhs = rhs //. Min[xx_, yy_] -> fmin[xx, yy];

             rhs = rhs //. Power[xx_, power_] -> pow[xx, power]],

           rhs = rhs //. Power[xx_, power_] -> xx^power
       ];
(*       Print[rhs//FullForm];*)
    rhs
    ];

(* Convert an expression to CForm, but remove the quotes from any
   strings present *)
CFormHideStrings[x_, opts___] := StringReplace[ToString[CForm[x,opts]], "\"" -> ""];

End[];



EndPackage[];

