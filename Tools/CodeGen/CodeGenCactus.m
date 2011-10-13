
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

AssignVariableInLoop::usage = "AssignVariableInLoop[dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
StoreVariableInLoop::usage = "StoreVariableInLoop[dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
StoreLowPartialVariableInLoop::usage = "StoreLowPartialVariableInLoop[dest_, src_, count_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
StoreHighPartialVariableInLoop::usage = "StoreHighPartialVariableInLoop[dest_, src_, count_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
StoreMiddlePartialVariableInLoop::usage = "StoreMiddlePartialVariableInLoop[dest_, src_, countLow_, countHigh_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
StorePartialVariableInLoop::usage = "StorePartialVariableInLoop[dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
DeclareAssignVariableInLoop::usage = "DeclareAssignVariableInLoop[type_, dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
MaybeAssignVariableInLoop::usage = "MaybeAssignVariableInLoop[dest_, src_, cond_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
DeclareMaybeAssignVariableInLoop::usage = "DeclareMaybeAssignVariableInLoop[type_, dest_, src_, cond_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
TestForNaN::usage = "TestForNaN[expr_] returns a block of code " <>
  "that tests 'expr' for nan.";
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
ConditionalOnParameterTextual::usage = "";
DeclareFDVariables::usage = "";
InitialiseFDVariables::usage = "";
ReplacePowers::usage = "";
BoundaryLoop::usage = "";
BoundaryWithGhostsLoop::usage = "";
GenericGridLoop::usage = "";
NameRoot::usage = "";
PartitionVarList::usage = "";
DataType::usage = "DataType[] returns a string for the grid function data type (e.g. CCTK_REAL)";
SetDataType::usage = "SetDataType[type] sets a string for the grid function data type (e.g. CCTK_REAL)";

Begin["`Private`"];

DefFn[
  SetDataType[type_String] :=
  dataType = type];

DefFn[
  DataType[] :=
  If[dataType === Symbol["datatype"],
     Throw["DataType: Have not set a data type"],
     dataType]];

DefFn[
  AssignVariableInLoop[dest:(_String|_Symbol), src:CodeGenBlock, vectorise_:False] :=
  Module[
    {loader},
    loader[x_] := If[vectorise, {"vec_load(", x, ")"}, x];
    {dest, " = ", loader[src], EOL[]}]];

DefFn[
  StoreVariableInLoop[dest:(_String|_Symbol), src:(_String|_Symbol)] :=
  {"vec_store_nta(", dest, ",", src, ")", EOL[]}];

DefFn[
  StoreLowPartialVariableInLoop[dest:(_String|_Symbol), src:(_String|_Symbol), count_String] :=
  {"vec_store_nta_partial_lo(", dest, ",", src, ",", count, ")", EOL[]}];

DefFn[
  StoreHighPartialVariableInLoop[dest:(_String|_Symbol), src:(_String|_Symbol), count_String] :=
  {"vec_store_nta_partial_hi(", dest, ",", src, ",", count, ")", EOL[]}];

DefFn[
  StoreMiddlePartialVariableInLoop[dest:(_String|_Symbol), src:(_String|_Symbol), countLow_String, countHigh_String] :=
  {"vec_store_nta_partial_mid(", dest, ",", src, ",", countLow, ",", countHigh, ")", EOL[]}];

DefFn[
  StorePartialVariableInLoop[dest:(_String|_Symbol), src:(_String|_Symbol)] :=
  {"vec_store_nta_partial(", dest, ",", src, ")", EOL[]}];

DefFn[
  DeclareAssignVariableInLoop[type_String, dest:(_String|_Symbol), src:(_String|_Symbol)] :=
  {type, " const ", dest, " = vec_load(", src, ")", EOL[]}];

DefFn[
  MaybeAssignVariableInLoop[dest:(_String|_Symbol), src:(_String|_Symbol), cond:Boolean] :=
  If[cond,
     {dest, " = useMatter ? vec_load(", src, ") : ToReal(0.0)", EOL[]},
     {dest, " = vec_load(", src, ")", EOL[]}]];

DefFn[
  DeclareMaybeAssignVariableInLoop[type_String, dest:(_String|_Symbol), src:(_String|_Symbol),
                                 mmaCond:Boolean, codeCond:CodeGenBlock,
                                 vectorise:Boolean:False] :=
  Module[
    {loader},
    loader[x_] := If[vectorise, {"vec_load(", x, ")"}, x];
    If[mmaCond,
       {type, " ", dest, " = (", codeCond, ") ? ", loader[src], " : ToReal(0.0)", EOL[]},
       {type, " ", dest, " = ", loader[src], EOL[]}]]];

DefFn[
  TestForNaN[expr:CodeGenBlock] :=
  {"if (isnan(", expr, ")) {\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"NaN found\");\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"ipos: %d %d %d\", i, j, k);\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"lbnd: %d %d %d\", cctk_lbnd[0], cctk_lbnd[1], cctk_lbnd[2]);\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"lsh: %d %d %d\", cctk_lsh[0], cctk_lsh[1], cctk_lsh[2]);\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"LSSH: %d %d %d\", CCTK_LSSH(0,0), CCTK_LSSH(0,1), CCTK_LSSH(0,2));\n",
   "  CCTK_VInfo(CCTK_THORNSTRING, \"", expr, ": %.17g\", (double)", expr, ");\n",
   "}\n"}];

DefFn[
  loopOverInteger[name_String, start_String, endplusone_String, block:CodeGenBlock] :=
  If[SOURCELANGUAGE == "C" || SOURCELANGUAGE == "C++",
     {"for (", name, " = ", start, "; ", name, " < ", endplusone, "; ", name, "++)\n",
      "{\n",
      IndentBlock[block],
      "}\n"},
     
     {"Do ", name, " = ", start, ", ", endplusone, "\n",
      "\n",
      IndentBlock[block],
      "End Do\n"}
    ]];

(* This is a Cactus-callable function *)
DefFn[
  DefineCCTKFunction[name_String, type_String, contents:CodeGenBlock] :=
  DefineFunction[
    name, "extern \"C\" " <> type, "CCTK_ARGUMENTS", 
    {
      "DECLARE_CCTK_ARGUMENTS;\n",
      "DECLARE_CCTK_PARAMETERS;\n\n",
      contents
    }]];

(* This is a Cactus-callable subroutine *)
DefFn[
  DefineCCTKSubroutine[name_String, contents:CodeGenBlock] :=
  DefineSubroutine[
    name, "CCTK_ARGUMENTS", 
    {
      "DECLARE_CCTK_ARGUMENTS;\n",
      "DECLARE_CCTK_PARAMETERS;\n\n",
      contents
    }]];

DefFn[
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
  CommentedBlock["Declare finite differencing variables", {}]];

DefFn[
  InitialiseFDSpacingVariablesC[] :=
  {
    (* DeclareAssignVariable["ptrdiff_t", "di", "CCTK_GFINDEX3D(cctkGH,1,0,0) - CCTK_GFINDEX3D(cctkGH,0,0,0)"], *)
    DeclareAssignVariable["ptrdiff_t", "di", "1"],
    DeclareAssignVariable["ptrdiff_t", "dj", "CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0)"],
    DeclareAssignVariable["ptrdiff_t", "dk", "CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0)"],
    DeclareAssignVariable["ptrdiff_t", "cdi", "sizeof(CCTK_REAL) * di"],
    DeclareAssignVariable["ptrdiff_t", "cdj", "sizeof(CCTK_REAL) * dj"],
    DeclareAssignVariable["ptrdiff_t", "cdk", "sizeof(CCTK_REAL) * dk"],
    DeclareAssignVariable[DataType[], "dx", "ToReal(CCTK_DELTA_SPACE(0))"],
    DeclareAssignVariable[DataType[], "dy", "ToReal(CCTK_DELTA_SPACE(1))"],
    DeclareAssignVariable[DataType[], "dz", "ToReal(CCTK_DELTA_SPACE(2))"],
    DeclareAssignVariable[DataType[], "dt", "ToReal(CCTK_DELTA_TIME)"],
    DeclareAssignVariable[DataType[], "t", "ToReal(cctk_time)"]}];

DefFn[
  InitialiseFDSpacingVariablesFortran[] := 
  {
    AssignVariable["dt", "CCTK_DELTA_TIME"],
    AssignVariable["dx", "CCTK_DELTA_SPACE(1)"],
    AssignVariable["dy", "CCTK_DELTA_SPACE(2)"],
    AssignVariable["dz", "CCTK_DELTA_SPACE(3)"]
  }];

DefFn[
  InitialiseFDVariables[vectorise:Boolean] :=
  CommentedBlock[
    "Initialise finite differencing variables",
    {
      If[SOURCELANGUAGE == "Fortran",
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
         (* else *)
         {DeclareAssignVariable[DataType[], "khalf", "0.5"],
          DeclareAssignVariable[DataType[], "kthird", "1/3.0"],
          DeclareAssignVariable[DataType[], "ktwothird", "2.0/3.0"],
          DeclareAssignVariable[DataType[], "kfourthird", "4.0/3.0"],
          DeclareAssignVariable[DataType[], "keightthird", "8.0/3.0"],
          DeclareAssignVariable[DataType[], "hdxi", "0.5 * dxi"],
          DeclareAssignVariable[DataType[], "hdyi", "0.5 * dyi"],
          DeclareAssignVariable[DataType[], "hdzi", "0.5 * dzi"]}]}]];

DefFn[
  GridName[x_Symbol] :=
  If[SOURCELANGUAGE == "C",
     ToString[x] <> "[index]",
     ToString[x] <> "(i,j,k)"]];

DefFn[
  DeclareGridLoopVariables[] :=
  SeparatedBlock[
    {InsertComment["Declare the variables used for looping over grid points"],
     Map[DeclareVariables[#, "CCTK_INT"] &, 
         {{"i", "j", "k"}
          (*, {"istart", "jstart", "kstart"}, 
            {"iend", "jend", "kend"},
            {"index_offset_x", "index_offset_y", "index_offset_z", "dir", "face"} *)}]
     (*, Map[DeclareArray[#, 6, "CCTK_INT"] &, {"is_symbnd", "is_physbnd", "is_ipbnd"}],
       Map[DeclareArray[#, 3, "CCTK_INT"] &, {"imin", "imax", "bmin", "bmax"}] *), 
     If[SOURCELANGUAGE == "C", DeclareVariable["index", "// CCTK_INT"], "\n"]}]];

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
  InitialiseGridLoopVariables[derivativesUsedSwitch:Boolean, addToStencilWidth_Integer] :=
  CommentedBlock[
    "Set up variables used in the grid loop for the physical grid points",

    If[derivativesUsedSwitch,
       {AssignVariable["index_offset_x", max[] <>"(stencil_width, stencil_width_x) + " <> ToString[addToStencilWidth]],
        AssignVariable["index_offset_y", max[] <>"(stencil_width, stencil_width_y) + " <> ToString[addToStencilWidth]],
        AssignVariable["index_offset_z", max[] <>"(stencil_width, stencil_width_z) + " <> ToString[addToStencilWidth]],
        "\n",
        AssignVariable["istart", arrayIndex["index_offset_x"]],
        AssignVariable["jstart", arrayIndex["index_offset_y"]],
        AssignVariable["kstart", arrayIndex["index_offset_z"]],
        "\n",
        AssignVariable["iend", {"CCTK_LSSH(0,0) - index_offset_x"}],
        AssignVariable["jend", {"CCTK_LSSH(0,1) - index_offset_y"}],
        AssignVariable["kend", {"CCTK_LSSH(0,2) - index_offset_z"}]},
       (* else *)
       {AssignVariable["istart", arrayIndex[0]],
        AssignVariable["jstart", arrayIndex[0]],
        AssignVariable["kstart", arrayIndex[0]],
        "\n",
        AssignVariable["iend", "CCTK_LSSH(0,0)"],
        AssignVariable["jend", "CCTK_LSSH(0,1)"],
        AssignVariable["kend", "CCTK_LSSH(0,2)"]}]]];

DefFn[
  ConditionalOnParameter[name_String, value_String, block:CodeGenBlock] :=
  SeparatedBlock[
    {"if (CCTK_EQUALS(", name, ", \"", value, "\"))\n",
     "{\n",
     IndentBlock[block],
     "}\n"}]];

DefFn[
  ConditionalOnParameterTextual[text:CodeGenBlock, block:CodeGenBlock] :=
  SeparatedBlock[
    {"if (", text, ")\n",
     "{\n",
     IndentBlock[block],
     "}\n"}]];

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
        IndentBlock[
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
        IndentBlock[block],
        "LC_ENDLOOP3 (unnamed)\n"
      }
    ]
  ];
*)

Options[GenericGridLoop] = ThornOptions;

DefFn[
  GenericGridLoop[functionName_String, block:CodeGenBlock, opts:OptionsPattern[]] :=
  If[OptionValue[UseLoopControl],
     GenericGridLoopUsingLoopControl[functionName, block, OptionValue[UseVectors]],
     GenericGridLoopTraditional[block]]];

DefFn[
  GenericGridLoopTraditional[block:CodeGenBlock] :=
  CommentedBlock[
    "Loop over the grid points",
    loopOverInteger[
      "k", "imin[2]", "imax[2]",
      loopOverInteger[
        "j", "imin[1]", "imax[1]",
        loopOverInteger[
          "i", "imin[0]", "imax[0]",
          {If[SOURCELANGUAGE == "C",
              DeclareAssignVariable["int", "index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"],
              ""],
            block}]]]]];

DefFn[
  GenericGridLoopUsingLoopControl[functionName_String, block:CodeGenBlock, vectorise:Boolean] :=
  If[SOURCELANGUAGE == "C",  
     CommentedBlock[
       "Loop over the grid points",
       {"#pragma omp parallel\n",
        If[vectorise, "LC_LOOP3VEC", "LC_LOOP3"],
        " (", functionName, ",\n",
        "  i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],\n",
        "  cctk_lsh[0],cctk_lsh[1],cctk_lsh[2]",
        If[vectorise, {",\n", "  CCTK_REAL_VEC_SIZE"}, ""],
        ")\n",
        "{\n",
        IndentBlock[
          {(* DeclareVariable["index", "// int"], *)
           (* DeclareAssignVariable["int", "index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"], *)
           DeclareAssignVariable["ptrdiff_t", "index", "di*i + dj*j + dk*k"],
           block}], "}\n",
        If[vectorise, "LC_ENDLOOP3VEC", "LC_ENDLOOP3"] <> " (", functionName, ");\n"}],
     (* else *)
     ""]];

DefFn[
  BoundaryLoop[block:CodeGenBlock] :=
  {
    "\nGenericFD_GetBoundaryInfo(cctkGH, cctk_lsh, cctk_lssh, cctk_bbox, cctk_nghostzones, imin, imax, is_symbnd, is_physbnd, is_ipbnd);\n",

    CommentedBlock[
      "Start by looping over the whole grid, minus the NON-PHYSICAL boundary points, which are set by synchronization.  ", {
        AssignVariable[arrayElement["bmin", 0], "is_physbnd[0*2+0] ? 0 : imin[0]"],
        AssignVariable[arrayElement["bmin", 1], "is_physbnd[1*2+0] ? 0 : imin[1]"],
        AssignVariable[arrayElement["bmin", 2], "is_physbnd[2*2+0] ? 0 : imin[2]"],
        AssignVariable[arrayElement["bmax", 0], "is_physbnd[0*2+1] ? CCTK_LSSH(0,0) : imax[0]"],
        AssignVariable[arrayElement["bmax", 1], "is_physbnd[1*2+1] ? CCTK_LSSH(0,1) : imax[1]"],
        AssignVariable[arrayElement["bmax", 2], "is_physbnd[2*2+1] ? CCTK_LSSH(0,2) : imax[2]"]}], 

    CommentedBlock[
      "Loop over all faces",
      loopOverInteger[
        "dir", "0", "3",
        loopOverInteger[
          "face", "0", "2",
          {CommentedBlock[
            "Now restrict to only the boundary points on the current face",
            SwitchStatement[
              "face", 
              {0,  {AssignVariable[arrayElement["bmax", "dir"], {arrayElement["imin", "dir"], ""}], 
                    AssignVariable[arrayElement["bmin", "dir"], {0, ""}]}},
              {1,  {AssignVariable[arrayElement["bmin", "dir"], {arrayElement["imax", "dir"], "" }],
                    AssignVariable[arrayElement["bmax", "dir"], {"CCTK_LSSH(0,dir)", ""}]}}]],
           conditional[
             arrayElement["is_physbnd", "dir * 2 + face"],
             loopOverInteger[
               "k", arrayElement["bmin",2], arrayElement["bmax",2],
               loopOverInteger[
                 "j", arrayElement["bmin",1], arrayElement["bmax",1],
                 loopOverInteger[
                   "i", arrayElement["bmin",0], arrayElement["bmax",0],
                   {If[SOURCELANGUAGE == "C", AssignVariable["index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"], ""],
                    block}]]]]}]]]}];

DefFn[
  BoundaryWithGhostsLoop[block:CodeGenBlock] :=
  {
  "\nGenericFD_GetBoundaryInfo(cctkGH, cctk_lsh, cctk_lssh, cctk_bbox, cctk_nghostzones, imin, imax, is_symbnd, is_physbnd, is_ipbnd);\n",

  CommentedBlock[
    "Start by looping over the whole grid, including the NON-PHYSICAL boundary points.  ", {
  AssignVariable[arrayElement["bmin", 0], "0"],
  AssignVariable[arrayElement["bmin", 1], "0"],
  AssignVariable[arrayElement["bmin", 2], "0"],
  AssignVariable[arrayElement["bmax", 0], "CCTK_LSSH(0,0)"],
  AssignVariable[arrayElement["bmax", 1], "CCTK_LSSH(0,1)"],
  AssignVariable[arrayElement["bmax", 2], "CCTK_LSSH(0,2)"]}], 

  CommentedBlock[
    "Loop over all faces",
   loopOverInteger[
     "dir", "0", "3",
     loopOverInteger[
       "face", "0", "2",
     {
      CommentedBlock[
        "Now restrict to only the boundary points on the current face",
       SwitchStatement[
         "face", 
        {0,  {AssignVariable[arrayElement["bmax", "dir"], {arrayElement["imin", "dir"], ""}], 
              AssignVariable[arrayElement["bmin", "dir"], {0, ""}]}},
        {1,  {AssignVariable[arrayElement["bmin", "dir"], {arrayElement["imax", "dir"], "" }],
              AssignVariable[arrayElement["bmax", "dir"], {"CCTK_LSSH(0,dir)]", ""}]}}]],
       conditional[arrayElement["is_physbnd", "dir * 2 + face"],
         loopOverInteger[
           "k", arrayElement["bmin",2], arrayElement["bmax",2],
           loopOverInteger[
             "j", arrayElement["bmin",1], arrayElement["bmax",1],
             loopOverInteger[
               "i", arrayElement["bmin",0], arrayElement["bmax",0],

         {If[SOURCELANGUAGE == "C", AssignVariable["index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"], ""],
	   block}]]]]}]]]}];

DefFn[
  onceInGridLoop[block:CodeGenBlock] :=
  Conditional[
    "i == 5 && j == 5 && k == 5",
    block]];

DefFn[
  InfoVariable[name_String] :=
  onceInGridLoop[
    {"char buffer[255];\n",
     "sprintf(buffer,\"" , name , " == %f\", " , name , ");\n",
     "CCTK_INFO(buffer);\n"}]];

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
  vectoriseExpression[exprp_] :=
  Module[
    {isNotMinusOneQ, isNotTimesMinusOneQ, fmaRules, isNotKneg, arithRules, undoRules, expr},
    expr = exprp;

    (* Constants *)
    (* expr = expr /. xx_Integer/; xx!=-1 :> ToReal[xx]; *)
    expr = expr /. xx_Integer -> ToReal[xx];
    expr = expr /. xx_Real -> ToReal[xx];

    removeToRealRules =
    {-ToReal[xx_] -> ToReal[- xx],
     ToReal[xx_] + ToReal[yy_] -> ToReal[xx + yy],
     ToReal[xx_] - ToReal[yy_] -> ToReal[xx - yy],
     ToReal[xx_] * ToReal[yy_] -> ToReal[xx * yy],
     ToReal[xx_] == ToReal[yy_] -> ToReal[xx == yy],
     ToReal[xx_] != ToReal[yy_] -> ToReal[xx != yy],
     pow[xx_, ToReal[power_]] -> pow[xx, power]};

    expr = expr //. removeToRealRules;

    (* Replace all operators and functions *)
    (* kneg, kadd, ksub, kmul, kdiv *)
    isNotKneg[n_] := ! MatchQ[n,kneg[_]];

    arithRules =
    { - xx_ -> kneg[xx],
      xx_ * yy_ -> kmul[xx,yy],
      xx_ / yy_ -> kdiv[xx,yy],
      xx_ + yy_ -> kadd[xx,yy],
      xx_ - yy_ -> ksub[xx,yy],
      kmul[-1,xx_]            -> kneg[xx],
      kmul[xx_,-1]            -> kneg[xx],
      kmul[ToReal[-1],xx_]    -> kneg[xx],
      kmul[xx_,ToReal[-1]]    -> kneg[xx],
      ToReal[- xx_]           -> kneg[ToReal[xx]],
      (* kmul[xx_,INV[yy_]]      -> kdiv[xx,yy], *)
      (* kmul[INV[xx_],yy_]      -> kdiv[yy,xx], *)
      kdiv[xx_,kdiv[yy_,zz_]] -> kdiv[kmul[xx,zz],yy],
      kdiv[kdiv[xx_,yy_],zz_] -> kdiv[xx,kmul[yy,zz]],
      kmul[kneg[xx_],yy_]     -> kneg[kmul[xx,yy]],
      kmul[xx_,kneg[yy_]]     -> kneg[kmul[xx,yy]],
      kdiv[kneg[xx_],yy_]     -> kneg[kdiv[xx,yy]],
      kdiv[xx_,kneg[yy_]]     -> kneg[kdiv[xx,yy]],
      kadd[kneg[xx_],yy_]     -> ksub[yy,xx],
      ksub[kneg[xx_],yy_]     -> kneg[kadd[xx,yy]],
      kadd[xx_,kneg[yy_]]     -> ksub[xx,yy],
      ksub[xx_,kneg[yy_]]     -> kadd[xx,yy],
      kneg[ksub[xx_,yy_]]     -> ksub[yy,xx],
      Abs[xx_]      -> kfabs[xx],
      Cos[xx_]      -> kcos[xx],
      Log[xx_]      -> klog[xx],
      Sin[xx_]      -> ksin[xx],
      Tan[xx_]      -> ktan[xx],
      exp[xx_]      -> kexp[xx],
      fabs[xx_]     -> kfabs[xx],
      fmax[xx_,yy_] -> kfmax[xx,yy],
      fmin[xx_,yy_] -> kfmin[xx,yy],
      log[xx_]      -> klog[xx],
      pow[xx_,yy_]  -> kpow[xx,yy],
      sqrt[xx_]     -> ksqrt[xx],
      kcos[kneg[xx_]]   -> kcos[xx],
      kfabs[kneg[xx_]]  -> kfabs[xx],
      kfnabs[kneg[xx_]] -> kfnabs[xx],
      kneg[kfabs[xx_]]  -> kfnabs[xx],
      kneg[kfnabs[xx_]] -> kfabs[xx],
      kneg[kneg[xx_]]   -> xx,
      ksin[kneg[xx_]]   -> kneg[ksin[xx]],
      ktan[kneg[xx_]]   -> kneg[ktan[xx]]};
    expr = expr //. arithRules;

    (* Undo some transformations *)
    undoRules = 
    { IfThen[_, aa_, aa_] -> aa,
      IfThen[xx_?IntegerQ, aa_, bb_] /; xx!=0 :> aa,
      IfThen[xx_?IntegerQ, aa_, bb_] /; xx==0 :> bb,
      IfThen[kmul[xx_,yy_], aa_, bb_] -> IfThen[xx*yy, aa, bb],
      IfThen[kmul[xx_,yy_] != zz_, aa_, bb_] -> IfThen[xx*yy!=zz, aa, bb],
      IfThen[ToReal[xx_], aa_, bb_] -> IfThen[xx, aa, bb],
      Scalar[kmul[xx_,yy_]] -> Scalar[xx*yy],
      Scalar[kmul[xx_,yy_] != zz_] -> Scalar[xx*yy!=zz],
      Scalar[ToReal[xx_]] -> Scalar[xx],
      Scalar[xx_ != ToReal[yy_]] -> Scalar[xx != yy],
      ToReal[kneg[xx_]] -> ToReal[-xx],
      ToReal[kadd[xx_,yy_]] -> ToReal[xx+yy],
      ToReal[ksub[xx_,yy_]] -> ToReal[xx-yy],
      ToReal[kmul[xx_,yy_]] -> ToReal[xx*yy],
      ToReal[xx_*kadd[yy_,zz_]] -> ToReal[xx*(yy+zz)],
      kpow[xx_, kneg[power_]] -> kpow[xx, -power]};
    expr = expr //. undoRules;
    
    (* FMA (fused multiply-add) instructions *)
    (* kmadd (x,y,z) =   xy+z
       kmsub (x,y,z) =   xy-z
       knmadd(x,y,z) = -(xy+z)
       knmsub(x,y,z) = -(xy-z) *)
    fmaRules =
    { kadd[kmul[xx_,yy_],zz_] -> kmadd[xx,yy,zz],
      kadd[zz_,kmul[xx_,yy_]] -> kmadd[xx,yy,zz],
      ksub[kmul[xx_,yy_],zz_] -> kmsub[xx,yy,zz],
      ksub[zz_,kmul[xx_,yy_]] -> knmsub[xx,yy,zz],
      kneg[kmadd [xx_,yy_,zz_]] -> knmadd[xx,yy,zz],
      kneg[kmsub [xx_,yy_,zz_]] -> knmsub[xx,yy,zz],
      kneg[knmadd[xx_,yy_,zz_]] -> kmadd [xx,yy,zz],
      kneg[knmsub[xx_,yy_,zz_]] -> kmsub [xx,yy,zz]
      (* we could match this and similar patterns
         kmul[xx_, kadd[yy_, ToReal[+1]]] -> kmadd[xx, yy, xx],
         kmul[xx_, kadd[yy_, ToReal[-1]]] -> kmsub[xx, yy, xx],
         *)};
    expr = expr //. fmaRules;
    
    Return[expr]]];

(* Take an expression x and replace occurrences of Powers with the C
  macros SQR, CUB, QAD *)
DefFn[
  ReplacePowers[expr_, vectorise:Boolean] :=
  Module[
    {rhs},
    rhs = expr /. Power[xx_, -1] -> INV[xx];
    If[SOURCELANGUAGE == "C",
       {rhs = rhs /. Power[xx_,  2  ] -> SQR[xx];
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
        
        (* Remove parentheses *)
        rhs = rhs //. Parenthesis[xx_] -> xx;

        (* Avoid rational numbers *)
        rhs = rhs /. Rational[xx_,yy_] :> N[xx/yy, 30];
        
        rhs = rhs //. IfThen[cond1_,xx1_,yy1_] + IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> 
        IfThen[cond1, Simplify[    xx1 +     xx2], Simplify[    yy1 +     yy2]];

        rhs = rhs //. ff1_ IfThen[cond1_,xx1_,yy1_] + IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :>
        IfThen[cond1, Simplify[ff1 xx1 +     xx2], Simplify[ff1 yy1 +     yy2]];

        rhs = rhs //.      IfThen[cond1_,xx1_,yy1_] + ff2_ IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> 
        IfThen[cond1, Simplify[    xx1 + ff2 xx2], Simplify[    yy1 + ff2 yy2]];

        rhs = rhs //. ff1_ IfThen[cond1_,xx1_,yy1_] + ff2_ IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> 
        IfThen[cond1, Simplify[ff1 xx1 + ff2 xx2], Simplify[ff1 yy1 + ff2 yy2]];

        (* Is this still a good idea when FMA instructions are used? *)
        rhs = rhs //. xx_ yy_ + xx_ zz_ -> xx (yy+zz);
        rhs = rhs //. xx_ yy_ - xx_ zz_ -> xx (yy-zz);

        rhs = rhs /. Power[E, power_] -> exp[power];

        (* there have been some problems doing the Max/Min
           replacement via the preprocessor for C, so we do it
           here *)
        (* Note: Mathematica simplifies Max[xx_] -> xx automatically *)
        rhs = rhs //. Max[xx_, yy__] -> fmax[xx, Max[yy]];
        rhs = rhs //. Min[xx_, yy__] -> fmin[xx, Min[yy]];

        rhs = rhs /. Power[xx_, power_] -> pow[xx, power];

        If[vectorise === True,
           rhs = vectoriseExpression[rhs]];

        (* Remove Scalar[] after vectorising *)
        rhs = rhs /. Scalar[xx_] -> xx},
       (* else *)
       rhs = rhs /. Power[xx_, power_] -> xx^power];
    (*       Print[rhs//FullForm];*)
    rhs]];

End[];

EndPackage[];
