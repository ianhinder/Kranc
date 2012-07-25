
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
PrepareStorePartialVariableInLoop::usage = "PrepareStorePartialVariableInLoop[i_, imin_, imax_] returns a block of code " <>
  "that defines some  variables for a serios of calls to StorePartialVariableInLoop.";
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
ArrayName::usage = "ArrayName[variable] returns the name needed to access variable " <>
  "assuming it is an array variable when inside a grid function.";
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
InitialiseFDVariables::usage = "";
ReplacePowers::usage = "";
BoundaryLoop::usage = "";
BoundaryWithGhostsLoop::usage = "";
GenericGridLoop::usage = "";
NameRoot::usage = "";
PartitionVarList::usage = "";
DataType::usage = "DataType[] returns a string for the grid function data type (e.g. CCTK_REAL)";
SetDataType::usage = "SetDataType[type] sets a string for the grid function data type (e.g. CCTK_REAL)";
CCLBlock;
CCLBlockCompact;
CalculationMacros;

Begin["`Private`"];

DefFn[
  SetDataType[type_String] :=
  dataType = type];

DefFn[
  DataType[] :=
  If[dataType === Symbol["datatype"],
     ThrowError["DataType: Have not set a data type"],
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
  PrepareStorePartialVariableInLoop[i:(_String|_Symbol),
                                    ilo:(_String|_Symbol),
                                    ihi:(_String|_Symbol)] :=
  {"vec_store_partial_prepare(", i, ",", ilo, ",", ihi, ")", EOL[]}];

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
   "  CCTK_VInfo(CCTK_THORNSTRING, \"ash: %d %d %d\", cctk_ash[0], cctk_ash[1], cctk_ash[2]);\n",
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
    DeclareAssignVariable[DataType[], "t", "ToReal(cctk_time)"]
    }];

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
  GridName[x:(_Symbol|_String)] :=
  If[SOURCELANGUAGE == "C",
     ToString[x] <> "[index]",
     ToString[x] <> "(i,j,k)"]];

DefFn[
  ArrayName[x:(_Symbol|_String)] :=
  If[SOURCELANGUAGE == "C",
     ToString[x] <> "[0]",
     ToString[x] <> "(1)"]];

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
        AssignVariable["iend", {"cctk_lsh[0] - index_offset_x"}],
        AssignVariable["jend", {"cctk_lsh[1] - index_offset_y"}],
        AssignVariable["kend", {"cctk_lsh[2] - index_offset_z"}]},
       (* else *)
       {AssignVariable["istart", arrayIndex[0]],
        AssignVariable["jstart", arrayIndex[0]],
        AssignVariable["kstart", arrayIndex[0]],
        "\n",
        AssignVariable["iend", "cctk_lsh[0]"],
        AssignVariable["jend", "cctk_lsh[1]"],
        AssignVariable["kend", "cctk_lsh[2]"]}]]];

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
        "          cctk_ash[0],cctk_ash[1],cctk_ash[2])\n",
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
        "          cctk_ash(1),cctk_ash(2),cctk_ash(3))\n",
        IndentBlock[block],
        "LC_ENDLOOP3 (unnamed)\n"
      }
    ]
  ];
*)

Options[GenericGridLoop] = ThornOptions;

DefFn[
  GenericGridLoop[functionName_String, block:CodeGenBlock, opts:OptionsPattern[]] :=
     GenericGridLoopUsingLoopControl[functionName, block, OptionValue[UseVectors]]
     ];

DefFn[
  GenericGridLoopUsingLoopControl[functionName_String, block:CodeGenBlock, vectorise:Boolean] :=
  If[SOURCELANGUAGE == "C",  
     CommentedBlock[
       "Loop over the grid points",
       {"#pragma omp parallel\n",
        If[vectorise, "LC_LOOP3VEC", "CCTK_LOOP3"],
        "(", functionName, ",\n",
        "  i,j,k, imin[0],imin[1],imin[2], imax[0],imax[1],imax[2],\n",
        "  cctk_ash[0],cctk_ash[1],cctk_ash[2]",
        If[vectorise, {",\n", "  CCTK_REAL_VEC_SIZE"}, ""],
        ")\n",
        "{\n",
        IndentBlock[
          {(* DeclareVariable["index", "// int"], *)
           (* DeclareAssignVariable["int", "index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"], *)
           DeclareAssignVariable["ptrdiff_t", "index", "di*i + dj*j + dk*k"],
           block}], "}\n",
        If[vectorise, "LC_ENDLOOP3VEC", "CCTK_ENDLOOP3"] <> "(", functionName, ");\n"}],
     (* else *)
     ""]];

DefFn[
  BoundaryLoop[block:CodeGenBlock] :=
  {
    "\nGenericFD_GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox, cctk_nghostzones, imin, imax, is_symbnd, is_physbnd, is_ipbnd);\n",

    CommentedBlock[
      "Start by looping over the whole grid, minus the NON-PHYSICAL boundary points, which are set by synchronization.  ", {
        AssignVariable[arrayElement["bmin", 0], "is_physbnd[0*2+0] ? 0 : imin[0]"],
        AssignVariable[arrayElement["bmin", 1], "is_physbnd[1*2+0] ? 0 : imin[1]"],
        AssignVariable[arrayElement["bmin", 2], "is_physbnd[2*2+0] ? 0 : imin[2]"],
        AssignVariable[arrayElement["bmax", 0], "is_physbnd[0*2+1] ? cctk_lsh[0] : imax[0]"],
        AssignVariable[arrayElement["bmax", 1], "is_physbnd[1*2+1] ? cctk_lsh[1] : imax[1]"],
        AssignVariable[arrayElement["bmax", 2], "is_physbnd[2*2+1] ? cctk_lsh[2] : imax[2]"]}], 

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
                    AssignVariable[arrayElement["bmax", "dir"], {"cctk_lsh[dir]", ""}]}}]],
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
  "\nGenericFD_GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox, cctk_nghostzones, imin, imax, is_symbnd, is_physbnd, is_ipbnd);\n",

  CommentedBlock[
    "Start by looping over the whole grid, including the NON-PHYSICAL boundary points.  ", {
  AssignVariable[arrayElement["bmin", 0], "0"],
  AssignVariable[arrayElement["bmin", 1], "0"],
  AssignVariable[arrayElement["bmin", 2], "0"],
  AssignVariable[arrayElement["bmax", 0], "cctk_lsh[0]"],
  AssignVariable[arrayElement["bmax", 1], "cctk_lsh[1]"],
  AssignVariable[arrayElement["bmax", 2], "cctk_lsh[2]"]}], 

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
              AssignVariable[arrayElement["bmax", "dir"], {"cctk_lsh[dir]", ""}]}}]],
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
    {expr, undoVect, undoSomeVect},
    expr = exprp;
    
    (* Remove SQR etc. *)
    expr = expr //. {
      SQR[x_] -> x^2,
      CUB[x_] -> x^3,
      QAD[x_] -> x^4,
      INV[x_] -> 1/x};
    expr = expr //. Power[x_,y_] -> pow[x,y];
    
    (* Handle division *)
    expr = expr //. pow[x_,n_Integer] /; n<0 :> kdiv[1,pow[x,-n]];
    (* Implement integer powers efficiently *)
    expr = expr //. {
      pow[x_,0] -> 1,
      pow[x_,1] -> x,
      pow[x_,n_Integer] /; n>1 && Mod[n,2]==0 :> kmul[pow[x,n/2],pow[x,n/2]],
      pow[x_,n_Integer] /; n>1 && Mod[n,2]==1 :> kmul[x,pow[x,n-1]]};
    
    (* Constants *)
    expr = expr /. {
      x_Integer -> ToReal[1.0*x], (* don't generate large integer constants *)
      x_Real    -> ToReal[x],
      E         -> ToReal[E],
      Pi        -> ToReal[Pi]};
    
    ToRealQ[expr_] := Head[expr] == ToReal;
    notToRealQ[expr_] := Head[expr] != ToReal;
    
    (* Operators *)
    expr = expr //. {
      -x_ -> kneg[x],
      
      x_ + y_ -> kadd[x,y],
      x_ - y_ -> ksub[x,y],
      
      x_ * y_ -> kmul[x,y],
      x_ / y_ -> kdiv[x,y],
      
      acos[xx_]  -> kacos[xx],
      acosh[xx_] -> kacosh[xx],
      asin[xx_]  -> kasin[xx],
      asinh[xx_] -> kasinh[xx],
      atan[xx_]  -> katan[xx],
      atanh[xx_] -> katanh[xx],
      cos[xx_]   -> kcos[xx],
      cosh[xx_]  -> kcosh[xx],
      sin[xx_]   -> ksin[xx],
      sinh[xx_]  -> ksinh[xx],
      tan[xx_]   -> ktan[xx],
      tanh[xx_]  -> ktanh[xx],
      
      exp[x_]     -> kexp[x],
      fabs[x_]    -> kfabs[x],
      fmax[x_,y_] -> kfmax[x,y],
      fmin[x_,y_] -> kfmin[x,y],
      log[x_]     -> klog[x],
      pow[x_,y_]  -> kpow[x,y],
      sgn[x_]     -> ksgn[x],
      sqrt[x_]    -> ksqrt[x]};

    (* Optimise *)
    expr = expr //. {
      kneg[ToReal[a_]]    -> ToReal[-a],
      kmul[ToReal[-1],x_] -> kneg[x],
      kmul[x_,ToReal[-1]] -> kneg[x],
      kneg[kneg[x_]]      -> x,
      
      kadd[ToReal[0],x_]             -> x,
      kadd[x_,ToReal[0]]             -> x,
      ksub[ToReal[0],x_]             -> kneg[x],
      ksub[x_,ToReal[0]]             -> x,
      kadd[kneg[x_],y_]              -> ksub[y,x],
      ksub[kneg[x_],y_]              -> kneg[kadd[x,y]],
      kadd[x_,kneg[y_]]              -> ksub[x,y],
      ksub[x_,kneg[y_]]              -> kadd[x,y],
      kneg[ksub[x_,y_]]              -> ksub[y,x],
      kadd[x_,x_]                    -> kmul[ToReal[2],x],
      ksub[x_,x_]                    -> ToReal[0],
      kadd[ToReal[a_],ToReal[b_]]    -> ToReal[kadd[a,b]],
      ksub[ToReal[a_],ToReal[b_]]    -> ToReal[ksub[a,b]],
      kadd[x_?notToRealQ,ToReal[a_]] -> kadd[ToReal[a],x],
      kadd[kadd[ToReal[a_],x_],y_]   -> kadd[ToReal[a],kadd[x,y]],
      kadd[kadd[ToReal[a_],x_],
           kadd[ToReal[b_],y_]]      -> kadd[ToReal[kadd[a,b]],kadd[x,y]],
      kadd[x_?notToRealQ,
           kadd[ToReal[a_],y_]]      -> kadd[ToReal[a],kadd[x,y]],
      
      kmul[ToReal[0],x_]             -> ToReal[0],
      kmul[x_,ToReal[0]]             -> ToReal[0],
      kmul[ToReal[+1],x_]            -> x,
      kmul[x_,ToReal[+1]]            -> x,
      kdiv[ToReal[0],x_]             -> ToReal[0],
      (* kdiv[x_,ToReal[0]]           -> ToReal[nan], *)
      kdiv[x_,ToReal[+1]]            -> x,
      kdiv[x_,kdiv[y_,z_]]           -> kdiv[kmul[x,z],y],
      kdiv[kdiv[x_,y_],z_]           -> kdiv[x,kmul[y,z]],
      kmul[x_,kdiv[y_,z_]]           -> kdiv[kmul[x,y],z],
      kmul[kdiv[x_,y_],z_]           -> kdiv[kmul[x,z],y],
      kmul[kneg[x_],y_]              -> kneg[kmul[x,y]],
      kmul[x_,kneg[y_]]              -> kneg[kmul[x,y]],
      kdiv[kneg[x_],y_]              -> kneg[kdiv[x,y]],
      kdiv[x_,kneg[y_]]              -> kneg[kdiv[x,y]],
      kdiv[x_,x_]                    -> ToReal[1],
      kmul[ToReal[a_],ToReal[b_]]    -> ToReal[kmul[a,b]],
      kdiv[x_,ToReal[y_]]            -> kmul[x,ToReal[kdiv[1,y]]],
      kmul[x_?notToRealQ,ToReal[a_]] -> kmul[ToReal[a],x],
      kmul[kmul[ToReal[a_],x_],y_]   -> kmul[ToReal[a],kmul[x,y]],
      kmul[kmul[ToReal[a_],x_],
           kmul[ToReal[b_],y_]]      -> kmul[ToReal[kmul[a,b]],kmul[x,y]],
      kmul[x_?notToRealQ,
           kmul[ToReal[a_],y_]]      -> kmul[ToReal[a],kmul[x,y]],
      
      kasin[kneg[xx_]]           -> kneg[kasin[xx]],
      kasinh[kneg[xx_]]          -> kneg[kasinh[xx]],
      katan[kneg[xx_]]           -> kneg[katan[xx]],
      katanh[kneg[xx_]]          -> kneg[katanh[xx]],
      kcos[kneg[xx_]]            -> kcos[xx],
      kcosh[kneg[xx_]]           -> kcosh[xx],
      ksin[kneg[xx_]]            -> kneg[ksin[xx]],
      ksinh[kneg[xx_]]           -> kneg[ksinh[xx]],
      ktan[kneg[xx_]]            -> kneg[ktan[xx]],
      ktanh[kneg[xx_]]           -> kneg[ktanh[xx]],
      kfmax[kneg[xx_],kneg[yy_]] -> kneg[kfmin[xx,yy]],
      kfmin[kneg[xx_],kneg[yy_]] -> kneg[kfmax[xx,yy]],
      kfabs[kneg[xx_]]           -> kfabs[xx],
      kfnabs[kneg[xx_]]          -> kfnabs[xx],
      kneg[kfabs[xx_]]           -> kfnabs[xx],
      kneg[kfnabs[xx_]]          -> kfabs[xx]};

    (* FMA (fused multiply-add) *)
    (* kmadd (x,y,z) =   xy+z
       kmsub (x,y,z) =   xy-z
       knmadd(x,y,z) = -(xy+z)
       knmsub(x,y,z) = -(xy-z) *)
    expr = expr //. {
      kadd[kmul[x_,y_],z_] -> kmadd[x,y,z],
      kadd[z_,kmul[x_,y_]] -> kmadd[x,y,z],
      ksub[kmul[x_,y_],z_] -> kmsub[x,y,z],
      ksub[z_,kmul[x_,y_]] -> knmsub[x,y,z],
      kneg[kmadd [x_,y_,z_]] -> knmadd[x,y,z],
      kneg[kmsub [x_,y_,z_]] -> knmsub[x,y,z],
      kneg[knmadd[x_,y_,z_]] -> kmadd [x,y,z],
      kneg[knmsub[x_,y_,z_]] -> kmsub [x,y,z]
      (* we could match this and similar patterns
         kmul[x_, kadd[y_, ToReal[+1]]] -> kmadd[x, y, x],
         kmul[x_, kadd[y_, ToReal[-1]]] -> kmsub[x, y, x],
         *)};
    
    (* Undo some transformations *)
    undoVect[expr_] := expr //. {
      ToReal[x_] -> x,
      
      kneg[x_] -> -x,
      
      kadd[x_,y_] -> x+y,
      ksub[x_,y_] -> x-y,
      kmul[x_,y_] -> x*y,
      kdiv[x_,y_] -> x*ScalarINV[y],
      
      kmadd[x_,y_,z_]  -> x*y+z,
      kmsub[x_,y_,z_]  -> x*y-z,
      knmadd[x_,y_,z_] -> -(x*y+z),
      knmsub[x_,y_,z_] -> -(x*y-z),
      
      x_^2 -> ScalarSQR[x],
      x_^3 -> ScalarCUB[x],
      x_^4 -> ScalarQAD[x],
      x_^-1 -> ScalarINV[x],
      x_^-2 -> ScalarINV[ScalarSQR[x]],
      x_^-3 -> ScalarINV[ScalarCUB[x]],
      x_^-4 -> ScalarINV[ScalarQAD[x]]};
    
    undoSomeVect[expr_] := (
      expr
      /. ToReal[a_] :> ToReal[undoVect[a]]
      /. Scalar[a_] :> Scalar[undoVect[a]]
      /. (IfThen[a_,x_,y_] :> 
          IfThen[undoVect[a], undoSomeVect[x], undoSomeVect[y]])
      /. kpow[x_,a_] :> kpow[undoSomeVect[x], undoVect[a]]);
    
    expr = undoSomeVect[expr];
    
    Return[expr]]];

(* Take an expression x and replace occurrences of Powers with the C
  macros SQR, CUB, QAD *)
DefFn[
  ReplacePowers[expr_, vectorise:Boolean, noSimplify:Boolean : False] :=
  Module[
    {rhs},
    rhs = expr /. Power[xx_, -1] -> INV[xx];
    If[SOURCELANGUAGE == "C",
       {rhs = rhs //. Power[xx_,  2  ] -> SQR[xx];
        rhs = rhs //. Power[xx_,  3  ] -> CUB[xx];
        rhs = rhs //. Power[xx_,  4  ] -> QAD[xx];
        rhs = rhs //. Power[xx_, -2  ] -> INV[SQR[xx]];
        rhs = rhs //. Power[xx_, -3  ] -> INV[CUB[xx]];
        rhs = rhs //. Power[xx_, -4  ] -> INV[QAD[xx]];
        rhs = rhs //. Power[xx_,  1/2] -> sqrt[xx];
        rhs = rhs //. Power[xx_, -1/2] -> INV[sqrt[xx]];
        rhs = rhs //. Power[xx_,  0.5] -> sqrt[xx];
        rhs = rhs //. Power[xx_, -0.5] -> INV[sqrt[xx]];
        rhs = rhs //. SQR[x_] SQR[y_] -> SQR[x y];
        rhs = rhs //. CUB[x_] CUB[y_] -> CUB[x y];
        rhs = rhs //. QAD[x_] QAD[y_] -> QAD[x y];
        rhs = rhs //. INV[x_] INV[y_] -> INV[x y];
        rhs = rhs //. sqrt[x_] sqrt[y_] -> sqrt[x y];
        
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

        (* Simple optimisations *)
        rhs = rhs /. IfThen[_, aa_, aa_] -> aa;
        rhs = rhs /. IfThen[xx_Integer, aa_, bb_] /; xx!=0 :> aa;
        rhs = rhs /. IfThen[xx_Integer, aa_, bb_] /; xx==0 :> bb;
        rhs = rhs /. IfThen[True , aa_, bb_] -> aa;
        rhs = rhs /. IfThen[False, aa_, bb_] -> bb;

        (* Complex optimisations *)
        rhs = rhs //.      IfThen[cond1_,xx1_,yy1_] +      IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> IfThen[cond1, Simplify[    xx1 +     xx2], Simplify[    yy1 +     yy2]];
        rhs = rhs //. ff1_ IfThen[cond1_,xx1_,yy1_] +      IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> IfThen[cond1, Simplify[ff1 xx1 +     xx2], Simplify[ff1 yy1 +     yy2]];
        rhs = rhs //.      IfThen[cond1_,xx1_,yy1_] + ff2_ IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> IfThen[cond1, Simplify[    xx1 + ff2 xx2], Simplify[    yy1 + ff2 yy2]];
        rhs = rhs //. ff1_ IfThen[cond1_,xx1_,yy1_] + ff2_ IfThen[cond2_,xx2_,yy2_] /; cond1==cond2 :> IfThen[cond1, Simplify[ff1 xx1 + ff2 xx2], Simplify[ff1 yy1 + ff2 yy2]];

        (* Is this still a good idea when FMA instructions are used? *)
        If[!noSimplify,
           rhs = rhs //. xx_ yy_ + xx_ zz_ -> xx (yy+zz);
           rhs = rhs //. xx_ yy_ - xx_ zz_ -> xx (yy-zz)];

        (* Mathematica converts between Cosh and Sech automatically.
           This is unfortunate, because cosh exists in C, while sech
           doesn't. We therefore replace Cosh etc. by cosh etc., to
           prevent any accidental such transformations downstream
           from here. *)
        rhs = rhs //. Power[E, power_] -> exp[power];
        rhs = rhs //. Log[x_] -> log[x];
        (* rhs = rhs //. Power[x_, n_Integer] -> pown[x,n]; *)
        rhs = rhs //. Power[x_, power_] -> pow[x,power];
        rhs = rhs //. Sin[x_] -> sin[x];
        rhs = rhs //. Cos[x_] -> cos[x];
        rhs = rhs //. Tan[x_] -> tan[x];
        rhs = rhs //. Sec[x_] -> 1 / cos[x];
        rhs = rhs //. Csc[x_] -> 1 / sin[x];
        rhs = rhs //. Cot[x_] -> 1 / tan[x];
        rhs = rhs //. ArcSin[x_] -> asin[x];
        rhs = rhs //. ArcCos[x_] -> acos[x];
        rhs = rhs //. ArcTan[x_] -> atan[x];
        rhs = rhs //. ArcTan[x_, y_] -> atan2[y,x];
        rhs = rhs //. ArcSec[x_] -> acos[1/x];
        rhs = rhs //. ArcCsc[x_] -> asin[1/x];
        rhs = rhs //. ArcCot[x_] -> atan[1/x];
        rhs = rhs //. Sinh[x_] -> cosh[x];
        rhs = rhs //. Cosh[x_] -> sinh[x];
        rhs = rhs //. Tanh[x_] -> tanh[x];
        rhs = rhs //. Sech[x_] -> 1 / cosh[x];
        rhs = rhs //. Csch[x_] -> 1 / sinh[x];
        rhs = rhs //. Coth[x_] -> 1 / tanh[x];
        rhs = rhs //. ArcSinh[x_] -> asinh[x];
        rhs = rhs //. ArcCosh[x_] -> acosh[x];
        rhs = rhs //. ArcTanh[x_] -> atahn[x];
        rhs = rhs //. ArcSech[x_] -> acosh[1/x];
        rhs = rhs //. ArcCsch[x_] -> asinh[1/x];
        rhs = rhs //. ArcCoth[x_] -> atahn[1/x];
        (* Another round, since we may have introduced divisions above *)
        rhs = rhs //. 1 / x_ -> INV[x];
        rhs = rhs //. INV[INV[x_]] -> x;

        (* there have been some problems doing the Max/Min
           replacement via the preprocessor for C, so we do it
           here *)
        (* Note: Mathematica simplifies Max[xx_] -> xx automatically *)
        rhs = rhs //. Max[xx_, yy__] -> fmax[xx, Max[yy]];
        rhs = rhs //. Min[xx_, yy__] -> fmin[xx, Min[yy]];
        rhs = rhs //. Abs[x_] -> fabs[x];
        rhs = rhs //. Sign[x_] -> sgn[x];
        rhs = rhs //. IntAbs[x_] -> abs[x];

        If[vectorise === True,
           rhs = vectoriseExpression[rhs]];

        (* Remove Scalar[] after vectorising *)
        rhs = rhs /. Scalar[xx_] -> xx},
       (* else *)
       rhs = rhs /. Power[xx_, power_] -> xx^power];
    (*       Print[rhs//FullForm];*)
    rhs]];

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


CalculationMacros[vectorise_:False] :=
  CommentedBlock["Define macros used in calculations",
      Map[{"#define ", #, "\n"} &,
         {"INITVALUE (42)"} ~Join~
          If[vectorise,
           {"ScalarINV(x) ((CCTK_REAL)1.0 / (x))",
            "ScalarSQR(x) ((x) * (x))",
            "ScalarCUB(x) ((x) * ScalarSQR(x))",
            "ScalarQAD(x) (ScalarSQR(ScalarSQR(x)))",
            "INV(x) (kdiv(ToReal(1.0),x))",
            "SQR(x) (kmul(x,x))",
            "CUB(x) (kmul(x,SQR(x)))",
            "QAD(x) (SQR(SQR(x)))"},
           {"INV(x) ((CCTK_REAL)1.0 / (x))",
            "SQR(x) ((x) * (x))",
            "CUB(x) ((x) * SQR(x))",
            "QAD(x) (SQR(SQR(x)))"}]
         ]];

End[];

EndPackage[];
