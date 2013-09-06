
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

BeginPackage["CodeGenCactus`", {"Errors`", "Kranc`", "CodeGenC`", "CodeGen`", "Vectorisation`"}];

AssignVariableInLoop::usage = "AssignVariableInLoop[dest_, src_] returns a block of code " <>
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
  AssignVariableInLoop[dest:(_String|_Symbol), src:CodeGenBlock,
                       vectorise : False : False] :=
  {dest, " = ", src, EOL[]}];

DefFn[
  AssignVariableInLoop[dest:(_String|_Symbol), src:CodeGenBlock,
                       True] :=
  VectorisationAssignVariableInLoop[dest, src]];

DefFn[
  LoadVariable[x_] := x];

DefFn[
  DeclareMaybeAssignVariableInLoop[type_String, dest:(_String|_Symbol), src:(_String|_Symbol),
                                 mmaCond:Boolean, codeCond:CodeGenBlock,
                                 vectorise:Boolean:False] :=
  Module[
    {loadVariable = If[vectorise, VectorisationLoadVariable, LoadVariable]},
    If[mmaCond,
       {type, " ", dest, " CCTK_ATTRIBUTE_UNUSED = (", codeCond, ") ? ", loadVariable[src], " : ToReal(0.0)", EOL[]},
       {type, " ", dest, " CCTK_ATTRIBUTE_UNUSED = ", loadVariable[src], EOL[]}]]];

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
       { (* Circumvent a compiler bug on Blue Gene/Q *)
         "const int imin0=imin[0];\n",
         "const int imin1=imin[1];\n",
         "const int imin2=imin[2];\n",
         "const int imax0=imax[0];\n",
         "const int imax1=imax[1];\n",
         "const int imax2=imax[2];\n",
         (* "// #undef VEC_COUNT\n", *)
         (* "// #define VEC_COUNT(x) x\n", *)
         (* "// double vec_iter_timer;\n", *)
         (* "// {\n", *)
         (* "//   timeval tv;\n", *)
         (* "//   gettimeofday(&tv, NULL);\n", *)
         (* "//   vec_iter_timer = -(tv.tv_sec + 1.0e-6 * tv.tv_usec);\n", *)
         (* "// }\n", *)
         (* "// ptrdiff_t vec_iter_counter = 0;\n", *)
         (* "// ptrdiff_t vec_op_counter = 0;\n", *)
         (* "// ptrdiff_t vec_mem_counter = 0;\n", *)
         "#pragma omp parallel // reduction(+: vec_iter_counter, vec_op_counter, vec_mem_counter)\n",
         If[vectorise, "CCTK_LOOP3STR", "CCTK_LOOP3"],
         "(", functionName, ",\n",
         "  i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,\n",
         "  cctk_ash[0],cctk_ash[1],cctk_ash[2]",
         If[vectorise, {",\n", "  vecimin,vecimax, CCTK_REAL_VEC_SIZE"}, ""],
         ")\n",
         "{\n",
         IndentBlock[
           {(* DeclareVariable["index", "// int"], *)
            (* DeclareAssignVariable["int", "index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"], *)
            DeclareAssignVariable["ptrdiff_t", "index", "di*i + dj*j + dk*k"],
            If[vectorise,
               "// vec_iter_counter+=CCTK_REAL_VEC_SIZE;\n",
               "// ++vec_iter_counter;\n"],
            block}],
         "}\n",
         If[vectorise, "CCTK_ENDLOOP3STR", "CCTK_ENDLOOP3"] <> "(", functionName, ");\n" (* ,
         "// {\n",
         "//   timeval tv;\n",
         "//   gettimeofday(&tv, NULL);\n",
         "//   vec_iter_timer += tv.tv_sec + 1.0e-6 * tv.tv_usec;\n",
         "// }\n",
         "// CCTK_VInfo(CCTK_THORNSTRING, \"function="<>functionName<>" time=%g points=%td fp_ops=%td mem_ops=%td\", vec_iter_timer, vec_iter_counter, vec_op_counter, vec_mem_counter);\n",
         "// #undef VEC_COUNT\n",
         "// #define VEC_COUNT(x)\n" *) }],
     (* else *)
     ""]];

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
        rhs = rhs //. INV[sqrt[x_]] sqrt[y_] -> sqrt[INV[x] y];
        
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

        (* Handle Piecewise function *)
        rhs = rhs /. Piecewise -> piecewise1
                  //. piecewise1[pairs_List, val_:0] :>
                         If[pairs==={}, val,
                            IfThen[First[pairs][[2]],
                                   First[pairs][[1]],
                                   piecewise1[Rest[pairs], val]]];
        
        (* Remove parentheses *)
        rhs = rhs //. Parenthesis[xx_] -> xx;

        (* Avoid rational numbers *)
        rhs = rhs /. xx_Rational :> N[xx, 30];
        (* Avoid integers *)
        (* rhs = rhs /. xx_Integer :> 1.0*xx; *)

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
        rhs = rhs //. Max[xx_, yy__] :> fmax[xx, Max[yy]];
        rhs = rhs //. Min[xx_, yy__] :> fmin[xx, Min[yy]];
        rhs = rhs //. Abs[x_] -> fabs[x];
        rhs = rhs //. Sign[x_] -> isgn[x];
        rhs = rhs //. IntAbs[x_] -> abs[x];

        If[vectorise === True,
           rhs = VectoriseExpression[rhs]];

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
