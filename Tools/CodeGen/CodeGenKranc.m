
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

(* Functions for generating blocks of code specific to Kranc *)

BeginPackage["CodeGenKranc`", {"Errors`", "Helpers`", "Kranc`", "CodeGenC`", "CodeGen`", "Vectorisation`"}];

SetDataType::usage = "SetDataType[type] sets a string for the grid function data type (e.g. CCTK_REAL)";
DataType::usage = "DataType[] returns a string for the grid function data type (e.g. CCTK_REAL)";
AssignVariableInLoop::usage = "AssignVariableInLoop[dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
DeclareMaybeAssignVariableInLoop::usage = "DeclareMaybeAssignVariableInLoop[type_, dest_, src_, cond_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
TestForNaN::usage = "TestForNaN[expr_] returns a block of code " <>
  "that tests 'expr' for nan.";
ArrayName::usage = "ArrayName[variable] returns the name needed to access variable " <>
  "assuming it is an array variable when inside a grid function.";
InitialiseFDVariables::usage = "";
GenericGridLoop::usage = "";
ProcessExpression::usage = "";
CalculationMacros;
AssignVariableFromExpression;
GenerateCodeFromExpression;
FileHeader;

Begin["`Private`"];

DefFn[lineComment["CCL"|"Makefile", s_] := {"# ", s, "\n"}];
DefFn[lineComment["C", s_] := {"/*  ", s, " */", "\n"}];
DefFn[lineComment["Fortran", s_] := {"! ", s, "\n"}];

DefFn[
  FileHeader[lang_] :=
  {lineComment[lang, "File produced by Kranc"], "\n"}];

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

DefFn[
  ArrayName[x:(_Symbol|_String)] :=
  ToString[x] <> "[0]"];

DefFn[
  InitialiseFDVariables[vectorise:Boolean] :=
  CommentedBlock[
    "Initialise finite differencing variables",
    {
      (* TODO: Generate the following using
         AssignVariableFromExpression.  This requires a richer
         expression language with type information so that
         scalars/vectors can be handled automatically. *)
      Apply[DeclareAssignVariable,
            {{"ptrdiff_t", "di", "1"},
             {"ptrdiff_t", "dj", "CCTK_GFINDEX3D(cctkGH,0,1,0) - CCTK_GFINDEX3D(cctkGH,0,0,0)"},
             {"ptrdiff_t", "dk", "CCTK_GFINDEX3D(cctkGH,0,0,1) - CCTK_GFINDEX3D(cctkGH,0,0,0)"},
             {"ptrdiff_t", "cdi", "sizeof(CCTK_REAL) * di"},
             {"ptrdiff_t", "cdj", "sizeof(CCTK_REAL) * dj"},
             {"ptrdiff_t", "cdk", "sizeof(CCTK_REAL) * dk"},
             {DataType[], "dx", "ToReal(CCTK_DELTA_SPACE(0))"},
             {DataType[], "dy", "ToReal(CCTK_DELTA_SPACE(1))"},
             {DataType[], "dz", "ToReal(CCTK_DELTA_SPACE(2))"},
             {DataType[], "dt", "ToReal(CCTK_DELTA_TIME)"},
             {DataType[], "t", "ToReal(cctk_time)"},

             (* Note that dx is already a vector, so should not be wrapped in ToReal *)
             {DataType[], "dxi", "INV(dx)"},
             {DataType[], "dyi", "INV(dy)"},
             {DataType[], "dzi", "INV(dz)"}},
            {1}],

      AssignVariableFromExpression["khalf", 0.5, True, vectorise, Const -> True],
      AssignVariableFromExpression["kthird", 1/3, True, vectorise, Const -> True],
      AssignVariableFromExpression["ktwothird", 2/3, True, vectorise, Const -> True],
      AssignVariableFromExpression["kfourthird", 4/3, True, vectorise, Const -> True],
      AssignVariableFromExpression["hdxi", 0.5 "dxi", True, vectorise, Const -> True],
      AssignVariableFromExpression["hdyi", 0.5 "dyi", True, vectorise, Const -> True],
      AssignVariableFromExpression["hdzi", 0.5 "dzi", True, vectorise, Const -> True]}]];

Options[GenericGridLoop] = Join[ThornOptions, {Tile -> False}];

DefFn[
  GenericGridLoop[functionName_String, block:CodeGenBlock, tile_, opts:OptionsPattern[]] :=
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
      If[OptionValue[UseVectors], "CCTK_LOOP3STR", "CCTK_LOOP3"],
      "(", functionName, ",\n",
      "  i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,\n",
      "  cctk_ash[0],cctk_ash[1],cctk_ash[2]",
      If[OptionValue[UseVectors], {",\n", "  vecimin,vecimax, CCTK_REAL_VEC_SIZE"}, ""],
      ")\n",
      "{\n",
      IndentBlock[
        {(* DeclareVariable["index", "// int"], *)
         (* DeclareAssignVariable["int", "index", "CCTK_GFINDEX3D(cctkGH,i,j,k)"], *)
         DeclareAssignVariable["ptrdiff_t", "index", "di*i + dj*j + dk*k"],
         If[OptionValue[UseVectors],
            "// vec_iter_counter+=CCTK_REAL_VEC_SIZE;\n",
            "// ++vec_iter_counter;\n"],
          If[tile,
            {"const int ti = i - kd.tile_imin[0];\n",
             "const int tj = j - kd.tile_imin[1];\n",
             "const int tk = k - kd.tile_imin[2];\n"},
            {}],
         block}],
      "}\n",
      If[OptionValue[UseVectors], "CCTK_ENDLOOP3STR", "CCTK_ENDLOOP3"] <>
      "(", functionName, ");\n" 
      (* ,
         "// {\n",
         "//   timeval tv;\n",
         "//   gettimeofday(&tv, NULL);\n",
         "//   vec_iter_timer += tv.tv_sec + 1.0e-6 * tv.tv_usec;\n",
         "// }\n",
         "// CCTK_VInfo(CCTK_THORNSTRING, \"function="<>functionName<>" time=%g points=%td fp_ops=%td mem_ops=%td\", vec_iter_timer, vec_iter_counter, vec_op_counter, vec_mem_counter);\n",
         "// #undef VEC_COUNT\n",
         "// #define VEC_COUNT(x)\n" *) }]];

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

(* Take an expression x and replace occurrences of Powers with the C
  macros SQR, CUB, QAD *)
DefFn[
  ProcessExpression[expr_, vectorise:Boolean, noSimplify:Boolean : False] :=
  Module[
    {rhs},
    rhs = expr /. Power[xx_, -1] -> INV[xx];
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

        (* Remove KrancScalar[] after vectorising *)
        rhs = rhs /. KrancScalar[xx_] -> xx};
    (*       Print[rhs//FullForm];*)
    rhs]];

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

(* Return a CodeGen block which assigns dest by evaluating expr *)
Options[AssignVariableFromExpression] = {"Const" -> False};
AssignVariableFromExpression[dest_, expr_, declare_, vectorise_, noSimplify:Boolean : False,
                             OptionsPattern[]] :=
  Module[{type, exprCode, code},
    type = If[StringMatchQ[ToString[dest], "dir*"], "ptrdiff_t", DataType[]];
    exprCode = GenerateCodeFromExpression[expr, vectorise, noSimplify];
    code = If[declare,
              DeclareAssignVariable[type, dest, exprCode, Const -> OptionValue[Const]],
              AssignVariable[dest, exprCode]];
    code = LineBreak[FlattenBlock[code], 70] <> "\n";
    {code}];

GenerateCodeFromExpression[expr_, vectorise_, noSimplify:Boolean : False] :=
  Module[{cleanExpr, code},
    cleanExpr = ProcessExpression[expr, vectorise, noSimplify];
    code = ToString[cleanExpr, CForm, PageWidth -> Infinity];
    code = StringReplace[code, "normal1"     -> "normal[0]"];
    code = StringReplace[code, "normal2"     -> "normal[1]"];
    code = StringReplace[code, "normal3"     -> "normal[2]"];
    code = StringReplace[code, "BesselJ"-> "gsl_sf_bessel_Jn"];
    code = StringReplace[code, "\"" -> ""];
    {code}];

End[];

EndPackage[];
