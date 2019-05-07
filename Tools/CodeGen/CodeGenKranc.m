
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

BeginPackage["CodeGenKranc`", {"Errors`", "Helpers`", "Kranc`", "CodeGenC`", "CodeGen`",
  "Vectorisation`", "OperationCount`", "Object`"}];

SetDataType::usage = "SetDataType[type] sets a string for the grid function data type (e.g. CCTK_REAL)";
DataType::usage = "DataType[] returns a string for the grid function data type (e.g. CCTK_REAL)";
AssignVariableInLoop::usage = "AssignVariableInLoop[dest_, src_] returns a block of code " <>
  "that assigns 'src' to 'dest'.";
TestForNaN::usage = "TestForNaN[expr_] returns a block of code " <>
  "that tests 'expr' for nan.";
ArrayName::usage = "ArrayName[variable] returns the name needed to access variable " <>
  "assuming it is an array variable when inside a grid function.";
InitialiseFDVariables::usage = "";
GenericGridLoop::usage = "";
ProcessExpression::usage = "";
AssignVariableFromExpression;
GenerateCodeFromExpression;
FileHeader;
ReadGridFunctionInLoop;
PostProcessExpression;
$CodeGenTarget = TargetC["UseVectors" -> False];

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
  {dest, " = ", src, ";\n"}];

DefFn[
  AssignVariableInLoop[dest:(_String|_Symbol), src:CodeGenBlock,
                       True] :=
  VectorisationAssignVariableInLoop[dest, src]];

DefFn[
  LoadVariable[x_] := x];

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
  InitialiseFDVariables[vectorise:Boolean, fdTile:Boolean, dgTile:Boolean] :=
  CommentedBlock[
    "Initialise finite differencing variables",
    {
      If[!(fdTile || dgTile),
        MapThread[Function[{dest,type,val}, AssignVariableFromExpression[dest,val,True,vectorise,fdTile,dgTile,Const->True,Type->type]],
              Transpose@{{"di", "ptrdiff_t", "1"},
               {"dj", "ptrdiff_t", "cctk_ash[0]"},
               {"dk", "ptrdiff_t", "cctk_ash[0] * cctk_ash[1]"},
               {"cdi", "ptrdiff_t", "sizeof(CCTK_REAL) * di"},
               {"cdj", "ptrdiff_t", "sizeof(CCTK_REAL) * dj"},
               {"cdk", "ptrdiff_t", "sizeof(CCTK_REAL) * dk"}}],
        {}],

      MapThread[Function[{dest,type,val}, AssignVariableFromExpression[dest,val,True,vectorise,fdTile,dgTile,Const->True,Type->type]],
            Transpose@{{"cctkLbnd1", "ptrdiff_t", "cctk_lbnd[0]"},
             {"cctkLbnd2", "ptrdiff_t", "cctk_lbnd[1]"},
             {"cctkLbnd3", "ptrdiff_t", "cctk_lbnd[2]"},
             {"t", DataType[], ToReal["cctk_time"]},
             {"cctkOriginSpace1", DataType[], ToReal["CCTK_ORIGIN_SPACE(0)"]},
             {"cctkOriginSpace2", DataType[], ToReal["CCTK_ORIGIN_SPACE(1)"]},
             {"cctkOriginSpace3", DataType[], ToReal["CCTK_ORIGIN_SPACE(2)"]},
             {"dt", DataType[], ToReal["CCTK_DELTA_TIME"]},
             {"dx", DataType[], ToReal["CCTK_DELTA_SPACE(0)"]},
             {"dy", DataType[], ToReal["CCTK_DELTA_SPACE(1)"]},
             {"dz", DataType[], ToReal["CCTK_DELTA_SPACE(2)"]},

             (* Note that dx is already a vector, so should not be wrapped in ToReal *)
             {"dxi", DataType[], 1/dx},
             {"dyi", DataType[], 1/dy},
             {"dzi", DataType[], 1/dz},
             {"dxxi", DataType[], "dxi" * "dxi"},
             {"dxyi", DataType[], "dxi" * "dyi"},
             {"dxzi", DataType[], "dxi" * "dzi"},
             {"dyyi", DataType[], "dyi" * "dyi"},
             {"dyzi", DataType[], "dyi" * "dzi"},
             {"dzzi", DataType[], "dzi" * "dzi"}}],

      AssignVariableFromExpression["khalf", 0.5, True, vectorise, dgTile, Const -> True],
      AssignVariableFromExpression["kthird", 1/3, True, vectorise, dgTile, Const -> True],
      AssignVariableFromExpression["ktwothird", 2/3, True, vectorise, dgTile, Const -> True],
      AssignVariableFromExpression["kfourthird", 4/3, True, vectorise, dgTile, Const -> True],
      AssignVariableFromExpression["hdxi", 0.5 "dxi", True, vectorise, dgTile, Const -> True],
      AssignVariableFromExpression["hdyi", 0.5 "dyi", True, vectorise, dgTile, Const -> True],
      AssignVariableFromExpression["hdzi", 0.5 "dzi", True, vectorise, dgTile, Const -> True]}]];

Options[GenericGridLoop] = Join[ThornOptions, {Tile -> False,
                                               FDTile -> False,
                                               DGTile -> False}];

DefFn[
  GenericGridLoop[functionName_String, block:CodeGenBlock,
                  tile_, fdtile_,dgtile_,
                  opts:OptionsPattern[]] :=
  If[fdtile || dgtile,
     CommentedBlock[
       "Loop over all grid points in the current tile",
       {
         "for (ptrdiff_t off = 0; off < tsz; off += tdi*vs) {\n",
         IndentBlock[block],
         "}\n"
       }],
     CommentedBlock[
       "Loop over the grid points",
       { If[!OptionValue[UseLoopControlQ],
         { (* Circumvent a compiler bug on Blue Gene/Q *)
           "const int imin0=imin[0];\n",
           "const int imin1=imin[1];\n",
           "const int imin2=imin[2];\n",
           "const int imax0=imax[0];\n",
           "const int imax1=imax[1];\n",
           "const int imax2=imax[2];\n",
           "#pragma omp parallel\n",
           If[OptionValue[UseVectors], "CCTK_LOOP3STROFF", "CCTK_LOOP3"],
           "(", functionName, ",\n",
           "  i,j,k, imin0,imin1,imin2, imax0,imax1,imax2,\n",
           "  cctk_ash[0],cctk_ash[1],cctk_ash[2]",
           If[OptionValue[UseVectors],
              {",\n",
               "  cctk_alignment,cctk_alignment_offset,\n",
               "  vecimin,vecimax, CCTK_REAL_VEC_SIZE"},
              ""],
           ")\n"
         },
         { (* LoopControlQ enabled *)
           "typedef LoopControlQ::iarray<3> iarray3;\n",
           "iarray3 iamin, iamax, iablock, iaash;\n",
           "for (int d=0; d<3; ++d) {\n",
           "  iamin[d] = imin[d];\n",
           "  iamax[d] = imax[d];\n",
           "  iaash[d] = cctk_ash[d];\n",
           "}\n",
           "const ptrdiff_t iaaln0 = cctk_alignment;\n",
           "const ptrdiff_t iaoff0 = cctk_alignment_offset;\n",
           "iablock[0] = block_size_i;\n",
           "iablock[1] = block_size_j;\n",
           "iablock[2] = block_size_k;\n",
           If[!OptionValue[UseVectors],
              {
                "rhs_loop(\n",
                "  [=](const iarray3& ipos) {\n"
              }, {
                "rhs_loop_str<CCTK_REAL_VEC_SIZE, VECTORISE && VECTORISE_ALIGNED_ARRAYS>(\n",
                "  [=](const iarray3& imin, const iarray3& imax, const iarray3& ipos) {\n",
                "    const ptrdiff_t vecimin = imin[0];\n",
                "    const ptrdiff_t vecimax = imax[0];\n"
                 }],
           "    const ptrdiff_t i = ipos[0];\n",
           "    const ptrdiff_t j = ipos[1];\n",
           "    const ptrdiff_t k = ipos[2];\n"
         }],
         "{\n",
         IndentBlock[
           {DefineConstant["index", "ptrdiff_t", "di*i + dj*j + dk*k"],
            If[OptionValue[UseVectors],
               {"const CCTK_REAL_VEC cctkIdx1 CCTK_ATTRIBUTE_UNUSED = kadd(ToReal(i), vec_index);\n",
                "const CCTK_REAL_VEC cctkIdx2 CCTK_ATTRIBUTE_UNUSED = ToReal(j);\n",
                "const CCTK_REAL_VEC cctkIdx3 CCTK_ATTRIBUTE_UNUSED = ToReal(k);\n"
               },
               {"const CCTK_REAL cctkIdx1 CCTK_ATTRIBUTE_UNUSED = i;\n",
                "const CCTK_REAL cctkIdx2 CCTK_ATTRIBUTE_UNUSED = j;\n",
                "const CCTK_REAL cctkIdx3 CCTK_ATTRIBUTE_UNUSED = k;\n"}],
            If[tile && OptionValue[UseVectors],
               {"const int ti CCTK_ATTRIBUTE_UNUSED = i - kd.tile_imin[0];\n",
                "const int tj CCTK_ATTRIBUTE_UNUSED = j - kd.tile_imin[1];\n",
                "const int tk CCTK_ATTRIBUTE_UNUSED = k - kd.tile_imin[2];\n"},
               {}],
            block}],
         "}\n",
         If[!OptionValue[UseLoopControlQ],
         {
           If[OptionValue[UseVectors], "CCTK_ENDLOOP3STROFF", "CCTK_ENDLOOP3"] <>
           "(", functionName, ");\n"
         }, {
           "  },\n",
           "  cctkGH, iamin, iamax, iablock, iaash, iaaln0, iaoff0);\n"
         }]
       }]]];

DefFn[
  onceInGridLoop[block:CodeGenBlock] :=
  Conditional[
    "i == 5 && j == 5 && k == 5",
    block]];

DefFn[
  InfoVariable[name_String] :=
  onceInGridLoop[
    {"char buffer[255];\n",
     "snprintf(buffer, sizeof buffer, \"" , name , " == %f\", " , name , ");\n",
     "CCTK_INFO(buffer);\n"}]];

(* Take an expression x and replace occurrences of Powers with the C
   macros SQR, CUB, QAD *)
DefFn[
  ProcessExpression[expr_, vectorise:Boolean, byteIndexing:Boolean,
                    noSimplify:Boolean : False] :=
  Module[
    {rhs},
    rhs = expr;
    
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

    mathematicaToCRules = {
      Power[E, power_] -> exp[power],
      Log[x_] -> log[x],
      Power[x_, n_Integer] -> pown[x,n],
      Power[x_, power_] -> pow[x,power],
      PowerN[x_, n_] -> pown[x,n],
      Sin[x_] -> sin[x],
      Cos[x_] -> cos[x],
      Tan[x_] -> tan[x],
      Sec[x_] -> 1 / cos[x],
      Csc[x_] -> 1 / sin[x],
      Cot[x_] -> 1 / tan[x],
      ArcSin[x_] -> asin[x],
      ArcCos[x_] -> acos[x],
      ArcTan[x_] -> atan[x],
      ArcTan[x_, y_] -> atan2[y,x],
      ArcSec[x_] -> acos[1/x],
      ArcCsc[x_] -> asin[1/x],
      ArcCot[x_] -> atan[1/x],
      Sinh[x_] -> sinh[x],
      Cosh[x_] -> cosh[x],
      Tanh[x_] -> tanh[x],
      Sech[x_] -> 1 / cosh[x],
      Csch[x_] -> 1 / sinh[x],
      Coth[x_] -> 1 / tanh[x],
      ArcSinh[x_] -> asinh[x],
      ArcCosh[x_] -> acosh[x],
      ArcTanh[x_] -> atahn[x],
      ArcSech[x_] -> acosh[1/x],
      ArcCsch[x_] -> asinh[1/x],
      ArcCoth[x_] -> atahn[1/x],

      (* Note: Mathematica simplifies Max[xx_] -> xx automatically *)
      Max[xx_, yy__] :> fmax[xx, Max[yy]],
      Min[xx_, yy__] :> fmin[xx, Min[yy]],
      Min3[xx_, yy_, zz_] :> fmin[fmin[xx, yy], zz],
      Mod[xx_, yy_] -> fmod[xx, yy],
      Abs[x_] -> fabs[x],
      Sign[x_] -> isgn[x],
      IntAbs[x_] -> abs[x]};

    rhs = rhs //. mathematicaToCRules;

    If[Cases[rhs, _Power, Infinity] =!= {},
      ThrowError["Power found in "<>ToString[rhs,InputForm]]];

    rhs = PostProcessExpression[$CodeGenTarget, rhs, byteIndexing];

    If[Cases[rhs, _Power, Infinity] =!= {},
      ThrowError["Power found in "<>ToString[rhs,InputForm]]];

    rhs = rhs //. {Parameter[xx_] -> xx};
    rhs]];

DefFn[PostProcessExpression[t_TargetC, expr_, byteIndexing:Boolean] :=
  Module[{expr2 = expr},

    If[byteIndexing,
      expr2 = expr2 //. {GFLocal[x_] :> CTileArray[ToString[x]<>"T", {"off"}]},
      expr2 = expr2 //. {GFLocal[x_] :> CArray[x,{"index"}]}];

    expr2 = If[GetObjectField[t,"UseVectors"],
      VectoriseExpression[expr2] ,
      expr2 //. {ToReal[x_] :> x}] /. {ConditionExpression[x_] :> x};

    expr2]];

(* Return a CodeGen block which assigns dest by evaluating expr *)
Options[AssignVariableFromExpression] = {"Const" -> False, "Type" -> Automatic};
DefFn[AssignVariableFromExpression[dest_, expr_, declare:Boolean,
                                   vectorise:Boolean,
                                   byteIndexing:Boolean,
                                   noSimplify:Boolean : False,
                             OptionsPattern[]] :=
  Module[{type, exprCode, code},
    type =
    If[OptionValue[Type] === Automatic,
      If[StringMatchQ[ToString[dest], "dir*"], "ptrdiff_t", DataType[]],
      OptionValue[Type]];
    exprCode = GenerateCodeFromExpression[expr, vectorise, byteIndexing,
                                          noSimplify];
    CountOperations[expr];
    code = If[declare,
              If[OptionValue[Const],DefineConstant,DefineVariable][dest, type, exprCode],
              AssignVariable[dest, exprCode]];
    code = LineBreak[FlattenBlock[code], 70] <> "\n";
    {code}]];

Format[CArray[id_, {args__}], CForm] :=
  SequenceForm[id, "[", Sequence @@ Riffle[{args}, ","], "]"];

DefFn[GenerateCodeFromExpression[expr_, vectorise_, byteIndexing:Boolean,
                                 noSimplify:Boolean : False] :=
  Module[{cleanExpr, code},
    cleanExpr = ProcessExpression[expr, vectorise, byteIndexing, noSimplify];
    code = ToString[cleanExpr, CForm, PageWidth -> Infinity];
    code = StringReplace[code, "normal1"     -> "normal[0]"];
    code = StringReplace[code, "normal2"     -> "normal[1]"];
    code = StringReplace[code, "normal3"     -> "normal[2]"];
    code = StringReplace[code, "BesselJ"-> "gsl_sf_bessel_Jn"];
    code = StringReplace[code, "\"" -> ""];
    {code}]];

DefFn[ReadGridFunctionInLoop[gf:(_Symbol|_String)] :=
  FlattenBlock[{gf,"[","index","]"}]];

End[];

EndPackage[];
