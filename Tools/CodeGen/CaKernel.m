
(*  Copyright 2012 Ian Hinder

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

BeginPackage["CaKernel`", {"Errors`", "Helpers`", "Kranc`", "CodeGenCactus`", "MapLookup`",
                           "Calculation`", "CodeGen`", "CalculationFunction`", "CodeGenC`"}];

CaKernelCCL;
CaKernelCode;
CaKernelEpilogue;
CaKernelSchedule;
CaKernelConfigurationCLL;

Begin["`Private`"];

DefFn[
  variableBlock[var_, intent_String] :=
  CCLBlock["CCTK_CUDA_KERNEL_VARIABLE", "", {"cached" -> "no", "intent" -> intent}, {var,"\n"}, ToString[var]]];

DefFn[
  parameterBlock[par_] :=
  CCLBlock["CCTK_CUDA_KERNEL_PARAMETER", "", {}, {par,"\n"}, ToString[par]]];

DefFn[
  variableBlocks[calc_] :=
  Module[
    {in,out,all,inOnly,outOnly,inOut,params},

    params = GetCalculationParameters[calc];
    in = Join[InputGridFunctions[calc]];
    out = OutputGridFunctions[calc];
    all = Union[in,out];

    inOnly = Join[Complement[in, out]];
    outOnly = Complement[out, in];
    inOut = Intersection[in,out];

    Riffle[
      Map[variableBlock[#, Which[MemberQ[inOnly, #], "in",
                                 MemberQ[outOnly, #], "out",
                                 MemberQ[inOut, #], "inout",
                                 True,ThrowError["Unable to determine use of variable "<>ToString[#]]]] &, all]~Join~
      Map[parameterBlock, params],
      "\n"]]];

DefFn[
  kernelCCLBlock[calc_] :=
  CCLBlock["CCTK_CUDA_KERNEL", lookup[calc, Name],
           {"TYPE" -> "gpu_cuda/3dblock",
            "STENCIL" -> Quote@FlattenBlock@Riffle[
              Flatten[Map[{#,#} &, CalculationStencilSize[calc]],1],","],
            "TILE" -> Quote["8,8,8"],
            "SHARECODE" -> "yes"},
           variableBlocks[calc]]];

DefFn[CaKernelCCL[calcs_List] :=
  Module[
    {},
    Map[kernelCCLBlock, calcs]]];

DefFn[CaKernelSchedule[] :=
  Module[
    {},
    {{
      Name          -> "CaKernel_CopyFromDev",
      SchedulePoint -> "at ANALYSIS", 
      Language      -> "C",
      Comment       -> "Copy variables from devices"
    }}]];

CaKernelConfigurationCLL[] :=
  "REQUIRES CUDA";

DefFn[codeBlock[macro_String, contents:CodeGenBlock] :=
  Module[
    {},
    {macro<>"_Begin", "\n",
     IndentBlock[{contents,"\n"}],
     macro<>"_End","\n"}]];

DefFn[
  CaKernelInitialiseFDVariables[] :=
  CommentedBlock[
    "Initialise finite differencing variables",
    {
      DeclareAssignVariable[DataType[], "dx", "params.cagh_dx"],
      DeclareAssignVariable[DataType[], "dy", "params.cagh_dy"],
      DeclareAssignVariable[DataType[], "dz", "params.cagh_dz"],
      DeclareAssignVariable[DataType[], "dt", "params.cagh_dt"],
      DeclareAssignVariable[DataType[], "t",  "params.cagh_time"],

      DeclareAssignVariable[DataType[], "dxi", "INV(dx)"],
      DeclareAssignVariable[DataType[], "dyi", "INV(dy)"],
      DeclareAssignVariable[DataType[], "dzi", "INV(dz)"],
      
      DeclareAssignVariable[DataType[], "khalf", "0.5"],
      DeclareAssignVariable[DataType[], "kthird", "1/3.0"],
      DeclareAssignVariable[DataType[], "ktwothird", "2.0/3.0"],
      DeclareAssignVariable[DataType[], "kfourthird", "4.0/3.0"],
      DeclareAssignVariable[DataType[], "keightthird", "8.0/3.0"],
      DeclareAssignVariable[DataType[], "hdxi", "0.5 * dxi"],
      DeclareAssignVariable[DataType[], "hdyi", "0.5 * dyi"],
      DeclareAssignVariable[DataType[], "hdzi", "0.5 * dzi"]}]];


DefFn[CaKernelCode[calc_List,opts___] :=
  Module[
    {kernel = "CAKERNEL_"<>GetCalculationName[calc], calc2},

    SetDataType["CCTK_REAL"];

    calc2 = Join[calc, {BodyFunction -> (codeBlock[kernel, #] &), 
                       CallerFunction -> False,
                       LoopFunction -> (codeBlock[kernel<>"_Computations", #] &),
                       GFAccessFunction -> ({"I3D(",Riffle[{#,0,0,0},","],")"} &),
                       InitFDVariables -> CaKernelInitialiseFDVariables[],
                       MacroPointer -> False}];

    {"#undef KRANC_DIFF_FUNCTIONS\n",
     "#define KRANC_" <> ToUpperCase[CodeGenC`SOURCELANGUAGE] <> "\n",
     Map[IncludeFile, {"Differencing.h", "GenericFD.h"}],

     "\n",
     "#define KRANC_GFOFFSET3D(u,i,j,k) I3D(u,i,j,k)\n",
     "\n",
     CalculationMacros[],

    "\n", CreateCalculationFunction[calc2,opts]}]];

End[];

EndPackage[];
