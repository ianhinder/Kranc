
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

BeginPackage["CaKernel`", {"Errors`", "Helpers`", "Kranc`", "CodeGenCactus`", "CodeGenKranc`", "MapLookup`",
                           "Calculation`", "CodeGen`", "CodeGenCalculation`", "CodeGenC`", "Code`", "Object`"}];

CaKernelCCL;
CaKernelCode;
CaKernelEpilogue;
CaKernelSchedule;
CaKernelConfigurationCLL;
CaKernelInterfaceCLL;
WithHostCalculations;
CaKernelProcessCode;
TargetCaKernel;

Begin["`Private`"];

DefFn[
  variableBlock[var_, intent_String, cached_] :=
  CCLBlockCompact["CCTK_CUDA_KERNEL_VARIABLE", "", {"cached" -> If[cached,"yes","no"], "intent" -> intent}, {var}, ToString[var]]];

DefFn[
  parameterBlock[par_] :=
  CCLBlock["CCTK_CUDA_KERNEL_PARAMETER", "", {}, {par,"\n"}, ToString[par]]];

DefFn[
  variableBlocks[calc_] :=
  Module[
    {in,out,all,inOnly,outOnly,inOut,params, cachedVars},

    params = GetCalculationParameters[calc];
    in = Join[InputGridFunctions[calc]];
    out = OutputGridFunctions[calc];
    all = Union[in,out];

    inOnly = Join[Complement[in, out]];
    outOnly = Complement[out, in];
    inOut = Intersection[in,out];

    cachedVars = lookupDefault[calc, CachedVariables, {}];

    Riffle[
      Map[variableBlock[#, Which[MemberQ[inOnly, #], "in",
                                 MemberQ[outOnly, #], "out",
                                 MemberQ[inOut, #], "inout",
                                 True,ThrowError["Unable to determine use of variable "<>ToString[#]]],
                       MemberQ[cachedVars,#]] &, all]~Join~
      Map[parameterBlock, params],
      ""]]];

DefFn[
  kernelCCLBlock[calc_, tileSize_List] :=
  Module[
    {bnd = BoundaryCalculationQ[calc],
     int, attrs, stencil},
    int = !bnd;

    attrs = {"TYPE" -> If[int, "gpu_cuda_dc/3dblock2", "gpu_cuda/boundary_s"],
             "TILE" -> Quote[StringJoin[Riffle[ToString/@tileSize,","]]],
             "SHARECODE" -> "yes"};

    stencil = CalculationStencilSize[calc];

    If[int,
       attrs = Append[attrs, 
                      "STENCIL" ->
                      Quote@FlattenBlock@Riffle[
                        Flatten[Map[{#,#} &,
                                    stencil],1],","]]];

    attrs = Append[attrs, "EXTERIOR" ->
      If[MemberQ[{Everywhere, BoundaryWithGhosts},
                 GetCalculationWhere[calc]] || 
         (MemberQ[stencil, 0] && Total[stencil] === 0 ),
        Quote["1,1,1,1,1,1"],
        Quote["0,0,0,0,0,0"]]];

    CCLBlock["CCTK_CUDA_KERNEL", lookup[calc, Name], attrs,
             variableBlocks[calc]]]];

Options[CaKernelCCL] = ThornOptions;
DefFn[CaKernelCCL[calcs_List, opts:OptionsPattern[]] :=
  Module[
    {},
    Map[kernelCCLBlock[#,OptionValue[TileSize]] &, Select[calcs, CalculationOnDevice]]]];

DefFn[CaKernelSchedule[thornName_] :=
{
    {
      Name          -> thornName <> "_Init",
      SchedulePoint -> "in CCTK_BASEGRID after Accelerator_SetDevice",
      Language      -> "C",
      Options       -> "local",
      Comment       -> "Initialize CUDA Device"
    }
}]

CaKernelConfigurationCLL[] :=
  "REQUIRES CUDA MPI CaKernel\n";

CaKernelInterfaceCLL[] :=
"
# These functions are provided by the CaKernel thorn

CCTK_INT FUNCTION Device_RegisterMem(CCTK_POINTER IN cctkGH, CCTK_INT IN vi, CCTK_INT IN num_tls)
REQUIRES FUNCTION Device_RegisterMem

CCTK_INT FUNCTION Device_UnRegisterMem(CCTK_POINTER IN cctkGH, CCTK_INT IN vi)
REQUIRES FUNCTION Device_UnRegisterMem

CCTK_POINTER FUNCTION Device_GetVarI (CCTK_POINTER IN cctkGH, CCTK_INT IN vi, CCTK_INT IN num_tls)
REQUIRES FUNCTION Device_GetVarI
";

DefFn[codeBlock[macro_String, contents_] :=
  Module[
    {},
    CheckBlock[contents];
    {macro<>"_Begin", "\n",
     IndentBlock[{contents,"\n"}],
     macro<>"_End","\n"}]];

DefFn[
  CaKernelInitialiseFDVariables[] :=
  CommentedBlock[
    "Initialise finite differencing variables",
    {
      "#define ConditionExpression(x) (x)\n", (* TODO *)
      DefineConstant["dx", DataType[], "params.cagh_dx"],
      DefineConstant["dy", DataType[], "params.cagh_dy"],
      DefineConstant["dz", DataType[], "params.cagh_dz"],
      DefineConstant["dt", DataType[], "params.cagh_dt"],
      DefineConstant["t", DataType[],  "params.cagh_time"],

      DefineConstant["dxi", DataType[], "1.0/dx"],
      DefineConstant["dyi", DataType[], "1.0/dy"],
      DefineConstant["dzi", DataType[], "1.0/dz"],
      
      DefineConstant["khalf", DataType[], "0.5"],
      DefineConstant["kthird", DataType[], "1/3.0"],
      DefineConstant["ktwothird", DataType[], "2.0/3.0"],
      DefineConstant["kfourthird", DataType[], "4.0/3.0"],
      DefineConstant["keightthird", DataType[], "8.0/3.0"],
      DefineConstant["hdxi", DataType[], "0.5 * dxi"],
      DefineConstant["hdyi", DataType[], "0.5 * dyi"],
      DefineConstant["hdzi", DataType[], "0.5 * dzi"]}]];

cakernelLoopFunctionInt[b_, opts___] :=
  codeBlock[LoopName /. {opts}, b];

cakernelLoopFunctionBnd[b_, opts___] :=
  b;

DefFn[CaKernelCode[calc_List,opts___] :=
  Block[{$CodeGenTarget = NewObject[TargetCaKernel,{}]},
    Module[
    {kernel = "CAKERNEL_"<>GetCalculationName[calc], calc2,
     compSuff, int, loopFunction},

    SetDataType["CCTK_REAL"];

    int = !BoundaryCalculationQ[calc];
    loopFunction = If[int, cakernelLoopFunctionInt, cakernelLoopFunctionBnd];

    calc2 = Join[calc, {BodyFunction -> (codeBlock[kernel, #] &), 
                       CallerFunction -> False,
                       LoopFunction -> loopFunction,
                       GFAccessFunction -> ({"I3D(",Riffle[{#,0,0,0},","],")"} &),
                       InitFDVariables -> CaKernelInitialiseFDVariables[],
                       MacroPointer -> False}];

    (* Process stencils *)
    calc2 = calc2 /.
      I3D[gf_,n1_,n2_,n3_] :>
        Module[{ info = Global`KrancProjectionInfo[gf] },
          If[ info === False, "I3D", "ILD" ] <>
          "("<>ToString[gf]<>","<>
          If[ info === False, "", 
              "cak__AX_" <> StringJoin[Global`Sizes/.info] <>"," ] <>
          ToString[n1]<>","<>
          ToString[n2]<>","<>
          ToString[n3]<>")" ];

    (* Kranc Tiling is not supported for CaKernel thorns *)
    calc2 = mapReplaceAdd[calc2, Tile, False];

    If[!int,

       calc2 = calc2 /. {normal1 -> "bound_x", normal2 -> "bound_y", normal3 -> "bound_z"}];


    {"#undef KRANC_DIFF_FUNCTIONS\n",
     "#define KRANC_" <> ToUpperCase[CodeGenC`SOURCELANGUAGE] <> "\n",
     Map[IncludeFile, {"Differencing.h", "Kranc.hh"}],
     (* TODO: Instead of using this namespace,
        put everything into this namespace *)
     "using namespace CCTK_THORN;\n",

     "\n",
     "#undef KRANC_GFOFFSET3D\n",
     "#define KRANC_GFOFFSET3D(u,i,j,k) I3D(u,i,j,k)\n",
     "\n",

    "\n", CreateCalculationFunction[calc2, LoopName -> kernel<>"_Computations",
                                    opts]}]]];

DefFn[
  splitHostCaKernel[calc_List] :=

    (* Each calculation is marked as ExecuteOn Host, Device or
       Automatic.  The default is Automatic.  Only calculations
       specified as Host are the special ones.  These ones will ALWAYS
       execute on the host.  Otherwise, they will execute on the
       device for a CaKernel run, and the host for a host run. 

       Any calculation which is scheduled to execute on the host
       should be left alone.  We don't need to modify it.  Any
       calculation which has ExecuteOn -> Automatic should be split
       into two.  One will be a host calculation and one a CaKernel
       one.  Which is scheduled will depend on the global variable.
       *)

  If[lookup[calc,ExecuteOn,Automatic] === Host,
     {calc},

     Module[
       {deviceCalc, hostCalc, newCalcs},

       deviceCalc =
       mapReplaceAdd[
         mapReplaceAdd[
           mapReplace[
             calc,
             Name,"DEVICE_"<>lookup[calc,Name]],
           UseCaKernel, True],
         ExecuteOn, Device];

       hostCalc =
       mapReplaceAdd[
         mapReplaceAdd[
           mapReplace[
             calc,
             Name,"HOST_"<>lookup[calc,Name]],
           UseCaKernel,False],
         ExecuteOn, Host];

       newCalcs = Map[InNewScheduleGroup[lookup[calc,Name],#] &, {hostCalc, deviceCalc}];

       (* Apply the conditional to the routines, not to the group *)
       newCalcs = MapThread[AddConditionSuffix,
                            {newCalcs,
                             {"ChemoraDevice::host_process",
                              "ChemoraDevice::device_process"}}];

       newCalcs]]];

DefFn[
  WithHostCalculations[calcs_List] :=
    Flatten[Map[splitHostCaKernel, calcs],1]];

Options[CaKernelProcessCode] = ThornOptions;

DefFn[
  CaKernelProcessCode[c_Code, opts:OptionsPattern[]] :=
  Module[
    {calcs = GetObjectField[c, "Calculations"], c2},

    (* Make the CaKernel option calculation-specific *)
    calcs = Map[Append[#,UseCaKernel -> OptionValue[UseCaKernel]] &, calcs];
    
    If[OptionValue[GenerateHostCode] && OptionValue[UseCaKernel],
       calcs = WithHostCalculations[calcs]];

    If[!And@@Map[ListQ, calcs], Print[Short[calcs//InputForm]]; ThrowError["Result of WithHostCalculations is not a list of lists"]];

    (* Add ExecuteOn -> Device to any CaKernel calculation that has no ExecuteOn option *)
    calcs = Map[If[!lookup[#,UseCaKernel,False], #, If[mapContains[#,ExecuteOn], #, Append[#,ExecuteOn->Device]]] &, calcs];

    c2 = SetObjectField[c, "Calculations", calcs];

    If[OptionValue[UseCaKernel],
       c2 = AppendObjectField[c2, "IncludeFiles", "chemora_cg_kranc_startup.h"];
       c2 = AppendObjectField[c2, "IncludeFiles", "CaCUDALib_driver_support.h"]];

    If[OptionValue[UseCaKernel],
       c2 = AppendObjectField[c2, "InheritedImplementations", "Accelerator"]];
    c2]];

DefFn[PostProcessExpression[_TargetCaKernel, expr_] :=
  expr //. 
   { GFLocal[gf_] :>
       Module[{info = Global`KrancProjectionInfo[gf]},
         If[ info === False,
             I3D[gf,0,0,0],
             "IVD"[gf, "cak__AX_" <> StringJoin[Global`Sizes/.info]] ] ],
     ToReal[x_] -> x }
   ];

End[];

EndPackage[];
