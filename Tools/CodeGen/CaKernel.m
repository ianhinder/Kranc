
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

    attrs = {"TYPE" -> If[int, "gpu_cuda/3dblock", "gpu_cuda/boundary_s"],
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
      If[MemberQ[{Everywhere, BoundaryWithGhosts, BoundaryNoSync},
                 GetCalculationWhere[calc]] || 
        (MemberQ[stencil, 0] && Total[stencil] =!= 0 (* both zero and nonzero elements *)),
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

cakernelLoopFunctionInt[b_, opts___] :=
  codeBlock[LoopName /. {opts}, b];

cakernelLoopFunctionBnd[b_, opts___] :=
  b;

DefFn[CaKernelCode[calc_List,opts___] :=
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

    If[!int,

       calc2 = calc2 /. {normal1 -> "bound_x", normal2 -> "bound_y", normal3 -> "bound_z"}];


    {"#undef KRANC_DIFF_FUNCTIONS\n",
     "#define KRANC_" <> ToUpperCase[CodeGenC`SOURCELANGUAGE] <> "\n",
     Map[IncludeFile, {"Differencing.h", "GenericFD.h"}],

     "\n",
     "#undef KRANC_GFOFFSET3D\n",
     "#define KRANC_GFOFFSET3D(u,i,j,k) I3D(u,i,j,k)\n",
     "\n",
     CalculationMacros[],

    "\n", CreateCalculationFunction[calc2, LoopName -> kernel<>"_Computations",
                                    opts]}]];

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
       c2 = AppendObjectField[c2, "IncludeFiles", "CaCUDALib_driver_support.h"]];

    If[OptionValue[UseCaKernel],
       c2 = AppendObjectField[c2, "InheritedImplementations", "Accelerator"]];
    c2]];


End[];

EndPackage[];
