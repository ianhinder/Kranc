
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

BeginPackage["CaKernel`", {"Errors`", "Helpers`", "Kranc`", "CodeGenCactus`", "MapLookup`", "Calculation`"}];

CaKernelCCL;

Begin["`Private`"];

DefFn[
  variableBlock[var_, intent_String] :=
  CCLBlock["CCTK_CUDA_KERNEL_VARIABLE", "", {"cached" -> "yes", "intent" -> intent}, {var,"\n"}, ToString[var]]];

DefFn[
  variableBlocks[calc_] :=
  Module[
    {in,out,all,inOnly,outOnly,inOut},
    in = InputGridFunctions[calc];
    out = OutputGridFunctions[calc];
    all = Union[in,out];

    inOnly = Complement[in, out];
    outOnly = Complement[out, in];
    inOut = Intersection[in,out];

    Riffle[
      Map[variableBlock[#, Which[MemberQ[inOnly, #], "in", 
                                 MemberQ[outOnly, #], "out", 
                                 MemberQ[inOut, #], "inout", 
                                 True,ThrowError["Unable to determine use of variable "<>ToString[#]]]] &, all],
          "\n"]]];

DefFn[
  kernelCCLBlock[calc_] :=
  CCLBlock["CCTK_CUDA_KERNEL", lookup[calc, Name], 
           {"TYPE" -> "gpu_cuda/3dblock",
            "STENCIL" -> "0,0,0,0,0,0",
            "TILE" -> "16,16,16",
            "SHARECODE" -> "yes"},
           variableBlocks[calc]]];

DefFn[CaKernelCCL[calcs_List] :=
  Module[
    {},
    Map[kernelCCLBlock, calcs]]];

End[];

EndPackage[];
