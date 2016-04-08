
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

BeginPackage["Interface`", {"Thorn`", "KrancGroups`", "MapLookup`", "Errors`",
                            "Helpers`", "Kranc`", "CaKernel`", "OpenCL`",
                            "CodeGenInterface`", "MoL`"}];

CreateKrancInterface;

Begin["`Private`"];

DefFn[declaredGroupInterfaceStructure[group_List] :=
  Module[
    {extras, gridType, dim},
    extras = GroupExtras[group];
    gridType = lookup[extras, GridType, "GF"];
    dim = lookup[extras, Dim, 3];
    {
      Name -> groupName[group], 
      VariableType -> "CCTK_REAL",
      Timelevels -> lookup[extras, Timelevels, lookup[extras, InterfaceTimelevels, 1]],
      GridType -> gridType,
      Dim -> dim,
      Comment -> groupName[group], 
      Visibility -> "public",
      Tags -> GroupTags[group],
      gridType /. {"array" -> Sequence[Dim -> 1, Size -> 1], _ -> Sequence[]},
      Variables -> groupVariables[group]
    }]];

Options[CreateKrancInterface] = ThornOptions;

DefFn[CreateKrancInterface[declaredGroups:{_String...}, groups_List,
  implementation_String, inheritedImplementations:{_String...},
  includeFiles:{_String...}, opts:OptionsPattern[]] :=

  Module[{diffCoeff, getMap, getBbox, getBoundarySpecification,
    multipatchGetBoundarySpecification, symTabHdl, declaredGroupStructures,
    interface},
    (* These are the aliased functions that are USED by this thorn from other thorns *)

    diffCoeff = 
    {
      Name -> "Diff_coeff",
      Type -> "SUBROUTINE",
      ArgString -> "CCTK_POINTER_TO_CONST IN cctkGH, CCTK_INT IN dir, CCTK_INT IN nsize, CCTK_INT OUT ARRAY imin, CCTK_INT OUT ARRAY imax, CCTK_REAL OUT ARRAY q, CCTK_INT IN table_handle"
    };

    getMap = 
    {
      Name -> "MultiPatch_GetMap",
      Type -> "CCTK_INT",
      ArgString -> "CCTK_POINTER_TO_CONST IN cctkGH"
    };

    getBbox = 
    {
      Name -> "MultiPatch_GetBbox",
      Type -> "CCTK_INT",
      ArgString -> "CCTK_POINTER_TO_CONST IN cctkGH, CCTK_INT IN size, CCTK_INT OUT ARRAY bbox"
    };

    getBoundarySpecification = 
    {
      Name -> "GetBoundarySpecification",
      Type -> "CCTK_INT",
      ArgString -> "CCTK_INT IN size, CCTK_INT OUT ARRAY nboundaryzones, CCTK_INT OUT ARRAY is_internal, CCTK_INT OUT ARRAY is_staggered, CCTK_INT OUT ARRAY shiftout"
    };

    multipatchGetBoundarySpecification = 
    {
      Name -> "MultiPatch_GetBoundarySpecification",
      Type -> "CCTK_INT",
      ArgString -> "CCTK_INT IN map, CCTK_INT IN size, CCTK_INT OUT ARRAY nboundaryzones, CCTK_INT OUT ARRAY is_internal, CCTK_INT OUT ARRAY is_staggered, CCTK_INT OUT ARRAY shiftout"
    };

    symTabHdl = 
    {
      Name -> "SymmetryTableHandleForGrid",
      Type -> "CCTK_INT",
      ArgString -> "CCTK_POINTER_TO_CONST IN cctkGH"
    };

    declaredGroupStructures = 
      Map[declaredGroupInterfaceStructure[groupFromName[#, groups]] &, 
          declaredGroups];

    interface = Join[CreateInterface[implementation, inheritedImplementations,
      Join[includeFiles, {CactusBoundary`GetIncludeFiles[]},
           If[OptionValue[UseLoopControl], {"loopcontrol.h"}, {}],
           If[OptionValue[UseOpenCL], OpenCLIncludeFiles[], {}],
           If[OptionValue[UseVectors], {"vectors.h"}, {}]],
      declaredGroupStructures,
      UsesFunctions ->
        Join[MoLUsedFunctions[],
          {diffCoeff, getMap, getBbox, getBoundarySpecification,
            multipatchGetBoundarySpecification,symTabHdl}, 
             CactusBoundary`GetUsedFunctions[]]],
   {If[OptionValue[UseCaKernel], CaKernelInterfaceCLL[], {}]}];

    Return[interface]]];

End[];

EndPackage[];
