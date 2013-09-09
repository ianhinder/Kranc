
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
                            "CodeGenInterface`"}];

CreateKrancInterface;

Begin["`Private`"];

(* --------------------------------------------------------------------------
   Interface and variable definitions
   -------------------------------------------------------------------------- *)

nonevolvedGroupInterfaceStructure[group_] := 
{
  Name -> groupName[group], 
  VariableType -> "CCTK_REAL",
  Timelevels -> NonevolvedTimelevels[group],
  GridType -> "GF",
  Comment -> groupName[group], 
  Visibility -> "public",
  Tags -> GroupTags[group],
  Variables -> groupVariables[group]
}

nonevolvedODEGroupInterfaceStructure[group_] := 
{
  Name -> groupName[group], 
  VariableType -> "CCTK_REAL",
  Timelevels -> NonevolvedTimelevels[group],
  GridType -> "array",
  Comment -> groupName[group], 
  Visibility -> "public",
  Tags -> GroupTags[group],
  Dim -> 1,
  Size -> 1,
  Variables -> groupVariables[group]
}

evolvedGroupInterfaceStructure[group_, timelevels_] := 
{
  Name -> groupName[group], 
  VariableType -> "CCTK_REAL",
  Timelevels -> timelevels, 
  GridType -> "GF",
  Comment -> groupName[group], 
  Visibility -> "public",
  Tags -> GroupTags[group],
  Variables -> groupVariables[group]
}

evolvedODEGroupInterfaceStructure[group_, timelevels_] := 
{
  Name -> groupName[group], 
  VariableType -> "CCTK_REAL",
  Timelevels -> timelevels, 
  GridType -> "array",
  Comment -> groupName[group], 
  Visibility -> "public",
  Tags -> GroupTags[group],
  Dim -> 1,
  Size -> 1,
  Variables -> groupVariables[group]
}

rhsGroupInterfaceStructure[group_, timelevels_] := 
{
  Name -> groupName[group], 
  VariableType -> "CCTK_REAL",
  Timelevels -> timelevels, 
  GridType -> "GF",
  Comment -> groupName[group], 
  Visibility -> "public",
  Tags -> GroupTags[group],
  Variables -> groupVariables[group]
}

rhsODEGroupInterfaceStructure[group_, timelevels_] := 
{
  Name -> groupName[group], 
  VariableType -> "CCTK_REAL",
  Timelevels -> timelevels, 
  GridType -> "array",
  Comment -> groupName[group], 
  Visibility -> "public",
  Tags -> GroupTags[group],
  Dim -> 1,
  Size -> 1,
  Variables -> groupVariables[group]
}

Options[CreateKrancInterface] = ThornOptions;

CreateKrancInterface[nonevolvedGroups_, evolvedGroups_, rhsGroups_,
  nonevolvedODEGroups_, evolvedODEGroups_, rhsODEGroups_, groups_,
  implementation_, inheritedImplementations_,
  includeFiles_, opts:OptionsPattern[]] :=

  Module[{registerEvolved, (*registerConstrained,*)
    nonevolvedGroupStructures, evolvedGroupStructures, rhsGroupStructures,
    nonevolvedODEGroupStructures, evolvedODEGroupStructures, rhsODEGroupStructures,
    groupStructures, interface, getMap},
    VerifyGroupNames[nonevolvedGroups];
    VerifyGroupNames[evolvedGroups];
    VerifyGroupNames[rhsGroups];
    VerifyGroupNames[nonevolvedODEGroups];
    VerifyGroupNames[evolvedODEGroups];
    VerifyGroupNames[rhsODEGroups];
    VerifyGroups[groups];
    VerifyString[implementation];
    VerifyStringList[inheritedImplementations, "InheritedImplementations"];
    VerifyStringList[includeFiles, "IncludeFiles"];
    (* These are the aliased functions that are USED by this thorn from other thorns *)
    registerEvolved = 
    {
      Name      -> "MoLRegisterEvolved",
      Type      -> "CCTK_INT",
      ArgString -> "CCTK_INT IN EvolvedIndex, CCTK_INT IN RHSIndex"
    };

    (*
    registerConstrained = 
    {
      Name      -> "MoLRegisterConstrained",
      Type      -> "CCTK_INT",
      ArgString -> "CCTK_INT IN ConstrainedIndex"
    };
    *)

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


    (* For each group declared in this thorn, we need an entry in the
        interface file.  Each evolved group needs an associated rhs
        group, but these are constructed at a higher level and are
        listed in the nonevolved groups. *)
    nonevolvedGroupStructures = 
      Map[nonevolvedGroupInterfaceStructure[groupFromName[#, groups]] &, 
          nonevolvedGroups];

    evolvedGroupStructures =
      Map[evolvedGroupInterfaceStructure[groupFromName[#, groups],
          OptionValue[EvolutionTimelevels]] &, evolvedGroups];

    rhsGroupStructures =
      Map[rhsGroupInterfaceStructure[groupFromName[#, groups],
          OptionValue[EvolutionTimelevels]] &, rhsGroups];

    nonevolvedODEGroupStructures = 
      Map[nonevolvedODEGroupInterfaceStructure[groupFromName[#, groups]] &, 
          nonevolvedODEGroups];

    evolvedODEGroupStructures =
      Map[evolvedODEGroupInterfaceStructure[groupFromName[#, groups],
          OptionValue[EvolutionTimelevels]] &, evolvedODEGroups];

    rhsODEGroupStructures =
      Map[rhsODEGroupInterfaceStructure[groupFromName[#, groups],
          OptionValue[EvolutionTimelevels]] &, rhsODEGroups];

    groupStructures = Join[nonevolvedGroupStructures,
                           evolvedGroupStructures, rhsGroupStructures,
                           nonevolvedODEGroupStructures,
                           evolvedODEGroupStructures, rhsODEGroupStructures];

    interface = Join[CreateInterface[implementation, inheritedImplementations,
      Join[includeFiles, {CactusBoundary`GetIncludeFiles[]},
           {"loopcontrol.h"},
           If[OptionValue[UseOpenCL], OpenCLIncludeFiles[], {}],
           If[OptionValue[UseVectors], {"vectors.h"}, {}]],
      groupStructures,
      UsesFunctions ->
        Join[{registerEvolved, (*registerConstrained,*)
                diffCoeff, getMap}, 
             CactusBoundary`GetUsedFunctions[]]],
   {If[OptionValue[UseCaKernel], CaKernelInterfaceCLL[], {}]}];

    Return[interface]];


End[];

EndPackage[];
