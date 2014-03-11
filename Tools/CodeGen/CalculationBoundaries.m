
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

BeginPackage["CalculationBoundaries`", {"Errors`", "Helpers`",
  "CodeGenKranc`", "Kranc`", "MapLookup`", "KrancGroups`", "CodeGen`", "CodeGenCactus`", "CodeGenC`"}];

CalculationBoundariesFunction;

Begin["`Private`"];

(* This function is duplicated in Schedule.m.  It should be moved into
   another place. *)
groupsSetInCalc[calc_, groups_] :=
  Module[{gfs, eqs, lhss, gfsInLHS, lhsGroupNames},
    gfs = allGroupVariables[groups];
    eqs = lookup[calc, Equations];
    lhss = Map[First, eqs];
    gfsInLHS = Union[Cases[lhss, _ ? (MemberQ[gfs,#] &), Infinity]];

    lhsGroupNames = containingGroups[gfsInLHS, groups];
    Return[lhsGroupNames]
  ];


(* Given a calculation, create a function which selects the groups set
   in that calculation for a boundary condition. This is designed to
   be used for calculations scheduled in MoL_PseudoEvolution, NOT for
   evolved variables.  Currently only a flat boundary condition is
   implemented. *)

CalculationBoundariesFunction[calc_List] :=
  Module[{funcName, selectGroup, groups, groupNamesSet, imp},

  funcName = lookup[calc, Name];
  imp = lookup[calc,Implementation];

  (* If the calculation sets every grid point, we don't need to apply
     any boundary condition *)
  If[lookupDefault[calc, Where, Everywhere] === Everywhere,
     Return[{}]];

    (* The boundary interface allows you to give different boundary
       widths for each boundary face.  However, this is not compatible
       with ReflectionSymmetry, so we don't allow it here.*)
    selectGroup[g_String] :=
      Module[{},
        {(* "table = GenericFD_BoundaryWidthTable(cctkGH);\n", *)
         "ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, GetBoundaryWidth(cctkGH), -1 /* no table */, ",
                 Quote[g], ",", Quote["flat"], ");\n",
        "if (ierr < 0)\n",
        "  CCTK_WARN(1, " <> Quote["Failed to register flat BC for "<>g<>"."] <> ");\n" }];

    groups = lookup[calc, Groups];
    groupNamesSet = qualifyGroupName[#, imp] & /@ groupsSetInCalc[calc, groups];

    DefineCCTKFunction[funcName <> "_SelectBCs", "void",
    {
      "if (cctk_iteration % " <> funcName <> "_calc_every != " <> funcName <> "_calc_offset)\n",
      "  return;\n",
      DefineVariable["ierr",   "CCTK_INT", "0"],
(*      DefineVariable["table",  "CCTK_INT", "-1"],*)
      Map[selectGroup, groupNamesSet],
      "return;\n"
    }]];

End[];

EndPackage[];
