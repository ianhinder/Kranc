
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
    along with Foobar; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(****************************************************************************)
(* Manipulate Kranc group structures                                        *)
(****************************************************************************)

BeginPackage["sym`"];
EndPackage[];

BeginPackage["KrancGroups`", 
             {"sym`"}];

groupsFromGFs::usage = "";
addrhs::usage = "";
variablesInGroup::usage = "";
evolvedGroupToRHSGroup::usage = "";
variablesFromGroups::usage = "";
groupFromName::usage = "";
groupName::usage = "";
renameGroup::usage = "";
qualifyGroupName::usage = "";
unqualifiedGroupName::usage = "";
qualifyGroups::usage = "";
containingGroups::usage = "";

Begin["`Private`"];

(* Return those group structures which contain any variables in GFs *)
groupsFromGFs[groups_, GFs_] := Module[{inter, check},
  (* Given a group structure, return those variables from it that are in
     GFs *)
  inter[y_] :=  Intersection[Last@y, Flatten@GFs];

  (* Check whether the group structure y contains any variables in GFs *)
  check[y_] := TrueQ[Length@inter[y] > 0];

  Select[groups, check]
];

addrhs[x_] := ToString[x] <> "rhs";

variablesInGroup[name_, groups_] :=
  Last[groupFromName[name, groups]];

evolvedGroupToRHSGroup[name_, groups_] := 
  Module[{group = First[Select[groups, groupName[#] === name &]]},
    {addrhs[name], Map[Symbol[addrhs[ToString[#]]] &, Last[group]]}];

variablesFromGroups[groupNames_, groups_] := 
  Flatten[Map[variablesInGroup[#, groups] &, groupNames], 1];

groupFromName[name_, groups_] :=
  Module[{gs},
    gs = Select[groups, First[#] === name &];
    If[Length[gs] != 1,
       Throw["Cannot find group " <> name <> " in " <> ToString[groups]]];
    First[gs]];

groupName[g_] := First[g];

renameGroup[{name_, vars_}, newName_] :=
  {newName, vars};

qualifyGroupName[name_, defaultImp_] :=
  If[StringMatchQ[name, "*::*"],
     name,
     defaultImp <> "::" <> name];


unqualifiedGroupName[name_] :=
  If[StringMatchQ[name, "*::*"],
     Module[{colon = First[First[StringPosition[name, ":", 1]]]},
       StringDrop[name, colon + 1]],
     name];


(* Given a list of group definitions, and a list of group names, and
   an implementation name, return a new list of group definitions
   where the groups specified have had the implemention name added to
   them.  Those groups not in the list of names will have the defImp
   name added to them.  The names should be unqualified.  If the
   groups already have implementations, they are left untouched. *)

qualifyGroups[groups_, names_, imp_, defImp_] :=
  Module[{namedGroups = Select[groups, MemberQ[names, groupName[#]] &],
          otherGroups = Select[groups, ! MemberQ[names, groupName[#]] &],
          renamedNamedGroups, renamedOtherGroups},
    renamedNamedGroups = Map[renameGroup[#, qualifyGroupName[groupName[#], imp]] &, 
                        namedGroups];
    renamedOtherGroups = Map[renameGroup[#, qualifyGroupName[groupName[#], defImp]] &, 
                        otherGroups];

    Join[renamedNamedGroups, renamedOtherGroups]];

(* This is in krancthorns as well *)

containingGroups[vars_, groups_] :=
  Module[{allVars},
    allVars = Apply[Join, Map[variablesInGroup, groups]];
    Map[If[!MemberQ[allVars, #],
           Throw[ToString[#] <> 
                 " is not a member of any of the following groups: " <> 
                 ToString[groups]]] &, vars];

    Union[Map[groupName, Select[groups, Intersection[variablesInGroup[#], vars] != {} &]]]];



End[];

EndPackage[];
