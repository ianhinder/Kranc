
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
    along with Kranc; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(****************************************************************************)
(* Manipulate Kranc group structures                                        *)
(****************************************************************************)

BeginPackage["KrancGroups`", 
             {"Kranc`", "Errors`", "MapLookup`"}];

CreateGroup;
groupsFromGFs::usage = "";
variablesInGroup::usage = "";
variablesFromGroups::usage = "";
groupFromName::usage = "";
groupName::usage = "";
renameGroup::usage = "";
qualifyGroupName::usage = "";
qualifyGFName::usage = "";
unqualifiedGroupName::usage = "";
implementationFromGroupName::usage = "";
qualifyGroups::usage = "";
containingGroups::usage = "";
groupVariables::usage = "";
GroupTags::usage = "";
SetGroupVariables;
VerifyGroup;
VerifyGroupName;
SetGroupName;
AddGroupTag;
AddGroupExtra;
GroupTimelevels;
allGroupVariables;
NonevolvedTimelevels;
CheckGroups;
VerifyGroupNames;
VerifyGroups;

Begin["`Private`"];

(* The Group structure is of the following form, but the user should
not assume this. All interaction with Group structures should be
through this file.  This way, we can modify the underlying
representation without other code having to be rewritten.

{name, {vars}, extras...}

The extras can be any of the following:

Tags -> {tag1, tag2, ...}
Timelevels -> x


  *)

(*********************************************************************)
(* The following functions know about the internal form of a Group
   structure*)
(*********************************************************************)

CreateGroup[name_, vars_, extras_] :=
  Module[{g},
    VerifyGroupName[name];
    VerifyList[vars];
    VerifyList[extras];
    g = Join[{name, vars}, extras];
    InfoMessage[InfoFull, "Created group: ", g];
    Return[g]];

AddGroupExtra[group_, extra_] :=
  Append[group, extra];


VerifyGroup[group_] :=
  Module[{},
    If[!ListQ[group] || Length[group] < 2 || !StringQ[group[[1]]] || ! ListQ[group[[2]]],
      ThrowError["Not a group definition:", group],
      True]];

VerifyGroupName[groupName_] :=
  If[!StringQ[groupName],
    ThrowError["Not a group name:", groupName],
    True];

GroupTimelevels[g_] :=
  Module[{extras},
    extras = Drop[g, 2];
    lookupDefault[extras, Timelevels, False]];

NonevolvedTimelevels[group_] :=
  Module[{tls = GroupTimelevels[group]},
    If[ tls === False, 1, tls]];



groupName[g_] := First[g];

groupVariables[group_] :=
  group[[2]];

GroupTags[g_] :=
  Module[{extras},
    extras = Drop[g, 2];
    lookupDefault[extras, Tags, {}]];

AddGroupTag[g_, t_] :=
  Module[{extras, tags},
    extras = Drop[g, 2];
    tags = lookupDefault[extras, Tags, {}];
    tags = Join[tags, {t}];
    Join[DeleteCases[g, Tags->_], {Tags -> tags}]];

SetGroupName[g_, n_] :=
  Join[{n}, Drop[g, 1]];

SetGroupVariables[g_, vars_] :=
  Join[{groupName[g], vars}, Drop[g, 2]];

(*********************************************************************)
(* The following functions DO NOT KNOW about the internal form of a Group
   structure*)
(*********************************************************************)


(* Return those group structures which contain any variables in GFs *)
groupsFromGFs[groups_, GFs_] := Module[{inter, check},
  (* Given a group structure, return those variables from it that are in
     GFs *)
  inter[y_] :=  Intersection[groupVariables[y], GFs]; (* The last arg was flattened; why? *)

  (* Check whether the group structure y contains any variables in GFs *)
  check[y_] := TrueQ[Length@inter[y] > 0];

  Select[groups, check]
];

renameGroup[g_, newName_] :=
  SetGroupName[g, newName];

variablesInGroup[name_, groups_] :=
  groupVariables[groupFromName[name, groups]];

variablesFromGroups[groupNames_, groups_] := 
  Flatten[Map[variablesInGroup[#, groups] &, groupNames], 1];

groupFromName[name_, groups_] :=
  Module[{gs},
    gs = Select[groups, groupName[#] === name &];
    If[Length[gs] == 0,
       ThrowError["Cannot find group ", name, "in", groups]];
    If[Length[gs] > 1,
       ThrowError["Group", name, "appears multiple times in", groups]];

    First[gs]];

qualifyGroupName[name_, defaultImp_] :=
  If[StringMatchQ[name, "*::*"],
     name,
     defaultImp <> "::" <> name];


unqualifiedGroupName[name_] :=
  If[StringQ@name && StringMatchQ[ToString@name, "*::*"],
     Module[{colon = First[First[StringPosition[name, ":", 1]]]},
       StringDrop[name, colon + 1]],
     name];

implementationFromGroupName[name_] :=
  If[StringQ@name && StringMatchQ[ToString@name, "*::*"],
     Module[{colon = First[First[StringPosition[name, ":", 1]]]},
       StringDrop[name, colon - 1 - StringLength@name]],
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
    allVars = Apply[Join, Map[groupVariables, groups]];
    Map[If[!MemberQ[allVars, #],
           ThrowError[ToString[#] <> 
                 " is not a member of any of the following groups: ",
                 groups]] &, vars];

    Union[Map[groupName, Select[groups, Intersection[groupVariables[#], vars] != {} &]]]];


qualifyGFName[gfname_, allgroups_, defaultImp_] := 
  If[StringQ@gfname && StringMatchQ[gfname, "*::*"],
     gfname,
     
     Module[{groupName, imp, newGFname},
       groupName = First[containingGroups[{gfname}, allgroups]];
       imp = implementationFromGroupName[qualifyGroupName[groupName, defaultImp]];
       newGFname = imp <> "::" <> ToString[gfname];
       newGFname
]
  ];

allGroupVariables[groups_] :=
  Flatten[Map[groupVariables, groups], 1];

CheckGroups[groups_] :=
  Module[{vs, names},
(*    If[!MatchQ[{_String, {_Symbol ...}, {_Symbol -> _} ...}],
        ThrowError["Groups structure should be of the form {name, {vars, ...}, extras}"]]; *)

    vs = Map[ToString, Union[Flatten[Map[groupVariables, groups]]]];
    names = Map[groupName, groups];

    If[(int = Intersection[vs,names]) =!= {},
      ThrowError["Variable names and group names must be distinct.  Group names which are also variable names:", int]];
  ];

VerifyGroups[gs_] := 
  If[!ListQ[gs],
   ThrowError["Not a list of group definitions: ", gs],
   Map[VerifyGroup, gs]];

VerifyGroupNames[gns_] := 
  If[!ListQ[gns],
   ThrowError["Not a list of group names: ", gns],
   Map[VerifyGroupName, gns]];

End[];

EndPackage[];
