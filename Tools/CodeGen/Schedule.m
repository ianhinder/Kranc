
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

BeginPackage["Schedule`", {"Thorn`", "KrancGroups`", "MapLookup`", "Errors`", "Helpers`", "Kranc`"}];

CreateKrancScheduleFile;

Begin["`Private`"];

(* --------------------------------------------------------------------------
   Scheduling
   -------------------------------------------------------------------------- *)

simpleGroupStruct[groupName_, timelevels_] := 
{
  Group -> groupName, 
  Timelevels -> timelevels
};

evolvedGroupStruct[groupName_, timelevels_, maxtimelevels_] := 
{
  Group -> groupName, 
  Timelevels -> timelevels,
  MaxTimelevels -> "timelevels"
};

rhsGroupStruct[groupName_, timelevels_, maxtimelevels_] := 
{
  Group -> groupName, 
  Timelevels -> timelevels,
  MaxTimelevels -> "rhs_timelevels"
};

groupsSetInCalc[calc_, groups_] :=
  Module[{gfs, eqs, lhss, gfsInLHS, lhsGroupNames},
    gfs = allGroupVariables[groups];
    eqs = lookup[calc, Equations];
    lhss = Map[First, eqs];
    gfsInLHS = Union[Cases[lhss, _ ? (MemberQ[gfs,#] &), Infinity]];

    lhsGroupNames = containingGroups[gfsInLHS, groups];
    Return[lhsGroupNames]
  ];

(* Each calculation can be scheduled at multiple points, so this
   function returns a LIST of schedule structures for each calculation
   *)
scheduleCalc[calc_, groups_] :=
  Module[{points, conditional, conditionals, keywordConditional,
          keywordConditionals, triggered, keyword, value, keywordvaluepairs,
          groupsToSync, groupName, userSchedule, groupSched, fnSched,
          selbcSched, appbcSched, bcGroupName, condParams, bcGroupSched, before, after, relStr},
    conditional = mapContains[calc, ConditionalOnKeyword];
    conditionals = mapContains[calc, ConditionalOnKeywords];
    triggered = mapContains[calc, TriggerGroups];
    If[conditional,
      keywordConditional = lookup[calc, ConditionalOnKeyword];
      If[! MatchQ[keywordConditional, {lhs_String, rhs_String}],
        ThrowError["ConditionalOnKeyword entry in calculation expected to be of the form {parameter, value}, but was ", keywordConditional, "Calculation is ", calc]];

      keyword = keywordConditional[[1]];
      value = keywordConditional[[2]];
      ];
    If[conditionals,
      keywordConditionals = lookup[calc, ConditionalOnKeywords];
      If[! MatchQ[keywordConditionals, {{_, _} ...}],
        ThrowError["ConditionalOnKeywords entry in calculation expected to be of the form {{parameter, value}}, but was ", keywordConditionals, "Calculation is ", calc]];

      keywordvaluepairs =
        Map[# /. {keyword_, value_} -> {Parameter -> keyword, Value -> value} &,
            keywordConditionals];
      ];

    groupsToSync = If[lookupDefault[calc, Where, Everywhere] === Interior || 
                      lookupDefault[calc, Where, Everywhere] === Boundary,
                      groupsSetInCalc[calc, groups],
                      {}];

    before = lookupDefault[calc, Before, None];
    after = lookupDefault[calc, After, None];

    relStr = If[before =!= None, " before " <> before, ""]
             <> If[after =!= None, " after " <> after, ""];

    userSchedule = lookupDefault[calc, Schedule, Automatic];
    If[userSchedule =!= Automatic,
    Return[Map[
      Join[
      {
        Name               -> lookup[calc, Name],
        SchedulePoint      -> # <> relStr,
        SynchronizedGroups -> If[StringMatchQ[#, "*MoL_CalcRHS*", IgnoreCase -> True] || StringMatchQ[#, "*MoL_RHSBoundaries*", IgnoreCase -> True],
                                 {},
                                 groupsToSync],
        Language           -> CodeGen`SOURCELANGUAGE, 
        Comment            -> lookup[calc, Name]
      },
       If[triggered, {TriggerGroups -> lookup[calc, TriggerGroups]},
          {}],
       If[conditional, {Conditional -> {Parameter -> keyword, Value -> value}},
          {}],
       If[conditionals, {Conditionals -> keywordvaluepairs},
          {}]
      ] &,
      lookup[calc, Schedule]]],

      (* Scheduling is automatic.  For the moment, all automatically
      scheduled functions are going to be performed in
      MoL_PseudoEvolution in a new group, along with routines to apply
      boundary conditions to the variables set in the calculation. All
      variables set in the calculation will be synchronised. *)

      groupName = lookup[calc, Name] <> "_group";
      bcGroupName = lookup[calc, Name] <> "_bc_group";

      condParams = Join[
        If[conditional,
           {Conditional -> {Parameter -> keyword, Value -> value}}, {}],
        If[conditionals, {Conditionals -> keywordvaluepairs}, {}]];

      groupSched = {
        Name               -> "group " <> groupName,
        SchedulePoint      -> "in MoL_PseudoEvolution" <> relStr,
        SynchronizedGroups -> {},
        Language           -> "None",
        Comment            -> lookup[calc, Name]
      }
      ~Join~ condParams;

      fnSched = {
        Name               -> lookup[calc, Name],
        SchedulePoint      -> "in " <> groupName,
        Language           -> CodeGen`SOURCELANGUAGE,
        Comment            -> lookup[calc, Name]
      };

      bcGroupSched[where_] := {
        Name               -> "group " <> bcGroupName,
        SchedulePoint      -> where,
        SynchronizedGroups -> {},
        Language           -> "None",
        Comment            -> lookup[calc, Name]
      }
      ~Join~ condParams;

      selbcSched = {
        Name               -> lookup[calc, Name] <> "_SelectBCs",
        SchedulePoint      -> "in " <> bcGroupName,
        SynchronizedGroups -> groupsToSync,
        Options               -> "level",
        Language           -> CodeGen`SOURCELANGUAGE,
        Comment            -> lookup[calc, Name] <> "_SelectBCs"
      };

      appbcSched = {
        Name               -> "group ApplyBCs as " <> lookup[calc,Name] <> "_ApplyBCs",
        SchedulePoint      -> "in " <> bcGroupName <> " after " <> lookup[calc, Name] <> "_SelectBCs",
        Language           -> "None",
        Comment            -> "Apply BCs for groups set in " <> lookup[calc, Name]
      };

      Return[{groupSched, fnSched} ~Join~ If[groupsToSync =!= {},
        {selbcSched, appbcSched,
         bcGroupSched["in "<>groupName <> " after " <> lookup[calc, Name]],
         bcGroupSched["at CCTK_POSTRESTRICT"],
         bcGroupSched["at CCTK_POSTRESTRICTINITIAL"]},{}]]]];

CreateKrancScheduleFile[calcs_, groups_, evolvedGroups_, rhsGroups_, nonevolvedGroups_, thornName_, 
                        evolutionTimelevels_] :=
  Module[{scheduledCalcs, scheduledStartup, scheduleMoLRegister, globalStorageGroups, scheduledFunctions, schedule},

    scheduledCalcs = Flatten[Map[scheduleCalc[#, groups] &, calcs], 1];
    scheduledStartup = 
    {
      Name          -> thornName <> "_Startup",
      SchedulePoint -> "at STARTUP",
      Language      -> "C",
      Options       -> "meta",
      Comment       -> "create banner"
    };

    scheduleMoLRegister =
    {
      Name          -> thornName <> "_RegisterVars",
      SchedulePoint -> "in MoL_Register", 
      Language      -> "C", 
      Options       -> "meta",
      Comment       -> "Register Variables for MoL"
    };
    
    scheduleRegisterSymmetries =
    {
      Name          -> thornName <> "_RegisterSymmetries",
      SchedulePoint -> "in SymmetryRegister", 
      Language      -> "C",
      Options       -> "meta",
      Comment       -> "register symmetries"
    };

    globalStorageGroups =
      Join[
        Map[simpleGroupStruct[#,
              NonevolvedTimelevels[groupFromName[#, groups]]] &,
            nonevolvedGroups],
        Map[evolvedGroupStruct[#, evolutionTimelevels, evolutionTimelevels] &,
            evolvedGroups],
        Map[rhsGroupStruct[#, evolutionTimelevels, evolutionTimelevels] &,
            rhsGroups]];

    scheduledFunctions = 
      Join[{scheduledStartup, scheduleMoLRegister, scheduleRegisterSymmetries}, 
        scheduledCalcs, CactusBoundary`GetScheduledFunctions[thornName, evolvedGroups]];

    schedule = CreateSchedule[globalStorageGroups, 
      CactusBoundary`GetScheduledGroups[thornName], scheduledFunctions];

    Return[schedule]];

End[];

EndPackage[];
