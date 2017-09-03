
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

BeginPackage["Schedule`", {"Thorn`", "KrancGroups`", "MapLookup`", "Errors`", "Helpers`", "Kranc`", "CaKernel`", "Calculation`", "ParamCheck`", "CodeGenSchedule`"}];

CreateKrancScheduleFile;

Begin["`Private`"];

(* --------------------------------------------------------------------------
   Scheduling
   -------------------------------------------------------------------------- *)

storageStructure[groupName_, timelevels_] := 
{
  KrancGroup -> groupName,
  Timelevels -> timelevels
};

groupsSetInCalc[calc_, groups_] :=
  Module[{gfsInLHS, lhsGroupNames},
    gfsInLHS = variablesSetInCalc[calc, groups];
    lhsGroupNames = containingGroups[gfsInLHS, groups];
    Return[lhsGroupNames]
  ];

groupsReadInCalc[calc_, groups_] :=
  Module[{gfsInRHS, rhsGroupNames},
    gfsInRHS = variablesReadInCalc[calc, groups];
    rhsGroupNames = containingGroups[gfsInRHS, groups];
    Return[rhsGroupNames]
  ];

variablesSetInCalc[calc_, groups_] :=
  Module[{gfs, eqs, lhss, gfsInLHS},
    gfs = allGroupVariables[groups];
    eqs = lookup[calc, Equations];
    lhss = Map[First, eqs];
    gfsInLHS = Union[Cases[lhss, _ ? (MemberQ[gfs,#] &), Infinity]];
    Return[gfsInLHS]
  ];

variablesReadInCalc[calc_, groups_] :=
  Module[{eqsHaveVar, firstEqWithVar, eqHasVarInRHS,
          gfs, eqs, gfsInEqs, gfsInRHS},
    (* Look for the first equation in which the variable occurs in the
       calculation. If it is a RHS, then the variable is read. *)
    eqsHaveVar[eqs_, gf_] := MemberQ[eqs, gf, Infinity];
    firstEqWithVar[eqs_, gf_] := First[Select[eqs, MemberQ[#, gf, Infinity]&]];
    eqHasVarInRHS[eq_, gf_] := MemberQ[{Last[eq]}, gf, Infinity];
    gfs = allGroupVariables[groups];
    eqs = lookup[calc, Equations];
    gfsInEqs = Select[gfs, eqsHaveVar[eqs, #]&];
    gfsInRHS = Select[gfsInEqs, eqHasVarInRHS[firstEqWithVar[eqs, #], #]&];
    Return[gfsInRHS]
  ];


(* Each calculation can be scheduled at multiple points, so this
   function returns a LIST of schedule structures for each calculation
   *)

Options[scheduleCalc] = ThornOptions;
scheduleCalc[calc_, groups_, thornName_, OptionsPattern[]] :=
  Module[{points, conditional, conditionals, keywordConditional,
          keywordConditionals, triggered, keyword, value, keywordvaluepairs,
          groupsToSync,
          prefixWithScope, groupsToRequire, groupsToProvide,
          groupName, userSchedule, groupSched, fnSched,
          selbcSched, appbcSched, bcGroupName, condParams, bcGroupSched, before, after, relStr,
          variablesToRead, variablesToWrite},
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
            keywordConditionals],
      keywordvaluepairs = {}
      ];

    groupsToSync = If[lookupDefault[calc, Where, Everywhere] === Interior || 
                      lookupDefault[calc, Where, Everywhere] === Boundary,
                      groupsSetInCalc[calc, groups],
                      {}];

    prefixWithScope[group_] :=
      If[StringMatchQ[ToString[group], __~~"::"~~__],
         ToString[group],
         thornName <> "::" <> ToString[group]];
    (* groupsToRequire = prefixWithScope /@ groupsReadInCalc[calc, groups]; *)
    (* groupsToProvide = prefixWithScope /@ groupsSetInCalc[calc, groups]; *)

    variablesToRead = qualifyGFName[#, groups, thornName] & /@ variablesReadInCalc[calc, groups];
    variablesToWrite = qualifyGFName[#, groups, thornName] & /@ variablesSetInCalc[calc, groups];


    before = lookupDefault[calc, Before, None];
    after = lookupDefault[calc, After, None];

    relStr = If[before =!= None, " before " <> before, ""]
             <> If[after =!= None, " after " <> after, ""];

    applyBCs = lookupDefault[calc, ApplyBCs, False];
    userSchedule = GetSchedule[calc];


    If[userSchedule =!= Automatic && !applyBCs,
    Return[Map[
      Join[
      {
        Name               ->
           If[ #2, GetCalculationScheduleName[calc],
               GetCalculationName[calc] ]
           <> " as " <> GetCalculationName[calc],
        SchedulePoint      -> #1 <> relStr,
        SynchronizedGroups -> If[StringMatchQ[#1, "*MoL_CalcRHS*", IgnoreCase -> True] || StringMatchQ[#1, "*MoL_RHSBoundaries*", IgnoreCase -> True],
                                 {},
                                 groupsToSync],
        Language           -> CodeGenC`SOURCELANGUAGE, 
        Tags               -> If[ #2 || OptionValue[UseOpenCL],
                                  "Device=1", "" ],
        RequiredGroups     -> variablesToRead,
        RequiredRegion     -> Everywhere,   (* TODO: be more accurate *)
        ProvidedGroups     -> variablesToWrite,
        ProvidedRegion     -> lookupDefault[calc, Where, Everywhere],
        Comment            -> lookup[calc, Name],
        Conditionals ->
           Append[ keywordvaluepairs,
                   If[ CalculationOnDevice[calc],
                       { Parameter -> "use_kranc_c", Value -> Not[#2] },
                       {} ] ]
      },
       If[triggered, {TriggerGroups -> lookup[calc, TriggerGroups]},
          {}],
       If[conditional, {Conditional -> {Parameter -> keyword, Value -> value}},
          {}],
        If[mapContains[calc, Conditional], {NewConditional -> lookup[calc,Conditional]}, {}]
      ] & @@ # &,
         Join @@ ( If[ CalculationOnDevice[calc],
                       { {#,True}, {#,False} }, { {#,False} } ] &
                   /@ GetSchedule[calc] ) ] ],

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
        SchedulePoint      -> If[applyBCs, First[userSchedule] <> relStr, "in MoL_PseudoEvolution" <> relStr],
        SynchronizedGroups -> {},
        Language           -> "None",
        Comment            -> lookup[calc, Name]
      }
      ~Join~ condParams;

      (* We set required/provided groups here with the actual
         function, since (at least in principle) the driver should be
         able to deduce the synchronization and boundary condition
         treatment from this information. *)
      fnSched = {
        Name               -> lookup[calc, Name],
        SchedulePoint      -> "in " <> groupName,
        Language           -> CodeGenC`SOURCELANGUAGE,
        Tags               -> 
           (* TODO: Pass this as {keyword,value} pair instead of a string,
           once Thorn.m understands this format *)
           If[OptionValue[UseOpenCL] || CalculationOnDevice[calc],
              "Device=1", ""],
        RequiredGroups     -> variablesToRead,
        RequiredRegion     -> Everywhere,   (* TODO: be more accurate *)
        ProvidedGroups     -> variablesToWrite,
        ProvidedRegion     -> Interior, (* since we apply boundary conditions *)
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
        Language           -> CodeGenC`SOURCELANGUAGE,
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
         bcGroupSched["in MoL_PseudoEvolutionBoundaries after MoL_PostStep"]},{}]]]];

Options[CreateKrancScheduleFile] = ThornOptions;
CreateKrancScheduleFile[calcs_, declaredGroups_, groups_, thornName_, 
                        evolutionTimelevels_, opts:OptionsPattern[]] :=
  Module[{scheduledCalcs, scheduledStartup, scheduleMoLRegister, globalStorageGroups, scheduledFunctions, schedule, allParams, calcGroups, scheduleTimelevels},

    scheduledCalcs = Flatten[Map[scheduleCalc[#, groups, thornName, opts] &, calcs], 1];
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

    scheduleTimelevels[gn_] :=
      Module[
        {groupDef, extras},
        groupDef = groupFromName[gn, groups];
        extras = GroupExtras[groupDef];
        lookup[extras, ScheduleTimelevels, 
               (* Nonevolved groups do not have a ScheduleTimelevels
                  extra; we use this old code for now *)
                 (* Number of timelevels requested for this group, or 1 if no request made *)
                 If[NonevolvedTimelevels[groupDef]===1,
                    "other_timelevels",
                    "timelevels"]]];

    globalStorageGroups = Map[storageStructure[#, scheduleTimelevels[#]] &, declaredGroups];

    (* Schedule groups defined in calculations *)
    calcGroups = Union[Flatten[Map[lookup[#, ScheduleGroups, {}] &, calcs],1]];

    scheduledFunctions = 
      Join[{scheduledStartup, scheduleRegisterSymmetries}, 
        scheduledCalcs, CactusBoundary`GetScheduledFunctions[thornName,
                                                             declaredGroups,
                                                             groups],
           {scheduleMoLRegister}, If[Length[OptionValue[ParameterConditions]] > 0,
                                     {ParameterCheckSchedule[thornName]},
                                     {}]];

    If[OptionValue[UseCaKernel],
       scheduledFunctions = Join[scheduledFunctions, CaKernelSchedule[thornName]]];

    allParams = Union@@((lookup[#,Parameters] &) /@ calcs);
    schedule = CreateSchedule[globalStorageGroups, 
      Join[CactusBoundary`GetScheduledGroups[thornName], calcGroups], scheduledFunctions, allParams];

    Return[schedule]];

End[];

EndPackage[];
