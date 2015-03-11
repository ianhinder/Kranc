
(*  Copyright 2004-2013 Sascha Husa, Ian Hinder, Christiane Lechner

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

BeginPackage[
  "CodeGenSchedule`",
  {"Errors`", "Helpers`", "Kranc`", "CodeGen`", "CodeGenC`", "MapLookup`",
   "CodeGenKranc`", "CodeGenCactus`"}];

CreateSchedule::usage = "Create the content of the schedule.ccl file.";

Begin["`Private`"];

(* ------------------------------------------------------------------------ 
   Scheduling
   ------------------------------------------------------------------------ *)

(* storage group 

  (represents the fact that we want to allocate storage for a Cactus
  variable group with the given number of timelevels)

  {KrancGroup -> "admbase::metric", Timelevels -> 3,
   Conditional -> {Parameter -> "", Value -> ""},
   Conditionals -> {{Parameter -> "", Value -> ""}},
   Conditional -> {Textual -> "CCTK_EQUALS(name,value)"}}

   A "conditional" structure looks like this: {Parameter -> "", Value -> ""}

  scheduled function: (a function to be scheduled at a particular point)

  {Name -> "ADM_BSSN_CalcRHS_fn", SchedulePoint -> "in POSTINITIAL before ExternalLapse", 
   Language -> "C", Comment -> "", 
   (optional) SynchronizedGroups -> {ADM_BSSN_gamma, ...}, 
   (optional) Options -> {"meta", "level", ...},
   (optional) StorageGroups -> {KrancGroup -> "mygroup", Timelevels -> 1},
   (optional) Conditional -> {Parameter -> "", Value -> ""},
   (optional) Conditionals -> {{Parameter -> "", Value -> ""}}}

  scheduled group:

  {... sameish}

*)

(* Given a storage group structure defined above, return a CodeGen
   structure for inclusion in the schedule.ccl file to allocate
   storage for this group. *)
groupStorage[spec_] :=
  Module[
    {tls = lookup[spec,Timelevels],
     group = lookup[spec, KrancGroup]},
    Which[
      IntegerQ[tls],
      {"STORAGE: ", group, "[", tls, "]\n"},

      StringQ[tls],
      {"STORAGE: ", group, "[", tls, "]\n"},

      True, Error["Unrecognized Timelevels value "<>ToString[tls]]]];


(* Given a function scheduling specification as defined above, return
   a CodeGen block to schedule the function for the schedule.ccl file *)
scheduleUnconditionalFunction[spec_] :=
  {"schedule ", lookup[spec, Name], " ", lookup[spec,SchedulePoint], "\n",
   SuffixedCBlock[
     {If[lookup[spec, Language] != "None",
         "LANG: " <> lookup[spec, Language] <> "\n",
         ""],

      If[lookupDefault[spec, Options, ""] != "",
         "OPTIONS: " <> lookup[spec, Options] <> "\n",
         ""],

      (* Insert a SYNC line for each group we want to synchronize. *)
      Map[{"SYNC: ", #, "\n"}     &, lookupDefault[spec, SynchronizedGroups, {}]],

      Map[{"TRIGGERS: ", #, "\n"} &, lookupDefault[spec, TriggerGroups, {}]],

      (* TODO: Expect a set of keyword/value pairs instead of a string *)
      If[lookupDefault[spec, Tags, ""] != "",
         "TAGS: " <> lookup[spec, Tags] <> "\n",
         ""],

      translateRegion[r_] := Switch[r,
                                    Everywhere, "Everywhere",
                                    Interior, "Interior",
                                    InteriorNoSync, "Interior",
                                    Boundary, "Boundary",
                                    BoundaryNoSync, "Boundary",
                                    Automatic,"Interior", (* TODO: is this right? *)
                                    _, "ERROR(" <> ToString[r] <> ")"];
      Map[{"READS: ", #, "(",
           translateRegion[lookupDefault[spec, RequiredRegion, "ERROR"]],
           ")\n"} &,
          lookupDefault[spec, RequiredGroups, {}]],
      Map[{"WRITES: ", #, "(",
           translateRegion[lookupDefault[spec, ProvidedRegion, "ERROR"]],
           ")\n"} &,
          lookupDefault[spec, ProvidedGroups, {}]],

      (* Insert a storage block for each group we want to allocate
         storage for *)
      Map[groupStorage, lookupDefault[spec, StorageGroups, {}]]},

      Quote[lookup[spec, Comment]]]};

(* Handle the aspect of scheduling the function conditionally *)
scheduleFunction[spec_,params_] :=
  Module[{condition, conditions, parameter, value, u, v, w, x, y},

    u = scheduleUnconditionalFunction[spec];

    v = If[mapContains[spec, Conditional],

           (* Output the conditional structure *)
           condition = lookup[spec, Conditional];
    
           If[mapContains[condition, Textual],
              
              ConditionalOnParameterTextual[lookup[condition, Textual], u],
              
              If[mapContains[condition, Parameter],
                 
                 parameter = lookup[condition, Parameter];
                 value     = lookup[condition, Value];
                 ConditionalOnParameter[parameter, value, u],
                 
                 If[condition != {},
                    ThrowError["Unrecognized conditional structure", condition],
                    u]]],
           u];

    w = If[mapContains[spec, Conditionals],
    
           (* Output the conditionals structure *)
           conditions = lookup[spec, Conditionals];
    
           Fold[Function[{x, condition},
    
                If[mapContains[condition, Textual],
                  
                   ConditionalOnParameterTextual[lookup[condition, Textual], x],
                   
                   If[mapContains[condition, Parameter],
                   
                      parameter = lookup[condition, Parameter];
                      value     = lookup[condition, Value];
                      ConditionalOnParameter[parameter, value, x],
                        
                      If[condition != {},
                         ThrowError["Unrecognized conditional structure", condition],
                         x]],
                   x]],
                v, conditions],
           v];

    y = If[mapContains[spec, NewConditional],
           cond = lookup[spec, NewConditional];
           Module[
             {render, renderbool, paramPattern},

             paramPattern = Except[True | False, _Symbol | _Parameter];

             renderbool[Equal[a:paramPattern,b_String]] := {"CCTK_EQUALS(", rendervalue[a], ",\"", b,"\")"};
             renderbool[Unequal[a:paramPattern,b_String]] := {"!CCTK_EQUALS(", rendervalue[a], ",\"", b,"\")"};
             renderbool[Equal[a:paramPattern,b_?NumberQ]] := {rendervalue[a], " == ", rendervalue[b]};
             renderbool[Unequal[a:paramPattern,b_?NumberQ]] := {rendervalue[a], " != ", rendervalue[b]};

             renderbool[Or[a_,b_]] := {"(",renderbool[a]," || ", renderbool[b],")"};
             renderbool[And[a_,b_]] := {"(",renderbool[a]," && ", renderbool[b],")"};
             renderbool[Not[a_]] := {"(!", renderbool[a],")"};
             renderbool[a:paramPattern] := ToString[a/.(Parameter[x_]->x)]; (* Boolean parameter *)

             (* rendervalue[a_String] := a; -- Allow literal pass-through *)
             rendervalue[a_?NumberQ] := ToString[a];
             rendervalue[Parameter[a_String]] := a;
             rendervalue[a_ /; MemberQ[params,a]] := ToString[a];
             renderbool[x_] := ThrowError["Unexpected value in run-time conditional expression (boolean):", x, "in", cond];
             render[x_] := ThrowError["Unexpected value in run-time conditional expression (value):", x, "in", cond];

             unparen[s_] := 
             Module[
               {s2 = FlattenBlock[s],result},
               result = StringReplace[FlattenBlock[s2],StartOfString ~~ "(" ~~ any__ ~~ ")" ~~ EndOfString :> any];
               If[result === s2, result, unparen[result]]];

             ConditionalOnParameterTextual[unparen@renderbool[cond], w]],
           w];

    y];


(* Schedule a schedule group.  Use a slightly dirty trick; given that
   the structure is identical to that for a function except with the
   word "GROUP" added before the function name, just use the existing
   function. *)
scheduleGroup[spec_,params_] :=
  scheduleFunction[mapReplace[spec, Name, "group " <> lookup[spec, Name]],params];

mylookup[list_List,key_] := 
  Map[Last,Select[list,MatchQ[#,key->_]&]];
mylookup[_,_] := ThrowError["Need list as first arg to mylookup"];

mergeRules[rules_] := Module[{key,keys},
  keys=DeleteDuplicates[Map[First,rules]];
  (*Table[Print[key,"==>",mylookup[rules,key]],{key,keys}]*)
 Table[Rule[key,Flatten[mylookup[rules,key]]],{key,keys}]
];

(* Copied from the web:
   http://mathematica.stackexchange.com/questions/18100/removing-elements-from-a-list-which-appear-in-another-list
*)
removeFrom[b_List, a_List] := Module[{f,g},
  (f[#] = -#2) & @@@ Tally[a];
  g[x_] /; f[x] < 0 := f[x]++;
  g[_] := True;
  Select[b,g]]

buildAfterRules[sf_] := Module[{pg,sp,co,gr},
  co = mylookup[sf,Comment];
  sp = mylookup[sf,SchedulePoint];
  rq = Flatten[mylookup[sf,RequiredGroups]];
  pg = Flatten[mylookup[sf,ProvidedGroups]];
  If[Length[pg] == 0,Return[Null]];
  Rule[sp[[1]],Table[Rule[gr,Dep[co[[1]]]],{gr,pg}]]
];

multiWriterWarning[a_,b_List] := 
  If[Length[b]>1,
    Print["Warning: multiple writers for "<>a<>". Writers include "<>StringJoin[Riffle[Map[First,b],", "]]]];

applyAfterRules[sf_,rules_] := Module[{co,myrules,rq,pg,ap,aps,sfn,is},
  co = mylookup[sf,Comment];
  sp = mylookup[sf,SchedulePoint];
  myrules = sp[[1]] /. rules;
  If[MatchQ[myrules,_String],Return[sf]];
  rq = Flatten[mylookup[sf,RequiredGroups]];
  pg = Flatten[mylookup[sf,ProvidedGroups]];
  is = Intersection[rq,pg];
  If[Length[is] > 0,
    Print["Warning: grid function(s) "<>StringJoin[Riffle[is,", "]]<>" are both read and written by "<>co[[1]]];
    rq=removeFrom[rq,pg];
  ];
  mr = mergeRules[myrules];
  (* mr /. Rule[a_,b_] :> multiWriterWarning[a,b]; *)
  (* ap = Map[(# /. Dep[dep_] :> dep)&,Select[rq /. myrules,MatchQ[#,Dep[_]]&]]; *)
  ap = DeleteDuplicates[Map[(# /. Dep[dep_] :> dep)&,Select[Flatten[rq /. mergeRules[myrules]],MatchQ[#,Dep[_]]&]]];
  If[Length[ap]===0,Return[sf]];
  aps = StringJoin[" after ",Riffle[ap,", "]];
  sfn=sf /. (SchedulePoint->schedpt_) :> (SchedulePoint->StringJoin[schedpt,aps]);
  Return[sfn];
];

(* Taking a list of group storage specifications for global storage,
   and lists of scheduled function and scheduled group structures,
   return a CodeGen block representing a schedule.ccl file. *)
CreateSchedule[globalStorageGroups_, scheduledGroups_, scheduledFunctions_, params_] := Module[{sf,updatedScheduleFunctions},
  rules = mergeRules[Select[Map[buildAfterRules,scheduledFunctions],(# =!= Null)&]];
  updatedScheduledFunctions=Map[applyAfterRules[#,rules]&,scheduledFunctions];
  {FileHeader["CCL"],
   NewlineSeparated[Map[groupStorage[#]             &, globalStorageGroups]],
   NewlineSeparated[Map[scheduleFunction[#,params]  &, updatedScheduledFunctions]],
   NewlineSeparated[Map[scheduleGroup[#,params]     &, scheduledGroups]]}];

End[];

EndPackage[];
