
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

BeginPackage["Calculation`", {"Errors`", "Helpers`", "Kranc`", "KrancGroups`",
                              "MapLookup`","Differencing`"}];

InputGridFunctions;
OutputGridFunctions;
AllGridFunctions;
GetCalculationName;
GetCalculationScheduleName;
GetEquations;
GetCalculationParameters;
CalculationStencilSize;
CalculationPointwiseQ;
CalculationOnDevice;
GetCalculationWhere;
SplitCalculations;
SeparateDerivatives;
AddCondition;
AddConditionSuffix;
InNewScheduleGroup;
BoundaryCalculationQ;
GetSchedule;
FunctionsInCalculation;
GroupsInCalculation;
RemoveUnusedShorthands;
VerifyCalculation;
CalculationSymbols;
VerifyNewCalculation;
TileCalculationQ;
CalculationLoopControlQ;
SetCalculationLoopControl;
CalculationExpressionSymbols;

Begin["`Private`"];

DefFn[
  GetGridFunctions[calc_List] :=
  Module[
    {eqs,rhss,lhss,gfs,gfsInRHS,gfsInLHS},
    eqs = GetEquations[calc];

    rhss = Map[#[[2]] &, eqs];
    lhss = Map[#[[1]] &, eqs];

    gfs = Union[ allGroupVariables[lookup[calc,Groups]],
                 lookupDefault[calc,InheritedVariables,{}] ];

    gfsInRHS = Union[Cases[rhss, _ ? (MemberQ[gfs,#] &), Infinity]];
    gfsInLHS = Union[Cases[lhss, _ ? (MemberQ[gfs,#] &), Infinity]];

    {gfsInLHS, gfsInRHS}]];

DefFn[
  InputGridFunctions[calc_List] :=
  Last[GetGridFunctions[calc]]];

DefFn[
  OutputGridFunctions[calc_List] :=
  First[GetGridFunctions[calc]]];

DefFn[
  AllGridFunctions[calc_List] :=
  Union@@GetGridFunctions[calc]];

(* This is from the user's input calculation, not after processing/optimisation *)
DefFn[
  GetEquations[calc_List] :=
  lookup[calc,Equations]];

DefFn[
  GetPartialDerivatives[calc_List] :=
  lookup[calc,PartialDerivatives]];

DefFn[
  GetDerivatives[calc_] :=
  GridFunctionDerivativesInExpression[
    GetPartialDerivatives[calc], GetEquations[calc],{(* TODO: implement ZeroDimensions here *)}]];

DefFn[
  GetCalculationParameters[calc_List] :=
  Module[
    {syms,pdSyms,params},
    syms = Cases[GetEquations[calc], _?AtomQ|_String, {-1}];
    pdSyms = Cases[GetPartialDerivatives[calc], _?AtomQ|_String, {-1}];
    params = Union[ lookup[calc,Parameters],
                    lookupDefault[calc,InheritedParameters,{}] ];
    Intersection[params,Join[syms,pdSyms]]]];

DefFn[
  GetCalculationName[calc_List] :=
  lookup[calc,Name]];

DefFn[
  GetCalculationScheduleName[calc_List] :=
  If[lookup[calc, UseCaKernel, False] && CalculationOnDevice[calc],
     "CAKERNEL_Launch_",""]
  <>lookup[calc, Name]];

DefFn[
  GetCalculationWhere[calc_List] :=
  (* TODO: the default for Where is Everywhere; it should probably be Automatic *)
  lookup[calc,Where, Everywhere] /.
  (Automatic -> If[!CalculationPointwiseQ[calc], Interior, Everywhere])];

DefFn[
  BoundaryCalculationQ[calc_List] :=
  (* NB: CaKernel does not distinguish between these two.  It
     ALWAYS computes everywhere that it can, based on the stencil
     description.  *)
  MemberQ[{Boundary, BoundaryWithGhosts, BoundaryNoSync}, GetCalculationWhere[calc]]];

DefFn[
  CalculationPointwiseQ[calc_List] :=
  Module[
    {stencilSize = CalculationStencilSize[calc]},
    If[!MatchQ[stencilSize, {_Integer, _Integer, _Integer}],
      Error["Internal error: Invalid stencil size"]];
    MatchQ[stencilSize, {0,0,0}]]];

CombineStencils[{i1_,j1_,k1_},I3D[i2_,j2_,k2_]] :=
  {Max[i1,Abs[i2]],Max[j1,Abs[j2]],Max[k1,Abs[k2]]};
CombineStencils[arg1_,{}] := arg1;
CombineStencils[arg1_,{arg2_,arg3___}] :=
  CombineStencils[CombineStencils[arg1,arg2],{arg3}];

DefFn[
  CalculationStencilSize[calc_List] :=
  Module[
    {pddefs, eqs, stencilSize, explicit, name, i, j, k},

    pddefs = lookup[calc, PartialDerivatives, {}];
    eqs    = lookup[calc, Equations];
    explicit = {};
    calc /. I3D[name_,i_?NumberQ,j_?NumberQ,k_?NumberQ] :> (AppendTo[explicit,I3D[i,j,k]];I3D[name,i,j,k]);

    stencilSize = StencilSize[pddefs, eqs, "not needed", {} (*ZeroDimensions*),
                              lookupDefault[calc, IntParameters, {}]];

    (* We cannot handle a stencil size which depends on a parameter,
       so take the maximum that it could possibly be *)
    If[!VectorQ[stencilSize],
       stencilSize = MapThread[Max,Map[Last,stencilSize[[2]]]]];
    (* TODO: decide what to do about stencil sizes that depend on runtime parameters *)
    stencilSize = CombineStencils[stencilSize,explicit];
    stencilSize]];

DefFn[
  CalculationOnDevice[calc_List] :=
  lookupDefault[calc, ExecuteOn, Automatic] =!= Host];

partialCalculation[calc_, suffix_, updates_, evolVars_] :=
Module[
  {name, calc1, replaces, calc2, vars, patterns, eqs, calc3},
  (* Add suffix to name *)
  name     = lookup[calc, Name] <> suffix;
  calc1    = mapReplace[calc, Name, name];
  (* Replace some entries in the calculation *)
  replaces = updates //. (lhs_ -> rhs_) -> (mapReplace[#, lhs, rhs]&);
  calc2 = Apply[Composition, replaces][calc1];
  (* Remove unnecessary equations *)
  vars     = Join[evolVars, lookup[calc2, Shorthands]];
  patterns = Replace[vars, {    Tensor[n_,__]  ->     Tensor[n,__] ,
                            dot[Tensor[n_,__]] -> dot[Tensor[n,__]]}, 1];
  eqs      = FilterRules[lookup[calc, Equations], patterns];
  calc3    = mapReplace[calc2, Equations, eqs]
  (* Append[calc3,CachedVariables -> (GetDerivatives[calc3]/.pd_[var_,___]->var)] *)
];

DefFn[
  SplitCalculations[calcs_List] :=
  Flatten[SplitCalculation/@calcs,1]];

(* Split a calculation into a set of equivalent calculations.  Any
   required shorthands are recomputed in each calculation.  The split
   is determined by the SplitVars calculation option, which is a list
   of split specifications.  A split specification can be a single
   variable, in which case the calculation will compute only that
   variable, and will be named oldName_<varname>, or it can be a list
   of variables, in which case the calculation will compute all those
   variables and will be named with a numeric index. *)
DefFn[
  SplitCalculation[calc_] :=
  Module[
    {splitBy = lookup[calc,SplitBy, {}],
     oldName = lookup[calc,Name],
     oldSchedule = GetSchedule[calc],
     newGroup},

    (* If there is nothing to split, return the calculation without any changes *)
    If[Intersection[Flatten[splitBy,1],OutputGridFunctions[calc]] === {},
       Return[{calc}]];

    If[ListQ[oldSchedule] && Length[oldSchedule] > 1,
       ThrowError["Cannot split a calculation which is scheduled in more than one place"]];

    newGroup = {Name          -> oldName,
                Language      -> "None", (* groups do not have a language *)
                SchedulePoint -> oldSchedule,
                Comment       -> ""};

    MapIndexed[
      Function[
        {var,i},
        Module[
          {nameSuffix, splitVars},
          nameSuffix = If[ListQ[var],
                          ToString[i[[1]]],
                          "_"<>StringReplace[ToString[var],{"["->"","]"->"",","->""}]];
          splitVars = If[ListQ[var], var, {var}];

          newCalc = partialCalculation[calc, nameSuffix, {}, splitVars];

          newCalc =
          mapReplaceAdd[
            mapReplaceAdd[
              newCalc,
              Schedule, {"in "<>oldName}],
            ScheduleGroups, Append[lookup[calc, ScheduleGroups, {}],newGroup]]]],
      splitBy]
  ]
];

DefFn[
  SeparateDerivatives[calcs_List] :=
  Flatten[separateDerivativesInCalculation/@calcs,1]];

(* If the calculation contains a SeparatedDerivatives key, split the
   calculation into three.  The first and second will compute all the
   derivatives matching the SeparatedDerivatives{,2} pattern and store the
   results in grid functions.  The third will then use these grid
   functions instead of computing the derivatives. *)

separateDerivativesInCalculation[calc_] :=
  Module[
    {sepPat  = lookup[calc, SeparatedDerivatives , None],
     sepPat2 = lookup[calc, SeparatedDerivatives2, None]},
    If[sepPat === None, {calc},
       If[GetSchedule[calc] === Automatic,
          ThrowError["Separating derivatives in an automatically scheduled function is not supported"]];

       Module[
         {derivGFName, derivGFName2, derivs, sepDerivs, sepDerivs2, calc2,
          replaceSymmetric, replaceMixed, derivCalcs, derivCalcs2, addAfter,
          compCalcName},

         (* Removing duplicate "DPDstandardNth" in derivative variable
            names *)
         derivGFName[pd_[var_,inds___]] :=
         Symbol[StringReplace["Global`D"<>ToString[pd]<>ToString[var]<>Apply[StringJoin,Map[ToString,{inds}]], "Global`D"<>ToString[pd]<>"D"<>ToString[pd] -> "Global`D"<>ToString[pd]]];

         derivGFName2[pd_[var_,inds___]] :=
         StringReplace["D"<>ToString[pd]<>ToString[var]<>"_"<>Apply[StringJoin,Map[ToString,{inds}]],
                       "D"<>ToString[pd]<>"D"<>ToString[pd] -> "D"<>ToString[pd]];

         compCalcName = lookup[calc,Name]<>"_NonDerivatives";

         replaceSymmetric = pd_[var_,i_,j_] /; i > j :> pd[var,j,i];
         (* Replace mixed derivatives with first derivatives of
            derivatives we already take. Ensure that we prefer to take
            x derivatives instead of z derivatives, since these are
            likely to be faster. This works because derivatives are
            currently calculated where possible, including on ghost
            zones. *)
         replaceMixed =
         If[sepPat2=!=None && lookupDefault[calc, UseCaKernel, False],
            pd_[var_,i_,j_] /; i < j :> pd[derivGFName[pd[var,j]],i],
            {}];
         derivs = DeleteDuplicates[GetDerivatives[calc] /. replaceSymmetric];

         sepDerivs  = Flatten[Map[Cases[derivs, #] &, sepPat],1];
         sepDerivs2 = If[sepPat2===None, {},
                         Flatten[Map[Cases[derivs, #] &, sepPat2],1]];
         sepDerivs2 = sepDerivs2 /. replaceMixed;

         (* Group _i and _ii derivatives together in the same calculation *)
         (* NOTE: This should really be "close together if they are in
            the same calculation"? *)
         sepDerivs  = GatherBy[sepDerivs , Function[d, d /. {pd_[var_, i_] -> pd[i], pd_[var_, i_, i_] -> pd[i]}]];
         sepDerivs2 = GatherBy[sepDerivs2, Function[d, d /. {pd_[var_, i_] -> pd[i], pd_[var_, i_, i_] -> pd[i]}]];

         derivCalc[derivs_List] :=
         Module[
           {calc1, currentGroups, localGroups, derivNames = Map[derivGFName,derivs]},
           calc1 = mapReplace[calc, 
                              Equations, 
                              (* Sort by the differentiated variable to localise accesses to its data *)
                              Sort[Thread[derivNames -> derivs],
                                   OrderedQ[{ToString[#1[[2,1]]]<>ToString[#1[[2,2]]],
                                             ToString[#2[[2,1]]]<>ToString[#2[[2,2]]]}] &]];

           calc1 = mapReplace[calc1, Name,
                              StringReplace[lookup[calc,Name]<>"_"<>derivGFName2[derivs[[1]]]<>
                                            If[Length[derivs]>1,"_"<>"etc",""],"PDstandardNth"->""]];
           If[Length[derivs] === 1,
              calc1 = Append[calc1, CachedVariables -> (First/@derivs)]];
           currentGroups = lookup[calc, LocalGroups, {}];
           localGroups = Join[currentGroups, Map[{ToString@#<>"_group", {#}} &, derivNames]];
           calc1 = mapReplaceAdd[calc1, LocalGroups, localGroups];
           calc1 = Append[calc1, SimpleCode -> True];
           calc1];

         derivCalcs  = Map[derivCalc, sepDerivs ];
         derivCalcs2 = Map[derivCalc, sepDerivs2];

         derivCalcs  = Map[InNewScheduleGroup[lookup[calc,Name], #] &, derivCalcs ];
         derivCalcs2 = Map[InNewScheduleGroup[lookup[calc,Name], #] &, derivCalcs2];

         addAfter[theCalc_, otherCalcs_] := Module[
           {otherNames, afterNames, thisSchedule, newSchedule},
           otherNames = Map[lookup[#, Name]&, otherCalcs];
           (* TODO: "after" modifiers currently don't work with
              CaKernel *)
           afterNames = StringJoin[Map[" after " <> # &, otherNames]];
           thisSchedule = GetSchedule[theCalc];
           newSchedule = Map[# <> afterNames &, thisSchedule];
           mapReplace[theCalc, Schedule, newSchedule]];
         (* TODO: could instead enforce order only between those
            derivative calculations that require it *)
         derivCalcs2 = Map[addAfter[#, derivCalcs]&, derivCalcs2];

         calc2 = mapReplace[mapReplace[calc, Name, compCalcName],
                            Equations,
                            (GetEquations[calc]/.replaceSymmetric/.replaceMixed) /. 
                            Map[# -> derivGFName[#] &, Flatten[Join[sepDerivs,sepDerivs2],1]]];

         derivCalcs  = Map[mapReplace[#, Schedule, Map[#<>" before "<>GetCalculationName[calc2] &, GetSchedule[#]]] &, derivCalcs ];
         derivCalcs2 = Map[mapReplace[#, Schedule, Map[#<>" before "<>GetCalculationName[calc2] &, GetSchedule[#]]] &, derivCalcs2];

         calc2 = InNewScheduleGroup[lookup[calc,Name], calc2];

         Join[derivCalcs, derivCalcs2, {calc2}]]]];

DefFn[
  AddCondition[calc_List, condition_] :=
  mapReplaceAdd[calc, Conditional, lookup[calc,Conditional, True] && condition]];

DefFn[
  AddConditionSuffix[calc_List, condition_] :=
  mapReplaceAdd[calc, Schedule, Map[#<>" IF "<>condition &, GetSchedule[calc]]]];

InNewScheduleGroup[groupName_String, calc_List] :=
  Module[
    {newGroup},
    newGroup = {Name          -> groupName,
                Language      -> "None", (* groups do not have a language *)
                SchedulePoint -> GetSchedule[calc],
                Comment       -> ""};
    mapReplaceAdd[
      mapReplaceAdd[
        calc,
        Schedule, {"in "<>groupName}],
      ScheduleGroups, Append[lookup[calc, ScheduleGroups, {}],newGroup]]];

DefFn[
  GetSchedule[calc_List] :=
  Module[
    {s = lookup[calc,Schedule,Automatic]},
    If[s =!= Automatic && !ListQ[s],
       ThrowError["Calculation "<>lookup[calc,Name]<>" has an invalid Schedule entry: ",
                  s]];
    s]];

calculationSymbolsRHS[calc_] :=
  Module[{allAtoms},
    allAtoms = Union[Map[Last, Flatten@{lookup[calc, Equations],
      lookup[calc,PartialDerivatives]} ]];
    allAtoms = Union[Level[allAtoms, {-1}]];
    Cases[allAtoms, x_Symbol]];

calculationSymbolsLHS[calc_] :=
  Module[{allAtoms},
    allAtoms = Union[Map[First, Flatten@lookup[calc, Equations] ]];
    Cases[allAtoms, x_Symbol]];

(* Return the names of any shorthands used in the RHSs of calculation *)
calculationRHSUsedShorthands[calc_] :=
  Module[{calcSymbols, allShorthands},
    calcSymbols = calculationSymbolsRHS[calc];
    allShorthands = lookupDefault[calc, Shorthands, {}];
    Intersection[calcSymbols, allShorthands]];

(* Return the names of any shorthands used in the LHSs of calculation *)
calculationLHSUsedShorthands[calc_] :=
  Module[{calcSymbols, allShorthands},
    calcSymbols = calculationSymbolsLHS[calc];
    allShorthands = lookupDefault[calc, Shorthands, {}];
    Intersection[calcSymbols, allShorthands]];

CalculationExpressionSymbols[expr_] :=
  Module[{allAtoms},
    allAtoms = Union[Level[expr, {-1}]];
    Cases[allAtoms, x_Symbol]];

CalculationSymbols[calc_] :=
  Module[{allAtoms},
    allAtoms = Union[Level[lookup[calc, Equations], {-1}]];
    Cases[allAtoms, x_Symbol]];

(* Return all the functions used in a calculation *)
(* Not currently used *)
FunctionsInCalculation[calc_] :=
  Module[{eqs, x, y},
    eqs = lookup[calc, Equations];
    x = Cases[eqs, f_[x___] -> f, Infinity];
    y = Union[Select[x, Context[#] === "Global`" &]];
    y];

VerifyCalculation[calc_] :=
  Module[{calcName},
    calcName = lookupDefault[calc, Name, "<unknown>"];
    If[Head[calc] != List,
      ThrowError["Invalid Calculation structure: " <> ToString[calc]]];
    VerifyListContent[calc, Rule,
      " while checking the calculation with name " <> ToString[calcName]];
    If[mapContains[calc, Shorthands],
      VerifyListContent[lookup[calc, Shorthands], Symbol,
        " while checking the Shorthands member of the calculation called " <> ToString[calcName]]];
    If[mapContains[calc, Equations],
      VerifyListContent[lookup[calc, Equations], Rule,
        " while checking the equation" <> ToString[calcName]],
      ThrowError["Invalid Calculation structure. Must contain Equations element: ",
        ToString[calc], " while checking the calculation called ", ToString[calcName]]];

    allowedKeys = {BodyFunction, CallerFunction, ExecuteOn,
         GFAccessFunction, Groups, Implementation, InitFDVariables,
         InheritedVariables, InheritedParameters,
         LoopFunction, MacroPointer, Name, ODEGroups, Parameters, IntParameters,
         PartialDerivatives, PreDefinitions, Schedule,Equations,
         PreDefinitionsExpr,
         Shorthands, ConditionalOnKeyword, Before, After,
         ConditionalOnTextuals, Where, ConditionalOnKeywords,
         CollectList, AllowedSymbols, ApplyBCs, Conditional, CachedVariables, SplitBy,
         SeparatedDerivatives, SeparatedDerivatives2,
         LocalGroups, NoSimplify, UseDGFE, SimpleCode, UseCaKernel,
         UseJacobian,
         ScheduleGroups, TriggerGroups, ThornName, Tile, UseLoopControl,
         ChemoraContents };

    usedKeys = Map[First, calc];
    unknownKeys = Complement[usedKeys, allowedKeys];
    If[unknownKeys =!= {},
      ThrowError["Unrecognised key(s) in calculation: ", unknownKeys]]];

(* Remove equations in the calculation which assign to shorthands
   which are never used. Do not modify the Shorthands entry. An unused
   shorthand might be missed if the order of assignments is
   pathalogical enough (e.g. {s1 -> s2, s2 -> s1} would not be
   removed). *)
RemoveUnusedShorthands[calc_] :=
  Module[{rhsShorthands, lhsShorthands, unusedButAssignedShorthands, removeShorts,
    eqs, neweqs, newCalc},

    removeShorts[eqlist_] :=
      Select[eqlist, (!MemberQ[unusedButAssignedShorthands, First[#]]) &];

    rhsShorthands = calculationRHSUsedShorthands[calc];
    lhsShorthands = calculationLHSUsedShorthands[calc];
    unusedButAssignedShorthands = Complement[lhsShorthands, rhsShorthands];
    InfoMessage[InfoFull, "Removing definitions for shorthands: "<>ToString[unusedButAssignedShorthands,InputForm]];
    eqs = lookup[calc, Equations];
    neweqs = removeShorts[eqs];
    newCalc = mapReplace[calc, Equations, neweqs];
    If[!(eqs === neweqs),
      RemoveUnusedShorthands[newCalc],
      newCalc]];

(* Return all the groups that are used in a given calculation *)
GroupsInCalculation[calc_, imp_] :=
  Module[{groups,gfs,eqs,gfsUsed, groupNames},
    groups = lookup[calc, Groups];
    gfs = allGroupVariables[groups];
    eqs = lookup[calc, Equations];
    gfsUsed = Union[Cases[eqs, _ ? (MemberQ[gfs,#] &), Infinity]];
    groupNames = containingGroups[gfsUsed, groups];
    Map[qualifyGroupName[#, imp] &, groupNames]];

VerifyNewCalculation[calc_] :=
  Module[{calcName},
    If[Head[calc] != List,
      ThrowError["Invalid Calculation structure: " <> ToString[calc]]];

    calcName = lookupDefault[calc, Name, "<unknown>"];

    VerifyListContent[calc, Rule, " while checking the calculation called " <> ToString[calcName]];

    If[mapContains[calc, Shorthands],
      VerifyListContent[lookup[calc, Shorthands], Symbol," while checking the calculation called " <> ToString[calcName] ]];

    If[mapContains[calc, Equations],
      VerifyListContent[lookup[calc, Equations], Rule," while checking the calculation called " <> ToString[calcName]],
      ThrowError["Invalid Calculation structure. Must contain Equations element: " <> ToString[calc]]]];

DefFn[TileCalculationQ[calc_] :=
  lookup[calc,Tile]];

DefFn[CalculationLoopControlQ[calc_] :=
  lookup[calc,UseLoopControl]];

Options[SetCalculationLoopControl] = ThornOptions;
DefFn[SetCalculationLoopControl[calc_, opts:OptionsPattern[]] :=
  mapReplaceAdd[calc, UseLoopControl,
    OptionValue[UseLoopControl] &&
    lookupDefault[calc, UseLoopControl, True] =!= False]];

End[];

EndPackage[];
