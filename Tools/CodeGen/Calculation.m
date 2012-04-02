
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
GetEquations;
GetCalculationParameters;
CalculationStencilSize;
CalculationOnDevice;
GetCalculationWhere;
SplitCalculations;

Begin["`Private`"];

DefFn[
  GetGridFunctions[calc_List] :=
  Module[
    {eqs,rhss,lhss,gfs,gfsInRHS,gfsInLHS},
    eqs = GetEquations[calc];

    rhss = Map[#[[2]] &, eqs];
    lhss = Map[#[[1]] &, eqs];

    gfs = allGroupVariables[lookup[calc,Groups]];

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
  GetCalculationParameters[calc_List] :=
  Module[
    {syms,pdSyms,params},
    syms = Cases[GetEquations[calc], _?AtomQ|_String, {-1}];
    pdSyms = Cases[GetPartialDerivatives[calc], _?AtomQ|_String, {-1}];
    params = lookup[calc,Parameters];
    Intersection[params,Join[syms,pdSyms]]]];

DefFn[
  GetCalculationName[calc_List] :=
  lookup[calc,Name]];

DefFn[
  GetCalculationWhere[calc_List] :=
  lookup[calc,Where, Everywhere]];

DefFn[
  CalculationStencilSize[calc_List] :=
  Module[
    {pddefs,eqs},

    pddefs = lookup[calc, PartialDerivatives, {}];
    eqs    = lookup[calc, Equations];

    StencilSize[pddefs, eqs, "not needed", {} (*ZeroDimensions*)]]];

DefFn[
  CalculationOnDevice[calc_List] :=
  lookupDefault[calc, ExecuteOn, Automatic] === Device];

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
  calc3    = mapReplace[calc2, Equations, eqs];
  calc3
];

DefFn[
  SplitCalculations[calcs_List] :=
  Flatten[SplitCalculation/@calcs,1]];

DefFn[
  SplitCalculation[calc_] :=
  Module[
    {splitBy = lookup[calc,SplitBy, {}]},
    If[splitBy === {},
       {calc},
       Table[partialCalculation[calc, 
                                "_"<>StringReplace[ToString[var],{"["->"","]"->"",","->""}],
                                {},
                                {var}]~Join~{CachedVariables -> {(* var[[1]] *)}}, (* This is not general *)
             {var, splitBy}]]]];

End[];

EndPackage[];
