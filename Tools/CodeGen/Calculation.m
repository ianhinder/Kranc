
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
SeparateDerivatives;
AddCondition;
AddConditionSuffix;

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
  GetDerivatives[calc_] :=
  GridFunctionDerivativesInExpression[
    GetPartialDerivatives[calc], GetEquations[calc],{(* TODO: implement ZeroDimensions here *)}]];

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
     oldName = lookup[calc,Name]},

    If[Intersection[Flatten[splitBy,1],OutputGridFunctions[calc]] === {},
       {calc},
       MapIndexed[
         Function[
           {var,i},
           Module[
             {nameSuffix, splitVars},
             nameSuffix = If[ListQ[var],
                             ToString[i[[1]]],
                             "_"<>StringReplace[ToString[var],{"["->"","]"->"",","->""}]];
             splitVars = If[ListQ[var], var, {var}];
             partialCalculation[calc, nameSuffix, {}, splitVars]]],
         splitBy]]]];

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
       If[lookupDefault[calc, Schedule, Automatic] === Automatic,
          ThrowError["Separating derivatives in an automatically scheduled function is not supported"]];

       Module[
         {derivGFName, derivGFName2, derivs, sepDerivs, sepDerivs2,
          sepDerivsx, sepDerivsy, sepDerivsz,
          sepDerivs2xy,sepDerivs2xz, sepDerivs2yz,
          calc2,
          replaceSymmetric, replaceMixed, derivCalcs, derivCalcs2,
          derivCalcsx, derivCalcsy, derivCalcsz,
          derivCalcs2xy, derivCalcs2xz, derivCalcs2yz,
          addAfter},

         (* Removing duplicate "DPDstandardNth" in derivative variable
            names *)
         derivGFName[pd_[var_,inds___]] :=
         Symbol[StringReplace["Global`D"<>ToString[pd]<>ToString[var]<>Apply[StringJoin,Map[ToString,{inds}]], "Global`D"<>ToString[pd]<>"D"<>ToString[pd] -> "Global`D"<>ToString[pd]]];

         derivGFName2[pd_[var_,inds___]] :=
         StringReplace["D"<>ToString[pd]<>ToString[var]<>"_"<>Apply[StringJoin,Map[ToString,{inds}]], "D"<>ToString[pd]<>"D"<>ToString[pd] -> "D"<>ToString[pd]];


         replaceSymmetric = pd_[var_,i_,j_] /; i > j :> pd[var,j,i];
         (* Replace mixed derivatives with first derivatives of
            derivatives we already take. Ensure that we prefer to take
            x derivatives insted of z derivatives, since these are
            likely to be faster. This works because derivatives are
            currently calculated where possible, including on ghost
            zones. *)
         replaceMixed =
         If[sepPat2===None, {},
            pd_[var_,i_,j_] /; i < j :> pd[derivGFName[pd[var,j]],i]];
         derivs = DeleteDuplicates[GetDerivatives[calc] /. replaceSymmetric];

         (* TODO: This code is evil: (1) it treats 3 dimensions
            explicitly instead of in looops, (2) it assumes that
            sepPat has been written in terms of global symbols i and
            j, (3) it performs pattern replacement on patterns, which
            is difficult to understand *)
         (* TODO: Ian suggests to use GatherBy for this splitting *)
         (*
         sepDerivs  = Flatten[Map[Cases[derivs, #] &, sepPat],1];
         *)
         sepDerivsx = Flatten[Map[Cases[derivs, #] &, sepPat/.{Verbatim[Global`i_]->1}],1];
         sepDerivsy = Flatten[Map[Cases[derivs, #] &, sepPat/.{Verbatim[Global`i_]->2}],1];
         sepDerivsz = Flatten[Map[Cases[derivs, #] &, sepPat/.{Verbatim[Global`i_]->3}],1];
         (*
         sepDerivs2 = If[sepPat2===None, {},
                         Flatten[Map[Cases[derivs, #] &, sepPat2],1]];
         sepDerivs2 = sepDerivs2 /. replaceMixed;
          *)
         Print["sepPat2",sepPat2];
         sepDerivs2xy = If[sepPat2===None, {},
                           Flatten[Map[Cases[derivs, #] &,
                                       sepPat2/.{Verbatim[Global`i_]->1,Verbatim[Global`j_]->2,Global`i->1,Global`j->2}],1]];
         sepDerivs2xz = If[sepPat2===None, {},
                           Flatten[Map[Cases[derivs, #] &,
                                       sepPat2/.{Verbatim[Global`i_]->1,Verbatim[Global`j_]->3,Global`i->1,Global`j->3}],1]];
         sepDerivs2yz = If[sepPat2===None, {},
                           Flatten[Map[Cases[derivs, #] &,
                                       sepPat2/.{Verbatim[Global`i_]->2,Verbatim[Global`j_]->3,Global`i->2,Global`j->3}],1]];
         sepDerivs2xy = sepDerivs2xy /. replaceMixed;
         sepDerivs2xz = sepDerivs2xz /. replaceMixed;
         sepDerivs2yz = sepDerivs2yz /. replaceMixed;

         (* Group _i and _ii derivatives together in the same calculation *)
         (* NOTE: This should really be "close together if they are in
            the same calculation"? *)
         (* TODO: Use pd[i] instead of pd1 when splitting directions
            via GatherBy *)
         sepDerivsx  = GatherBy[sepDerivsx , Function[d, d /. {pd_[var_, i_] -> pd1, pd_[var_, i_, i_] -> pd1}]];
         sepDerivsy  = GatherBy[sepDerivsy , Function[d, d /. {pd_[var_, i_] -> pd1, pd_[var_, i_, i_] -> pd1}]];
         sepDerivsz  = GatherBy[sepDerivsz , Function[d, d /. {pd_[var_, i_] -> pd1, pd_[var_, i_, i_] -> pd1}]];
         sepDerivs2xy = GatherBy[sepDerivs2xy, Function[d, d /. {pd_[var_, i_] -> pd1, pd_[var_, i_, i_] -> pd1}]];
         sepDerivs2xz = GatherBy[sepDerivs2xz, Function[d, d /. {pd_[var_, i_] -> pd1, pd_[var_, i_, i_] -> pd1}]];
         sepDerivs2yz = GatherBy[sepDerivs2yz, Function[d, d /. {pd_[var_, i_] -> pd1, pd_[var_, i_, i_] -> pd1}]];
         Print["sepDerivsx=",sepDerivsx];
         Print["sepDerivsy=",sepDerivsy];
         Print["sepDerivsz=",sepDerivsz];
         Print["sepDerivs2xy=",sepDerivs2xy];
         Print["sepDerivs2xz=",sepDerivs2xz];
         Print["sepDerivs2yz=",sepDerivs2yz];

         derivCalc[derivs_List] :=
         Module[
           {calc1, currentGroups, localGroups, derivNames = Map[derivGFName,derivs]},
           calc1 = mapReplace[calc, 
                              Equations, 
                              (* Sort by the differentiated variable to localise accesses to its data *)
                              Sort[Thread[derivNames -> derivs],
                                   OrderedQ[{ToString[#1[[2,1]]]<>ToString[#1[[2,2]]],
                                             ToString[#2[[2,1]]]<>ToString[#2[[2,2]]]}] &]];

           calc1 = mapReplace[calc1, Schedule, Map[#<>" before "<>lookup[calc,Name] &, lookup[calc,Schedule]]];
           calc1 = mapReplace[calc1, Name,
                              StringReplace[lookup[calc,Name]<>"_"<>derivGFName2[derivs[[1]]]<>
                                            If[Length[derivs]>1,"_"<>"etc",""],"PDstandardNth"->""]];
           (* TODO: Ian suggests the line below when splitting
              directions via GatherBy:
           calc1 = mapReplace[calc1, Name,
                              StringReplace[lookup[calc,Name]<>"_"<>derivGFName2[derivs[[1]]]<>
                                            If[Length[derivs]>1,"_"<>ToString[derivs[[1,2]]]<>"_etc",""],"PDstandardNth"->""]];
           *)
           If[Length[derivs] === 1,
              calc1 = Append[calc1, CachedVariables -> (First/@derivs)]];
           currentGroups = lookup[calc, LocalGroups, {}];
           localGroups = Join[currentGroups, Map[{ToString@#<>"_group", {#}} &, derivNames]];
           calc1 = mapReplaceAdd[calc1, LocalGroups, localGroups];
           calc1 = Append[calc1, SimpleCode -> True];
           calc1];

         (*
         derivCalcs  = Map[derivCalc, sepDerivs ];
         derivCalcs2 = Map[derivCalc, sepDerivs2];
         *)
         derivCalcsx   = Map[derivCalc, sepDerivsx  ];
         derivCalcsy   = Map[derivCalc, sepDerivsy  ];
         derivCalcsz   = Map[derivCalc, sepDerivsz  ];
         derivCalcs2xy = Map[derivCalc, sepDerivs2xy];
         derivCalcs2xz = Map[derivCalc, sepDerivs2xz];
         derivCalcs2yz = Map[derivCalc, sepDerivs2yz];
         Print["derivCalcsx=",derivCalcsx];
         Print["derivCalcsy=",derivCalcsy];
         Print["derivCalcsz=",derivCalcsz];
         Print["derivCalcs2xy=",derivCalcs2xy];
         Print["derivCalcs2xz=",derivCalcs2xz];
         Print["derivCalcs2yz=",derivCalcs2yz];
         addAfter[theCalc_, otherCalcs_] := Module[
           {otherNames, afterNames, thisSchedule, newSchedule},
           otherNames = Map[lookup[#, Name]&, otherCalcs];
           (* TODO: "after" modifiers currently don't work with
              CaKernel *)
           afterNames = StringJoin[Map[" after " <> # &, otherNames]];
           thisSchedule = lookup[theCalc, Schedule];
           newSchedule = Map[# <> afterNames &, thisSchedule];
           mapReplace[theCalc, Schedule, newSchedule]];
         derivCalcs2xy = Map[addAfter[#, derivCalcsy]&, derivCalcs2xy];
         derivCalcs2xz = Map[addAfter[#, derivCalcsz]&, derivCalcs2xz];
         derivCalcs2yz = Map[addAfter[#, derivCalcsz]&, derivCalcs2yz];

         calc2 = mapReplace[calc,
                            Equations,
                            (GetEquations[calc]/.replaceSymmetric/.replaceMixed) /. Map[# -> derivGFName[#] &, Flatten[Join[sepDerivsx,sepDerivsy,sepDerivsz,
                                                                                                                            sepDerivs2xy,sepDerivs2xz,sepDerivs2yz],1]]];

         Join[derivCalcsx, derivCalcsy, derivCalcsz,
              derivCalcs2xy, derivCalcs2xz, derivCalcs2yz, {calc2}]]]];

DefFn[
  AddCondition[calc_List, condition_] :=
  mapReplaceAdd[calc, Conditional, lookup[calc,Conditional, True] && condition]];

DefFn[
  AddConditionSuffix[calc_List, condition_] :=
  mapReplaceAdd[calc, Schedule, Map[#<>" IF "<>condition &, lookup[calc,Schedule]]]];


End[];

EndPackage[];
