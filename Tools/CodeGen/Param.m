
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

BeginPackage["Param`", {"Thorn`", "Errors`", "Helpers`", "MapLookup`", "KrancGroups`",
                        "Kranc`", "Jacobian`", "CodeGenParam`",
                        "ConservationCalculation`", "MoL`"}];

CreateKrancParam;
MakeFullParamDefs;
ParamName;
AllNumericParameters;
ParameterDatabase;

Begin["`Private`"];

(* ------------------------------------------------------------------------ 
   Parameter Database
   ------------------------------------------------------------------------ *)

(* We want to collect all parameter definitions together into a single
object.  This forms a "parameter database".  

params = {
  Declared -> {...},
  Inherited -> {...},
  Extended -> {...}
};

How similar is this to what needs to be passed to the param.ccl
functions?  That requires a low-level representation of the parameter
file from a Cactus point of view.  We need a representation from the
Kranc point of view, which is similar to that of the user.
Eventually, we might decide to merge these representations, but for
the moment, let's keep things high-level.

*)

ParameterType = "Real" | "Integer" | "Keyword";

Options[ParameterDatabase] = ThornOptions;

(* ParameterDatabase[implementation_String, OptionsPattern[]] := *)
(*   Module[ *)
(*     {addType, *)
(*      realParams, intParams, keywordParams, *)
(*      inheritedRealParams, inheritedIntParams, inheritedKeywordParams, *)
(*      extendedRealParams, extendedIntParams, extendedKeywordParams, *)
(*      realParamDefs, intParamDefs}, *)

(*     addType[def_List, type:ParameterType] :=  *)
(*     Append[def, "Type" -> type]; *)

(*     realParams = OptionValue[RealParameters]; *)
(*     If[OptionValue[ConservationCalculations] =!= {}, *)
(*        realParams = Join[realParams,ConservationDifferencingRealParameters[]]]; *)
(*     intParams = OptionValue[IntParameters]; *)

(*     realParamDefs = addType[#,"Real"] & /@ MakeFullParamDefs[realParams]; *)
(*     intParamDefs = addType[#,"Integer"] & /@ MakeFullParamDefs[intParams]; *)

(*     keywordParams = addType[#,"Keyword"] & /@ OptionValue[KeywordParameters]; *)

(*     inheritedRealParams = addType[#,"Real"] & /@ OptionValue[InheritedRealParameters]; *)
(*     inheritedIntParams = addType[#,"Integer"] & /@ OptionValue[InheritedIntParameters]; *)
(*     inheritedKeywordParams = addType[#,"Keyword"] & /@  *)
(*     OptionValue[InheritedKeywordParameters]; *)

(*     extendedRealParams = addType[#,"Real"] & /@ OptionValue[ExtendedRealParameters]; *)
(*     extendedIntParams = addType[#,"Integer"] & /@ OptionValue[ExtendedIntParameters]; *)
(*     extendedKeywordParams = addType[#,"Keyword"] & /@ OptionValue[ExtendedKeywordParameters]; *)

(*     declared = Join[intParamDefs, realParamDefs, keywordParams]; *)
(*     inherited = Join[inheritedRealParams, inheritedIntParams, inheritedKeywordParams]; *)
(*     extended = Join[extendedRealParams, extendedIntParams, extendedKeywordParams]; *)

(*     {"Declared" -> declared, *)
(*      "Inherited" -> inherited, *)
(*      "Extended" -> extended}]; *)

(* allParameters[pdb_List] := *)
(*   Flatten[pdb, 1]; *)

(* parameterType[p_List] := *)
(*   lookup[p, "Type"]; *)

(* numericParameterNames[pdb_List] := *)
(*   unqualifiedName /@ ParamName /@  *)
(*   Select[allParameters, MemberQ[{"Real", "Integer"}, parameterType[#]] &]; *)

DefFn[ParameterDatabase[OptionsPattern[]] :=
  Module[
    {realParams, intParams, keywordParams,
     inheritedRealParams, inheritedIntParams, inheritedKeywordParams,
     extendedRealParams, extendedIntParams, extendedKeywordParams,
     realParamDefs, intParamDefs, allParams},

    realParams = OptionValue[RealParameters];
    If[OptionValue[ConservationCalculations] =!= {},
       realParams = Join[realParams,ConservationDifferencingRealParameters[]]];
    intParams = OptionValue[IntParameters];
    realParamDefs = MakeFullParamDefs[realParams];
    intParamDefs = MakeFullParamDefs[intParams];
    keywordParams = OptionValue[KeywordParameters];
    inheritedRealParams = OptionValue[InheritedRealParameters];
    inheritedIntParams = OptionValue[InheritedIntParameters];
    inheritedKeywordParams = OptionValue[InheritedKeywordParameters];
    extendedRealParams = OptionValue[ExtendedRealParameters];
    extendedIntParams = OptionValue[ExtendedIntParameters];
    extendedKeywordParams = OptionValue[ExtendedKeywordParameters];

    allParams = Join[Map[ParamName, realParamDefs],
                     Map[ParamName, intParamDefs],
                     Map[unqualifiedName, inheritedRealParams], 
                     Map[unqualifiedName, inheritedIntParams], 
                     Map[unqualifiedName, inheritedKeywordParams]];

    {"Reals" -> realParamDefs,
     "Integers" -> intParamDefs,
     "Keywords" -> keywordParams,

     "InheritedReals" -> inheritedRealParams,
     "InheritedIntegers" -> inheritedIntParams,
     "InheritedKeywords" -> inheritedKeywordParams,
     
     "ExtendedReals" -> extendedRealParams,
     "ExtendedIntegers" -> extendedIntParams,
     "ExtendedKeywords" -> extendedKeywordParams,

     "AllNumeric" -> allParams}]];

DefFn[AllNumericParameters[pdb_List] :=
  lookup[pdb, "AllNumeric"]];

DefFn[realParameterDefinitions[pdb_List] :=
  lookup[pdb, "Reals"]];

DefFn[integerParameterDefinitions[pdb_List] :=
  lookup[pdb, "Integers"]];

DefFn[keywordParameterDefinitions[pdb_List] :=
  lookup[pdb, "Keywords"]];

DefFn[inheritedRealParameterNames[pdb_List] :=
  lookup[pdb, "InheritedReals"]];

DefFn[inheritedIntegerParameterNames[pdb_List] :=
  lookup[pdb, "InheritedIntegers"]];

DefFn[inheritedKeywordParameterNames[pdb_List] :=
  lookup[pdb, "InheritedKeywords"]];

DefFn[extendedRealParameterDefinitions[pdb_List] :=
  lookup[pdb, "ExtendedReals"]];

DefFn[extendedIntegerParameterDefinitions[pdb_List] :=
  lookup[pdb, "ExtendedIntegers"]];

DefFn[extendedKeywordParameterDefinitions[pdb_List] :=
  lookup[pdb, "ExtendedKeywords"]];


(* ------------------------------------------------------------------------ 
   Parameter utility functions
   ------------------------------------------------------------------------ *)

VerifyQualifiedName[name_] :=
  If[! StringQ[name] || ! StringMatchQ[name, "*::*"],
    ThrowError["Not a name with an implementation:", name]];

implementationFromQualifiedName[name_] :=
  Module[{colon},
    VerifyQualifiedName[name];
    colon = First[First[StringPosition[name, ":", 1]]];
    StringDrop[name, colon - 1 - StringLength[name]]];

unqualifiedName[name_] :=
  Module[{colon},
    VerifyQualifiedName[name];
    colon = First[First[StringPosition[name, ":", 1]]];
    Return[StringDrop[name, colon + 1]]];

(* ------------------------------------------------------------------------ 
   Code generation functions
   ------------------------------------------------------------------------ *)

krancParamStruct[definition_, type_, inherited_] :=
  Module[{description, name},
    name = lookup[definition, Name];
    description = lookupDefault[definition, Description, name];
    Join[
    {Name        -> name,
     Type        -> type, 
     Description -> description,
     Default     -> lookup[definition, Default],
     Visibility  -> "restricted"},
    If[mapContains[definition, Steerable],
      {Steerable -> lookup[definition, Steerable]},
      {}],
    If[inherited,
      {},
      {AllowedValues -> {{Value -> "*:*", Description -> ""}}}]]];

krancParamStructExtended[definition_, type_] :=
  Module[{allowedValues, description, name},
    name = unqualifiedName[lookup[definition, Name]];
    description = lookupDefault[definition, Description, name];
    allowedValues = lookup[definition, AllowedValues];
    {Name        -> name,
     Type        -> type, 
     Description -> description,
     Default     -> "",
     Visibility  -> "restricted",
     AllowedValues -> Map[{Value -> #, Description -> ""} &, allowedValues]}];

krancKeywordParamStruct[struct_] :=
  Join[
  {Name -> lookup[struct, Name],
   Type -> "KEYWORD",
   Default -> lookup[struct, Default],
   Description -> lookupDefault[struct, Description, lookup[struct, Name]],
   Visibility -> lookupDefault[struct, Visibility, "private"]},
  If[mapContains[struct, Steerable],
    {Steerable -> lookup[struct, Steerable]},
    {}],
  {AllowedValues -> Map[{Value -> #, Description -> #} &, lookup[struct, AllowedValues]]}];

MakeFullParamDefs[params_] :=
  Module[{p},
    p = Map[If[!ListQ[#], {Name -> #, Default -> 0}, #] &, params];
    p];

ParamName[paramDef_] :=
  lookup[paramDef, Name];

inheritParameters[imp_, reals_, ints_, keywords_] :=
  Module[{theseReals, theseInts, theseKeywords, theseRealsNoImp, theseIntsNoImp, theseKeywordsNoImp, realStructs, intStructs, keywordStructs},
    theseReals = Select[reals, implementationFromQualifiedName[#] == imp &];
    theseInts = Select[ints, implementationFromQualifiedName[#] == imp &];
    theseKeywords = Select[keywords, implementationFromQualifiedName[#] == imp &];
    theseRealsNoImp = MakeFullParamDefs[Map[unqualifiedName, theseReals]];
    theseIntsNoImp = MakeFullParamDefs[Map[unqualifiedName, theseInts]];
    theseKeywordsNoImp = MakeFullParamDefs[Map[unqualifiedName, theseKeywords]];
    realStructs = Map[krancParamStruct[#, "CCTK_REAL", True] &, theseRealsNoImp];
    intStructs = Map[krancParamStruct[#, "CCTK_INT", True] &, theseIntsNoImp];
    keywordStructs = Map[krancParamStruct[#, "CCTK_KEYWORD", True] &, theseKeywordsNoImp];
    If[(Length[theseReals] + Length[theseInts] + Length[theseKeywords]) > 0,
      Return[{Name -> imp, UsedParameters -> Join[realStructs, intStructs, keywordStructs]}], 
      Return[{}]]];

extendParameters[imp_, reals_, ints_, keywords_] :=
  Module[{theseReals, theseInts, theseKeywords, realStructs, intStructs, keywordStructs},
    theseReals = Select[reals, implementationFromQualifiedName[lookup[#, Name]] == imp &];
    theseInts = Select[ints, implementationFromQualifiedName[lookup[#, Name]] == imp &];
    theseKeywords = Select[keywords, implementationFromQualifiedName[lookup[#, Name]] == imp &];
    realStructs = Map[krancParamStructExtended[#, "CCTK_REAL"] &, theseReals];
    intStructs = Map[krancParamStructExtended[#, "CCTK_INT"] &, theseInts];
    keywordStructs = Map[krancParamStructExtended[#, "CCTK_KEYWORD"] &, theseKeywords];
    If[(Length[theseReals] + Length[theseInts] + Length[theseKeywords]) > 0,
      Return[{Name -> imp, ExtendedParameters -> Join[realStructs, intStructs, keywordStructs]}],
      Return[{}]]];

Options[usedParameters] = ThornOptions;

DefFn[
  usedParameters[parameters_List, OptionsPattern[]] :=
  Module[
    {genericfdStruct, allInherited, allExtended, implementationNames,
     userImplementations, userImplementations2},

    genericfdStruct =
    {
      Name -> "GenericFD",
      UsedParameters -> 
        Join[{{Name -> "assume_stress_energy_state", Type -> "CCTK_INT"}},
             If[OptionValue[UseJacobian], JacobianGenericFDParameters[], {}]]
    };

    allInherited = Join[inheritedRealParameterNames[parameters],
                        inheritedIntegerParameterNames[parameters],
                        inheritedKeywordParameterNames[parameters]];

    allExtended = Join[extendedRealParameterDefinitions[parameters],
                       extendedIntegerParameterDefinitions[parameters],
                       extendedKeywordParameterDefinitions[parameters]];
         
    implementationNames = Union[Map[implementationFromQualifiedName, allInherited],
                                Map[implementationFromQualifiedName[lookup[#, Name]] &, allExtended]];

    userImplementations = Map[
      inheritParameters[
        #,
        inheritedRealParameterNames[parameters],
        inheritedIntegerParameterNames[parameters],
        inheritedKeywordParameterNames[parameters]] &, 
      implementationNames];

    userImplementations2 =
         Map[extendParameters[#, extendedRealParameterDefinitions[parameters],
                              extendedIntegerParameterDefinitions[parameters],
                              extendedKeywordParameterDefinitions[parameters]] &, 
             implementationNames];

    userImplementations = If[userImplementations=={{}},{},userImplementations];
    userImplementations2 = If[userImplementations2=={{}},{},userImplementations2];

    Join[userImplementations, userImplementations2, {genericfdStruct, MoLUsedParameters[]}]]];

Options[CreateKrancParam] = ThornOptions;
CreateKrancParam[evolvedGroups_, nonevolvedGroups_,
  evolvedODEGroups_, nonevolvedODEGroups_, groups_, thornName_, 
  parameters_,
  evolutionTimelevels_, defaultEvolutionTimelevels_,
  calcs_, opts:OptionsPattern[]] :=
  Module[
    {evolvedGFs, otherTimelevelsParam, genericfdStruct, realStructs,
     intStructs, keywordStructs, verboseStruct, calcEveryStructs, calcOffsetStructs,
     allInherited, allExtended, implementationNames,
     userImplementations, userImplementations2, implementations,
     params, paramspec, param},

    (* reals and ints are symbols containing parameter names.  The
       inherited ones have implementation names as well *)

    evolvedGFs = variablesFromGroups[evolvedGroups, groups];

    otherTimelevelsParam =
    {
      Name -> "other_timelevels",
      Type -> "CCTK_INT",
      Default -> 1,
      Description -> "Number of active timelevels for non-evolved grid functions",
      Visibility -> "restricted",
      AllowedValues -> {{Value -> ToString[0] <> ":" <> ToString[evolutionTimelevels],
                         Description -> ""}},
      Steerable -> Recover
    };

    realStructs = Map[krancParamStruct[#, "CCTK_REAL", False] &, 
                      realParameterDefinitions[parameters]];
    intStructs = Map[krancParamStruct[#, "CCTK_INT", False] &, 
                     integerParameterDefinitions[parameters]];
    keywordStructs = Map[krancKeywordParamStruct, keywordParameterDefinitions[parameters]];

    userStructs = Join[realStructs, intStructs, keywordStructs];

    verboseStruct = krancParamStruct[{Name -> "verbose", Default -> 0, Steerable -> Always}, "CCTK_INT", False];
    calcEveryStructs = Map[krancParamStruct[{Name -> lookup[#, Name] <> "_calc_every", Default -> 1, Steerable -> Always}, "CCTK_INT", False] &, calcs];
    calcOffsetStructs = Map[krancParamStruct[{Name -> lookup[#, Name] <> "_calc_offset", Default -> 0, Steerable -> Always}, "CCTK_INT", False] &, calcs];

    params = Join[{verboseStruct},
                  userStructs,
                  MoLParameterStructures[
                    thornName, evolvedGroups, evolvedODEGroups, groups,
                    evolutionTimelevels, defaultEvolutionTimelevels],
                  {otherTimelevelsParam},
                  calcEveryStructs,
                  calcOffsetStructs,
                  CactusBoundary`GetParameters[evolvedGFs, evolvedGroups]];

    paramspec = {Implementations -> usedParameters[parameters, opts],
                 NewParameters   -> params};

(*    Print["paramspec = ", paramspec];*)

    param = CreateParam[paramspec];
    Return[param]
  ];



End[];

EndPackage[];
