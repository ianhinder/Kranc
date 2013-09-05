
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

BeginPackage["Param`", {"Thorn`", "Errors`", "Helpers`", "MapLookup`", "KrancGroups`", "Kranc`", "Jacobian`"}];

CreateKrancParam;
MakeFullParamDefs;
ParamName;

Begin["`Private`"];

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

Options[CreateKrancParam] = ThornOptions;
CreateKrancParam[evolvedGroups_, nonevolvedGroups_,
  evolvedODEGroups_, nonevolvedODEGroups_, groups_, thornName_, 
  reals_, ints_, keywords_,
  inheritedReals_, inheritedInts_, inheritedKeywords_,
  extendedReals_, extendedInts_, extendedKeywords_,
  evolutionTimelevels_, defaultEvolutionTimelevels_,
  calcs_, opts:OptionsPattern[]] :=
  Module[{nEvolved, evolvedMoLParam, evolvedGFs,
     nEvolvedODE, evolvedODEMoLParam,
    (*constrainedMoLParam,*) genericfdStruct, realStructs, intStructs,
    allInherited, allExtended, implementationNames, molImplementation,
    userImplementations, implementations, params, paramspec, param,
    verboseStruct, calcOffsetStructs, calcEveryStructs},

    (* reals and ints are symbols containing parameter names.  The
       inherited ones have implementation names as well *)

    evolvedGFs = variablesFromGroups[evolvedGroups, groups];

    nEvolved   = Length[variablesFromGroups[evolvedGroups, groups]];
(*    nPrimitive = Length[variablesFromGroups[nonevolvedGroups, groups]];*)
(*    nPrimitive = Length[getConstrainedVariables[evolvedGroups, groups]];*)

    evolvedMoLParam =
    {
      Name -> thornName <> "_MaxNumEvolvedVars",
      Type -> "CCTK_INT",
      Default -> nEvolved,
      Description -> "Number of evolved variables used by this thorn",
      Visibility -> "restricted",
      AccumulatorBase -> "MethodofLines::MoL_Num_Evolved_Vars",
      AllowedValues -> {{Value -> ToString[nEvolved] <> ":" <> ToString[nEvolved] , 
                         Description -> "Number of evolved variables used by this thorn"}},
      Steerable -> Recover
    };

    nEvolvedODE   = Length[variablesFromGroups[evolvedODEGroups, groups]];

    evolvedODEMoLParam =
    {
      Name -> thornName <> "_MaxNumArrayEvolvedVars",
      Type -> "CCTK_INT",
      Default -> nEvolvedODE,
      Description -> "Number of Array evolved variables used by this thorn",
      Visibility -> "restricted",
      AccumulatorBase -> "MethodofLines::MoL_Num_ArrayEvolved_Vars",
      AllowedValues -> {{Value -> ToString[nEvolvedODE] <> ":" <> ToString[nEvolvedODE] , 
                         Description -> "Number of Array evolved variables used by this thorn"}},
      Steerable -> Recover
    };

    (*
    constrainedMoLParam =
    {
      Name -> thornName <> "_MaxNumConstrainedVars",
      Type -> "CCTK_INT",
      Default -> nPrimitive,
      Description -> "Number of constrained variables used by this thorn",
      Visibility -> "restricted",
      AccumulatorBase -> "MethodofLines::MoL_Num_Constrained_Vars",
      AllowedValues -> {{Value -> ToString[nPrimitive] <> ":" <> ToString[nPrimitive] , 
                         Description -> "Number of constrained variables used by this thorn"}}
    };
    *)

    timelevelsParam =
    {
      Name -> "timelevels",
      Type -> "CCTK_INT",
      Default -> defaultEvolutionTimelevels,
      Description -> "Number of active timelevels",
      Visibility -> "restricted",
      AllowedValues -> {{Value -> ToString[0] <> ":" <> ToString[evolutionTimelevels],
                         Description -> ""}},
      Steerable -> Recover
    };

    rhsTimelevelsParam =
    {
      Name -> "rhs_timelevels",
      Type -> "CCTK_INT",
      Default -> 1,
      Description -> "Number of active RHS timelevels",
      Visibility -> "restricted",
      AllowedValues -> {{Value -> ToString[0] <> ":" <> ToString[evolutionTimelevels],
                         Description -> ""}},
      Steerable -> Recover
    };

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

    genericfdStruct =
    {
      Name -> "GenericFD",
      UsedParameters -> 
        Join[{{Name -> "assume_stress_energy_state", Type -> "CCTK_INT"}},
             If[OptionValue[UseJacobian], JacobianGenericFDParameters[], {}]]
    };

    realStructs = Map[krancParamStruct[#, "CCTK_REAL", False] &, reals];
    verboseStruct = krancParamStruct[{Name -> "verbose", Default -> 0, Steerable -> Always}, "CCTK_INT", False];
    intStructs = Map[krancParamStruct[#, "CCTK_INT", False] &, ints];
    calcEveryStructs = Map[krancParamStruct[{Name -> lookup[#, Name] <> "_calc_every", Default -> 1, Steerable -> Always}, "CCTK_INT", False] &, calcs];
    calcOffsetStructs = Map[krancParamStruct[{Name -> lookup[#, Name] <> "_calc_offset", Default -> 0, Steerable -> Always}, "CCTK_INT", False] &, calcs];
    keywordStructs = Map[krancKeywordParamStruct, keywords];

    allInherited = Join[inheritedReals, inheritedInts, inheritedKeywords];
    allExtended = Join[extendedReals, extendedInts, extendedKeywords];

    implementationNames = Union[Map[implementationFromQualifiedName, allInherited],
                                Map[implementationFromQualifiedName[lookup[#, Name]] &, allExtended]];

    molImplementation =
    {
      Name -> "MethodOfLines",
      UsedParameters -> 
      {
        {Name -> "MoL_Num_Evolved_Vars",        Type -> "CCTK_INT"},
        {Name -> "MoL_Num_ArrayEvolved_Vars",   Type -> "CCTK_INT"}
        (* {Name -> "MoL_Num_Constrained_Vars", Type -> "CCTK_INT"} *)
      }
    };

    userImplementations = Map[inheritParameters[#, inheritedReals,inheritedInts,inheritedKeywords] &, 
                              implementationNames];
    userImplementations2 = Map[extendParameters[#, extendedReals,extendedInts,extendedKeywords] &, 
                               implementationNames];

    userImplementations = If[userImplementations=={{}},{},userImplementations];
    userImplementations2 = If[userImplementations2=={{}},{},userImplementations2];

    implementations = Join[userImplementations, userImplementations2, {genericfdStruct, molImplementation}];
    params = Join[{verboseStruct}, realStructs, intStructs, keywordStructs, {evolvedMoLParam,
                  evolvedODEMoLParam, (*constrainedMoLParam,*) timelevelsParam, rhsTimelevelsParam, otherTimelevelsParam},
                  calcEveryStructs, calcOffsetStructs,
      CactusBoundary`GetParameters[evolvedGFs, evolvedGroups]];

    paramspec = {Implementations -> implementations,
                 NewParameters   -> params};

(*    Print["paramspec = ", paramspec];*)

    param = CreateParam[paramspec];
    Return[param]
  ];



End[];

EndPackage[];
