(* ::Package:: *)

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

BeginPackage["CodeGenCalculation`", {"CodeGenCactus`", "CodeGenC`", "CodeGen`",
  "CodeGenKranc`",
  "MapLookup`", "KrancGroups`", "Differencing`", "Errors`",
  "Helpers`", "Kranc`", "Optimize`", "Jacobian`", "Profile`", "Vectorisation`",
  "Calculation`", "DGFE`", "OpenCL`", "CalculationBoundaries`", "OperationCount`"}];

CreateCalculationFunction::usage = "";
CreateSetterSource::usage = "";
GridFunctionsInExpression;

Begin["`Private`"];

(* This flag determines whether you want to generate debugging code to
   do CCTK_INFO on the variables as they are translated.  This can
   help find problems in the construction of the translator maps. *)
debugInLoop = False;

(* --------------------------------------------------------------------------
   General Utility Functions (could be moved outside this package)
   -------------------------------------------------------------------------- *)

DefFn[VerifyListContent[l_, type_, while_] :=
  Module[{types},
    If[!(Head[l] === List),
      ThrowError["Expecting a list of ", type,
        " objects, but found the following, which is not a List object.  Error occured ", while, l]];
    types = Union[Map[Head, l]];
    If [!(types === {type}) && !(types === {}),
      ThrowError["Expecting a list of ", type ,
        " objects, but found the following types of object: ",
      ToString[types], " in ", l, while]]]];

(* ------------------------------------------------------------------------ 
   Setter
   ------------------------------------------------------------------------ *)

(* calculation = {Name                -> "ClassicADM_Setter", 
                  optional Before     -> {functions},
                  optional After      -> {functions},
                  Shorthands          -> {gInv11, ...},
	          GridFunctions       -> {g11rhs, K11},
                  CollectList         -> {hInv11, hInv22, ...},
         optional DeclarationIncludes -> {include file list},
         optional LoopPreIncludes     -> {include file list},
	          Equations           -> {{K11_rhs -> 2 A K11, ...}...}} *)


(* Given a list of Calculation structures as defined above, create a
   CodeGen representation of a source file that defines a function for
   each Calculation. *)

Options[CreateSetterSource] = ThornOptions;

DefFn[CreateSetterSource[calcs_, debug_, include_,
  opts:OptionsPattern[]] :=
  Module[{calc = First[calcs],bodyFunction,tiledBodyFunction},

  If[!MatchQ[include, _List],
    ThrowError["CreateSetterSource: Include should be a list but is in fact " <> ToString[include]]];

  SetDataType[If[OptionValue[UseVectors],VectorisationType[], "CCTK_REAL"]];

  {FileHeader["C"],

   "#define KRANC_" <> ToUpperCase[CodeGenC`SOURCELANGUAGE] <> "\n\n",

   If[CodeGenC`SOURCELANGUAGE == "C",
         {IncludeSystemFile["algorithm"],
          IncludeSystemFile["assert.h"],
          IncludeSystemFile["math.h"],
          IncludeSystemFile["stdio.h"],
          IncludeSystemFile["stdlib.h"],
          IncludeSystemFile["string.h"]},
         {"\n"}
      ],

   Map[IncludeFile, Join[{"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h",
                         (*"precomputations.h",*) "GenericFD.h", "Differencing.h"},
                         include,
                         If[CalculationLoopControlQ[calc], {"loopcontrol.h"},{}],
                         {"Kranc.hh"},
                         If[OptionValue[UseOpenCL], OpenCLIncludeFiles[], {}],
                         If[OptionValue[UseVectors], VectorisationIncludeFiles[], {}]]],
   CalculationMacros[OptionValue[UseVectors]],

   (* For each function structure passed, create the function and
      insert it *)

   CalculationBoundariesFunction[First[calcs]],

   bodyFunction = DefineFunction[lookup[calc,Name]<>"_Body", "static void", "const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[]",
  {
    "DECLARE_CCTK_ARGUMENTS;\n",
    "DECLARE_CCTK_PARAMETERS;\n\n", 
    #
  }] &;

   tiledBodyFunction = DefineFunction[lookup[calc,Name]<>"_Body", "static void", "const cGH* restrict const cctkGH, const KrancData & restrict kd",
  {
    "DECLARE_CCTK_ARGUMENTS;\n",
    "DECLARE_CCTK_PARAMETERS;\n\n", 

    "const int dir CCTK_ATTRIBUTE_UNUSED = kd.dir;\n",
    "const int face CCTK_ATTRIBUTE_UNUSED = kd.face;\n",
    "const int imin[3] = {std::max(kd.imin[0], kd.tile_imin[0]),\n",
    "                     std::max(kd.imin[1], kd.tile_imin[1]),\n",
    "                     std::max(kd.imin[2], kd.tile_imin[2])};\n",
    "const int imax[3] = {std::min(kd.imax[0], kd.tile_imax[0]),\n",
    "                     std::min(kd.imax[1], kd.tile_imax[1]),\n",
    "                     std::min(kd.imax[2], kd.tile_imax[2])};\n",

    #
  }] &;

    calc = Append[calc,Tile->(MatchQ[GetCalculationWhere[calc],
                                     Everywhere | Interior | InteriorNoSync]
      && OptionValue[Tile])];


   calc = Join[calc, {BodyFunction -> If[TileCalculationQ[calc],
                                         tiledBodyFunction,
                                         bodyFunction], 
                      CallerFunction -> True,
                      LoopFunction -> (GenericGridLoop[lookup[calc,Name],#,TileCalculationQ[calc], opts] &),
                      GFAccessFunction -> ({#,"[","index","]"} &),
                      InitFDVariables -> InitialiseFDVariables[OptionValue[UseVectors]],
                      MacroPointer -> True}];

   CreateCalculationFunction[calc, opts]}]];

(* --------------------------------------------------------------------------
   Calculations
   -------------------------------------------------------------------------- *)

DefFn[CheckGroupStorage[groupNames_, calcName_] :=
  Module[{ignoreGroups, groupsNames2},
    ignoreGroups = {"TmunuBase::stress_energy_scalar", "TmunuBase::stress_energy_vector",
      "TmunuBase::stress_energy_tensor"};
    groupNames2 = Select[groupNames, !MemberQ[ignoreGroups, #] &];
    {"\nconst char* const groups[] = {\n  ",
    Riffle[Map[Quote,groupNames2], ",\n  "],
    "};\n",
    "GenericFD_AssertGroupStorage(cctkGH, ", Quote[calcName],", ", Length[groupNames2], ", groups);\n"}]];

(* --------------------------------------------------------------------------
   Variables
   -------------------------------------------------------------------------- *)

DefFn[removeRHS[x_] :=
  Module[{string = ToString[x]},
    If[StringMatchQ[string, "*rhs"],
       ToExpression@StringDrop[string, -3],
       x]]];

(* Take a grid function name and return a name suitable for use in a local
   computation *)
DefFn[localName[x_] :=
  ToString[x] <> "L"];

(* --------------------------------------------------------------------------
   Predefinitions
   -------------------------------------------------------------------------- *)

DefFn[definePreDefinitions[pDefs_] :=
  CommentedBlock["Initialize predefined quantities",
    Map[DefineConstant[#[[1]], DataType[], #[[2]]] &, pDefs]]];

(* --------------------------------------------------------------------------
   Equations
   -------------------------------------------------------------------------- *)

(* Not using DefFn here because the function is called a very large
   number of times *)
equationUsesShorthand[eq_, shorthand_] :=
  Length[Cases[{Last[eq]}, shorthand, Infinity]] != 0;

(* Check that the given list of equations assigns things in the
   correct order.  Specifically, shorthands must not be used before
   they are assigned.  *)
DefFn[checkEquationAssignmentOrder[eqs_, shorthands_] :=
  Module[{},
    Map[checkShorthandAssignmentOrder[eqs,#] &, shorthands]]];

DefFn[printEq[eq_] :=
  Module[{lhs, rhs, rhsSplit, split, rhsString},
    lhs = First@eq;
    rhs = Last@eq;
    split[ x_ + y___] := { x, " + ..."};
    split[-x_ + y___] := {-x, " + ..."};
    split[ x_       ] := { x, ""};
    rhsSplit = split[Expand@ProcessExpression[rhs,False]];
    rhsString = ToString@CForm[rhsSplit[[1]]] <> rhsSplit[[2]];
    InfoMessage[InfoFull, " " <> ToString@lhs <> " -> " <> rhsString]]];

(* Collect and simplify terms *)
DefFn[simpCollect[collectList_, eqrhs_, localvar_, debug_] :=
  Module[{rhs, collectCoeff, all, localCollectList},
    InfoMessage[InfoFull, localvar];

    rhs = eqrhs;

    rhs = rhs /. Abs[MathTensor`Detg] -> MathTensor`Detg;
    InfoMessage[InfoFull, "ByteCount[rhs]: ", ByteCount@rhs];

    localCollectList = collectList /. VAR :> removeRHS@localvar;

    collectCoeff = Collect[rhs, localCollectList];
    InfoMessage[InfoFull, "ByteCount[terms collected]: ", ByteCount@collectCoeff];

    all = Profile["Collect/Simplify", Collect[rhs, localCollectList, Simplify]];
    InfoMessage[InfoFull, "ByteCount[simplified rhs]: ", ByteCount@all];

    all]];


(* --------------------------------------------------------------------------
   Shorthands
   -------------------------------------------------------------------------- *)

DefFn[defContainsShorthand[def_, shorthands_] :=
Module[{allAtoms, c},
  allAtoms = Union[Level[def, {-1}]];
  c = Intersection[shorthands, allAtoms];
  c != {}]];

DefFn[checkShorthandAssignmentOrder[eqs_, shorthand_] :=
  Module[{useBooleans, uses, firstUse, lhss, assignments},

  (* Make a list of booleans describing, for each equation, whether it
     uses the given shorthand *)
  useBooleans = Map[equationUsesShorthand[#, shorthand] &, eqs];
  uses = Position[useBooleans, True];
  lhss = Map[First, eqs];

  (* The equation numbers that define this shorthand *)
  assignments = Position[lhss, shorthand];

  If[Length[uses] == 0 && Length[assignments] >= 1,
    InfoMessage[Warning, "WARNING: Shorthand ", shorthand,
      " is defined but not used in this equation list."]];

  If[Length[uses] == 0, Return[]];

  (* The number of the first equation to use this shorthand *)
  firstUse = First[uses];

  If[Length[assignments] > 1,
     InfoMessage[Warning, "WARNING: Shorthand ", shorthand, " is defined more than once."]];

  If[Length[assignments] == 0,
    ThrowError["Shorthand", shorthand, "is not defined in this equation list", eqs]];

  If[assignments[[1]] >= firstUse,
    ThrowError["Shorthand", shorthand,
      "is used before it is defined in this equation list", eqs]]]];

(* --------------------------------------------------------------------------
   Partial derivatives
   -------------------------------------------------------------------------- *)

(* Split the list of partial derivative definitions into those
   containing shorthands, and those that do not.  *)
DefFn[splitPDDefsWithShorthands[pddefs_, shorthands_] :=
  Module[{defsWithShorts, defsWithoutShorts},
    defsWithShorts = Select[pddefs, defContainsShorthand[#, shorthands] &];
    defsWithoutShorts = Select[pddefs, ! defContainsShorthand[#, shorthands] &];
    Return[{defsWithoutShorts, defsWithShorts}]]];

DefFn[pdCanonicalOrdering[name_[inds___] -> x_] :=
  Module[{is},
    is = {inds};
    If[Length[is] == 2,
      Return[{name[f_,2,1] -> name[f,1,2],
              name[f_,3,1] -> name[f,1,3],
              name[f_,3,2] -> name[f,2,3]}],
      {}]]];

(* --------------------------------------------------------------------------
   Calculation function generation
   -------------------------------------------------------------------------- *)

Options[CreateCalculationFunction] = Join[ThornOptions,{Debug -> False}];

DefFn[
  CreateCalculationFunction[calcp_, opts:OptionsPattern[]] :=
  Module[{gfs, allSymbols, knownSymbols,
          shorts, eqs, parameters, parameterRules, odeGroups,
          functionName, dsUsed, groups, pddefs, cleancalc, eqLoop, where,
          addToStencilWidth, pDefs, haveCondTextuals, condTextuals, calc,
          kernelCall, DGFEDefs, DGFEInit, DGFECallCode, debug, useJacobian,
          imp, gridName, stencilSize},

  debug = OptionValue[Debug];
  useJacobian = OptionValue[UseJacobian] && lookupDefault[calcp, UseJacobian, True];
  imp = lookup[calcp, Implementation];
  gridName = lookup[calcp, GFAccessFunction];

  functionName = ToString@lookup[calcp, Name];
  bodyFunctionName = functionName <> "_Body";

  InfoMessage[Terse, "Creating calculation function: " <> functionName];

  InfoMessage[InfoFull, "Calculation sets "<>ToString[Map[First,lookup[calcp, Equations]]]];

  calc = If[useJacobian, InsertJacobian[calcp, opts], calcp];

  cleancalc = RemoveUnusedShorthands[calc];
  If[OptionValue[CSE],
    cleancalc = EliminateCommonSubexpressions[cleancalc, opts]];

  shorts = lookupDefault[cleancalc, Shorthands, {}];

  eqs    = lookup[cleancalc, Equations];
  parameters = lookupDefault[cleancalc, Parameters, {}];
  groups = lookup[cleancalc, Groups];
  odeGroups = lookupDefault[cleancalc, ODEGroups, {}];
  If[useJacobian, groups = Join[groups, JacobianGroups[]]];
  pddefs = lookupDefault[cleancalc, PartialDerivatives, {}];
  addToStencilWidth = lookupDefault[cleancalc, AddToStencilWidth, 0];
  pDefs = lookup[cleancalc, PreDefinitions];
  haveCondTextuals = mapContains[cleancalc, ConditionalOnTextuals];

  VerifyCalculation[cleancalc];

  gfs = allGroupVariables[groups];

  InfoMessage[InfoFull, " " <> ToString[Length@shorts] <> " shorthands"];
  InfoMessage[InfoFull, " " <> ToString[Length@gfs] <> " grid functions"];
  InfoMessage[InfoFull, " " <> ToString[Length@groups] <> " groups"];

  InfoMessage[InfoFull, "Shorthands: ", shorts];
  InfoMessage[InfoFull, "Grid functions: ", gfs];
  InfoMessage[InfoFull, "Groups: ", Map[groupName, groups]];

  If[Length@lookupDefault[cleancalc, CollectList, {}] > 0,
    eqs = Map[First[#] -> simpCollect[lookup[cleancalc, CollectList],
                                      Last[#],
                                      First[#], debug] &,
              eqs],
    (* else *)
    If[!lookupDefault[cleancalc, NoSimplify, False],
       eqs = Map[(InfoMessage[InfoFull, "Simplifying "<>ToString[#[[1]], InputForm]<>" -> ..."]; Simplify[#, {r>=0}]) &, eqs]]];

  InfoMessage[InfoFull, "Equations:"];

  (* Wrap parameters with ToReal unless they are part of the condition in an IfThen *)
  parameterRules = Map[(#->ToReal[#])&, parameters];
  eqs = eqs /. Prepend[parameterRules, IfThen[cond_, x_, y_] :> IfThen[cond, x/.parameterRules, y/.parameterRules]];

  (* Map[printEq, eqs]; *)

  Scan[InfoMessage[InfoFull, "  " <> ToString@First[#]<>" = ..."] &, eqs];

  (* Compute necessary stencil size *)
  stencilSize = StencilSize[pddefs, eqs, functionName, OptionValue[ZeroDimensions],
                               lookup[{opts}, IntParameters, {}]];
  If[!VectorQ[stencilSize],
     stencilSize = MapThread[Max,Map[Last,stencilSize[[2]]]]];

  where = GetCalculationWhere[cleancalc];
  If[where === Automatic,
     where = If[MatchQ[stencilSize, {0,0,0}] =!= True, Interior, Everywhere]];

  (* Check all the function names *)
  functionsPresent = FunctionsInCalculation[cleancalc]; (* Not currently used *)

  (* Check that there are no shorthands defined with the same name as a grid function *)
  If[!(Intersection[shorts, gfs] === {}),
    ThrowError["The following shorthands are already declared as grid functions:",
      Intersection[shorts, gfs]]];

  (* check that the passed in textual condition makes sense *)
  If[haveCondTextuals,
    condTextuals = lookup[cleancalc, ConditionalOnTextuals];
    If[! MatchQ[condTextuals, {_String ...}],
      ThrowError["ConditionalOnTextuals entry in calculation expected to be of the form {string, ...}, but was ", condTextuals, "Calculation is ", calc]];
    ];

  (* Check that there are no unknown symbols in the calculation *)
  allSymbols = CalculationSymbols[cleancalc];
  knownSymbols = Join[lookupDefault[cleancalc, AllowedSymbols, {}], gfs, shorts, parameters,
    {dx,dy,dz,dt,idx,idy,idz,t, usejacobian, Pi, E, Symbol["i"], Symbol["j"], Symbol["k"], Symbol["ti"], Symbol["tj"], Symbol["tk"], normal1, normal2,
    normal3, tangentA1, tangentA2, tangentA3, tangentB1, tangentB2, tangentB3},
    If[useJacobian, JacobianSymbols[], {}]];

  unknownSymbols = Complement[allSymbols, knownSymbols];

  unknownSymbols /.
    {{} :> True,
      {s_} :> ThrowError["Unknown symbol " <> ToString[s] <> " in calculation " <> GetCalculationName[cleancalc]],
      ss_ :> ThrowError["Unknown symbols " <> StringJoin[Riffle[ToString/@ss,", "]] <> " in calculation " <> GetCalculationName[cleancalc]],
    _ :> ThrowError["Internal error: unrecognised value for unknownSymbols: "<>ToString[unknownSymbols, InputForm]]};

  kernelCall = Switch[where,
    Everywhere,
    If[TileCalculationQ[cleancalc],
      lookup[cleancalc,ThornName]<>"_TiledLoopOverEverything(cctkGH, " <> bodyFunctionName <> ");\n",
      "GenericFD_LoopOverEverything(cctkGH, " <> bodyFunctionName <> ");\n"],
    Interior | InteriorNoSync,
    If[TileCalculationQ[cleancalc],
      lookup[cleancalc,ThornName]<>"_TiledLoopOverInterior(cctkGH, " <> bodyFunctionName <> ");\n",
      "GenericFD_LoopOverInterior(cctkGH, " <> bodyFunctionName <> ");\n"],
    Boundary | BoundaryNoSync,
      "GenericFD_LoopOverBoundary(cctkGH, " <> bodyFunctionName <> ");\n",
    BoundaryWithGhosts,
      "GenericFD_LoopOverBoundaryWithGhosts(cctkGH, " <> bodyFunctionName <> ");\n",
    _,
      ThrowError["Unknown 'Where' entry in calculation " <>
        functionName <> ": " <> ToString[where]]];

  DGFEDefs = If[OptionValue[UseDGFE], DGFEDefinitions[cleancalc, eqs, gfs], {}];

  DGFEInit = If[OptionValue[UseDGFE], DGFEInitialise[cleancalc], {}];

  DGFECallCode = If[OptionValue[UseDGFE] && lookupDefault[cleancalc, UseDGFE, False],
       DGFECall[cleancalc],
       {}];

  InfoMessage[InfoFull,"Generating function"];
  {
    DGFEDefs,
    lookup[calcp,BodyFunction][{
    (* OpenCL kernel prologue *)
    (* We could (or probably should) write this into a source file of its own *)
    If[OptionValue[UseOpenCL], {OpenCLPrologue[]}, {}],

    Module[
      {kernelCode},
      kernelCode =
      {
    CommentedBlock["Include user-supplied include files",
      Map[IncludeFile, lookupDefault[cleancalc, DeclarationIncludes, {}]]],

    lookup[calcp,InitFDVariables],

    definePreDefinitions[pDefs],

    If[useJacobian, CreateJacobianVariables[], {}],

    If[Cases[{pddefs}, SBPDerivative[_], Infinity] != {},
      CommentedBlock["Compute Summation By Parts derivatives",
        IncludeFile["sbp_calc_coeffs.h"]],
      {}],

    If[gfs != {},
      {
	eqLoop = If[lookup[calcp, SimpleCode, False], 
                    simpleEquationLoop,
                    equationLoop][eqs, cleancalc, gfs, shorts, {}, groups, odeGroups,
          pddefs, where, addToStencilWidth, opts]},
      {}]

    };

    If[OptionValue[UseOpenCL], kernelCode = OpenCLProcessKernel[kernelCode]];

    kernelCode],

    (* OpenCL kernel epilogue *)
    If[OptionValue[UseOpenCL], OpenCLEpilogue[cleancalc, imp, functionName], {}]
    }], (* <BodyFunction *)
  
    If[lookup[calcp,CallerFunction],
      DefineCCTKSubroutine[functionName,
      FlattenBlock[{
        If[haveCondTextuals, Map[ConditionalOnParameterTextual["!(" <> # <> ")", "return;\n"] &,condTextuals], {}],

        ConditionalOnParameterTextual["verbose > 1",
          "CCTK_VInfo(CCTK_THORNSTRING,\"Entering " <> bodyFunctionName <> "\");\n"],

        ConditionalOnParameterTextual["cctk_iteration % " <> functionName <> "_calc_every != " <>
          functionName <> "_calc_offset", "return;\n"],
  
        CheckGroupStorage[GroupsInCalculation[cleancalc, imp], functionName],
        "\n",

        CheckStencil[pddefs, eqs, functionName, OptionValue[ZeroDimensions],
                     lookup[{opts}, IntParameters, {}]],

        If[where === Everywhere && !OptionValue[UseDGFE] && MatchQ[stencilSize, {0,0,0}] =!= True,
           ThrowError["Calculation "<>functionName<>" uses derivative operators but is computed Everywhere.  Specify Where -> Interior for calculations that use derivative operators."]];
        "\n",

        DGFEInit,

        kernelCall,

        DGFECallCode,

        ConditionalOnParameterTextual["verbose > 1",
          "CCTK_VInfo(CCTK_THORNSTRING,\"Leaving " <> bodyFunctionName <> "\");\n"]
      }]],
      (* else *)
       ""]
  }]];

Options[equationLoop] = ThornOptions;

DefFn[
  equationLoop[eqs_, cleancalc_, gfs_, shorts_, incs_, groups_, odeGroups_, pddefs_,
             where_, addToStencilWidth_,
             opts:OptionsPattern[]] :=
  Module[{rhss, lhss, gfsInRHS, gfsInLHS, gfsOnlyInRHS, localGFs,
          localMap, eqs2, derivSwitch, code, functionName, calcCode,
          gfsInBoth, gfsDifferentiated,
          gfsDifferentiatedAndOnLHS, declare, eqsReplaced,
          arraysInRHS, arraysInLHS, arraysOnlyInRHS, odeVars,
          generateEquationCode, groupedIfs, IfThenGroup, noSimplify,
          gridName, useJacobian, allCode, opCounts},

    InfoMessage[InfoFull, "Equation loop"];

    {allCode, opCounts} = Reap[

    useJacobian = OptionValue[UseJacobian] && lookupDefault[cleancalc, UseJacobian, True];

    gridName = Function[x,FlattenBlock[lookup[cleancalc, GFAccessFunction][x]]];

    rhss = Map[#[[2]] &, eqs];
    lhss = Map[#[[1]] &, eqs];

    orderings = Flatten[Map[pdCanonicalOrdering, pddefs], 1];
    eqsOrdered = (eqs //. orderings);

    odeVars = variablesFromGroups[odeGroups, groups];

    gfsInRHS = Union[Cases[rhss, _ ? (MemberQ[gfs,#] &), Infinity]];
    gfsInLHS = Union[Cases[lhss, _ ? (MemberQ[gfs,#] &), Infinity]];
    gfsOnlyInRHS = Complement[gfsInRHS, gfsInLHS];

    gfsInBoth = Intersection[gfsInRHS,gfsInLHS];

    If[OptionValue[ProhibitAssignmentToGridFunctionsRead] &&
       gfsInBoth =!= {},
      ThrowError["The calculation " <> ToString@lookup[cleancalc, Name] <>
        " has the grid functions " <> ToString[gfsInBoth] <>
        " on both the left hand side and the right hand side.  This is" <>
        " not allowed with the option" <>
        " ProhibitAssignmentToGridFunctionsRead -> True."]];

    localGFs = Map[localName, gfs];
    localMap = Map[# -> localName[#] &, gfs];

    derivSwitch =
      GridFunctionDerivativesInExpression[pddefs, eqsOrdered,
                                          OptionValue[ZeroDimensions]] != {};

    gfsDifferentiated = Map[First,
      GridFunctionDerivativesInExpression[pddefs, eqsOrdered,
                                          OptionValue[ZeroDimensions]]];

    gfsDifferentiatedAndOnLHS = Intersection[gfsDifferentiated, gfsInLHS];

    If[gfsDifferentiatedAndOnLHS =!= {},
      ThrowError["The calculation " <> ToString@lookup[cleancalc, Name] <>
        " has both assignments to, and derivatives of, the grid functions " <>
        ToString[gfsDifferentiatedAndOnLHS] <>
        ".  This is not allowed, as it gives results which are dependent" <>
        " on the ordering of the loop over grid points."]];

    (* Replace the partial derivatives *)
    {defsWithoutShorts, defsWithShorts} = splitPDDefsWithShorthands[pddefs, shorts];
    eqs2 = ReplaceDerivatives[defsWithoutShorts, eqsOrdered, True,
                              OptionValue[ZeroDimensions],lookup[cleancalc, MacroPointer]];
    eqs2 = ReplaceDerivatives[defsWithShorts, eqs2, False, OptionValue[ZeroDimensions], lookup[cleancalc, MacroPointer]];

    checkEquationAssignmentOrder[eqs2, shorts];
    functionName = ToString@lookup[cleancalc, Name];

    (* Replace grid functions with their local forms *)
    eqsReplaced = eqs2 /. Join[{g:GFOffset[___]:>g},localMap];

    gridFunctionsFreeQ[x_] :=
    Module[
      {r},
      (* Print["groups = ", groups]; *)
      (* Print["gridFunctionsFreeQ[", x, "] ="]; *)
      r = GridFunctionsInExpression[x, groups/.localMap] === {};
      (* Print["  ", r]; *)
      r];

    (* Construct a list, corresponding to the list of equations,
       marking those which need their LHS variables declared.  We
       declare variables at the same time as assigning to them as it
       gives a performance increase over declaring them separately at
       the start of the loop.  The local variables for the grid
       functions which appear in the RHSs have been declared and set
       already (DeclareMaybeAssignVariableInLoop below), so assignments
       to these do not generate declarations here. *)
    declare = Block[{$RecursionLimit=Infinity},MarkFirst[First /@ eqsReplaced, Map[localName, gfsInRHS]]];

    (* Replace consecutive IfThen statements with the same condition by a single IfThenGroup *)
    groupedIfs = Thread[{declare, eqsReplaced}] //.
      {{x___, {deca_, a_->IfThen[cond_, at_, af_]}, {decb_, b_->IfThen[cond_, bt_, bf_]}, y___} :>
         {x, {{deca, decb}, IfThenGroup[cond, {a->at, b->bt}, {a->af, b->bf}]}, y} /; gridFunctionsFreeQ[cond],
       {x___, {deca_, IfThenGroup[cond_, at_, af_]}, {decb_, b_->IfThen[cond_, bt_, bf_]}, y___} :>
         {x, {Join[deca, {decb}], IfThenGroup[cond, Join[at, {b->bt}], Join[af, {b->bf}]]}, y},
       {x___, {deca_, IfThenGroup[cond_, at_, af_]}, {decb_, IfThenGroup[cond_, bt_, bf_]}, y___} :>
         {x, {Join[deca, decb], IfThenGroup[cond, Join[at, bt], Join[af, bf]]}, y}};

    noSimplify = lookupDefault[cleancalc, NoSimplify, False];

    (* Generate actual code strings. Try to declare variables as they are assigned, but
       it is only possible to do this outside all if(){} statements. *)
    generateEquationCode[{declare2_, eq2_}] :=
      Module[{ret, vars, preDeclare, cond},
        InfoMessage[InfoFull, "Generating code for " <> ToString[eq2[[1]], InputForm]];
        Which[
        SameQ[Head[eq2[[2]]], IfThen],
          ret = AssignVariableFromExpression[eq2[[1]],
            eq2[[2]] /. IfThen[cond_, x__]:> IfThen[Scalar[cond], x], declare2, OptionValue[UseVectors], noSimplify];,
        SameQ[Head[eq2], IfThenGroup],
          vars = eq2[[2,All,1]];
          cond = eq2[[1]];
          preDeclare = Pick[vars, declare2];
          ret = {Map[DeclareVariable[#, DataType[]] &, Complement[Union[preDeclare], localName/@gfsInRHS]], {"\n"},
                 Conditional[GenerateCodeFromExpression[Scalar[cond], False],
                  Riffle[AssignVariableFromExpression[#[[1]], #[[2]], False, OptionValue[UseVectors], noSimplify]& /@ eq2[[2]], "\n"],
                  Riffle[AssignVariableFromExpression[#[[1]], #[[2]], False, OptionValue[UseVectors], noSimplify]& /@ eq2[[3]], "\n"]]};,
        True,
          ret = AssignVariableFromExpression[eq2[[1]], eq2[[2]], declare2, OptionValue[UseVectors], noSimplify];
        ];
        ret
    ];

    groupedIfsArrays = Select[groupedIfs, MemberQ[Map[localName, odeVars], #[[2]][[1]]]  &];
    groupedIfs = Select[groupedIfs, !MemberQ[Map[localName, odeVars], #[[2]][[1]]]  &];

    calcCode = Riffle[generateEquationCode /@ groupedIfs, "\n"];
    calcCodeArrays = Riffle[generateEquationCode /@ groupedIfsArrays, "\n"];
    InfoMessage[InfoFull, "Finished generating equation code"];

    assignLocalFunctions[gs_, useVectors_, useJacobian_, NameFunc_] :=
      Module[{conds, varPatterns, varsInConds, simpleVars, code},
        conds =
          {{"eT" ~~ _ ~~ _, "assume_stress_energy_state>=0 ? assume_stress_energy_state : *stress_energy_state", "ToReal(0.0)"}}; (* This should be passed as an option *)
        If[useJacobian,
          conds = Append[conds, JacobianConditionalGridFunctions[]]];

        varPatterns = Map[First, conds];
        varsInConds = Map[Function[pattern, Select[gs,StringMatchQ[ToString[#], pattern] &]], varPatterns];
        simpleVars = Complement[gs, Flatten[varsInConds]];
        code = {"\n",
         Map[DeclareMaybeAssignVariableInLoop[
              DataType[], localName[#], NameFunc[#],
              False,"", useVectors] &, simpleVars],
         {"\n",
         Riffle[
           MapThread[
             If[Length[#2] > 0,
               {DeclareVariables[localName/@#2, DataType[]],"\n",
                Conditional[#1,
                  Table[AssignVariableInLoop[localName[var], NameFunc[var], useVectors], {var, #2}],
                  Sequence@@If[#3 =!= None, {Table[AssignVariableInLoop[localName[var], #3, False (*useVectors*)], {var, #2}]}, {}]]},
               (* else *)
               {}] &,
             {Map[#[[2]]&, conds], varsInConds, Map[#[[3]]&, conds]}], "\n"]}};
         code
      ];

    assignLocalGridFunctions[gs_, useVectors_, useJacobian_] := assignLocalFunctions[gs, useVectors, useJacobian, gridName];
    assignLocalArrayFunctions[gs_] := assignLocalFunctions[gs, False, False, ArrayName];

    (* separate grid and array variables *)
    arraysInRHS = Intersection[odeVars, gfsInRHS];
    arraysInLHS = Intersection[odeVars, gfsInLHS];
    arraysOnlyInRHS = Complement[arraysInRHS, arraysInLHS];

    gfsInRHS = Complement[gfsInRHS, odeVars];
    gfsInLHS = Complement[gfsInLHS, odeVars];
    gfsOnlyInRHS = Complement[gfsInRHS, gfsInLHS];

    {
    CommentedBlock["Assign local copies of arrays functions",
      assignLocalArrayFunctions[arraysInRHS]],

    CommentedBlock["Calculate temporaries and arrays functions", calcCodeArrays],

    CommentedBlock["Copy local copies back to grid functions",
        Map[AssignVariableInLoop[ArrayName[#], localName[#]] &, arraysInLHS]],

    lookup[cleancalc,LoopFunction][
    {
      (* TODO: Only make local copies for variables that are actually
         used later on; see e.g. variablesReadInCalc for how to make the
         distinction *)
      CommentedBlock["Assign local copies of grid functions",
        assignLocalGridFunctions[gfsInRHS, OptionValue[UseVectors], useJacobian]],

      CommentedBlock["Include user supplied include files",
        Map[IncludeFile, incs]],

      CommentedBlock["Precompute derivatives",
        PrecomputeDerivatives[defsWithoutShorts, eqsOrdered, 
                              lookup[{opts}, IntParameters, {}],
                              OptionValue[ZeroDimensions],
                              lookup[cleancalc, MacroPointer]]],

      CommentedBlock["Calculate temporaries and grid functions", calcCode],

      If[debugInLoop,
        Map[InfoVariable[#[[1]]] &, (eqs2 /. localMap)],
        ""],

      localsToGridFunctions[gfsInLHS, gridName,
                            Which[OptionValue[UseOpenCL], "OpenCL",
                                  OptionValue[UseVectors], "Vectors",
                                  True, "Default"]],

      If[debugInLoop, Map[InfoVariable[gridName[#]] &, gfsInLHS], ""]}, opts]}, 

        CountOperations];
  
    If[OptionValue[CountOperations],
      ProcessOperationCount[opCounts, GetCalculationName[cleancalc]]];
  
    allCode]];

Options[simpleEquationLoop] = ThornOptions;
DefFn[
  simpleEquationLoop[eqs_, cleancalc_, gfs_, shorts_, incs_, groups_, odeGroups_, pddefs_,
                     where_, addToStencilWidth_,
                     opts:OptionsPattern[]] :=
  Module[
    {functionName, eqs2, gridName, allCode, opCounts},
    functionName = ToString@lookup[cleancalc, Name];
    eqs2 = eqs;

    {allCode, opCounts} = Reap[

    CountDerivativeOperations[pddefs, eqs2, OptionValue[ZeroDimensions]];

    eqs2 = ReplaceDerivatives[pddefs, eqs2, False, OptionValue[ZeroDimensions], 
                              lookup[cleancalc, MacroPointer]];
    gridName = lookup[cleancalc, GFAccessFunction];
    
    lookup[cleancalc,LoopFunction][
      {
        CommentedBlock[
          "Calculate temporaries and grid functions", 
          If[OptionValue[UseVectors],
             VectorisationSimpleAssign[FlattenBlock[gridName[#]] & /@ Map[First, eqs2],
                                       Map[Last, eqs2]],
             Map[
               AssignVariableFromExpression[FlattenBlock@gridName[#[[1]]], #[[2]], False, False, True] &, eqs2]]]
      }, opts], 

        CountOperations];
  
    ProcessOperationCount[opCounts, GetCalculationName[cleancalc]];
  
    allCode]];


(* Unsorted *)

DefFn[GridFunctionsInExpression[x_, groups_] :=
  Union[Cases[x, _ ? (MemberQ[allGroupVariables[groups],#] &), Infinity]]];

(* Copy local variables back to their grid functions *)

DefFn[
  localsToGridFunctions[gfsInLHS_List, gridName_, method_String] :=
  CommentedBlock[
    "Copy local copies back to grid functions",
    localsToGridFunctions2[gridName /@ gfsInLHS, localName /@ gfsInLHS, method]]];

DefFn[
  localsToGridFunctions2[gridNames_List, localNames_List, "OpenCL"] :=
  OpenCLLocalsToGridFunctions[gridNames, localNames]];

DefFn[
  localsToGridFunctions2[gridNames_List, localNames_List, "Vectors"] :=
  VectorisationLocalsToGridFunctions[gridNames, localNames]];

DefFn[
  localsToGridFunctions2[gridNames_List, localNames_List, "Default"] :=
  MapThread[AssignVariableInLoop, {gridNames, localNames}]];

End[];

EndPackage[];
