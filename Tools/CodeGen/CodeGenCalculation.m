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
  "Calculation`", "DGFE`", "OpenCL`", "CalculationBoundaries`", "OperationCount`", "Object`"}];

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

DefFn[CreateSetterSource[calcs_, debug_, include_, thornName_,
  opts:OptionsPattern[]] :=
  Block[{$CodeGenTarget = NewObject[TargetC, {"UseVectors" -> OptionValue[UseVectors]}]},
    Module[{calc = First[calcs],
            bodyFunction, tiledBodyFunction, dgTiledBodyFunction},

  If[!MatchQ[include, _List],
    ThrowError["CreateSetterSource: Include should be a list but is in fact " <> ToString[include]]];

  SetDataType[If[OptionValue[UseVectors],VectorisationType[], "CCTK_REAL"]];

  {FileHeader["C"],

   NewlineSeparated[
     {"#define KRANC_" <> ToUpperCase[CodeGenC`SOURCELANGUAGE] <> "\n",

   Join[If[CodeGenC`SOURCELANGUAGE == "C",
         {IncludeSystemFile["algorithm"],
          IncludeSystemFile["assert.h"],
          IncludeSystemFile["math.h"],
          IncludeSystemFile["stdio.h"],
          IncludeSystemFile["stdlib.h"],
          IncludeSystemFile["string.h"]}, {}],

   Map[IncludeFile, Join[{"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h",
                          (* This has to be before anything which includes
                             vectors.h (including Differencing.h) *)
                          "Kranc.hh"
                          (*"precomputations.h"*)},
                         If[!OptionValue[DGTile], {"Differencing.h"}, {}],
                         include,
                         If[CalculationLoopControlQ[calc], {"loopcontrol.h"},{}],
                         If[OptionValue[UseDGFE], {"hrscc.hh"}, {}],
                         If[OptionValue[DGTile], {"StencilOps.hh"}, {}],
                         If[OptionValue[UseOpenCL], OpenCLIncludeFiles[], {}],
                         If[OptionValue[UseVectors], VectorisationIncludeFiles[], {}]]]],

   (* For each function structure passed, create the function and
      insert it *)

   WithNamespace[thornName, NewlineSeparated[
     {CalculationBoundariesFunction[First[calcs]],

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

   dgTiledBodyFunction = DefineFunction[
     lookup[calc, Name] <> "_Body",
     "static void",
     "const cGH* restrict const cctkGH, const KrancData& restrict kd",
     {
       "DECLARE_CCTK_ARGUMENTS;\n",
       "DECLARE_CCTK_PARAMETERS;\n",
       "\n",
       "// Define stencil family\n",
       "constexpr ptrdiff_t order = "<>ToString[OptionValue[DGOrder]]<>";\n",
       "\n",
       "// Define stencil operators\n",
       "typedef dgop_derivs<order> dgop_PD;\n",
       "typedef dgop_filter<order> dgop_Filter;\n",
       "static_assert(dgop_PD::order == order, \"\")\n";
       "static_assert(dgop_Filter::order == order, \"\")\n";
       "\n",
       "// Determine tile shape from stencil\n",
       "constexpr ptrdiff_t npoints_i = order + 1;\n",
       "constexpr ptrdiff_t npoints_j = order + 1;\n",
       "constexpr ptrdiff_t npoints_k = order + 1;\n",
       "\n",
       "// Grid function shape (in grid points)\n",
       "const ptrdiff_t ni = cctk_lsh[0];\n",
       "const ptrdiff_t nj = cctk_lsh[1];\n",
       "const ptrdiff_t nk = cctk_lsh[2];\n",
       "\n",
       "// Check tile shape consistency\n",
       "assert((ni - 2) % npoints_i == 0);\n",
       "assert((nj - 2) % npoints_j == 0);\n",
       "assert((nk - 2) % npoints_k == 0);\n",
       "\n",
       "// Check function arguments\n",
       "assert(kd.tile_imax[0] - kd.tile_imin[0] == npoints_i);\n",
       "assert(kd.tile_imax[1] - kd.tile_imin[1] == npoints_j);\n",
       "assert(kd.tile_imax[2] - kd.tile_imin[2] == npoints_k);\n",
       "\n",
       "// Location of current tile in grid function (in grid points)\n",
       "const ptrdiff_t i1 = kd.tile_imin[0];\n",
       "const ptrdiff_t j1 = kd.tile_imin[1];\n",
       "const ptrdiff_t k1 = kd.tile_imin[2];\n",
       "\n",
       "// Grid function strides (in bytes)\n",
       "constexpr ptrdiff_t di = sizeof(CCTK_REAL);\n",
       "const ptrdiff_t dj = di * cctk_ash[0];\n",
       "const ptrdiff_t dk = dj * cctk_ash[1];\n",
       "\n",
       "// Calculate grid function offset (in bytes)\n",
       "const ptrdiff_t off1 = i1 * di + j1 * dj + k1 * dk;\n",
       "\n",
       "// Vector size (in elements)\n",
       "constexpr ptrdiff_t vs = CCTK_REAL_VEC_SIZE;\n",
       "\n",
       "// Padded tile shape (in grid points)\n",
       "// constexpr ptrdiff_t tni = alignup(npoints_i, vs);\n",
       "constexpr ptrdiff_t tni = npoints_i;\n",
       "constexpr ptrdiff_t tnj = npoints_j;\n",
       "constexpr ptrdiff_t tnk = npoints_k;\n",
       "\n",
       "// Padded tile strides (in bytes)\n",
       "constexpr ptrdiff_t tdi = sizeof(CCTK_REAL);\n",
       "constexpr ptrdiff_t tdj = tdi * tni;\n",
       "constexpr ptrdiff_t tdk = tdj * tnj;\n",
       "// constexpr ptrdiff_t tsz = tdk * tnk;\n",
       "constexpr ptrdiff_t tsz = alignup(tdk * tnk, CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n",
       "\n",
       #
     }] &;

   calc = Append[calc,
                 Tile -> (MatchQ[GetCalculationWhere[calc],
                                 (* Everywhere | *)
                                 Interior | InteriorNoSync] &&
                          OptionValue[Tile])];
  (* Not all DGTile calculation are actually tiled.
     Calculations that include the boundaries cannot be DG-tiled,
     and are treated as regulare calculations.
     We assume that they don't evaluate any derivatives. *)
   calc = Append[calc,
                 DGTile -> (MatchQ[GetCalculationWhere[calc],
                                   Interior | InteriorNoSync] && 
                            OptionValue[DGTile])];

   calc = Join[calc, {BodyFunction -> Which[DGTileCalculationQ[calc],
                                            dgTiledBodyFunction,
                                            TileCalculationQ[calc],
                                            tiledBodyFunction,
                                            True,
                                            bodyFunction],
                      CallerFunction -> True,
                      LoopFunction -> (GenericGridLoop[lookup[calc,Name], #,
                                                       TileCalculationQ[calc],
                                                       DGTileCalculationQ[calc],
                                                       opts] &),
                      GFAccessFunction -> ReadGridFunctionInLoop,
                      InitFDVariables -> (InitialiseFDVariables
                                          [OptionValue[UseVectors],
                                           DGTileCalculationQ[calc]]),
                      MacroPointer -> True}];

   CreateCalculationFunction[calc, opts]}]]}]}]]];

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
    "AssertGroupStorage(cctkGH, ", Quote[calcName],", ", Length[groupNames2], ", groups);\n",
    "\n"}]];

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
    rhsSplit = split[Expand@ProcessExpression[rhs,False,False]];
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
  Module[
    {debug, useJacobian,
     imp, gridName, functionName, bodyFunctionName,
     calc, cleancalc,
     shorts, eqs,
     parameters,
     groups, odeGroups,
     pddefs,
     addToStencilWidth,
     pDefs,
     haveCondTextuals,
     gfs,
     parameterRules,
     singlePointStencil,
     where,
     functionsPresent,
     condTextuals,
     allSymbols, knownSymbols,
     unknownSymbols,
     kernelCall,
     DGFEDefs, DGFEInit, DGFECallCode
    },

    debug = OptionValue[Debug];
    useJacobian =
      OptionValue[UseJacobian] && lookupDefault[calcp, UseJacobian, True];
    imp = lookup[calcp, Implementation];
    gridName = lookup[calcp, GFAccessFunction];

    functionName = ToString@lookup[calcp, Name];
    bodyFunctionName = functionName <> "_Body";

    InfoMessage[Terse, "Creating calculation function: " <> functionName];
    InfoMessage[InfoFull,
                "Calculation sets " <> ToString[Map[First,
                                                    lookup[calcp, Equations]]]];

    calc = If[useJacobian, InsertJacobian[calcp, opts], calcp];
    cleancalc = RemoveUnusedShorthands[calc];
    If[OptionValue[CSE],
       cleancalc = EliminateCommonSubexpressions[cleancalc, opts]];

    shorts = lookupDefault[cleancalc, Shorthands, {}];
    eqs = lookup[cleancalc, Equations];
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
          eqs = Map[(InfoMessage[InfoFull,
                                 "Simplifying " <>
                                 ToString[#[[1]], InputForm] <> " -> ..."];
                     Simplify[#, {r>=0}]) &,
                    eqs]]];

    InfoMessage[InfoFull, "Equations:"];
    parameterRules = Map[(#->Parameter[#])&, parameters];
    eqs = eqs /. parameterRules;
    (* Map[printEq, eqs]; *)
    Scan[InfoMessage[InfoFull, "  " <> ToString@First[#]<>" = ..."] &, eqs];

    singlePointStencil = CalculationPointwiseQ[cleancalc];
    where = GetCalculationWhere[cleancalc];

    (* Check all the function names *)
    functionsPresent =
      FunctionsInCalculation[cleancalc]; (* Not currently used *)

    (* Check that there are no shorthands defined with the same name
       as a grid function *)
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
    knownSymbols = Join[lookupDefault[cleancalc, AllowedSymbols, {}],
                        gfs, shorts, parameters,
                        {t,
                         cctkOriginSpace1, cctkOriginSpace2, cctkOriginSpace3,
                         dx,dy,dz,dt, idx,idy,idz,
                         usejacobian,
                         Pi, E,
                         cctkIdx1, cctkIdx2, cctkIdx3,
                         cctkLbnd1, cctkLbnd2, cctkLbnd3,
                         Symbol["i"], Symbol["j"], Symbol["k"],
                         Symbol["ti"], Symbol["tj"], Symbol["tk"],
                         normal1, normal2, normal3,
                         tangentA1, tangentA2, tangentA3,
                         tangentB1, tangentB2, tangentB3},
                        If[useJacobian, JacobianSymbols[], {}]];

    unknownSymbols = Complement[allSymbols, knownSymbols];

    unknownSymbols /.
    {{} :> True,
     {s_} :> ThrowError["Unknown symbol " <> ToString[s] <>
                        " in calculation " <> GetCalculationName[cleancalc]],
     ss_ :> ThrowError["Unknown symbols " <>
                       StringJoin[Riffle[ToString/@ss,", "]] <>
                       " in calculation " <> GetCalculationName[cleancalc]],
     _ :> ThrowError["Internal error: unrecognised value for unknownSymbols: "<>
                     ToString[unknownSymbols, InputForm]]};

    kernelCall = Switch[
      where,
      Everywhere,
      (* If[TileCalculationQ[cleancalc],
         "TiledLoopOverEverything(cctkGH, " <> bodyFunctionName <> ");\n",
         "LoopOverEverything(cctkGH, " <> bodyFunctionName <> ");\n"], *)
      "LoopOverEverything(cctkGH, " <> bodyFunctionName <> ");\n",
      Interior | InteriorNoSync,
      If[TileCalculationQ[cleancalc] || DGTileCalculationQ[cleancalc],
         "TiledLoopOverInterior(cctkGH, " <> bodyFunctionName <> ");\n",
         "LoopOverInterior(cctkGH, " <> bodyFunctionName <> ");\n"],
      Boundary | BoundaryNoSync,
      "LoopOverBoundary(cctkGH, " <> bodyFunctionName <> ");\n",
      BoundaryWithGhosts,
      "LoopOverBoundaryWithGhosts(cctkGH, " <> bodyFunctionName <> ");\n",
      _,
      ThrowError["Unknown 'Where' entry in calculation " <>
                 functionName <> ": " <> ToString[where]]];

    DGFEDefs =
      If[OptionValue[UseDGFE], DGFEDefinitions[cleancalc, eqs, gfs], {}];
    DGFEInit = If[OptionValue[UseDGFE], DGFEInitialise[cleancalc], {}];
    DGFECallCode =
      If[OptionValue[UseDGFE] && lookupDefault[cleancalc, UseDGFE, False],
         DGFECall[cleancalc],
         {}];

    InfoMessage[InfoFull,"Generating function"];

    {
      DGFEDefs,

      lookup[calcp,BodyFunction]
      [{
        (* OpenCL kernel prologue *)
        (* We could (or probably should) write this into a source file
           of its own *)
        If[OptionValue[UseOpenCL], {OpenCLPrologue[]}, {}],

        Module[
          {kernelCode},
          kernelCode =
          {
            CommentedBlock[
              "Include user-supplied include files",
              Map[IncludeFile,
                  lookupDefault[cleancalc, DeclarationIncludes, {}]]],
            "\n",

            lookup[calcp,InitFDVariables],
            "\n",

            definePreDefinitions[pDefs],
            "\n",

            If[useJacobian, CreateJacobianVariables[], {}],
            "\n",

            If[Cases[{pddefs}, SBPDerivative[_], Infinity] != {},
               CommentedBlock["Compute Summation By Parts derivatives",
                              IncludeFile["sbp_calc_coeffs.h"]],
               {}],

            If[gfs != {},
               {
                 If[lookup[calcp, SimpleCode, False], 
                    simpleEquationLoop,
                    equationLoop]
                 [eqs, cleancalc, gfs, shorts, {}, groups, odeGroups,
                  pddefs, where, addToStencilWidth, opts]},
               {}]
          };

          If[OptionValue[UseOpenCL],
             kernelCode = OpenCLProcessKernel[kernelCode]];
          kernelCode],

        (* OpenCL kernel epilogue *)
        If[OptionValue[UseOpenCL],
           OpenCLEpilogue[cleancalc, imp, functionName], {}]
       }], (* <BodyFunction *)
  
      If[lookup[calcp,CallerFunction],
         DefineCCTKSubroutine
         [functionName,
          FlattenBlock[
            {If[haveCondTextuals,
                Map[ConditionalOnParameterTextual["!(" <> # <> ")",
                                                  "return;\n"] &, condTextuals],
                {}],

             ConditionalOnParameterTextual[
               "verbose > 1",
               "CCTK_VInfo(CCTK_THORNSTRING,\"Entering " <> bodyFunctionName <>
               "\");\n"],

             ConditionalOnParameterTextual[
               "cctk_iteration % " <> functionName <> "_calc_every != " <>
               functionName <> "_calc_offset", "return;\n"],
  
             CheckGroupStorage[GroupsInCalculation[cleancalc, imp],
                               functionName],

             CheckStencil[pddefs, eqs, functionName,
                          OptionValue[ZeroDimensions],
                          DGTileCalculationQ[calc],
                          lookup[{opts}, IntParameters, {}]],

             If[where === Everywhere && !OptionValue[UseDGFE] &&
                !singlePointStencil,
                ThrowError["Calculation "<>functionName<>" uses derivative operators but is computed Everywhere.  Specify Where -> Interior for calculations that use derivative operators."]];
             "\n",

             DGFEInit,
             kernelCall,
             DGFECallCode,

             ConditionalOnParameterTextual[
               "verbose > 1",
               "CCTK_VInfo(CCTK_THORNSTRING,\"Leaving " <> bodyFunctionName <>
               "\");\n"]
            }]],
         (* else *)
         ""]
    }]];

(* Create definitions for the local copies of gridfunctions or arrays *)
DefFn[
  assignLocalFunctions[gs:{_Symbol...}, useVectors:Boolean, dgTile:Boolean,
                       useJacobian:Boolean, nameFunc_] :=
  Module[
    {conds, varPatterns, varsInConds, simpleVars, code},

    (* Conditional access to grid variables *)
    (* Format is {var, cond, else-value} *)
    conds =
    {{"eT" ~~ _ ~~ _,
      "assume_stress_energy_state>=0 ? assume_stress_energy_state : *stress_energy_state",
      ToReal[0.]}}; (* This should be passed as an option *)
    If[useJacobian,
       conds = Append[conds, JacobianConditionalGridFunctions[]]];

    (* Split into conditional and simple grid variables *)
    varPatterns = Map[First, conds];
    varsInConds =
      Map[Function[pattern, Select[gs,StringMatchQ[ToString[#], pattern] &]],
          varPatterns];
    simpleVars = Complement[gs, Flatten[varsInConds]];

    code = {
      (* Simple grid variables *)
      Map[AssignVariableFromExpression[localName[#], GFLocal[#],
                                       True, useVectors, dgTile, True] &,
          simpleVars],
      {
        (* Conditional grid variables *)
        NewlineSeparated@
        MapThread[
          If[Length[#2] > 0,
             {
               DeclareVariables[localName/@#2, DataType[]], "\n",
               Conditional
                 [#1,
                  Table[AssignVariableFromExpression
                        [localName[var], GFLocal[var],
                         False, useVectors, dgTile],
                        {var, #2}],
                  Sequence@@
                  If[#3 =!= None,
                     {Table[AssignVariableFromExpression
                            [localName[var], #3,
                             False (*declare*), False (*useVectors*),
                             False (*dgTile*)],
                            {var, #2}]},
                     {}]]},
             (* else *)
             {}] &,
          {Map[#[[2]]&, conds], varsInConds, Map[#[[3]]&, conds]}]}};
    code]];

Options[equationLoop] = ThornOptions;
DefFn[
  equationLoop[eqs_, cleancalc_, gfs_, shorts_, incs_, groups_, odeGroups_,
               pddefs_, where_, addToStencilWidth_, opts:OptionsPattern[]] :=
  Module[
    {allCode, opCounts,
     useJacobian,
     gridName,
     rhss, lhss,
     orderings, eqsOrdered,
     odeVars,
     gfsInRHS, gfsInLHS, gfsOnlyInRHS, gfsOnlyInLHS, gfsInBoth,
     localGFs, localMap,
     gfsDifferentiated, gfsDifferentiatedAndOnLHS,
     defsWithoutShorts, defsWithShorts,
     eqs2,
     functionName,
     eqsReplaced,
     gridFunctionsFreeQ,
     declare,
     groupedIfs, IfThenGroup,
     noSimplify,
     generateEquationCode,
     groupedIfsArrays,
     calcCode, calcCodeArrays,
     assignLocalGridFunctions, assignLocalArrayFunctions,
     generateTileDefinition,
     generateTileLoad, generateTileStore,
     arraysInRHS, arraysInLHS, arraysOnlyInRHS},

    InfoMessage[InfoFull, "Equation loop"];

    {allCode, opCounts} = Reap[

      useJacobian =
        OptionValue[UseJacobian] && lookupDefault[cleancalc, UseJacobian, True];

      gridName =
        Function[x, FlattenBlock[lookup[cleancalc, GFAccessFunction][x]]];

      rhss = Map[#[[2]] &, eqs];
      lhss = Map[#[[1]] &, eqs];

      orderings = Flatten[Map[pdCanonicalOrdering, pddefs], 1];
      eqsOrdered = eqs //. orderings;

      odeVars = variablesFromGroups[odeGroups, groups];

      gfsInRHS = Union[Cases[rhss, _ ? (MemberQ[gfs,#] &), Infinity]];
      gfsInLHS = Union[Cases[lhss, _ ? (MemberQ[gfs,#] &), Infinity]];
      gfsOnlyInRHS = Complement[gfsInRHS, gfsInLHS];
      gfsOnlyInLHS = Complement[gfsInLHS, gfsInRHS]; 
      gfsInBoth = Intersection[gfsInRHS, gfsInLHS];

      If[OptionValue[ProhibitAssignmentToGridFunctionsRead] &&
         gfsInBoth =!= {},
         ThrowError[
           "The calculation " <> ToString@lookup[cleancalc, Name] <>
           " has the grid functions " <> ToString[gfsInBoth] <>
           " on both the left hand side and the right hand side.  This is" <>
           " not allowed with the option" <>
           " ProhibitAssignmentToGridFunctionsRead -> True."]];

      localGFs = Map[localName, gfs];
      localMap = Map[# -> localName[#] &, gfs];

      gfsDifferentiated =
        Map[First,
            GridFunctionDerivativesInExpression[pddefs, eqsOrdered,
                                                OptionValue[ZeroDimensions]]];
      gfsDifferentiatedAndOnLHS = Intersection[gfsDifferentiated, gfsInLHS];

      If[gfsDifferentiatedAndOnLHS =!= {},
         ThrowError[
           "The calculation " <> ToString@lookup[cleancalc, Name] <> " " <>
           "has both assignments to, and derivatives of, the grid functions " <>
           ToString[gfsDifferentiatedAndOnLHS] <> ".  " <>
           "This is not allowed, as it gives results which are dependent " <>
           "on the ordering of the loop over grid points."]];

      (* Replace the partial derivatives *)
      {defsWithoutShorts, defsWithShorts} =
        splitPDDefsWithShorthands[pddefs, shorts];
      eqs2 = ReplaceDerivatives[defsWithoutShorts, eqsOrdered, True,
                                OptionValue[ZeroDimensions],
                                lookup[cleancalc, MacroPointer]];
      eqs2 = ReplaceDerivatives[defsWithShorts, eqs2, False,
                                OptionValue[ZeroDimensions],
                                lookup[cleancalc, MacroPointer]];

      checkEquationAssignmentOrder[eqs2, shorts];
      functionName = ToString@lookup[cleancalc, Name];

      (* Replace grid functions with their local forms *)
      eqsReplaced = eqs2 /. Join[{g:GFOffset[___]:>g}, localMap];

      gridFunctionsFreeQ[x_] :=
        Module[
          {r},
          (* Print["groups = ", groups]; *)
          (* Print["gridFunctionsFreeQ[", x, "] ="]; *)
          r = GridFunctionsInExpression[x, groups/.localMap] === {};
          (* Print["  ", r]; *)
          r];

      (* Construct a list, corresponding to the list of equations,
         marking those which need their LHS variables declared. We
         declare variables at the same time as assigning to them as it
         gives a performance increase over declaring them separately
         at the start of the loop. The local variables for the grid
         functions which appear in the RHSs have been declared and set
         already, so assignments to these do not generate declarations
         here. *)
      declare =
        Block[{$RecursionLimit=Infinity},
              MarkFirst[First /@ eqsReplaced, Map[localName, gfsInRHS]]];

      (* Replace consecutive IfThen statements with the same condition
         by a single IfThenGroup *)
      groupedIfs =
        Thread[{declare, eqsReplaced}] //.
        {
          {x___, {deca_, a_->IfThen[cond_, at_, af_]},
                 {decb_, b_->IfThen[cond_, bt_, bf_]}, y___} :>
          {x, {{deca, decb},
               IfThenGroup[cond, {a->at, b->bt}, {a->af, b->bf}]}, y} /;
          gridFunctionsFreeQ[cond],
          {x___, {deca_, IfThenGroup[cond_, at_, af_]},
                 {decb_, b_->IfThen[cond_, bt_, bf_]}, y___} :>
          {x, {Join[deca, {decb}],
               IfThenGroup[cond, Join[at, {b->bt}], Join[af, {b->bf}]]}, y},
          {x___, {deca_, IfThenGroup[cond_, at_, af_]},
                 {decb_, IfThenGroup[cond_, bt_, bf_]}, y___} :>
          {x, {Join[deca, decb],
               IfThenGroup[cond, Join[at, bt], Join[af, bf]]}, y}};

      noSimplify = lookupDefault[cleancalc, NoSimplify, False];

      (* Generate actual code strings. Try to declare variables as
         they are assigned, but it is only possible to do this outside
         all if(){} statements. *)
      generateEquationCode[{declare2_, eq2_}] :=
        Module[
          {ret, vars, code, preDeclare},
          InfoMessage[InfoFull,
                      "Generating code for " <> ToString[eq2[[1]], InputForm]];
          Which[
            SameQ[Head[eq2[[2]]], IfThen],
            ret = AssignVariableFromExpression[
              eq2[[1]], eq2[[2]], declare2,
              OptionValue[UseVectors], OptionValue[DGTile],
              noSimplify],
            SameQ[Head[eq2], IfThenGroup],
            vars = eq2[[2,All,1]];
            cond = eq2[[1]];
            preDeclare = Pick[vars, declare2];
            ret = {Map[DeclareVariable[#, DataType[]] &,
                       Complement[Union[preDeclare], localName/@gfsInRHS]],
                   {"\n"},
                   Conditional[
                     GenerateCodeFromExpression[ConditionExpression[cond],
                                                False, False],
                     Riffle[
                       AssignVariableFromExpression[#[[1]], #[[2]], False,
                                                    OptionValue[UseVectors],
                                                    OptionValue[DGTile],
                                                    noSimplify]& /@ eq2[[2]],
                       "\n"],
                     Riffle[
                       AssignVariableFromExpression[#[[1]], #[[2]], False,
                                                    OptionValue[UseVectors],
                                                    OptionValue[DGTile],
                                                    noSimplify]& /@ eq2[[3]],
                       "\n"]]},
            True,
            ret = AssignVariableFromExpression[eq2[[1]], eq2[[2]], declare2,
                                               OptionValue[UseVectors],
                                               OptionValue[DGTile],
                                               noSimplify]];
          ret];

      groupedIfsArrays =
        Select[groupedIfs, MemberQ[Map[localName, odeVars], #[[2]][[1]]] &];
      groupedIfs =
        Select[groupedIfs, !MemberQ[Map[localName, odeVars], #[[2]][[1]]] &];

      calcCode = Riffle[generateEquationCode /@ groupedIfs, "\n"];
      calcCodeArrays = Riffle[generateEquationCode /@ groupedIfsArrays, "\n"];
      InfoMessage[InfoFull, "Finished generating equation code"];

      assignLocalGridFunctions[gs_, useVectors_, dgTile_, useJacobian_] :=
        assignLocalFunctions[gs, useVectors, dgTile, useJacobian, gridName];
      assignLocalArrayFunctions[gs_] :=
        assignLocalFunctions[gs, False, False, False, ArrayName];

      generateTileDefinition[gf_] :=
      "unsigned char "<>ToString[gf]<>"T[tsz] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n";

      generateTileLoad[gf:(_String|_Symbol)] :=
      Module[
        {gfn = ToString[gf],
         jcgfs = JacobianConditionalGridFunctions[],
         decl, load, isjac, jaccond, ishydro, hydrocond, maybeload},
        decl = {"unsigned char "<>gfn<>"T[tsz] CCTK_ATTRIBUTE_ALIGNED(CCTK_REAL_VEC_SIZE * sizeof(CCTK_REAL));\n"};
        load = {"assert("<>gfn<>"!=NULL);\n",
                "load_dg_dim3<dgop_PD>(&((const unsigned char *)"<>gfn<>")[off1], "<>gfn<>"T, dj, dk);\n"};
        isjac = StringMatchQ[gfn, jcgfs[[1]]];
        jaccond = jcgfs[[2]];
        ishydro = StringMatchQ[gfn, "eT" ~~ _ ~~ _];
        hydrocond = ("assume_stress_energy_state>=0 ? "<>
                     "assume_stress_energy_state : *stress_energy_state");
        maybeload = Which[isjac,
                          {"if ("<>jaccond<>") {\n",
                           IndentBlock[load],
                           "}\n"},
                          ishydro,
                          {"if ("<>hydrocond<>") {\n",
                           IndentBlock[load],
                           "}\n"},
                          True,
                          load];
        {decl, maybeload}];

      generateTileStore[gf:(_String|_Symbol)] :=
      {
        "assert("<>ToString[gf]<>"!=NULL);\n",
        "store_dg_dim3<dgop_PD>(&((unsigned char *)"<>ToString[gf]<>")[off1], "<>ToString[gf]<>"T, dj, dk);\n"
      };

      (* separate grid and array variables *)
      arraysInRHS = Intersection[odeVars, gfsInRHS];
      arraysInLHS = Intersection[odeVars, gfsInLHS];
      arraysOnlyInRHS = Complement[arraysInRHS, arraysInLHS];

      gfsInRHS = Complement[gfsInRHS, odeVars];
      gfsInLHS = Complement[gfsInLHS, odeVars];
      gfsOnlyInRHS = Complement[gfsInRHS, gfsInLHS];

      {
        CommentedBlock[
          "Assign local copies of arrays functions",
          assignLocalArrayFunctions[arraysInRHS]],

        CommentedBlock[
          "Calculate temporaries and arrays functions",
          calcCodeArrays],

        CommentedBlock[
          "Copy local copies back to grid functions",
          Map[AssignVariableInLoop[ArrayName[#], localName[#]] &, arraysInLHS]],

        If[DGTileCalculationQ[cleancalc],
           CommentedBlock[
             "Load tiles",
             generateTileLoad /@ gfsInRHS],
           {}],

        If[DGTileCalculationQ[cleancalc],
           CommentedBlock[
             "Calculate derivatives into tiles",
             PrecomputeTiledDerivatives[defsWithoutShorts, eqsOrdered,
                                        OptionValue[ZeroDimensions]]],
           {}],

        If[DGTileCalculationQ[cleancalc],
           CommentedBlock[
             "Declare result tiles",
             generateTileDefinition /@ gfsOnlyInLHS],
           {}],

        lookup[cleancalc, LoopFunction][
          {
            (* TODO: Only make local copies for variables that are
               actually used later on; see e.g. variablesReadInCalc
               for how to make the distinction *)
            CommentedBlock[
              "Assign local copies of grid functions",
              assignLocalGridFunctions[gfsInRHS,
                                       OptionValue[UseVectors],
                                       DGTileCalculationQ[cleancalc],
                                       useJacobian]],

            CommentedBlock[
              "Include user supplied include files",
              Map[IncludeFile, incs]],

            CommentedBlock[
              "Precompute derivatives",
              PrecomputeDerivatives[defsWithoutShorts, eqsOrdered, 
                                    lookup[{opts}, IntParameters, {}],
                                    OptionValue[ZeroDimensions],
                                    lookup[cleancalc, MacroPointer],
                                    DGTileCalculationQ[cleancalc]]],

            CommentedBlock[
              "Calculate temporaries and grid functions",
              calcCode],

            If[debugInLoop,
               Map[InfoVariable[#[[1]]] &, (eqs2 /. localMap)],
               ""],

            localsToGridFunctions[gfsInLHS, gridName,
                                  Which[DGTileCalculationQ[cleancalc], "DGTile",
                                        OptionValue[UseOpenCL], "OpenCL",
                                        OptionValue[UseVectors], "Vectors",
                                        True, "Default"]],

            If[debugInLoop,
               Map[InfoVariable[gridName[#]] &, gfsInLHS],
               ""]
          },
          opts],                (* LoopFunction *)

        If[DGTileCalculationQ[cleancalc],
           CommentedBlock[
             "Store tiles",
             generateTileStore /@ gfsInLHS],
           {}]

          },

      CountOperations];         (* Reap *)
  
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
               AssignVariableFromExpression[FlattenBlock@gridName[#[[1]]], #[[2]], False, False, False, True] &, eqs2]]]
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
    localsToGridFunctions2[gfsInLHS, gridName, method]]];

DefFn[
  localsToGridFunctions2[gfsInLHS_List, gridName_, "OpenCL"] :=
  OpenCLLocalsToGridFunctions[gridName /@ gfsInLHS, localName /@ gfsInLHS]];

DefFn[
  localsToGridFunctions2[gfsInLHS_List, gridName_, "Vectors"] :=
  VectorisationLocalsToGridFunctions[gridName /@ gfsInLHS,
                                     localName /@ gfsInLHS]];

DefFn[
  localsToGridFunctions2[gfsInLHS_List, gridName_, "DGTile"] :=
  MapThread[
    Function[
      {gf,gfL},
      "vec_store(getelt("<>ToString[gf]<>"T, off), "<>ToString[gfL]<>");\n"],
    {gfsInLHS, localName /@ gfsInLHS}]];

DefFn[
  localsToGridFunctions2[gfsInLHS_List, gridName_, "Default"] :=
  MapThread[AssignVariableInLoop,
            {gridName /@ gfsInLHS, localName /@ gfsInLHS}]];

End[];

EndPackage[];
