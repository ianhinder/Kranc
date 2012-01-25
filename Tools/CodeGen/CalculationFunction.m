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

BeginPackage["CalculationFunction`", {"CodeGenCactus`", "CodeGenC`", "CodeGen`",
  "MapLookup`", "KrancGroups`", "Differencing`", "Errors`",
  "Helpers`", "Kranc`", "Optimize`", "Jacobian`", "Profile`"}];

CreateCalculationFunction::usage = "";
VerifyCalculation::usage = "";
calculationSymbols::usage = "";
GridFunctionsInExpression;

Begin["`Private`"];

(* This flag determines whether you want to generate debugging code to
   do CCTK_INFO on the variables as they are translated.  This can
   help find problems in the construction of the translator maps. *)
debugInLoop = False;

(* --------------------------------------------------------------------------
   General Utility Functions (could be moved outside this package)
   -------------------------------------------------------------------------- *)

(* Take a string s and break it into separate lines using l as a guide
   to the line length.  If a word (sequence of non-whitespace
   characters) is longer than l, do not break it.  Add two spaces of
   indentation after each line break (this will push the line length
   over l). Algorithm essentially taken from
   http://en.wikipedia.org/wiki/Word_wrap *)
lineBreak[s_, l_] :=
  Module[{spaceLeft, words, word, i, lineWidth = l, spaceWidth = 1,
    len},
   spaceLeft = l;
   words = StringSplit[s];
   Do[
    word = words[[i]];
    len = StringLength[word];
    If[len > spaceLeft,
     words[[i]] = "\n  " <> word;
     spaceLeft = lineWidth - len,
     spaceLeft = spaceLeft - (len + spaceWidth)], {i, 1, Length[words]}];
   Return[StringJoin[Riffle[words, " "]]]];

(* Given an input list l, return a list L such that L[[i]] is True
   only if l[[i]] is the first occurrence of l[[i]] in l and is not in
   the list "already" *)
markFirst[l_List, already_List] :=
  If[l =!= {},
    {!MemberQ[already, First[l]]} ~Join~ markFirst[Rest[l], already ~Join~ {First[l]}],
    {}];

VerifyListContent[l_, type_, while_] :=
  Module[{types},
    If[!(Head[l] === List),
      ThrowError["Expecting a list of ", type,
        " objects, but found the following, which is not a List object.  Error occured ", while, l]];
    types = Union[Map[Head, l]];
    If [!(types === {type}) && !(types === {}),
      ThrowError["Expecting a list of ", type ,
        " objects, but found the following types of object: ",
      ToString[types], " in ", l, while]]];

(* --------------------------------------------------------------------------
   Calculations
   -------------------------------------------------------------------------- *)

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

calculationSymbols[calc_] :=
  Module[{allAtoms},
    allAtoms = Union[Level[lookup[calc, Equations], {-1}]];
    Cases[allAtoms, x_Symbol]];

calculationSymbolsLHS[calc_] :=
  Module[{allAtoms},
    allAtoms = Union[Map[First, Flatten@lookup[calc, Equations] ]];
    Cases[allAtoms, x_Symbol]];

calculationSymbolsRHS[calc_] :=
  Module[{allAtoms},
    allAtoms = Union[Map[Last, Flatten@{lookup[calc, Equations],
      lookup[calc,PartialDerivatives]} ]];
    allAtoms = Union[Level[allAtoms, {-1}]];
    Cases[allAtoms, x_Symbol]];

(* Return all the functions used in a calculation *)
(* Not currently used *)
functionsInCalculation[calc_] :=
  Module[{eqs, x},
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
        ToString[calc], " while checking the calculation called ", ToString[calcName]]]];

(* Remove equations in the calculation which assign to shorthands
   which are never used. Do not modify the Shorthands entry. An unused
   shorthand might be missed if the order of assignments is
   pathalogical enough (e.g. {s1 -> s2, s2 -> s1} would not be
   removed). *)
removeUnusedShorthands[calc_] :=
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
      removeUnusedShorthands[newCalc],
      newCalc]];

(* Return all the groups that are used in a given calculation *)
groupsInCalculation[calc_, imp_] :=
  Module[{groups,gfs,eqs,gfsUsed, groupNames},
    groups = lookup[calc, Groups];
    gfs = allGroupVariables[groups];
    eqs = lookup[calc, Equations];
    gfsUsed = Union[Cases[eqs, _ ? (MemberQ[gfs,#] &), Infinity]];
    groupNames = containingGroups[gfsUsed, groups];
    Map[qualifyGroupName[#, imp] &, groupNames]];

CheckGroupStorage[groupNames_, calcName_] :=
  Module[{ignoreGroups, groupsNames2},
    ignoreGroups = {"TmunuBase::stress_energy_scalar", "TmunuBase::stress_energy_vector",
      "TmunuBase::stress_energy_tensor"};
    groupNames2 = Select[groupNames, !MemberQ[ignoreGroups, #] &];
    {"\nconst char *groups[] = {",
    Riffle[Map[Quote,groupNames2], ","],
    "};\n",
    "GenericFD_AssertGroupStorage(cctkGH, ", Quote[calcName],", ", Length[groupNames2], ", groups);\n"}];

(* --------------------------------------------------------------------------
   Variables
   -------------------------------------------------------------------------- *)

removeRHS[x_] :=
  Module[{string = ToString[x]},
    If[StringMatchQ[string, "*rhs"],
       ToExpression@StringDrop[string, -3],
       x]];

(* Take a grid function name and return a name suitable for use in a local
   computation *)
localName[x_] :=
  ToString[x] <> "L";

(* --------------------------------------------------------------------------
   Predefinitions
   -------------------------------------------------------------------------- *)

definePreDefinitions[pDefs_] :=
  CommentedBlock["Initialize predefined quantities",
    Map[DeclareAssignVariable[DataType[], #[[1]], #[[2]]] &, pDefs]];

(* --------------------------------------------------------------------------
   Equations
   -------------------------------------------------------------------------- *)

equationUsesShorthand[eq_, shorthand_] :=
  Length[Cases[{Last[eq]}, shorthand, Infinity]] != 0;

(* Check that the given list of equations assigns things in the
   correct order.  Specifically, shorthands must not be used before
   they are assigned.  *)
checkEquationAssignmentOrder[eqs_, shorthands_] :=
  Module[{},
    Map[checkShorthandAssignmentOrder[eqs,#] &, shorthands]];

printEq[eq_] :=
  Module[{lhs, rhs, rhsSplit, split, rhsString},
    lhs = First@eq;
    rhs = Last@eq;
    split[ x_ + y___] := { x, " + ..."};
    split[-x_ + y___] := {-x, " + ..."};
    split[ x_       ] := { x, ""};
    rhsSplit = split[Expand@ReplacePowers[rhs,False]];
    rhsString = ToString@CForm[rhsSplit[[1]]] <> rhsSplit[[2]];
    InfoMessage[InfoFull, " " <> ToString@lhs <> " -> " <> rhsString]];

(* Collect and simplify terms *)
simpCollect[collectList_, eqrhs_, localvar_, debug_] :=
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

    all];

(* Return a CodeGen block which assigns dest by evaluating expr *)
assignVariableFromExpression[dest_, expr_, declare_, vectorise_, noSimplify:Boolean : False] :=
  Module[{tSym, type, cleanExpr, code},
    tSym = Unique[];
    type = If[StringMatchQ[ToString[dest], "dir*"], "ptrdiff_t", DataType[]];
    cleanExpr = ReplacePowers[expr, vectorise, noSimplify] /. Kranc`t -> tSym;

    If[SOURCELANGUAGE == "C",
      code = If[declare, type <> " ", ""] <> ToString[dest] <> " = " <>
        ToString[cleanExpr, CForm,         PageWidth -> Infinity] <> ";\n",
      code = ToString@dest <> ".eq." <> ToString[cleanExpr, FortranForm, PageWidth -> 120]
        <> "\n"];

    If[SOURCELANGUAGE != "C",
      code = StringReplace[code, "\n  "      -> " &\n"];
      code = StringReplace[code, "   -  "    -> " &  "];
      code = StringReplace[code, ".eq."      -> " = "];
      code = StringReplace[code, "=        " -> "="];
      code = StringReplace[code, "\\"        -> ""];
      code = StringReplace[code, "(index)"   -> "(i,j,k)"]];

    code = lineBreak[code, 70] <> "\n";
    code = StringReplace[code, "normal1"     -> "normal[0]"];
    code = StringReplace[code, "normal2"     -> "normal[1]"];
    code = StringReplace[code, "normal3"     -> "normal[2]"];
    code = StringReplace[code, "BesselJ"-> "gsl_sf_bessel_Jn"];
    code = StringReplace[code, ToString@tSym -> "cctk_time"];
    code = StringReplace[code, "\"" -> ""];

    {code}];

(* This and assignVariableFromExpression should be combined *)
generateCodeFromExpression[expr_, vectorise_, noSimplify:Boolean : False] :=
  Module[{tSym, type, cleanExpr, code},
    tSym = Unique[];
    cleanExpr = ReplacePowers[expr, vectorise, noSimplify] /. Kranc`t -> tSym;

    If[SOURCELANGUAGE == "C",
      code =
        ToString[cleanExpr, CForm,         PageWidth -> Infinity],
      code = ToString[cleanExpr, FortranForm, PageWidth -> 120]];

    If[SOURCELANGUAGE != "C",
      code = StringReplace[code, "\n  "      -> " &\n"];
      code = StringReplace[code, "   -  "    -> " &  "];
      code = StringReplace[code, ".eq."      -> " = "];
      code = StringReplace[code, "=        " -> "="];
      code = StringReplace[code, "\\"        -> ""];
      code = StringReplace[code, "(index)"   -> "(i,j,k)"]];

    code = StringReplace[code, "normal1"     -> "normal[0]"];
    code = StringReplace[code, "normal2"     -> "normal[1]"];
    code = StringReplace[code, "normal3"     -> "normal[2]"];
    code = StringReplace[code, "BesselJ"-> "gsl_sf_bessel_Jn"];
    code = StringReplace[code, ToString@tSym -> "cctk_time"];
    code = StringReplace[code, "\"" -> ""];

    {code}];

(* --------------------------------------------------------------------------
   Shorthands
   -------------------------------------------------------------------------- *)

defContainsShorthand[def_, shorthands_] :=
Module[{allAtoms, c},
  allAtoms = Union[Level[def, {-1}]];
  c = Intersection[shorthands, allAtoms];
  c != {}];

checkShorthandAssignmentOrder[eqs_, shorthand_] :=
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
      "is used before it is defined in this equation list", eqs]]];

(* --------------------------------------------------------------------------
   Partial derivatives
   -------------------------------------------------------------------------- *)

(* Split the list of partial derivative definitions into those
   containing shorthands, and those that do not.  *)
splitPDDefsWithShorthands[pddefs_, shorthands_] :=
  Module[{defsWithShorts, defsWithoutShorts},
    defsWithShorts = Select[pddefs, defContainsShorthand[#, shorthands] &];
    defsWithoutShorts = Select[pddefs, ! defContainsShorthand[#, shorthands] &];
    Return[{defsWithoutShorts, defsWithShorts}]];

pdCanonicalOrdering[name_[inds___] -> x_] :=
  Module[{is},
    is = {inds};
    If[Length[is] == 2,
      Return[{name[f_,2,1] -> name[f,1,2],
              name[f_,3,1] -> name[f,1,3],
              name[f_,3,2] -> name[f,2,3]}],
      {}]];

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
          kernelCall,debug,imp},

  debug = OptionValue[Debug];
  imp = lookup[calcp, Implementation];

  functionName = ToString@lookup[calcp, Name];
  bodyFunctionName = functionName <> "_Body";

  InfoMessage[Terse, "Creating calculation function: " <> functionName];

  InfoMessage[InfoFull, "Calculation sets "<>ToString[Map[First,lookup[calcp, Equations]]]];

  calc = If[OptionValue[UseJacobian], InsertJacobian[calcp, opts], calcp];

  cleancalc = removeUnusedShorthands[calc];
  If[OptionValue[CSE],
    cleancalc = EliminateCommonSubexpressions[cleancalc, opts]];

  shorts = lookupDefault[cleancalc, Shorthands, {}];

  eqs    = lookup[cleancalc, Equations];
  parameters = lookupDefault[cleancalc, Parameters, {}];
  groups = lookup[cleancalc, Groups];
  odeGroups = lookupDefault[cleancalc, ODEGroups, {}];
  If[OptionValue[UseJacobian], groups = Join[groups, JacobianGroups[]]];
  pddefs = lookupDefault[cleancalc, PartialDerivatives, {}];
  where = lookupDefault[cleancalc, Where, Everywhere];
  addToStencilWidth = lookupDefault[cleancalc, AddToStencilWidth, 0];
  pDefs = lookup[cleancalc, PreDefinitions];
  haveCondTextuals = mapContains[cleancalc, ConditionalOnTextuals];

  SetDataType[If[OptionValue[UseVectors],"CCTK_REAL_VEC", "CCTK_REAL"]];

  VerifyCalculation[cleancalc];

  gfs = allGroupVariables[groups];

  InfoMessage[InfoFull, " ", Length@shorts, " shorthands"];
  InfoMessage[InfoFull, " ", Length@gfs, " grid functions"];
  InfoMessage[InfoFull, " ", Length@groups, " groups"];

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

  (* Check all the function names *)
  functionsPresent = functionsInCalculation[cleancalc]; (* Not currently used *)

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
  allSymbols = calculationSymbols[cleancalc];
  knownSymbols = Join[lookupDefault[cleancalc, AllowedSymbols, {}], gfs, shorts, parameters,
    {dx,dy,dz,dt,idx,idy,idz,t, Pi, E, Symbol["i"], Symbol["j"], Symbol["k"], normal1, normal2,
    normal3, tangentA1, tangentA2, tangentA3, tangentB1, tangentB2, tangentB3},
    If[OptionValue[UseJacobian], JacobianSymbols[], {}]];

  unknownSymbols = Complement[allSymbols, knownSymbols];

  If[unknownSymbols != {},
     ThrowError["Unknown symbols in calculation.  Symbols are:", unknownSymbols,
       "Calculation is:", cleancalc]];

  kernelCall = Switch[where,
    Everywhere,
      "GenericFD_LoopOverEverything(cctkGH, &" <> bodyFunctionName <> ");\n",
    Interior,
      "GenericFD_LoopOverInterior(cctkGH, &" <> bodyFunctionName <> ");\n",
    InteriorNoSync,
      "GenericFD_LoopOverInterior(cctkGH, &" <> bodyFunctionName <> ");\n",
    Boundary,
      "GenericFD_LoopOverBoundary(cctkGH, &" <> bodyFunctionName <> ");\n",
    BoundaryWithGhosts,
      "GenericFD_LoopOverBoundaryWithGhosts(cctkGH, &" <> bodyFunctionName <> ");\n",
    _,
      ThrowError["Unknown 'Where' entry in calculation " <>
        functionName <> ": " <> ToString[where]]];

  InfoMessage[InfoFull,"Generating function"];
  {
    lookup[calcp,BodyFunction][{
    (* OpenCL kernel prologue *)
    (* We could (or probably should) write this into a source file of its own *)
    If[OptionValue[UseOpenCL],
       {
         "char const * const source =\n"
       },
       {
       }],

    If[OptionValue[UseOpenCL], Stringify, Identity][{

    DeclareFDVariables[],

    CommentedBlock["Include user-supplied include files",
      Map[IncludeFile, lookupDefault[cleancalc, DeclarationIncludes, {}]]],

    InitialiseFDVariables[OptionValue[UseVectors]],
    definePreDefinitions[pDefs],

    If[OptionValue[UseJacobian], CreateJacobianVariables[], {}],

    If[Cases[{pddefs}, SBPDerivative[_], Infinity] != {},
      CommentedBlock["Compute Summation By Parts derivatives",
        IncludeFile["sbp_calc_coeffs.h"]],
      {}],

    If[gfs != {},
      {
	eqLoop = equationLoop[eqs, cleancalc, gfs, shorts, {}, groups, odeGroups,
          pddefs, where, addToStencilWidth, opts]},
      {}]

    }],

    (* OpenCL kernel epologue *)
    If[OptionValue[UseOpenCL],
       {
         ";\n\n",
         Module[
           {ignoreGroups, groupsNames, groupNameList},
           ignoreGroups = {"TmunuBase::stress_energy_scalar",
                           "TmunuBase::stress_energy_vector",
                           "TmunuBase::stress_energy_tensor"};
           groupNames = groupsInCalculation[cleancalc, imp];
           groupNames = Select[groupNames, !MemberQ[ignoreGroups, #] &];
           {
             "char const * const groups[] = {",
               Riffle[Join[Map[Quote, groupNames], {"NULL"}], ","],
               "};\n\n"
           }
         ],
         "static struct OpenCLKernel * kernel = NULL;\n",
         "char const * const sources[] = {differencing, source, NULL};\n",
         "OpenCLRunTime_CallKernel (cctkGH, CCTK_THORNSTRING, \"" <> functionName <> "\",\n",
         "                          sources, groups, NULL, NULL, NULL, -1,\n",
         "                          imin, imax, &kernel);\n\n"
       },
       {
       }]
    }],
  
    If[lookup[calcp,CallerFunction],
      DefineCCTKSubroutine[functionName,
      FlattenBlock[{
        ConditionalOnParameterTextual["verbose > 1",
          "CCTK_VInfo(CCTK_THORNSTRING,\"Entering " <> bodyFunctionName <> "\");\n"],

        ConditionalOnParameterTextual["cctk_iteration % " <> functionName <> "_calc_every != " <>
          functionName <> "_calc_offset", "return;\n"],
  
        CheckGroupStorage[groupsInCalculation[cleancalc, imp], functionName],
        "\n",

        CheckStencil[pddefs, eqs, functionName, OptionValue[ZeroDimensions],
                     lookup[{opts}, IntParameters, {}]],

        Module[
          {stencilSize = StencilSize[pddefs, eqs, functionName, OptionValue[ZeroDimensions],
                                     lookup[{opts}, IntParameters, {}]]},
          If[!VectorQ[stencilSize],
             stencilSize = MapThread[Max,Map[Last,stencilSize[[2]]]]];

          If[where === Everywhere && MatchQ[stencilSize, {0,0,0}] =!= True,
             ThrowError["Calculation "<>functionName<>" uses derivative operators but is computed Everywhere.  Specify Where -> Interior for calculations that use derivative operators."]]];
        "\n",
  
        If[haveCondTextuals, Map[ConditionalOnParameterTextual["!(" <> # <> ")", "return;\n"] &,condTextuals], {}],

        kernelCall,

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
          loopFunction, gfsInBoth, gfsDifferentiated,
          gfsDifferentiatedAndOnLHS, declare, eqsReplaced,
          arraysInRHS, arraysInLHS, arraysOnlyInRHS, odeVars,
          generateEquationCode, groupedIfs, IfThenGroup, noSimplify},

    InfoMessage[InfoFull, "Equation loop"];

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
      Throw["The calculation " <> ToString@lookup[cleancalc, Name] <>
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
      Throw["The calculation " <> ToString@lookup[cleancalc, Name] <>
        " has both assignments to, and derivatives of, the grid functions " <>
        ToString[gfsDifferentiatedAndOnLHS] <>
        ".  This is not allowed, as it gives results which are dependent" <>
        " on the ordering of the loop over grid points."]];

    (* Replace the partial derivatives *)
    {defsWithoutShorts, defsWithShorts} = splitPDDefsWithShorthands[pddefs, shorts];
    eqs2 = ReplaceDerivatives[defsWithoutShorts, eqsOrdered, True,
                              OptionValue[ZeroDimensions]];
    eqs2 = ReplaceDerivatives[defsWithShorts, eqs2, False, OptionValue[ZeroDimensions]];

    checkEquationAssignmentOrder[eqs2, shorts];
    functionName = ToString@lookup[cleancalc, Name];

    (* Replace grid functions with their local forms *)
    eqsReplaced = eqs2 /. localMap;

    (* Construct a list, corresponding to the list of equations,
       marking those which need their LHS variables declared.  We
       declare variables at the same time as assigning to them as it
       gives a performance increase over declaring them separately at
       the start of the loop.  The local variables for the grid
       functions which appear in the RHSs have been declared and set
       already (DeclareMaybeAssignVariableInLoop below), so assignments
       to these do not generate declarations here. *)
    declare = Block[{$RecursionLimit=Infinity},markFirst[First /@ eqsReplaced, Map[localName, gfsInRHS]]];

    (* Replace consecutive IfThen statements with the same condition by a single IfThenGroup *)
    groupedIfs = Thread[{declare, eqsReplaced}] //.
      {{x___, {deca_, a_->IfThen[cond_, at_, af_]}, {decb_, b_->IfThen[cond_, bt_, bf_]}, y___} :>
         {x, {{deca, decb}, IfThenGroup[cond, {a->at, b->bt}, {a->af, b->bf}]}, y},
       {x___, {deca_, IfThenGroup[cond_, at_, af_]}, {decb_, b_->IfThen[cond_, bt_, bf_]}, y___} :>
         {x, {Join[deca, {decb}], IfThenGroup[cond, Join[at, {b->bt}], Join[af, {b->bf}]]}, y},
       {x___, {deca_, IfThenGroup[cond_, at_, af_]}, {decb_, IfThenGroup[cond_, bt_, bf_]}, y___} :>
         {x, {Join[deca, decb], IfThenGroup[cond, Join[at, bt], Join[af, bf]]}, y}};

    noSimplify = lookupDefault[cleancalc, NoSimplify, False];

    (* Generate actual code strings. Try to declare variables as they are assigned, but
       it is only possible to do this outside all if(){} statements. *)
    generateEquationCode[{declare2_, eq2_}] :=
      Module[{ret, vars, preDeclare, cond, vectorize},
        vectorize = OptionValue[UseVectors];
        InfoMessage[InfoFull, "Generating code for " <> ToString[eq2[[1]], InputForm]];
        Which[
        SameQ[Head[eq2[[2]]], IfThen],
          ret = assignVariableFromExpression[eq2[[1]],
            eq2[[2]] /. IfThen[cond_, x__]:> IfThen[Scalar[cond], x], declare2, vectorize, noSimplify];,
        SameQ[Head[eq2], IfThenGroup],
          vars = eq2[[2,All,1]];
          cond = eq2[[1]];
          preDeclare = Pick[vars, declare2];
          ret = {Map[DeclareVariableNoInit[#, DataType[]] &, Complement[Union[preDeclare], localName/@gfsInRHS]], {"\n"},
                 Conditional[generateCodeFromExpression[Scalar[cond], False],
                  Riffle[assignVariableFromExpression[#[[1]], #[[2]], False, vectorize, noSimplify]& /@ eq2[[2]], "\n"],
                  Riffle[assignVariableFromExpression[#[[1]], #[[2]], False, vectorize, noSimplify]& /@ eq2[[3]], "\n"]]};,
        True,
          ret = assignVariableFromExpression[eq2[[1]], eq2[[2]], declare2, OptionValue[UseVectors], noSimplify];
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
          {{"eT" ~~ _ ~~ _, "*stress_energy_state", "ToReal(0.0)"}}; (* This should be passed as an option *)
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

    assignLocalGridFunctions[gs_, useVectors_, useJacobian_] := assignLocalFunctions[gs, useVectors, useJacobian, GridName];
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
      (* DeclareDerivatives[defsWithoutShorts, eqsOrdered], *)

      CommentedBlock["Assign local copies of grid functions",
        assignLocalGridFunctions[gfsInRHS, OptionValue[UseVectors], OptionValue[UseJacobian]]],

      CommentedBlock["Include user supplied include files",
        Map[IncludeFile, incs]],

      CommentedBlock["Precompute derivatives",
        PrecomputeDerivatives[defsWithoutShorts, eqsOrdered, 
                              lookup[{opts}, IntParameters, {}],
                              OptionValue[ZeroDimensions]]],

      CommentedBlock["Calculate temporaries and grid functions", calcCode],

      If[debugInLoop,
        Map[InfoVariable[#[[1]]] &, (eqs2 /. localMap)],
        ""],

      Which[OptionValue[UseOpenCL],
              CommentedBlock["Copy local copies back to grid functions",
                Map[StorePartialVariableInLoop[GridName[#], localName[#]] &, gfsInLHS]],
            OptionValue[UseVectors],
              {
                CommentedBlock["If necessary, store only partial vectors after the first iteration",
                  ConditionalOnParameterTextual["CCTK_REAL_VEC_SIZE > 2 && CCTK_BUILTIN_EXPECT(i < lc_imin && i+CCTK_REAL_VEC_SIZE > lc_imax, 0)",
                  {
                    DeclareAssignVariable["ptrdiff_t", "elt_count_lo", "lc_imin-i"],
                    DeclareAssignVariable["ptrdiff_t", "elt_count_hi", "lc_imax-i"],
                    Map[StoreMiddlePartialVariableInLoop[GridName[#], localName[#], "elt_count_lo", "elt_count_hi"] &, gfsInLHS],
                    "break;\n"
                  }]],
                CommentedBlock["If necessary, store only partial vectors after the first iteration",
                  ConditionalOnParameterTextual["CCTK_REAL_VEC_SIZE > 1 && CCTK_BUILTIN_EXPECT(i < lc_imin, 0)",
                  {
                    DeclareAssignVariable["ptrdiff_t", "elt_count", "lc_imin-i"],
                    Map[StoreHighPartialVariableInLoop[GridName[#], localName[#], "elt_count"] &, gfsInLHS],
                    "continue;\n"
                  }]],
                CommentedBlock["If necessary, store only partial vectors after the last iteration",
                  ConditionalOnParameterTextual["CCTK_REAL_VEC_SIZE > 1 && CCTK_BUILTIN_EXPECT(i+CCTK_REAL_VEC_SIZE > lc_imax, 0)",
                  {
                    DeclareAssignVariable["ptrdiff_t", "elt_count", "lc_imax-i"],
                    Map[StoreLowPartialVariableInLoop[GridName[#], localName[#], "elt_count"] &, gfsInLHS],
                    "break;\n"
                  }]],
                Map[StoreVariableInLoop[GridName[#], localName[#]] &, gfsInLHS]
              },
            True,
              CommentedBlock["Copy local copies back to grid functions",
                Map[AssignVariableInLoop[GridName[#], localName[#]] &, gfsInLHS]]],

      If[debugInLoop, Map[InfoVariable[GridName[#]] &, gfsInLHS], ""]}, opts]}]];

(* Unsorted *)

GridFunctionsInExpression[x_, groups_] :=
  Union[Cases[x, _ ? (MemberQ[allGroupVariables[groups],#] &), Infinity]];

End[];

EndPackage[];
