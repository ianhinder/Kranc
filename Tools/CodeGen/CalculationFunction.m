
(* $Id$ *)

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
    along with Foobar; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

BeginPackage["sym`"];

{GridFunctions, Shorthands, Equations, t, DeclarationIncludes,
LoopPreIncludes, GroupImplementations, DerivativeDiscretizations,Dplus1, Dplus2, Dplus3}

{INV, SQR, CUB, QAD, dot, pow, exp} 

EndPackage[];

BeginPackage["CalculationFunction`", {"CodeGen`", "sym`", "MapLookup`", "KrancGroups`"}];

(* This is the only externally callable function *)
CreateCalculationFunction::usage = "";

Begin["`Private`"];

(* -------------------------------------------------------------------------- 
   General Utility Functions
   -------------------------------------------------------------------------- *)

(* Take an expression x and replace occurrences of Powers with the C
macros SQR, CUB, QAD *)
replacePowers[x_] :=
  Module[{rhs},
    rhs = x   /. Power[xx_, 2] -> SQR[xx];
    rhs = rhs /. Power[xx_, 3] -> CUB[xx];
    rhs = rhs /. Power[xx_, 4] -> QAD[xx];
    rhs = rhs /. Power[xx_, -1] -> INV[xx];

    If[SOURCELANGUAGE == "C",
           Module[{},
             rhs = rhs /. Power[E, power_] -> exp[power];
             rhs = rhs /. Power[xx_, power_] -> pow[xx, power]],

           rhs = rhs /. Power[xx_, power_] -> xx^power
       ];
    rhs
    ];

(* remove the 'rhs' at the end of a symbol or string *)
removeRHS[x_] := Module[{string = ToString[x]},
      
      If[StringMatchQ[string, "*rhs"],
           ToExpression@StringDrop[string, -3],
           x ]
      ];

(* collect and simplify terms *)
simpCollect[collectList_, eqrhs_, localvar_, debug_] :=
  Module[{rhs, collectCoeff, all, localCollectList},

      If[debug,
        Print[];
        Print[localvar];
      ];

      rhs = eqrhs;

      rhs = rhs /. Abs[MathTensor`Detg] -> MathTensor`Detg;
      If[debug, Print["ByteCount[rhs]: ", ByteCount@rhs];];

      localCollectList = collectList /. VAR :> removeRHS@localvar;

      collectCoeff = Collect[rhs, localCollectList];
      If[debug, Print["ByteCount[terms collected]: ", ByteCount@collectCoeff];];

      all = Collect[rhs, localCollectList, Simplify];
      If[debug, Print["ByteCount[simplified rhs]: ", ByteCount@all];];

      all
      ];


(* Take a grid function name and return a name suitable for use in a local
   computation *)
localName[x_] := ToExpression[ToString[x] <> "L"];

(* Given a map (i.e. a list of rules { a -> A, b -> B, ... } return the
   inverse map { A -> a, B -> b, ...} *) 
invertMap[m_] := Map[#[[2]] -> #[[1]] &, m];

(* A convenient list of the derivative symbols *)
derivativeHeads = {D1, D2, D3, 
                          D11,
                          D21, D22,
                          D31, D32, D33, Dplus1, Dplus2, Dplus3};

(* Take an expression containing derivatives (i.e. D1[<stuff> etc) and
   "hide" all the derivatives and their contents by replacing them
   with unique new symbols.  So D1[g11] would become $NEWSYMBOL1 or
   something.  Return a list whose first element is the replaced
   expression and whose second element is a map (list of rules) which
   can be used to return the symbols to the derivatives *)
hideDerivatives[x_] :=
  Module[{derivatives, unhide, hide},
    derivatives = Union[Cases[x, _ ? (MemberQ[derivativeHeads, #] &)[__],Infinity]];
    Print["expression == ", x];
    Print["derivatives == ", derivatives];
    unhide = Map[Unique[] -> # &, derivatives];
    hide = invertMap[unhide];
    Print["hide map == ", hide];
(*    Print["Expression with derivatives hidden: ", x /. hide]; *)
    {x /. hide, unhide}];

(* Apply the map (list of rules) to the expression x, but avoid replacing
   anything in a derivative (D1[<stuff>]) by first "hiding" the derivatives. *)
replaceWithDerivativesHidden[x_, map_] :=
  Module[{hide = hideDerivatives[x], newExpr, unhide},
    newExpr = hide[[1]]; (* The expression with the derivatives hidden *)
    unhide = hide[[2]]; (* The map to unhide the derivatives *)
    (newExpr /. map) /. unhide];

(* Change D21[g11] into D21[g11,i,j,k] (for example)*)
replaceDerivatives[x_, derivRules_] :=
  Module[{},
    replaceStandard = (d_ ? (MemberQ[derivativeHeads, #] &)[f_] -> d[f,Symbol["i"],Symbol["j"],Symbol["k"]]);

    replaceCustom = Flatten[derivRules,1];
(*    Print["derivRules == ", derivRules//FullForm];
    Print["replaceCustom == ", replaceCustom];*)

(*    If[Length[derivRules] != 0,
      Print["Before replace: ", x];
      Print["After replace: ",x /. replaceCustom]];*)

    x /. replaceStandard];

(* Return a CodeGen block which assigns dest by evaluating expr *)
assignVariableFromExpression[dest_, expr_] := Module[{tSym, cleanExpr, code},
      
      tSym = Unique[];
      
      cleanExpr = replacePowers[expr] /. sym`t -> tSym;
  
      If[SOURCELANGUAGE == "C",      
        code = ToString[dest == cleanExpr, CForm,       PageWidth -> 80] <> ";\n",
        code = ToString[dest] <> ".eq." <> ToString[cleanExpr, FortranForm, PageWidth -> 70] <> "\n"
       ];
 
      If[SOURCELANGUAGE != "C",
         Module[{},
           code = StringReplace[code, "\n  "      -> " &\n"];
           code = StringReplace[code, "   -  "    -> " &  "];
           code = StringReplace[code, ".eq."      -> " = "];
           code = StringReplace[code, "=        " -> "="];
           code = StringReplace[code, "\\"         -> ""];
           code = StringReplace[code, "(index)"      -> "(i,j,k)"];
         ];
      ];

      code = StringReplace[code, "=="          -> " = "];
      code = StringReplace[code, ToString@tSym -> "cctk_time"];
      
      {code}];

(* This flag determines whether you want to generate debugging code to
   do CCTK_INFO on the variables as they are translated.  This can
   help find problems in the construction of the translator maps. *)

debugInLoop = False;


declareVariablesForCalculation[calc_] :=
  Module[{shorthands, localGFs},
    shorthands = calculationUsedShorthands[calc];
    localGFs = Map[localName, calculationUsedGFs[calc]];

    {CommentedBlock["Declare shorthands",
       Map[DeclareVariable[#, "CCTK_REAL"] &, shorthands]],

     CommentedBlock["Declare local copies of grid functions",
       Map[DeclareVariable[#, "CCTK_REAL"] &, localGFs]]}];

(* Derivative precomputation *)

derivativesUsed[x_] :=
  Union[Cases[x, _ ? (MemberQ[derivativeHeads, #] &)[_],Infinity]];

(* Expects a list of the form {D11[h22], ...} *)
declarePrecomputedDerivatives[derivs_] :=
  Module[{vars},
    vars = Map[ToString[Head[#]] <> ToString[First[#]] &, derivs];
    {Map[DeclareVariable[#, "CCTK_REAL"] &, vars], "\n\n"}];

precomputeDerivative[d_] :=
  Module[{h=ToString[Head[d]], f = ToString[First[d]]},
  {h <> f, " = ", h, "gf(", f, ",i,j,k)" <> EOL[] <>"\n"}];

printEq[eq_] := 
  Module[{lhs, rhs, rhsSplit, split, rhsString},

    lhs = First@eq;
    rhs = Last@eq;

    split[ x_ + y___] := { x, " + ..."};
    split[-x_ + y___] := {-x, " + ..."};
    split[ x_       ] := { x, ""};

    rhsSplit = split[Expand@replacePowers@rhs];

    rhsString = ToString@CForm[rhsSplit[[1]]] <> rhsSplit[[2]];

    Print[" " <> ToString@lhs <> " = " <> rhsString]];

(* Return the names of any gridfunctions used in the calculation *)
calculationUsedGFs[calc_] :=
  Module[{calcSymbols, allGFs},
    calcSymbols = calculationSymbols[calc];
    allGFs = allVariables[lookup[calc, Groups]];
    Intersection[calcSymbols, allGFs]];

(* Return the names of any gridfunctions used in the calculation *)
calculationUsedShorthands[calc_] :=
  Module[{calcSymbols, allShorthands},
    calcSymbols = calculationSymbols[calc];
    allShorthands = lookupDefault[calc, Shorthands, {}];
    Intersection[calcSymbols, allShorthands]];

calculationSymbols[calc_] :=
  Module[{allAtoms},
    allAtoms = Union[Level[lookup[calc, Equations], {-1}]];
    Cases[allAtoms, x_Symbol]];

simplifyEquationList[eqs_] :=
  Map[simplifyEquation, eqs];

simplifyEquation[lhs_ -> rhs_] :=
  lhs -> Simplify[rhs];
  

(* Calculation function generation *)

CreateCalculationFunction[calc_, debug_] :=
  Module[{gfs, allSymbols, knownSymbols,
          shorts = lookupDefault[calc, Shorthands, {}],
          eqs    = lookup[calc, Equations],
          syncGroups = lookupDefault[calc, SyncGroups, {}],
          parameters = lookupDefault[calc, Parameters, {}],
          functionName, dsUsed, 
          groups = lookup[calc, Groups],
          derivDiscs = lookupDefault[calc, DerivativeDiscretizations, {}]},

  gfs = allVariables[groups];
  functionName = ToString@lookup[calc, Name];
  dsUsed = derivativesUsed[eqs];

  Print["Creating Calculation Function: " <> functionName];

  Print[" ", Length@shorts, "  shorthands / ",  
             Length@gfs,    "  grid functions / ",
             Length@groups, "  groups"];

  Print[];

  Print[" shorthands:"];
  Print[" ", shorts];
  Print[" groups:"];
  Print[" ", Map[groupName, groups]]; 

  If[debug, Print[" grid functions:", gfs]];

   If[Length@lookupDefault[calc, CollectList, {}] > 0,

     eqs = Table[Map[First[#] -> simpCollect[lookup[calc, CollectList], 
                                             Last[ #], 
                                             First[#], debug]&, 
                 eqs[[i]] ], {i, 1, Length@eqs}]
   ];

  Print["\n\nEquations:"];

   Map[Map[printEq, #]&, eqs];
   Print[];

  (* Check that there are no unknown symbols in the calculation *)

  allSymbols = calculationSymbols[calc];
  knownSymbols = Join[gfs, shorts, parameters, {t, Pi, E}];

  unknownSymbols = Complement[allSymbols, knownSymbols];

  If[unknownSymbols != {},
     Module[{},
       Print["Unknown symbols in calculation: ", unknownSymbols];
       Print["Failed verification of calculation: ", calc];
       Throw["Unknown symbols in calculation"]]];

  DefineCCTKSubroutine[lookup[calc, Name],
  { DeclareGridLoopVariables[],
    DeclareFDVariables[],
    declareVariablesForCalculation[calc],
    declarePrecomputedDerivatives[dsUsed],

    CommentedBlock["Include user-supplied include files",
      Map[IncludeFile, lookupDefault[calc, DeclarationIncludes, {}]]],

    InitialiseFDVariables[],

    If[gfs != {},
      {
       InitialiseGridLoopVariables[],

       (* Have removed ability to include external header files here.
          Can be put back when we need it. *)

       Map[equationLoop[#, gfs, shorts, {}, groups, syncGroups, derivDiscs] &, eqs]},
      {}]}]];

allVariables[groups_] :=
  Flatten[Map[variablesInGroup, groups], 1];

variablesInGroup[g_] :=
  g[[2]];

groupName[g_] :=
  g[[1]];

syncGroup[name_] :=
  If[SOURCELANGUAGE == "C",
    {"CCTK_SyncGroup(cctkGH, \"", name, "\");\n"},
    {"call CCTK_SyncGroup(i, cctkGH, \"", name, "\")\n"}
  ];


equationLoop[eqs_, gfs_, shorts_, incs_, groups_, syncGroups_, derivDiscs_] :=
  Module[{rhss, lhss, gfsInRHS, gfsInLHS, localGFs, localMap, eqs2},

    rhss = Map[#[[2]] &, eqs];
    lhss = Map[#[[1]] &, eqs];

    gfsInRHS = Union[Cases[rhss, _ ? (MemberQ[gfs,#] &), Infinity]];
    gfsInLHS = Union[Cases[lhss, _ ? (MemberQ[gfs,#] &), Infinity]];

    localGFs = Map[localName, gfs];
    localMap = Map[# -> localName[#] &, gfs];

    eqs2 = eqs /. Flatten[derivDiscs,1];

  {GridLoop[
   {CommentedBlock["Assign local copies of grid functions",
                   Map[AssignVariable[localName[#], GridName[#]] &, 
                       gfsInRHS]],

    CommentedBlock["Include user supplied include files",
                   Map[IncludeFile, incs]],

    CommentedBlock["Precompute derivatives",
                   Map[precomputeDerivative, derivativesUsed[eqs]]],

    CommentedBlock["Calculate temporaries and grid functions",
                   Map[{assignVariableFromExpression[#[[1]], #[[2]]], "\n"}  &,
                   replaceDerivatives[
                     replaceWithDerivativesHidden[eqs2, localMap],derivDiscs]]],

    If[debugInLoop, Map[InfoVariable[#[[1]]] &, eqs2 /. localMap], ""], 

    CommentedBlock["Copy local copies back to grid functions",
                   Map[AssignVariable[GridName[#], localName[#]] &, 
                       gfsInLHS]],

    If[debugInLoop, Map[InfoVariable[GridName[#]] &, gfsInLHS], ""]}],

    CommentedBlock["Synchronize the groups that have just been set",

    lhsGroupNames = containingGroups[gfsInLHS, groups];
    Print["Synchronizing groups: ", lhsGroupNames];
    Map[syncGroup, Intersection[lhsGroupNames, syncGroups]]]
   }];

End[];

EndPackage[];
