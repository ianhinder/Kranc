(* ::Package:: *)

(*  Copyright 2011 Barry Wardell

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

BeginPackage["Optimize`", {"Kranc`", "Errors`"}];

EliminateCommonSubexpressions::usage = "EliminateCommonSubexpressions[calc] identifies common subexpressions in calc and introduces new shorthands for them.";
TopologicallySortEquations::usage = "TopologicallySortEquations[eqs, v] sorts eqs topologically, including Head v as a possible vertex."

Begin["`Private`"];

CSEPrint[___] = null;
(* CSEPrint[values___] := Apply[Print, Map[InputForm, {values}]]; *)

Options[EliminateCommonSubexpressions] = ThornOptions;
EliminateCommonSubexpressions[calc_List, OptionsPattern[]] :=
 Module[{eqs, shorts, name, pdDefs, derivs, newShorts, newEqs, allShorts, newCalc},
  name   = (Name /. calc);

  InfoMessage[InfoFull, "Doing common subexpression elimination for "<>name];

  eqs    = (Equations /. calc) /. Equations -> {};
  shorts = (Shorthands /. calc) /. Shorthands -> {};

  (* Get a list of symbols used for derivatives. We will not eliminate these as subexpressions. *)
  pdDefs = OptionValue[PartialDerivatives];
  derivs = DeleteDuplicates[Head/@(First/@pdDefs)];

  (* Generate new equations with subexpressions eliminated. *)
  {newShorts, newEqs} = cse[eqs, Symbol["csetemp"], derivs];

  If[Length[newShorts]>0,
    InfoMessage[Info, "Extracted "<>ToString[Length[newShorts]]<>" common subexpressions from "<>name];
  ];

  allShorts = Join[shorts, newShorts];
  newCalc = Join[calc /. {(Shorthands->_) -> Sequence[], (Equations->_) -> Sequence[]},
                 {Shorthands->allShorts}, {Equations->newEqs}];

  newCalc
];

(* Hide Piecewise[] by replacing it with piecewise[], expanding the
   nested list arguments. We may be able to handle Piecewise specially
   in the replacements below, introducing additional rules when
   creating the subexpressions and performing the replacements, but
   this would probably be more cumbersome. Also, matching Piecewise[]
   in patterns is difficult, since e.g. even the pattern Piecewise[__]
   already leads to warnings about the arguments not being nested
   lists. *)
hidePiecewise[eqs_] :=
  Module[{piecewise1},
         eqs
         (* Replace Piecewise by piecewise1, so that we can write
            patterns below without warnings *)
         /. Piecewise -> piecewise1
         /. {piecewise1[pairs_List] :>
                Apply[piecewise, Flatten[pairs,2]],
             piecewise1[pairs_List, val_] :>
                Apply[piecewise, Append[Flatten[pairs,2], val]]}];
(* Re-instate Piecewise[] from piecewise[], re-creating the nested
   list arguments *)
showPiecewise[eqs_] :=
  Module[{piecewise1},
         eqs
         /. {piecewise[args__] :>
             If[Mod[Length[{args}],2]==0,
                piecewise1[Partition[{args},2]],
                piecewise1[Partition[{args},2], Last[{args}]]]}
         /. piecewise1 -> Piecewise]

cse[eqs1_, v_, exceptions_, minSaving_:0] :=
 Module[{eps, subexprs, replacements, replace, newEqs, defs, newDefs, i, relabelVars, allEqs, sortedEqs, newVars},
  (* Find all possible subexpressions and how many times they occur *)
  CSEPrint["CSE"];
  eqs = hidePiecewise[eqs1];
  CSEPrint["CSE: eqs=", eqs];
  subexprs = Reap[Scan[If[! AtomQ[#], Sow[#]] &, eqs[[All,2]], Infinity]];
  CSEPrint["CSE: subexprs=", subexprs];
  If[subexprs[[2]]=={}, Return[{{}, eqs}]];
  subexprs = Tally[subexprs[[2, 1]]];

  (* Discard subexpressions which only appear once *)
  subexprs = Select[subexprs, #[[2]] >= 2 &];

  (* Sort subexpressions in ascending order by size (LeafCount) *)
  subexprs = Sort[subexprs, LeafCount[#1] < LeafCount[#2] &];

  (* Ony keep subexpressions larger than minSaving=(numoccurances-1)*size *)
  subexprs = Select[subexprs, (#[[2]]-1) LeafCount[#[[1]]] >= minSaving &][[All,1]];

  (* Discard some specific cases *)
  subexprs = Cases[subexprs, Except[_?AtomQ]];
  subexprs = Cases[subexprs, Except[Times[-1, _?AtomQ]]]; (* -x *)
  subexprs = Cases[subexprs, Except[Alternatives@@(Blank/@exceptions)]]; (* specified exceptions *)
  subexprs = Cases[subexprs, Except[Times[-1, Alternatives@@(Blank/@exceptions)]]]; (* -exceptions *)

  (* Get the list of replacements for our original expression *)
  replacements = Thread[subexprs -> Table[v[i], {i, Length[subexprs]}]];

  (* Replace common subexpressions with new variables *)
  (* Do not replace certain terms, e.g. the first argument of IfThen. *)
  (* newEqs = eqs //. replacements; *)
  CSEPrint["CSE: eqs=", eqs];
  CSEPrint["CSE: replacements=", replacements];
  replace[expr_] := Replace[Switch[expr,
                                   IfThen[_,_,_], IfThen[expr[[1]], replace[expr[[2]]], replace[expr[[3]]]],
                                   (* ToReal[_],     ToReal[expr[[1]]], *)
                                   _?AtomQ,       expr,
                                   _,             Map[replace, expr]],
                            replacements];
  newEqs = FixedPoint[replace, eqs];
  CSEPrint["CSE: newEqs=", newEqs];

  (* Build up definitions for the new variables *)
  defs = Reverse/@replacements;
  CSEPrint["CSE: defs=", defs];
  For[i = 2, i <= Length[subexprs], i++,
    defs[[i,2]] = defs[[i,2]] /. replacements[[1;;i-1]];
  ];
  CSEPrint["CSE: defs=", defs];

  (* Select only the definitions which are needed for the new expressions.
     This accounts for cases where a subexpression appears multiple times,
     but always as part of the same larger subexpression. For example, in
       expr = Sqrt[(a+b)(a-b)c]+(a+b)(a-b)c+(a+b)d+Sqrt[(a+b)d+(a+b)c];
     we would identify the subexpressions
       {v[1]->a+b,v[2]->d v[1],v[3]->a-b,v[4]->c v[1] v[3]};
     whereas all we really want it to identify is
       {v[1]->a+b,v[2]->d v[1],v[4]->(a-b) c v[1]};
     and the introduction of v[3] is unnecessary. To achieve this, we only
     keep temporary variables which appear in the expression after substitution
     or which appear more than once in the definition of the temporary variables.
  *)
  newDefs = Select[defs, (Count[newEqs, #[[1]], Infinity] > 0) ||
                         (Count[defs[[All,2]], #[[1]], Infinity] > 1) &];
  CSEPrint["CSE: newDefs=", newDefs];

  (* Replace any temporaries eliminated by the previous procedure with their definition *)
  newDefs = newDefs //. Complement[defs, newDefs];
  CSEPrint["CSE: newDefs2=", newDefs];

  (* Check we actually have subexpressions to eliminate. Otherwise just return the original expression *)
  If[Length[newDefs]==0, Return[{{}, eqs}]];

  (* This is our new system of equations *)
  allEqs = Join[newDefs, newEqs];
  CSEPrint["CSE: allEqs=", allEqs];

  sortedEqs = Fold[InsertNewEquation, newEqs, Reverse[newDefs]];
  CSEPrint["CSE: sortedEqs=", sortedEqs];

  (* Relabel new temporary variables so that they are sequential and C friendly *)
  newVars = Select[sortedEqs[[All,1]], MemberQ[newDefs[[All, 1]],#]&];
  CSEPrint["CSE: newVars=", newVars];
  i = 0;
  relabelVars = (# -> Symbol[ToString[v] <> ToString[i++]]) & /@ newVars;

  (* Return the list of new variables and the new equations *)
  showPiecewise[{newVars, sortedEqs} /. relabelVars]
];

TopologicallySortEquations[eqs_] := Module[{lhs, rhs, lhsInrhs, dag, sortedVars, indVars, allVars, sortedEqs},
  lhs = eqs[[All,1]];
  rhs = eqs[[All,2]];

  (* Generate an directed acyclic graph for the system of equations *)
  lhsInrhs = DeleteDuplicates[Cases[{#}, _?(MemberQ[lhs, #] &), Infinity]] & /@ rhs;
  dag = Graph[ Flatten[MapThread[Thread[Rule[#1, #2]] &, {lhsInrhs, lhs}]] ];

  (* Topologically sort the DAG *)
  sortedVars = Quiet[TopologicalSort[dag], TopologicalSort::argx];

  (* Check if the topological sorting failed. This can happen if the graph for
     the equations is cyclic. For example, we could have a->a+1 or {b->a*a, a->b} *)
  If[SameQ[Head[sortedVars], TopologicalSort],
    InfoMessage[Info, "Failed to topologically sort equations."];
    Return[$Failed]
  ];

  (* Some variables might be independent. Add them back in. *)
  indVars = Complement[lhs, sortedVars];
  allVars = Join[indVars, sortedVars];

  sortedEqs = Thread[allVars -> (allVars/.eqs)];
  sortedEqs
];

InsertNewEquation[oldEqs_, newEq_] := Module[{before},
  CSEPrint["InsertNewEquation oldEqs=", oldEqs, " newEq=", newEq];
  (* For some reason, we can be asked to insert an equation that is
     not actually needed. This should not be the case. However, handle
     it gracefully for now. *)
  (* before = Position[oldEqs[[All,2]], newEq[[1]]][[1,1]]; *)
  before = Position[oldEqs[[All,2]], newEq[[1]]];
  If[before=={},
     oldEqs,
     Insert[oldEqs, newEq, before[[1,1]]]]
];

End[];

EndPackage[];
