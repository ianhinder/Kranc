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

cse[eqs_, v_, exceptions_, minSaving_:0] :=
 Module[{subexprs, replacements, newEqs, defs, newDefs, i, relabelVars, allEqs, sortedEqs, newVars},
  (* Find all possible subexpressions and how many times they occur *)
  subexprs = Tally[Reap[Scan[If[! AtomQ[#], Sow[#]] &, eqs[[All,2]], Infinity]][[2, 1]]];

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
  newEqs = eqs //. replacements;

  (* Build up definitions for the new variables *)
  defs = Reverse/@replacements;
  For[i = 2, i <= Length[subexprs], i++,
    defs[[i,2]] = defs[[i,2]] /. replacements[[1;;i-1]];
  ];

  (* Select only the definitions which are needed for the new expressions.
     This accounts for cases where a subespression appears multiple times,
     but always as part of the same larger subexpression. For example, in
       expr = Sqrt[(a+b)(a-b)c]+(a+b)(a-b)c+(a+b)d+Sqrt[(a+b)d+(a+b)c];
     we would identify the subexpressions
       {v[1]->a+b,v[2]->d v[1],v[3]->a-b,v[4]->c v[1] v[3]};
     whereas all we really want it to identify is
       {v[1]->a+b,v[2]->d v[1],v[4]->(a-b) c v[1]};
     and the introduction of v[3] is unnecessary. To achieve this, we only
     keep temporary variables which appear in the expression after substition
     or which appear more than once in the definition of the temporary variables.
  *)
  newDefs = Select[defs, (Count[newEqs, #[[1]], Infinity] > 0) ||
                         (Count[defs[[All,2]], #[[1]], Infinity] > 1) &];

  (* Replace any temporaries eliminated by the previous procedure with their definition *)
  newDefs = newDefs //. Complement[defs, newDefs];

  (* Check we actually have subexpressions to eliminate. Otherwise just return the original expression *)
  If[Length[newDefs]==0, Return[{{}, eqs}]];

  (* This is our new system of equations *)
  allEqs = Join[newDefs, newEqs];

  sortedEqs = Fold[InsertNewEquation, newEqs, Reverse[newDefs]];

  (* Relabel new temporary variables so that they are sequential and C friendly *)
  newVars = Select[sortedEqs[[All,1]], MemberQ[newDefs[[All, 1]],#]&];
  i = 0;
  relabelVars = (# -> Symbol[ToString[v] <> ToString[i++]]) & /@ newVars;

  (* Return the list of new variables and the new equations *)
  {newVars, sortedEqs} /. relabelVars
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
  before = Position[oldEqs[[All,2]], newEq[[1]]][[1,1]];
  Insert[oldEqs, newEq, before]
];

End[];

EndPackage[];
