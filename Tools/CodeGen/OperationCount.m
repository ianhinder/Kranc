
(*  Copyright 2014 Ian Hinder

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

BeginPackage["OperationCount`", {"Errors`", "Helpers`", "Kranc`", "Object`",
  "Code`"}];

CountOperations;
ProcessOperationCount;
OperationCountProcessCode;

Begin["`Private`"];

CountOperations[_Integer] := Null;
CountOperations[_Real] := Null;
CountOperations[_Rational] := Null;
CountOperations[_Symbol] := Null;
CountOperations[_String] := Null;

CountOperations[HoldPattern[operands:Times[__]]] :=
  Module[{count},
    count = Length[operands]-1;
    (* Print["Times: ", count]; *)
    Sow[Times -> count, CountOperations];
    Scan[CountOperations, operands]];

CountOperations[HoldPattern[operands:Plus[__]]] :=
  Module[{count},
    count = Length[operands]-1;
    (* Print["Plus: ", count]; *)
    Sow[Plus -> count, CountOperations];
    Scan[CountOperations, operands]];

CountOperations[HoldPattern[operands:Power[a_, b_]]] :=
  Module[{count},
    count = 1;
    (* Print["Power: ", count]; *)
    Sow[Power -> count, CountOperations];
    Scan[CountOperations, {a, b}]];

fpFn= Sin|Cos|Tan|Log;

CountOperations[fpFn[e_]] :=
  Module[{count},
    count = 1;
    (* Print["Trig: ", count]; *)
    Sow[Trig -> count, CountOperations];
    CountOperations[e]];
  
CountOperations[IfThen[cond_, a_, b_]] :=
  (* Assume the first branch is actually taken *)
  Scan[CountOperations, {cond, a}];

CountOperations[HoldPattern[(Equal|Unequal|Less|Greater|Max|Min)[a_, b_]]] :=
  Scan[CountOperations, {a, b}];

CountOperations[(Scalar|Sign|ToReal|Abs)[e_]] :=
  CountOperations[e];

CountOperations[e_] :=
  Module[{},
    Sow[Missing -> e, CountOperations];
    Print["Unsupported operations found when counting operations for ", e//InputForm]];

DefFn[ProcessOperationCount[counts_List, name_String] :=
  Module[{keys, collectKeys, opMap, flops},
    keys = DeleteCases[Union@Flatten[Cases[counts, (a_ -> b_) -> a, Infinity]], Missing];
    collectKeys[k_] :=
      Total[Flatten[Cases[counts, (k -> x_)->x, Infinity]]];
    opMap = Map[# -> collectKeys[#] &, keys];
    (* For now, we assume that each operation is a floating point operation *)
    flops = Total[Map[Last, opMap]];
    Sow[name -> flops, ProcessOperationCount];
    Print[name, " flops-per-grid-point ", flops];
    flops]];

DefFn[
  OperationCountProcessCode[cIn_Code, opCounts_List] :=
  Module[{c},
    c = AppendObjectField[cIn, "Files", 
      {Filename -> "doc/OperationCounts.txt",
        Contents -> Join[{{"# 1:Calculation 2:flops/gp","\n\n"}}, Map[{#[[1]], "\t", #[[2]], "\n"} &, opCounts]]}];
    c]];

End[];

EndPackage[];
