
(* Copyright 2010-2012 Ian Hinder

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

BeginPackage["Stack`"];

StackPush;
StackPop;
StackPeek;
StackRead;

Begin["`Private`"];

$stackSize[_] = 0;

SetAttributes[StackPush, HoldFirst];
StackPush[stack_,elem_] :=
  ((* $stackSize[stack] = $stackSize[stack]+1; *)
    stack = {elem,stack});

SetAttributes[StackPop,HoldFirst];
StackPop[{}] := Null;
StackPop[stack_] :=
  With[{elem = First[stack]},
    (* $stackSize[stack] = $stackSize[stack]-1; *)
    stack = Last[stack]; elem];

(* SetAttributes[StackPeek,HoldFirst]; *)
(* StackPeek[{}] := Null; *)
(* StackPeek[stack_] := *)
(*   First[stack]; *)

StackRead[stack_] :=
  Flatten[Reverse[stack]];

End[];

EndPackage[];
