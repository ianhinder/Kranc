
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
{INV, SQR, CUB, QAD, exp, pow};

EndPackage[];

BeginPackage["Helpers`", {"sym`"}];

TensorName::usage = "get the base name of a tensor object, i.e. TensorName[g[la, lb]]";
EnsureDirectory::usage = "create a directory if it does not already exist"
AddSuffix::usage = "AddSuffix[object_,suffixString_] adds a suffix to an object";
GFsFromGroupList::usage = "GFsFromGroupList[g_] gives the GFs from a group list in the from {\"group\", {gf1, gf2, ...}}";

Begin["`Private`"];

TensorName[t_[i__]] := t 

AddSuffix[object_,suffix_]:=ToExpression[ToString@object<>ToString@suffix]

EnsureDirectory[name_] := If[FileType[name] == None, CreateDirectory[name]];

GFsFromGroupList[g_] := Flatten@Map[Last,g]

End[];



EndPackage[];
