
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
SafeDelete::usage = "SafeDelete[filename_] deletes a file only after checking that the file actually exists and is a normal file";

AddSuffix::usage = "AddSuffix[object_,suffixString_] adds a suffix to an object";

GFsFromGroupList::usage = "GFsFromGroupList[g_] gives the GFs from a group list in the form {\"group\", {gf1, gf2, ...}}";
GroupStruct::usage = "GroupStruct[g_] returns a group structure the from {\"group\", {gf1, gf2, ...}}";

ComponentList::usage = "ComponentList[T_[index_]] creates a list of tensor components.";
TextComponentList::usage = "TextComponentList[T_[index_]] call ComponentList and converts to plain text format (no sub- or superscripts).";

Begin["`Private`"];

TensorName[t_[i__]] := t 

AddSuffix[object_,suffix_]:=ToExpression[ToString@object<>ToString@suffix]

EnsureDirectory[name_] := If[FileType[name] == None, CreateDirectory[name]];

SafeDelete[filename_?StringQ] := If[FileType@filename == File, DeleteFile@filename];

GFsFromGroupList[g_] := Flatten@Map[Last,g]

If[ValueQ@Global`tensorNames2componentNames[x],
(* DecomposeTools.m is loaded *)

  ComponentList[T_]     := T /. Global`makeSplitRules[T];
  TextComponentList[T_] := Global`tensorNames2componentNames@ComponentList[T];

  ComponentList[expr_,     T_?AtomQ[index__]] := expr /. Global`makeSplitRules[T[index]];
  TextComponentList[expr_, T_?AtomQ[index__]] := Global`tensorNames2componentNames@ComponentList[expr, T[index]];

  GroupStruct[t_[i__]]  := {ToString@t, ComponentList@t[i]} // Global`tensorNames2componentNames;
  GroupStruct[S_?AtomQ] := {ToString@S,{S}};
];

End[];



EndPackage[];
