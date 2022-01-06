
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
    along with Kranc; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

BeginPackage["Helpers`", {"Kranc`"}];

TensorName::usage = "get the base name of a tensor object, i.e. TensorName[g[la, lb]] -> TensorName";

EnsureDirectory::usage = "create a directory if it does not already exist"
SafeDelete::usage = 
"SafeDelete[filename] deletes a file only after checking that the file actually exists\
 and is a normal file";

AddSuffix::usage = "AddSuffix[object,suffixString] adds a suffix to an object";

GFsFromGroupList::usage = 
"GFsFromGroupList[g] gives the GFs from a group list in the form {\"group\", {gf1, gf2, ...}}";

IsNotEmptyString::usage  = "IsNotEmptyString[x] returns False if x == \"\" and True otherwise.";
PickMatch::usage         = "PickMatch[x, form] returns string x if it matches form, and \"\" otherwise.";
SafeStringReplace::usage = 
"SafeStringReplace[x, string1, string2] replaces string1 in x by string2 if x is a string\
and otherwise returns x.";
String2Char::usage = "String2Char[string] splits a string into a list of characters.";
SortString::usage =  "SortString[string] returns a string where the characters in the original string
have been sorted lexicographically.";

GroupStruct::usage = "GroupStruct[g_] returns a group structure the from {\"group\", {gf1, gf2, ...}}";

ComponentList::usage = "ComponentList[T[index]] creates a list of tensor components.";
TextComponentList::usage = 
"TextComponentList[T[index]] call ComponentList and converts to plain text format \
(no sub- or superscripts).";

SortTensorComponentsCactusStyle::usage = "sorts a list of strings in Cactus tensor-components style,\
e.g. {\"vect3\", \"vect2\", \"vect1\"} -> {\"vect1\", \"vect2\", \"vect3\"}";

BreakTensorComponentName::usage = "BreakTensorComponentName[x] maps \"bla11rhs\" -> {\"bla\", \"11\", \"rhs\"}\
and \"blu321\" -> {\"blu\", \"321\", \"\"}";

LineBreak;
MarkFirst;

Begin["`Private`"];

TensorName[t_[i__]] := t  (* remove indices from a tensor *)

AddSuffix[object_,suffix_] := ToExpression[ToString@object <> ToString@suffix]

EnsureDirectory[name_] := If[FileType[name] == None, CreateDirectory[name, CreateIntermediateDirectories -> True]];

SafeDelete[filename_?StringQ] := If[FileType@filename == File, DeleteFile@filename];

GFsFromGroupList[g_] := Flatten@Map[Last,g]

IsNotEmptyString[x_] := TrueQ[x != ""];

PickMatch[x_?StringQ, form_?StringQ] := If[StringMatchQ[x, form], x, ""];

SafeStringReplace[x_, string1_?StringQ, string2_?StringQ] := 
            If[StringQ@x, StringReplace[x, string1 -> string2], x];


String2Char[x_?StringQ] := Table[StringTake[x, {n}], {n, 1, StringLength@x}];

SortString[x_?StringQ] := StringJoin@Sort@String2Char@x;

If[ValueQLegacy@Global`tensorNames2componentNames[dummy_tensor],
(* we assume DecomposeTools.m is loaded -- need to load DecomposeTools before Helpers.m ! *)

(* ComponentList creates a list of independent tensor components                 *)
(* TextComponent List produces a plain text version of the output of             *)
(* the 2-argument versions can handle expressions instead of just single tensors *)

  ComponentList[T_]     := T /. Global`makeSplitRules[T];
  TextComponentList[T_] := Global`tensorNames2componentNames@ComponentList[T]; 

  ComponentList[expr_,     T_?AtomQ[index__]] := expr /. Global`makeSplitRules[T[index]];
  TextComponentList[expr_, T_?AtomQ[index__]] := Global`tensorNames2componentNames@ComponentList[expr, T[index]];

  ComponentList[TensorList_?ListQ] := Flatten@Map[ComponentList, TensorList];
  TextComponentList[TensorList_?ListQ] := Global`tensorNames2componentNames@ComponentList[TensorList];

(* GroupStruct produces a Kranc-style group structure for tensors or scalars*)
  GroupStruct[t_[i__]]  := {ToString@t, ComponentList@t[i]} // Global`tensorNames2componentNames;
  GroupStruct[S_?AtomQ] := {ToString@S,{S}};
];

(* functions to sort variables in the standard Cactus lexigraphic style, e.g. for interface.ccl entries *)

BreakTensorComponentName[x_?StringQ] := 
    Module[{DropNumberRule, base, index, suffix},
      
      DropNumberRule = {
          "1" -> "", "2" -> "", "3" -> "", "4" -> "", "5" -> "", "6" -> "", 
          "7" -> "", "8" -> "", "9" -> "", "0" -> "", 
          "rhs" -> "", "RHS" -> ""};
      
      suffix = "";
      If[StringMatchQ[x, "*rhs"], suffix = "rhs"];
      If[StringMatchQ[x, "*RHS"], suffix = "RHS"];
      
      base = StringReplace[x, DropNumberRule];

      index = StringDrop[x, StringLength@base];
      index = StringDrop[index, -StringLength@suffix];
      
      {base, index, suffix}
      
      ];

tensorCompLexicalReorder[x_?StringQ] := 
  Module[{break, base, index, suffix},
  
    break  = BreakTensorComponentName[x];

    base   = break[[1]];
    index  = break[[2]];
    suffix = break[[3]];
  
    newOrder = base <> SortString@index <> suffix            
      ];


compareReordered[x_?StringQ, y_?StringQ] := Module[{reordered, stringlist},
      
      stringlist = {x, y};
      reordered = Map[tensorCompLexicalReorder, stringlist];
       
      OrderedQ@reordered
      ];

SortTensorComponentsCactusStyle[stringlist_?ListQ] := 
  Sort[stringlist, compareReordered]; (* this is only correct for totally symmetric objects! *)


SortTensorComponentsCactusStyle[stringlist_?ListQ, type_?StringQ] :=
    
    Switch[type,
      "scalar",  stringlist,
      "U",       Sort[stringlist, compareReordered],
      "D",       stringlist,
      "DD",      Sort@stringlist,
      "UU",      Sort@stringlist,
      "DD_sym",  Sort[stringlist, compareReordered],
      "UU_sym",  Sort[stringlist, compareReordered],
      "DDD_sym", Sort[stringlist, compareReordered],
      _, stringlist
      ];

(* Take a string s and break it into separate lines using l as a guide
   to the line length.  If a word (sequence of non-whitespace
   characters) is longer than l, do not break it.  Add two spaces of
   indentation after each line break (this will push the line length
   over l). Algorithm essentially taken from
   http://en.wikipedia.org/wiki/Word_wrap *)
LineBreak[s_, l_] :=
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
MarkFirst[l_List, already_List] :=
  If[l =!= {},
    {!MemberQ[already, First[l]]} ~Join~ MarkFirst[Rest[l], already ~Join~ {First[l]}],
    {}];


End[];



EndPackage[];
