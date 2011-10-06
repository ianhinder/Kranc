
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

BeginPackage["CodeGen`", {"Errors`", "Kranc`"}];

FlattenBlock::usage = "FlattenBlock[block] converts 'block' to a string.";
SeparatedBlock::usage = "SeparatedBlock[block] returns a version of 'block' with " <>
  "a newline before it.";
GenerateFile::usage = "GenerateFile[name, block] writes 'block' to a file of the " <>
  "specified 'name'.";
AddToFile::usage = "AddToFile[name, block] appends 'block' to a file of the " <>
    "specified 'name'.";
SpaceSeparated::usage = "";
NewlineSeparated::usage = "";
InfoVariable::usage = "";
CommaNewlineSeparated::usage = "";
CommaSeparated::usage = "";
Stringify::usage = "";
Quote::usage = "Quote[x] returns x surrounded by quotes";
IndentBlock::usage = "";
CheckBlock::usage = "";

CodeGenBlock := _String | _?AtomQ | List[(_?(MatchQ[#, CodeGenBlock] &)) ...];
Boolean = (True | False);

Begin["`Private`"];

(* Code generation utilities; not specific to any language *)

CheckBlock[s_String] := s;
CheckBlock[a_?AtomQ] := a;
CheckBlock[l_List] := Map[CheckBlock, l];
ErrorDefinition[CheckBlock];

FlattenBlock[b_] :=
  Module[
    {flattenBlock},
    flattenBlock[x_String] := x;
    flattenBlock[l_List] := StringJoin@@Map[FlattenBlock, l];
    flattenBlock[a_?AtomQ] := ToString[a];

    CheckBlock[b];
    flattenBlock[b]];
         
ErrorDefinition[FlattenBlock];

IndentBlock[block:CodeGenBlock] :=
  StringDrop["  " <> StringReplace[FlattenBlock[block], {"\n" -> "\n  "}],-2];
ErrorDefinition[IndentBlock];

SeparatedBlock[block:CodeGenBlock] := {"\n", block};
ErrorDefinition[SeparatedBlock];

GenerateFile[filename_String, contents_] :=
  Module[
    {fp = OpenWrite[filename]},
    CheckBlock[contents];
    WriteString[fp, FlattenBlock[contents]];
    Close[fp]];
ErrorDefinition[GenerateFile];

AddToFile[filename_String, contents:CodeGenBlock] :=
  Module[
    {fp = OpenAppend[filename]},
    WriteString[fp, FlattenBlock[contents]];
    Close[fp]];
ErrorDefinition[AddToFile];

CommaNewlineSeparated[l_List] :=
  Riffle[l, ",\n"];
ErrorDefinition[CommaNewlineSeparated];

SpaceSeparated[l_List] :=
  Riffle[l, " "];
ErrorDefinition[SpaceSeparated];

CommaSeparated[l_List] :=
  Riffle[l, ", "];
ErrorDefinition[CommaSeparated];

NewlineSeparated[l_List] :=
  Riffle[l, "\n"];
ErrorDefinition[NewlineSeparated];

CommaInitSeparated[l_List] :=
  Riffle[Map[{#," = INITVALUE"} &, l], ", "];
ErrorDefinition[CommaInitSeparated];

(* Turn a section of code into a string:
   1. quote all quotes (replace all quotes with backslash-quote)
   2. break the string into lines to make it readable (replace all newlines
      with quote-newline-quote)
   3. surround the result with quotes *)
Stringify[x:CodeGenBlock] :=
  "\"" <> StringReplace[StringReplace[FlattenBlock[x], "\"" -> "\\\""],
                        "\n" -> "\\n\"\n\""] <> "\"\n";
ErrorDefinition[Stringify];

PartitionVarList[list_List] :=
  Module[
    {partition, split},

    partition[locallist_] :=
    Module[
      {cutoff},
      cutoff = 6;
      If[Length@locallist > cutoff, Partition[locallist, cutoff, cutoff, {1,1}, {}],
         {locallist}]];

    split = Split[list, NameRoot[#1] == NameRoot[#2] &];
    split = Flatten[Map[partition, split], 1];

    split];
ErrorDefinition[PartitionVarList];

insertFile[name_String] :=
  Module[
    {istream_, contents_},
    istream = OpenRead[name];
    contents = ReadList[istream, String];
    Close[istream];
    contents];
ErrorDefinition[insertFile];

NameRoot[name_Symbol] :=
  Module[
    {dropNumberRule, root},
    dropNumberRule = {"1" -> "", "2" -> "", "3" -> "", "4" -> "", "5" -> "",
                      "6" -> "", "7" -> "", "8" -> "", "9" -> "", "0" -> "", "rhs" -> ""};
    root = StringReplace[ToString@name, dropNumberRule]];
ErrorDefinition[NameRoot];

Quote[x:CodeGenBlock] :=
  {"\"", x, "\""};
ErrorDefinition[Quote];

End[];

EndPackage[];
