
(*  Copyright 2004-2013 Sascha Husa, Ian Hinder, Christiane Lechner

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

BeginPackage[
  "CodeGenInterface`",
  {"Errors`", "Helpers`", "Kranc`", "MapLookup`", "CodeGenC`",
   "CodeGen`", "CodeGenKranc`"}];

CreateInterface::usage = "Create the content of the interface.ccl file.";

Begin["`Private`"];

(* ------------------------------------------------------------------------ 
   Interface file
   ------------------------------------------------------------------------ *)

(* The following "group" structure defines a Cactus group of variables
   to be included in an interface.ccl file.

  group:

  {Name -> "", VariableType -> "", Timelevels -> 2, GridType -> "GF",
   Comment -> "", Visibility -> "public", Tags -> {tag1, tag2, ...},
   Variables -> {phi, h11, ...}}

A 'tag' is of the form {"tensortypealias" -> "Scalar"}


 *)

(* Given the specification of a group structure, return a CodeGen
   block for the interface.ccl file to define that group *)
interfaceGroupBlock[spec_] :=
  {lookup[spec, Visibility], ":\n",
   lookup[spec, VariableType], " ", lookup[spec, Name], 
     " type=", lookup[spec,GridType],
     " timelevels=", lookup[spec, Timelevels], 
     If[mapContains[spec,Tags], {" tags='", interfaceTags[lookupDefault[spec,Tags, {}]], "'"}, ""], 
     If[mapContains[spec,Dim], {" dim=", lookup[spec,Dim] }, ""], 
     If[mapContains[spec,Size], {" size=", lookup[spec,Size] }, ""], 
     "\n",
   SuffixedCBlock[{CommaNewlineSeparated[lookup[spec, Variables]],"\n"}, 
                  "\"" <> lookup[spec, Comment] <> "\""]};

interfaceTag[tagName_String -> tagValue_String] :=
  tagName <> "=" <> "\"" <> tagValue <> "\"";

interfaceTag[tagName_String -> tagValue_?IntegerQ] :=
  tagName <> "=" <> ToString[tagValue];

interfaceTag[tagName_String -> tagValue_?NumberQ] :=
  tagName <> "=" <> ToString[N[tagValue, 20]];

interfaceTags[tags_] :=
  SpaceSeparated[Map[interfaceTag, tags]];





(* Function aliasing *)

(* A definition of an aliased function is:

  {Name -> "MoLRegisterEvolvedGroup",
   Type -> "CCTK_INT",
   ArgString -> "CCTK_INT IN EvolvedIndex, CCTK_INT IN RHSIndex"}  

*)

usesFunction[f_] :=
If[lookup[f, Type] == "SUBROUTINE",
{lookup[f, Type], " ", lookup[f, Name], "(", lookup[f,ArgString], ")\n",
   "USES FUNCTION ", lookup[f, Name], "\n\n"},
{lookup[f, Type], " FUNCTION ", lookup[f, Name], "(", lookup[f,ArgString], ")\n",
   "USES FUNCTION ", lookup[f, Name], "\n\n"}
];


providesFunction[f_] :=
If[lookup[f, Type] == "SUBROUTINE",
{lookup[f, Type], " ", lookup[f, Name], "(", lookup[f,ArgString], ")\n",
   "PROVIDES FUNCTION ", lookup[f, Name], "\n\n"},
{lookup[f, Type], " FUNCTION ", lookup[f, Name], "(", lookup[f,ArgString], ")\n",
   "PROVIDES FUNCTION ", lookup[f, Name], "\n\n"}
];


(* Given the name of an implementation, a list of implementation names
   that we inherit from, a list of include files to mention, and a
   list of group structures as defined above, return a CodeGen block
   representing the interface.ccl file.  As an optional argument, one
   can specify Friends -> {list of implementations we want as
   friends}. Can also have UsesFunction -> {functions}*)
CreateInterface[implementation_, inheritedImplementations_, includeFiles_, 
                groups_, opts___] :=
  {FileHeader["CCL"],
   "implements: ", implementation, "\n\n",
   "inherits:   ", SpaceSeparated[inheritedImplementations], "\n\n",
   If[mapContains[{opts}, Friends],
     {"friend:     ", SpaceSeparated[lookup[{opts}, Friends]]},{}],
   "\n\n",
   Map[{"USES INCLUDE: ", #, "\n"} &, includeFiles],
   "\n",

   Map[usesFunction,     lookupDefault[{opts}, UsesFunctions, {}]],

   Map[providesFunction, lookupDefault[{opts}, ProvidesFunctions, {}]],


   NewlineSeparated[Map[FlattenBlock[interfaceGroupBlock[#]] &, groups]]};

End[];

EndPackage[];
