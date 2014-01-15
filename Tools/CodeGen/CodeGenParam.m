
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

BeginPackage["CodeGenParam`", {"Errors`", "Helpers`", "Kranc`", "CodeGenKranc`",
                               "CodeGen`", "CodeGenC`", "MapLookup`"}];

CreateParam::usage = "Create the content of the param.ccl file.";

Begin["`Private`"];

(* ------------------------------------------------------------------------ 
   Parameter file
   ------------------------------------------------------------------------ *)

(* parameterFileSpec = {Implementations -> {}, NewParameters -> {}}

   implementation = {Name -> "", 
                     UsedParameters     (optional) -> {spec1, spec2, ...},
                     ExtendedParameters (optional) -> {spec1, spec2, ...}}

   (optional) allowedValue = {Value -> "", Description -> ""}

   parameter spec = {Name -> "", Type -> "", Default -> "", 
                     Description -> "", Visibility -> "private",
                     AllowedValues -> {...}} *)

(* To be used for parameters; will quote it if it is a keyword *)
renderValue[type_, value_] :=
  If[type == "KEYWORD",
    Quote[value],
    If[type == "CCTK_REAL",
      ToString[CForm[value]],
    value]];

(* Return a block defining a parameter with the given
   parameterSpec (defined above).  This is used for defining new
   parameters, as well as extending existing ones. *)
parameterBlock[spec_] :=
  {lookup[spec, Type], " ", 
   lookup[spec, Name], " ", 
   Quote[lookup[spec, Description]], 

   If[mapContains[spec, AccumulatorBase],
      {" ACCUMULATOR-BASE=", lookup[spec, AccumulatorBase]},
      {}],
  
   If[mapContains[spec, Steerable],
      {" STEERABLE=",Switch[lookup[spec, Steerable], 
        Never,"NEVER", 
        Always,"ALWAYS", 
        Recover, "RECOVER", 
        _,ThrowError["Unknown 'Steerable' entry in parameter " <> ToString[lookup[spec, Name]] <> ": " <> ToString[lookup[spec, Steerable]]]]},
      {}],

   "\n",
   SuffixedCBlock[

     (* For each allowed value of the parameter specified in the spec,
        create a line with the value and the description *)
     Map[{renderValue[lookup[spec,Type], lookup[#, Value]], " :: ", 
          Quote[lookup[#, Description]], "\n"} &, 
         lookupDefault[spec, AllowedValues, {}]],

     (* Output the line describing the default value of the parameter *)
     renderValue[lookup[spec,Type], lookup[spec, Default]]],

     "\n"};

(* Given a particular implementation, return a CodeGen block defining
   which parameters are used or extended from that implementation *)
parameterImplementationSection[spec_] :=
  {"\nshares: ", lookup[spec, Name], "\n", "\n",

    (* For each used parameter in the spec, output a line indicating
       that it is used *)
    Map[{"USES ", lookup[#, Type], " ", lookup[#, Name], "\n"} &, 
        lookupDefault[spec, UsedParameters, {}]], "\n",

    (* For each extended parameter in the spec, output a parameter
       block containing the specified extension prefixed with EXTENDS *)
    Map[{"EXTENDS ", parameterBlock[#], "\n"} &, 
        lookupDefault[spec, ExtendedParameters, {}]]};

(* Given a parameterFileSpec structure, return a CodeGen block for the
   param.ccl file *)
CreateParam[spec_] :=
  {FileHeader["CCL"],

    (* For each implementation defined in the spec, output a block
       which declares which parameters are used and extended by this
       implementation *)
    Map[parameterImplementationSection, 
        lookupDefault[spec, Implementations, {}]],

    (* For each new parameter being defined by this implementation,
       output a parameter block for it *)
    Map[{lookup[#, Visibility], ":\n", parameterBlock[#]} &, 
        lookupDefault[spec, NewParameters, {}]]};

End[];

EndPackage[];
