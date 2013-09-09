
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

(* This package provides a set of functions to create the various
   parts of a Cactus thorn and assemble them. *)

BeginPackage["Thorn`", "CodeGen`", "CodeGenC`", "CodeGenCactus`", "CodeGenKranc`", "CodeGenCalculation`",
  "MapLookup`", "KrancGroups`", "Helpers`",
  "Errors`", "Kranc`", "CaKernel`", "Vectorisation`", "DGFE`", "OpenCL`"];

(* These functions are externally visible, and comprise the public
   interface to this package. *)
CreateThorn::usage = "Create a general Cactus thorn from
a thorn specification structure";

Begin["`Private`"];



(* ------------------------------------------------------------------------ 
   Thorn creation
   ------------------------------------------------------------------------ *)

(* source = {Filename -> "MoLRegister.cc", Contents -> "#include ..."} *)

(* thorn = {Name -> "ClassicADMMolEvolve", Directory -> "ClassicADM",
            Interface -> i, Schedule -> s, Param -> p, Makefile -> m, 
            Sources -> {s1, s2, ...} *)

(* Given a thorn specification structure as defined above, create a
   thorn.  Note that if you specify a path to the thorn, then you are
   responsible for making sure that the parent directory exists; this
   function does not automatically create any parent directories. *)
CreateThorn[thorn_] :=
  Module[{thornDirectory, sourceDirectory},

    thornDirectory = lookup[thorn, Directory] <> "/" <> lookup[thorn, Name];
    sourceDirectory = thornDirectory <> "/src";

    Print["Creating thorns in directory ", thornDirectory];

    EnsureDirectory[thornDirectory];
    EnsureDirectory[sourceDirectory];

    GenerateFile[thornDirectory <> "/configuration.ccl", lookup[thorn, Configuration]];
    GenerateFile[thornDirectory <> "/interface.ccl",     lookup[thorn, Interface]];
    GenerateFile[thornDirectory <> "/param.ccl",         lookup[thorn, Param]];
    GenerateFile[thornDirectory <> "/schedule.ccl",      lookup[thorn, Schedule]];
    If[lookup[thorn, CaKernel] =!= None,
      GenerateFile[thornDirectory <> "/cakernel.ccl",      lookup[thorn, CaKernel]];
    ];
    
    Map[GenerateFile[sourceDirectory <> "/" <> lookup[#, Filename], 
                                               lookup[#, Contents]] &,
                                               lookup[thorn, Sources]];

    GenerateFile[sourceDirectory <> "/make.code.defn", lookup[thorn, Makefile]];

    (* Update thorn directory timestamp so that it can be used in makefiles *)
    GenerateFile[thornDirectory <> "/temp", {}];
    DeleteFile[thornDirectory <> "/temp"];

    Print["Thorn ", thornDirectory, " created successfully"];
];

End[];

EndPackage[];
