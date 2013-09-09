
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
  "CalculationBoundaries`", "MapLookup`", "KrancGroups`", "Helpers`",
  "Errors`", "Kranc`", "CaKernel`", "Vectorisation`", "DGFE`", "OpenCL`"];

(* These functions are externally visible, and comprise the public
   interface to this package. *)
CreateThorn::usage = "Create a general Cactus thorn from
a thorn specification structure";
CreateSetterSource::usage = "";

Begin["`Private`"];


(* ------------------------------------------------------------------------ 
   Setter
   ------------------------------------------------------------------------ *)

(* calculation = {Name                -> "ClassicADM_Setter", 
                  optional Before     -> {functions},
                  optional After      -> {functions},
                  Shorthands          -> {gInv11, ...},
	          GridFunctions       -> {g11rhs, K11},
                  CollectList         -> {hInv11, hInv22, ...},
         optional DeclarationIncludes -> {include file list},
         optional LoopPreIncludes     -> {include file list},
	          Equations           -> {{K11_rhs -> 2 A K11, ...}...}} *)


(* Given a list of Calculation structures as defined above, create a
   CodeGen representation of a source file that defines a function for
   each Calculation. *)

Options[CreateSetterSource] = ThornOptions;

CreateSetterSource[calcs_, debug_, include_,
  opts:OptionsPattern[]] :=
  Module[{calc = First[calcs],bodyFunction},

  If[!MatchQ[include, _List],
    ThrowError["CreateSetterSource: Include should be a list but is in fact " <> ToString[include]]];

  SetDataType[If[OptionValue[UseVectors],VectorisationType[], "CCTK_REAL"]];

  {FileHeader["C"],

   "#define KRANC_" <> ToUpperCase[CodeGenC`SOURCELANGUAGE] <> "\n\n",

   If[CodeGenC`SOURCELANGUAGE == "C",
         {IncludeSystemFile["assert.h"],
          IncludeSystemFile["math.h"],
          IncludeSystemFile["stdio.h"],
          IncludeSystemFile["stdlib.h"],
          IncludeSystemFile["string.h"]},
         {"\n"}
      ],

   Map[IncludeFile, Join[{"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h",
                         (*"precomputations.h",*) "GenericFD.h", "Differencing.h"},
                         include,
                         {"cctk_Loop.h", "loopcontrol.h"},
                         If[OptionValue[UseOpenCL], OpenCLIncludeFiles[], {}],
                         If[OptionValue[UseVectors], VectorisationIncludeFiles[], {}]]],
   CalculationMacros[OptionValue[UseVectors]],

   (* For each function structure passed, create the function and
      insert it *)

   CalculationBoundariesFunction[First[calcs]],

   bodyFunction = DefineFunction[lookup[calc,Name]<>"_Body", "static void", "const cGH* restrict const cctkGH, const int dir, const int face, const CCTK_REAL normal[3], const CCTK_REAL tangentA[3], const CCTK_REAL tangentB[3], const int imin[3], const int imax[3], const int n_subblock_gfs, CCTK_REAL* restrict const subblock_gfs[]",
  {
    "DECLARE_CCTK_ARGUMENTS;\n",
    "DECLARE_CCTK_PARAMETERS;\n\n", 
    #
  }] &;

   calc = Join[calc, {BodyFunction -> bodyFunction, 
                      CallerFunction -> True,
                      LoopFunction -> (GenericGridLoop[lookup[calc,Name],#,opts] &),
                      GFAccessFunction -> ({#,"[","index","]"} &),
                      InitFDVariables -> InitialiseFDVariables[OptionValue[UseVectors]],
                      MacroPointer -> True}];

   CreateCalculationFunction[calc, opts]}];

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
