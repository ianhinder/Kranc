
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

BeginPackage["Thorn`", "CodeGen`", "CodeGenC`", "CodeGenCactus`", "CodeGenKranc`", "CalculationFunction`",
  "CalculationBoundaries`", "MapLookup`", "KrancGroups`", "Helpers`",
  "Errors`", "Kranc`", "CaKernel`", "Vectorisation`", "DGFE`", "OpenCL`"];

(* These functions are externally visible, and comprise the public
   interface to this package. *)
CreateSchedule::usage = "Create the content of the schedule.ccl file.";
CreateMakefile::usage = "Create the content of the Cactus make.code.defn file.";
CreateConfiguration::usage = "Create the content of the configuration.ccl file.";
CreateInterface::usage = "Create the content of the interface.ccl file.";
CreateThorn::usage = "Create a general Cactus thorn from
a thorn specification structure";
CreateSymmetriesRegistrationSource::usage = "";
CreateMoLRegistrationSource::usage = "";
CreateMoLBoundariesSource::usage = "";
CreateMoLExcisionSource::usage = "";
CreateSetterSource::usage = "";
CreateMPCharSource::usage = "";
CreatePrecompMacros::usage = "";
CreateStartupFile::usage = "";

(* Ensure that we can refer to symbols without the `sym prefix *)
(*$ContextPath = Join[{"sym`"}, $ContextPath];*)

Begin["`Private`"];

(* ------------------------------------------------------------------------ 
   Miscellaneous definitions, could be moved elsewhere
   ------------------------------------------------------------------------ *)


(* ------------------------------------------------------------------------ 
   Makefile
   ------------------------------------------------------------------------ *)

(* Return a CodeGen block representing a makefile which refers to the
   list of filenames sourceFiles *)
CreateMakefile[sourceFiles_] :=
  {FileHeader["Makefile"],
   "SRCS = ", Map[{#, " "} &, sourceFiles], "\n"};

(* ------------------------------------------------------------------------ 
   Configuration file
   ------------------------------------------------------------------------ *)

Options[CreateConfiguration] = ThornOptions;

CreateConfiguration[opts:OptionsPattern[]] :=
  {FileHeader["CCL"],
   "REQUIRES GenericFD\n",
   If[OptionValue[UseVectors], 
      "REQUIRES LoopControl\n", "OPTIONAL LoopControl\n{\n}\n"],
   If[OptionValue[UseDGFE], DGFEConfigurationCCL[], {}],
   If[OptionValue[UseOpenCL], OpenCLConfigurationCCL[], {}],
   If[OptionValue[UseVectors], VectorisationConfigurationCCL[], {}],
   If[OptionValue[UseCaKernel], CaKernelConfigurationCLL[], {}]
  };

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
   
(* ------------------------------------------------------------------------ 
   Scheduling
   ------------------------------------------------------------------------ *)

(* storage group 

  (represents the fact that we want to allocate storage for a Cactus
  variable group with the given number of timelevels)

  {Group -> "admbase::metric", Timelevels -> 3, 
   Conditional -> {Parameter -> "", Value -> ""},
   Conditionals -> {{Parameter -> "", Value -> ""}},
   Conditional -> {Textual -> "CCTK_EQUALS(name,value)"}}

   A "conditional" structure looks like this: {Parameter -> "", Value -> ""}

  scheduled function: (a function to be scheduled at a particular point)

  {Name -> "ADM_BSSN_CalcRHS_fn", SchedulePoint -> "in POSTINITIAL before ExternalLapse", 
   Language -> "C", Comment -> "", 
   (optional) SynchronizedGroups -> {ADM_BSSN_gamma, ...}, 
   (optional) Options -> {"meta", "level", ...},
   (optional) StorageGroups -> {Group -> "mygroup", Timelevels -> 1},
   (optional) Conditional -> {Parameter -> "", Value -> ""},
   (optional) Conditionals -> {{Parameter -> "", Value -> ""}}}

  scheduled group:

  {... sameish}

*)

(* Given a storage group structure defined above, return a CodeGen
   structure for inclusion in the schedule.ccl file to allocate
   storage for this group. *)
groupStorage[spec_] :=
  Module[
    {tls = lookup[spec,Timelevels],
     group = lookup[spec, Group]},
    Which[
      IntegerQ[tls],
      {"STORAGE: ", group, "[", tls, "]\n"},

      ListQ[tls],
      If[!MatchQ[tls, {_String, _Integer}],
         Error["Unrecognized Timelevels value "<>ToString[tls]]];
      Module[
        {param,max},
        {param,max} = tls;
        {"STORAGE: ", group, "[", param, "]\n"}],

      True, Error["Unrecognized Timelevels value "<>ToString[tls]]]];


(* Given a function scheduling specification as defined above, return
   a CodeGen block to schedule the function for the schedule.ccl file *)
scheduleUnconditionalFunction[spec_] :=
  {"schedule ", lookup[spec, Name], " ", lookup[spec,SchedulePoint], "\n",
   SuffixedCBlock[
     {If[lookup[spec, Language] != "None",
         "LANG: " <> lookup[spec, Language] <> "\n",
         ""],

      If[lookupDefault[spec, Options, ""] != "",
         "OPTIONS: " <> lookup[spec, Options] <> "\n",
         ""],

      (* Insert a SYNC line for each group we want to synchronize. *)
      Map[{"SYNC: ", #, "\n"}     &, lookupDefault[spec, SynchronizedGroups, {}]],

      Map[{"TRIGGERS: ", #, "\n"} &, lookupDefault[spec, TriggerGroups, {}]],

      (* TODO: Expect a set of keyword/value pairs instead of a string *)
      If[lookupDefault[spec, Tags, ""] != "",
         "TAGS: " <> lookup[spec, Tags] <> "\n",
         ""],

      translateRegion[r_] := Switch[r,
                                    Everywhere, "Everywhere",
                                    Interior, "Interior",
                                    InteriorNoSync, "Interior",
                                    Boundary, "Boundary",
                                    BoundaryNoSync, "Boundary",
                                    _, "ERROR(" <> ToString[r] <> ")"];
      Map[{"READS: ", #, "(",
           translateRegion[lookupDefault[spec, RequiredRegion, "ERROR"]],
           ")\n"} &,
          lookupDefault[spec, RequiredGroups, {}]],
      Map[{"WRITES: ", #, "(",
           translateRegion[lookupDefault[spec, ProvidedRegion, "ERROR"]],
           ")\n"} &,
          lookupDefault[spec, ProvidedGroups, {}]],

      (* Insert a storage block for each group we want to allocate
         storage for *)
      Map[groupStorage, lookupDefault[spec, StorageGroups, {}]]},

      Quote[lookup[spec, Comment]]]};

(* Handle the aspect of scheduling the function conditionally *)
scheduleFunction[spec_,params_] :=
  Module[{condition, conditions, parameter, value, u, v, w, x, y},

    u = scheduleUnconditionalFunction[spec];

    v = If[mapContains[spec, Conditional],

           (* Output the conditional structure *)
           condition = lookup[spec, Conditional];
    
           If[mapContains[condition, Textual],
              
              ConditionalOnParameterTextual[lookup[condition, Textual], u],
              
              If[mapContains[condition, Parameter],
                 
                 parameter = lookup[condition, Parameter];
                 value     = lookup[condition, Value];
                 ConditionalOnParameter[parameter, value, u],
                 
                 If[condition != {},
                    ThrowError["Unrecognized conditional structure", condition],
                    u]]],
           u];

    w = If[mapContains[spec, Conditionals],
    
           (* Output the conditionals structure *)
           conditions = lookup[spec, Conditionals];
    
           Fold[Function[{x, condition},
    
                If[mapContains[condition, Textual],
                  
                   ConditionalOnParameterTextual[lookup[condition, Textual], x],
                   
                   If[mapContains[condition, Parameter],
                   
                      parameter = lookup[condition, Parameter];
                      value     = lookup[condition, Value];
                      ConditionalOnParameter[parameter, value, x],
                        
                      If[condition != {},
                         ThrowError["Unrecognized conditional structure", condition],
                         x]],
                   x]],
                v, conditions],
           v];

    y = If[mapContains[spec, NewConditional],
           cond = lookup[spec, NewConditional];
           Module[
             {render, renderbool, paramPattern},

             paramPattern = Except[True | False, _Symbol | _Parameter];

             renderbool[Equal[a:paramPattern,b_String]] := {"CCTK_EQUALS(", rendervalue[a], ",\"", b,"\")"};
             renderbool[Unequal[a:paramPattern,b_String]] := {"!CCTK_EQUALS(", rendervalue[a], ",\"", b,"\")"};
             renderbool[Equal[a:paramPattern,b_?NumberQ]] := {rendervalue[a], " == ", rendervalue[b]};
             renderbool[Unequal[a:paramPattern,b_?NumberQ]] := {rendervalue[a], " != ", rendervalue[b]};

             renderbool[Or[a_,b_]] := {"(",renderbool[a]," || ", renderbool[b],")"};
             renderbool[And[a_,b_]] := {"(",renderbool[a]," && ", renderbool[b],")"};
             renderbool[Not[a_]] := {"(!", renderbool[a],")"};
             renderbool[a:paramPattern] := ToString[a/.(Parameter[x_]->x)]; (* Boolean parameter *)

             (* rendervalue[a_String] := a; -- Allow literal pass-through *)
             rendervalue[a_?NumberQ] := ToString[a];
             rendervalue[Parameter[a_String]] := a;
             rendervalue[a_ /; MemberQ[params,a]] := ToString[a];
             renderbool[x_] := ThrowError["Unexpected value in run-time conditional expression (boolean):", x, "in", cond];
             render[x_] := ThrowError["Unexpected value in run-time conditional expression (value):", x, "in", cond];

             unparen[s_] := 
             Module[
               {s2 = FlattenBlock[s],result},
               result = StringReplace[FlattenBlock[s2],StartOfString ~~ "(" ~~ any__ ~~ ")" ~~ EndOfString :> any];
               If[result === s2, result, unparen[result]]];

             ConditionalOnParameterTextual[unparen@renderbool[cond], w]],
           w];

    y];


(* Schedule a schedule group.  Use a slightly dirty trick; given that
   the structure is identical to that for a function except with the
   word "GROUP" added before the function name, just use the existing
   function. *)
scheduleGroup[spec_,params_] :=
  scheduleFunction[mapReplace[spec, Name, "group " <> lookup[spec, Name]],params];

(* Taking a list of group storage specifications for global storage,
   and lists of scheduled function and scheduled group structures,
   return a CodeGen block representing a schedule.ccl file. *)
CreateSchedule[globalStorageGroups_, scheduledGroups_, scheduledFunctions_, params_] :=
  {FileHeader["CCL"],
   Map[SeparatedBlock[groupStorage[#]]            &, globalStorageGroups],
   Map[SeparatedBlock[scheduleFunction[#,params]] &, scheduledFunctions],
   Map[SeparatedBlock[scheduleGroup[#,params]]    &, scheduledGroups]};


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
   Symmetries Registration
   ------------------------------------------------------------------------ *)

(* Symmetries registration spec = {FullName -> "impl::GFname", 
                                    Sym      -> {symX, symY, symZ}} *)

SymmetriesBlock[spec_] :=

  Module[{i, KrancDim},

  If[!MatchQ[spec, {FullName -> _String, Sym -> {_,_,_}}],
    ThrowError["SymmetriesBlock: Expecting a symmetry registration spec but got ", spec]];

  KrancDim = 3;

  sym = lookup[spec, Sym];

  {Table["sym[" <> ToString[i - 1]   <> "] = " <> 
                   ToString@sym[[i]] <> ";\n", {i, 1, KrancDim}],

  "SetCartSymVN(cctkGH, sym, \"" <> lookup[spec, FullName] <> "\");\n\n"
}
];

(* syms is a list of rules mapping gridfunctions to their symmetry structures *)
calcSymmetry[gf_, syms_] :=
  Module[{},
    If[mapContains[syms, gf],
      Return[lookup[syms,gf]],
      (* FIXME: We are defaulting to scalar symmetries if no information is
         available.  This shouldn't happen, but I am bypassing this check
         temporarily. *)
      Print["WARNING: defaulting to symmetries of a scalar for "<>ToString[gf]];
      Return[{1,1,1}]]];


(* This function guesses the symmetries based on component names as we
   have not been given them *)
calcSymmetry[gf_] := Module[{sym, q, string},

sym = {1, 1, 1};  (* default *)

string = ToString@gf;

While[IntegerQ[q = ToExpression@StringTake[string, -1]], 

Module[{},
    sym[[q]] = -sym[[q]];
    string   = StringDrop[string, -1]
    ]
];
sym
];

(* Compatibility function to be called by KrancThorns because it
   doesn't understand reflection symmetries *)
CreateSymmetriesRegistrationSource[thornName_, implementationName_, GFs_, debug_] :=
  CreateSymmetriesRegistrationSource[thornName, implementationName, GFs, False, debug];



(* Given a symmetries registration structure as defined above, return a
   C CodeGen structure of a source file which will register the symmetries. *)
CreateSymmetriesRegistrationSource[thornName_, implementationName_, GFs_, reflectionSymmetries_, debug_] :=
  Module[{spec, j, lang, tmp},

  If[debug,
      Print["Registering Symmetries for: ", GFs];
    ];

  lang = CodeGenC`SOURCELANGUAGE;
  CodeGenC`SOURCELANGUAGE = "C";

  spec = Table[{FullName -> implementationName <> "::" <> ToString@GFs[[j]],
                Sym      -> If[reflectionSymmetries === False,
                              calcSymmetry[GFs[[j]]],
                              calcSymmetry[GFs[[j]], Union@reflectionSymmetries]]}, {j, 1, Length@GFs}];

  tmp = {FileHeader["C"],

   Map[IncludeFile, 
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h", "Symmetry.h"}],

   DefineCCTKFunction[ thornName <> "_RegisterSymmetries", "void", 
     If[Length[spec] > 0,
     {CommentedBlock["array holding symmetry definitions",

      "CCTK_INT sym[3];\n\n"],

      CommentedBlock["Register symmetries of grid functions",

      Map[SymmetriesBlock, spec]]},
     {}]
]
  };

  CodeGenC`SOURCELANGUAGE = lang;

tmp
];


(* ------------------------------------------------------------------------ 
   MoL Registration
   ------------------------------------------------------------------------ *)

(* MoL registration = {EvolvedGFs -> {h11, ...}, PrimitiveGFs -> {trK, ...}, 
                       BaseImplementation -> "ADMBase", ThornName -> "ADMMoL"} *)

(* Given a MoL registration structure as defined above, return a
   CodeGen structure of a source file which will register the
   variables given with MoL. *)
CreateMoLRegistrationSource[spec_, debug_] :=

  Module[{tmp, lang},

  If[debug,
    Print["Registering for MoL:"];
    Print[];
    Print["  Evolved   Gridfunctions: ", lookup[spec, EvolvedGFs]   ];
    Print["  Primitive Gridfunctions: ", lookup[spec, PrimitiveGFs] ];
    ];
	
    lang = CodeGenC`SOURCELANGUAGE;
    CodeGenC`SOURCELANGUAGE= "C";

    tmp = {FileHeader["C"],

    Map[IncludeFile, 
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],

    DefineCCTKFunction[lookup[spec,ThornName] <> "_RegisterVars", "void", 
      {DefineVariable["ierr", "CCTK_INT", "0"],

      CommentedBlock["Register all the evolved grid functions with MoL",

(* FIXME: We should clarify exactly what should happen with the implementation names here *)

(* OK. I think that the group name should be passed in qualified, as that is all that we can do. *)

      Map[{"ierr += MoLRegisterEvolved(CCTK_VarIndex(\"", #, "\"),  CCTK_VarIndex(\"",
            #, "rhs\"));\n"} &,
          lookup[spec, EvolvedGFs]]],

      CommentedBlock["Register all the evolved Array functions with MoL",
      Map[{"ierr += MoLRegisterEvolved(CCTK_VarIndex(\"", #, "\"),  CCTK_VarIndex(\"",
            #, "rhs\"));\n"} &,
          lookup[spec, EvolvedArrays]]],

      (* Registering all the remaining variables as constrained is
      just plain wrong.  Read the MoL documentation.  It is also not
      harmless, I think. *)

      (*CommentedBlock["Register all the primitive grid functions with MoL",
      (* We should check ierr *)
      Map[{"ierr += MoLRegisterConstrained(CCTK_VarIndex(\"", 
           #, "\"));\n"} &,
          lookup[spec, PrimitiveGFs]]],  *)
	"return;\n"}]};

      CodeGenC`SOURCELANGUAGE = lang;

tmp
];

(* ------------------------------------------------------------------------
   MoL Boundaries
   ------------------------------------------------------------------------ *)

(* boundaries spec = {Groups -> {trK, h11, ...},
                       BaseImplementation -> "ADMBase", ThornName -> "ADMMoL"} *)

(* the boundary treatment is split into 3 steps:
  1. excision                                      
  2. symmetries                                    
  3. "other" boundary conditions, e.g. flat, radiative   

To simplify scheduling, testing, etc. the 3 steps are currently applied in separate functions!*)

(* boundary conditions may have to be applied per GF or goup ; per group
should be more efficient, but sometimes there will be a GF-dependent parameter,
e.g. for radiation BCs *)

cleanCPP[x_] := Map[StringReplace[FlattenBlock[#],  "  #" -> "#"]&, x];

(* Given a BC registration structure as defined above, return a
   CodeGen structure of a source file which does nothing yet! *)
CreateMoLBoundariesSource[spec_] :=

  Module[{gfs, groups, unqualifiedGroups, tmp, lang},

  gfs = lookup[spec, EvolvedGFs];
  groups =  lookup[spec, Groups];  
  unqualifiedGroups = Map[unqualifiedGroupName, lookup[spec, Groups]];
  
    listBCparfileEntry[gforgroup_] := Module[{prefix, unqualName},
    (* include a comment block with template parameter file entries *)
    prefix =  "#$bound$#" <> lookup[spec, ThornImplementation] <> "::";
    unqualName = unqualifiedGroupName@ToString@gforgroup;

    {
     prefix <> unqualName <> "_bound       = \"skip\"\n",
     prefix <> unqualName <> "_bound_speed = 1.0\n",
     prefix <> unqualName <> "_bound_limit = 0.0\n",
     prefix <> unqualName <> "_bound_scalar = 0.0\n\n"
    }];


(*
    symmetryBCGroup[group_] := Module[{boundpar, fullgroupname},
    (* symmetry boundary conditions *)

    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];

    {"\n",

     "  ierr = CartSymGN(cctkGH, \"" <> fullgroupname <> "\");\n",

     "  if (ierr < 0)\n",
     "     CCTK_WARN(0, \"Failed to apply symmetry BC for " <> fullgroupname <> "!\");\n"}
     ];
*)

    trivialBCGroup[group_] := Module[{boundpar, fullgroupname},
    (* boundary conditions that do not have parameters besides their name *)

    boundpar      = unqualifiedGroupName@group <> "_bound";
    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"none\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"static\") ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"flat\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"zero\"  ) )\n",
     "{\n",

     "  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1,\n",
     "                    \"" <> fullgroupname <> "\", " <> boundpar <> ");\n",

     "  if (ierr < 0)\n",
     "     CCTK_WARN(0, \"Failed to register "<>boundpar<>" BC for "<>fullgroupname<>"!\");\n",

     "}\n"}];


    trivialBCGF[gf_] := Module[{boundpar, fullgfname},
    (* boundary conditions that do not have parameters besides their name *)

    boundpar   = unqualifiedGroupName@ToString@gf <> "_bound";
    fullgfname = ToString@gf;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"none\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"static\") ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"flat\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"zero\"  ) )\n",
     "{\n",

     "  ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1,\n",
     "                    \"" <> fullgfname <> "\", " <> boundpar <> ");\n",

     "  if (ierr < 0)\n",
     "     CCTK_WARN(0, \"Failed to register "<>boundpar<>" BC for "<>fullgfname<>"!\");\n",

     "}\n"}];

    radiationBCGroup[group_] := Module[{boundpar, fullgroupname, myhandle},
    (* a simple radiation boundary condition *)

    boundpar      = unqualifiedGroupName@ToString@group <> "_bound";
    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];

    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"radiative\"))\n",
     "{\n /* select radiation boundary condition */\n  ",

      DefineVariable[myhandle, "static CCTK_INT", "-1"],

      "  if ("<>myhandle<>" < 0) "<>myhandle<>" = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);\n",

      "  if ("<>myhandle<>" < 0) CCTK_WARN(0, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" , "<> boundpar <>"_limit, \"LIMIT\") < 0)\n",
      "     CCTK_WARN(0, \"could not set LIMIT value in table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_speed, \"SPEED\") < 0)\n",
      "     CCTK_WARN(0, \"could not set SPEED value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgroupname <> "\", \"Radiation\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(0, \"Failed to register Radiation BC for "<>fullgroupname<>"!\");\n",

      "\n}\n"}];


    radiationBCGF[gf_] := Module[{boundpar, fullgfname, myhandle},
    (* a simple radiation boundary condition *)

    boundpar   = unqualifiedGroupName@ToString@gf <> "_bound";
    fullgfname = ToString@gf;

    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"radiative\"))\n",
     "{\n /* select radiation boundary condition */\n  ",

      DefineVariable[myhandle, "static CCTK_INT", "-1"],

      "  if ("<>myhandle<>" < 0) "<>myhandle<>" = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);\n",

      "  if ("<>myhandle<>" < 0) CCTK_WARN(0, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" , "<> boundpar <>"_limit, \"LIMIT\") < 0)\n",
      "     CCTK_WARN(0, \"could not set LIMIT value in table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_speed, \"SPEED\") < 0)\n",
      "      CCTK_WARN(0, \"could not set SPEED value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgfname <> "\", \"Radiation\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(0, \"Failed to register Radiation BC for "<>fullgfname<>"!\");\n",

     "\n}\n"}];

    scalarBCGroup[group_] := Module[{boundpar, fullgroupname, myhandle},
    (* simple dirichlet boundary condition *)

    boundpar      = unqualifiedGroupName@group <> "_bound";
    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];
    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"scalar\"))\n",
     "{\n /* select scalar boundary condition */\n  ",

      DefineVariable[myhandle, "static CCTK_INT", "-1"],

      "  if ("<>myhandle<>" < 0) "<>myhandle<>" = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);\n",

      "  if ("<>myhandle<>" < 0) CCTK_WARN(0, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_scalar, \"SCALAR\") < 0)\n",
      "      CCTK_WARN(0, \"could not set SCALAR value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgroupname <> "\", \"scalar\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(0, \"Failed to register Scalar BC for "<>fullgroupname<>"!\");\n",

      "\n}\n"}];


    scalarBCGF[gf_] := Module[{boundpar, fullgfname, myhandle},
    (* simple dirichlet boundary condition *)

    boundpar   = unqualifiedGroupName@ToString@gf <> "_bound";
    fullgfname = ToString@gf;
    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"scalar\"))\n",
     "{\n /* select scalar boundary condition */\n  ",

      DefineVariable[myhandle, "static CCTK_INT", "-1"],

      "  if ("<>myhandle<>" < 0) "<>myhandle<>" = Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE);\n",

      "  if ("<>myhandle<>" < 0) CCTK_WARN(0, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_scalar, \"SCALAR\") < 0)\n",
      "    CCTK_WARN(0, \"could not set SCALAR value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgfname <> "\", \"scalar\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(0, \"Error in registering Scalar BC for "<>fullgfname<>"!\");\n",

     "\n}\n"}];


   lang = CodeGenC`SOURCELANGUAGE;
   CodeGenC`SOURCELANGUAGE = "C";

  tmp = {FileHeader["C"],


   Map[IncludeFile,
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h", 
         "cctk_Faces.h", "util_Table.h", "Symmetry.h"}],

   {"\n\n",
      "/* the boundary treatment is split into 3 steps:    */\n",
      "/* 1. excision                                      */\n",
      "/* 2. symmetries                                    */\n",
      "/* 3. \"other\" boundary conditions, e.g. radiative */\n\n",
      "/* to simplify scheduling and testing, the 3 steps  */\n",
      "/* are currently applied in separate functions      */\n\n"},


   cleanCPP@DefineCCTKFunction[lookup[spec,ThornName] <> "_CheckBoundaries",
   "void",
     {"return;\n"}],


   cleanCPP@DefineCCTKFunction[lookup[spec,ThornName] <> "_SelectBoundConds", 
   "void",
     {DefineVariable["ierr",   "CCTK_INT", "0"],

(*
       Map[symmetryBCGroup,  groups],
*)

       Map[trivialBCGroup,   groups],
       Map[trivialBCGF,      gfs],

       Map[radiationBCGroup, groups],
       Map[radiationBCGF,    gfs],

       Map[scalarBCGroup,    groups],
       Map[scalarBCGF,       gfs],

      "return;\n"
     }],

     "\n\n\n",
     "/* template for entries in parameter file:\n",
      Map[listBCparfileEntry, unqualifiedGroups],
      Map[listBCparfileEntry, gfs],
     "*/\n\n"
     };

	 CodeGenC`SOURCELANGUAGE = lang;
tmp
];

CreateMoLExcisionSource[spec_] :=

  Module[{gfs, currentlang, body, excisionExtrap},

  gfs = lookup[spec, ExcisionGFs];
 
  Print["Applying excision to GFs: ", gfs];
 
  currentlang = CodeGenC`SOURCELANGUAGE;
  CodeGenC`SOURCELANGUAGE = "Fortran";

  excisionExtrap[gf_] :=  "  call ExcisionExtrapolate(ierr, "
    <> ToString@gf <> ", " <> ToString@gf
    <> "_p, emask, exnormx, exnormy, exnormz, nx, ny, nz, "<> ToString@gf  <> "_bound_limit)\n";

  body = {FileHeader["Fortran"],

   Map[IncludeFile,
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],

   {"\n\n",
      "! the boundary treatment is split into 3 steps:    \n",
      "! 1. excision                                      \n",
      "! 2. symmetries                                    \n",
      "! 3. \"other\" boundary conditions, e.g. radiative \n",
      "! to simplify scheduling and testing, the 3 steps  \n",
      "! are currently applied in separate functions      \n\n"},

   cleanCPP@DefineCCTKSubroutine[lookup[spec,ThornName] <> "_FindBoundary",
     {"! APPLY EXCISION\n\n",
      DefineVariable["ierr", "CCTK_INT :: ", "0"],
      "",

      "integer  :: nx, ny, nz\n\n",

      "! grid parameters\n",

      "nx = cctk_lsh(1)\n",
      "ny = cctk_lsh(2)\n",
      "nz = cctk_lsh(3)\n\n",

      "if ( (excision .ne. 0).AND.(find_excision_boundary .ne. 0) ) then\n\n",

      "  call ExcisionFindBoundary(ierr, emask, nx, ny, nz)\n",
      "  if (ierr < 0) call CCTK_WARN(2, \"findboundary exited with an error\")\n\n",

     "endif\n\n",
     "return\n"}],

   cleanCPP@DefineCCTKSubroutine[lookup[spec,ThornName] <> "_FindNormals",
     {"! APPLY EXCISION\n\n",
      DefineVariable["ierr", "CCTK_INT :: ", "0"],
      "",

      "integer  :: nx, ny, nz\n\n",

      "! grid parameters\n",

      "nx = cctk_lsh(1)\n",
      "ny = cctk_lsh(2)\n",
      "nz = cctk_lsh(3)\n\n",

      "if ( (excision .ne. 0).AND.(find_excision_normals .ne. 0) ) then\n\n",

      "  call ExcisionFindNormals(ierr, emask, exnormx, exnormy, exnormz, nx, ny, nz)\n",
      "  if (ierr < 0) call CCTK_WARN(2, \"findnormals exited with an error\")\n\n",

     "endif\n\n",
     "return\n"}],


   cleanCPP@DefineCCTKSubroutine[lookup[spec,ThornName] <> "_ApplyExcision",
     {"! APPLY EXCISION\n\n",
      DefineVariable["ierr", "CCTK_INT :: ", "0"],
      "",

      "integer  :: nx, ny, nz\n\n",

      "! grid parameters\n",

      "nx = cctk_lsh(1)\n", 
      "ny = cctk_lsh(2)\n", 
      "nz = cctk_lsh(3)\n\n", 

      "if (excision .ne. 0) then\n",

      "  call CCTK_INFO(\"Applying LegoExcision\")\n\n",

     Map[excisionExtrap, gfs],
     "endif\n\n",
     "return\n"}]
};

CodeGenC`SOURCELANGUAGE = currentlang;

body
];



(* ------------------------------------------------------------------------ *)
(*   set Characteristic Info for MultiPatch                                 *)
(* ------------------------------------------------------------------------ *)

(* boundaries spec = {TO BE DEFINED} *)

charInfoFunction[type_, spec_, debug_]:= Module[{funcName, argString, headerComment1, headerComment2,
                                                 thornName, gfs, rhs, groups, tmp, lang, numvars, tab},

  gfs = Map[ToString, lookup[spec, EvolvedGFs]];
  rhs = Map[AddSuffix[#, "rhs"]&, gfs];

  Print["createCharInfoFunction with type:\n", type];

  thornName = lookup[spec, Name];
 
  numvars = Length@gfs;

  groups = Map[unqualifiedGroupName, lookup[spec, Groups]];

  tab = "\t\t\t";

If[type == "P2C", 

   funcName = lookup[spec,Name] <> "_MultiPatch_Prim2Char";
   argString =  "const CCTK_POINTER_TO_CONST cctkGH_,\n"      <>
        tab <>  "const CCTK_INT dir,\n"                       <>
        tab <>  "const CCTK_INT face,\n"                      <>
        tab <>  "const CCTK_REAL* restrict const base,\n"     <>
        tab <>  "const CCTK_INT* restrict const off,\n"       <>
        tab <>  "const CCTK_INT* restrict const len,\n"       <>
        tab <>  "const CCTK_INT rhs_flag,\n"                  <>
        tab <>  "const CCTK_INT num_modes,\n"                 <>
        tab <>  "const CCTK_POINTER* restrict const modes,\n" <>
        tab <>  "const CCTK_POINTER* restrict const speeds";

   headerComment1 = "/* translate from primary to characteristic variables           */\n";
   headerComment2 = "/* Output:                                                      */\n" <>
                    "/*       CCTK_POINTER ARRAY IN modes  ... array if char. vars   */\n" <>    
                    "/*       CCTK_POINTER ARRAY IN speeds ... array of char. speeds */\n\n";
];


If[type == "C2P", 

   funcName = lookup[spec,Name] <> "_MultiPatch_Char2Prim";
   argString =  "const CCTK_POINTER_TO_CONST cctkGH_,\n"      <>
        tab <>  "const CCTK_INT dir,\n"                       <>
        tab <>  "const CCTK_INT face,\n"                      <>
        tab <>  "const CCTK_REAL* restrict const base,\n"     <>
        tab <>  "const CCTK_INT* restrict const off,\n"       <>
        tab <>  "const CCTK_INT* restrict const len,\n"       <>
        tab <>  "const CCTK_INT rhs_flag,\n"                  <>
        tab <>  "const CCTK_INT num_modes,\n"                 <>
        tab <>  "const CCTK_POINTER_TO_CONST* restrict const modes";

   headerComment1 = "/* translate from characteristic to primary variables          */\n";
   headerComment2 = "/* Output:                                                     */\n" <>  
                    "/*       CCTK_POINTER ARRAY IN modes   ... array of char. vars */\n\n";
];




code = {

DefineFunction[funcName, "CCTK_INT", argString,

{headerComment1,
"/* Input:                                                         */\n",
"/*       CCTK_POINTER_TO_CONST cctkGH   ... CCTK grid hierarchy   */\n",
"/*       CCTK_INT              dir      ...              */\n",
"/*       CCTK_INT              face     ...              */\n",
"/*       CCTK_REAL ARRAY       base     ...              */\n",
"/*       CCTK_INT ARRAY        lbnd     ...              */\n",
"/*       CCTK_INT ARRAY        lsh      ...              */\n",
"/*       CCTK_INT ARRAY        from     ...              */\n",
"/*       CCTK_INT ARRAY        to       ...              */\n",
"/*       CCTK_INT              rhs_flag ...              */\n",
"/*       CCTK_INT              num_modes...              */\n",
headerComment2,
"{\n",
"  const cGH* restrict const cctkGH = cctkGH_;\n",
"  DECLARE_CCTK_ARGUMENTS;\n",
"  DECLARE_CCTK_PARAMETERS;\n\n",

"  const CCTK_REAL* restrict prims["   <> ToString@numvars <> "];\n",
"  CCTK_REAL      * restrict chars["   <> ToString@numvars <> "];\n",
"  CCTK_REAL      * restrict cspeeds[" <> ToString@numvars <> "];\n",

"  CCTK_REAL normal[3], normal_base[3];\n",
"  CCTK_REAL tangent[2][3];\n",

 (* "  CCTK_REAL gama[3][3], gamau[3][3], beta[3], alfa;\n", *)
"  CCTK_REAL lambda[" <> ToString@numvars <> "];\n",

"  CCTK_REAL xform[" <> ToString@numvars <> "][" <> ToString@numvars <> "];\n",
"  CCTK_REAL norm_normal;\n",
"  CCTK_REAL norm_tangent[2];\n",

"  int n, m;                     /* mode               */\n",
"  int i, j, k;                  /* grid point indices */\n",
"  int d, e;                     /* dimension          */\n\n",

"  /* Check arguments */\n",
"  assert (cctkGH);\n",
"  assert (cctk_dim == 3);\n",
"  assert (dir  >= 0 && dir  < cctk_dim);\n",
"  assert (face >= 0 && face < 2);\n",
"  assert (base);\n",
"  assert (off);\n",
"  assert (len);\n\n",

"  for (d = 0; d < 3; ++d) {\n",
"    assert (off[d] >= 0 && len[d] >= 0 && off[d] + len[d] <= cctk_lsh[d]);\n",
"  }\n\n",

"  assert (modes);\n",
"  assert (speeds);\n",

"  for (d = 0; d < 3; ++d) {\n",
"    normal_base[d] = base[d];\n",
"    tangent[ 0][d] = base[  cctk_dim+d];\n",
"    tangent[ 1][d] = base[2*cctk_dim+d];\n",
"  }\n\n",

"  {\n",
"    CCTK_REAL normal_length = 0;\n",
"    for (d = 0; d < 3; ++d) {\n",
"      normal_length += fabs(normal_base[d]);\n",
"    }\n",
"    assert (normal_length > 0);\n",
"  }\n\n",

"  assert (num_modes == " <> ToString@numvars <> ");\n",

"  for (n = 0; n < num_modes; ++n) {\n",
"    assert (modes[n]);\n",
"  }\n\n",

"  /* Get variable pointers */\n",
"  if (rhs_flag) {\n",
Table["    rhs[" <> ToString[i-1] <> "] = CCTK_VarIndex(\"" <> ToString@rhs[[i]] <> "\");\n", 
      {i, 1, numvars}],
"  } else {\n",
Table["    prim[" <> ToString[i-1] <> "] = CCTK_VarIndex(\"" <> gfs[[i]] <> "\");\n", 
      {i, 1, numvars}],
"  }\n\n",
"    for (n = 0; n < num_vars ; ++n) {\n",
"      chars[  n] = modes[ n];\n",
"      cspeeds[n] = speeds[n];\n",
"    }\n\n",
"/* compute characteristic variables and speeds */\n",





"  /* Return 0 for Success! */\n",
"  return 0;\n"}]
(* this was it, let`s go for a beer *)
};

code];

CreateMPCharSource[spec_, debug_] :=

  Module[{thornName, gfs, rhs, groups, tmp, lang, numvars},

  gfs = Map[ToString, lookup[spec, EvolvedGFs]];
  rhs = Map[AddSuffix[#, "rhs"]&, gfs];

  Print["CreateMPCharSource uses RHS GFs:\n", rhs];

  thornName = lookup[spec, Name];
 
  numvars = Length@gfs;

  groups = Map[unqualifiedGroupName, lookup[spec, Groups]];

  lang = CodeGenC`SOURCELANGUAGE;
  CodeGenC`SOURCELANGUAGE = "C";

  tmp = {FileHeader["C"],


   Map[IncludeFile,
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],
   Map[IncludeSystemFile,
        {"assert.h", "math.h"}],

(* declare lapack function DGESV: compute solution to system of linear equations  E * X = B *)
{"\n/* declare lapack function DGESV for solving linear systems */\n",
"void CCTK_FCALL\n",
"CCTK_FNAME(dgesv) (const int* n,\n",
"                   const int* nrhs,\n",
"                   double   * a,\n",
"                   const int* lda,\n",
"                   int      * ipiv,\n",
"                   double   * b,\n",
"                   const int* ldb,\n",
"                   int      * info);\n\n\n"},

DefineFunction[lookup[spec,Name] <> "_MultiPatch_SystemDescription", "CCTK_INT", 
   "const CCTK_POINTER_TO_CONST cctkGH_, const CCTK_INT nvars,\n"     <>
   "    CCTK_INT* restrict const prim, CCTK_INT* restrict const rhs,\n" <>
   "    CCTK_REAL* restrict const sigma", 

{
"/* this function is called twice:                                            */\n",
"/* first to set the number of modes, then to set the rest of the information */\n",
"  const cGH* restrict const cctkGH = cctkGH_;\n",
"  DECLARE_CCTK_PARAMETERS;\n\n",
"  int n;\n\n",
"  /* Check arguments */\n",
"  assert (cctkGH);\n",
"  assert (nvars >= 0);\n\n",
"  /* Fill in return values on second call */\n",
"  if (nvars == " <> ToString@numvars <> ") {\n",
"    assert (prim);\n\n",
Table["    prim[" <> ToString[i-1] <> "] = CCTK_VarIndex(\"" <> gfs[[i]] <> "\");\n", {i,1,numvars}],
"\n",
"    for (n = 0; n < " <> ToString@numvars <> "; ++n) {\n",
"      assert (prim[n] >= 0);\n",
"    }\n\n",
"    assert (rhs);\n\n",
Table["    rhs[" <> ToString[i-1] <> "] = CCTK_VarIndex(\"" <> ToString@rhs[[i]] <> "\");\n", {i,1,numvars}],
"\n",
"    for (n = 0; n < " <> ToString@numvars <> "; ++n) {\n",
"      assert (rhs[n] >= 0);\n",
"    }\n\n",
"  }\n\n",
"  /* Coefficient for the scalar product via SummationByParts Thorn */\n",
"  *sigma = GetScalProdCoeff();\n\n",
"  /* Return the number of modes -- needed at first call! */\n",
"  return " <> ToString@numvars <> ";\n"}],

(*  *)
charInfoFunction["P2C", spec, debug],      "\n\n",
charInfoFunction["C2P", spec, debug],      "\n\n",
charInfoFunction["WHATEVER", spec, debug]
};

CodeGenC`SOURCELANGUAGE = lang;
tmp
];


(* -------------------------------------------------------------------------- 
   Precompmacros
   -------------------------------------------------------------------------- *)

(* Argument to this is the same as for CreateSetterSource.  This is
   not implemented currently because the precomputations are performed
   in the setter file itself.  We want to change this for readability
   reasons.  The change will be to have this precompMacros.h file
   define a macro performing the precomputations for *each loop* in
   each function.  Then the setter source file just has a line
   invoking the macro in each loop. *)
CreatePrecompMacros[functions_] :=
  Module[{},
  {}];

(* ------------------------------------------------------------------------ 
   Startup file
   ------------------------------------------------------------------------ *)

CreateStartupFile[thornName_, bannerText_] :=
  Module[{tmp, lang},
  
  lang = CodeGenC`SOURCELANGUAGE;
  CodeGenC`SOURCELANGUAGE = "C";

  tmp = {FileHeader["C"],

   IncludeFile["cctk.h"],
   DefineFunction[thornName <> "_Startup", "extern \"C\" int", "void",
     {DefineVariable["banner", "const char*", Quote[bannerText]],
      "CCTK_RegisterBanner(banner);\n",
      "return 0;\n"}]};

  CodeGenC`SOURCELANGUAGE = lang;

  tmp
   ];

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
