
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

(* This package provides a set of functions to create the various
   parts of a Cactus thorn and assemble them. *)

BeginPackage["sym`"];

(* These symbols are used in this file.  Whenever this package is
   used, the symbols will be added into the sym` context.  You will
   need to make sure the sym` context is on your ContextPath to use
   the symbols without the sym` prefix. *)

{AccumulatorBase, Name, Type, Extend, Default, Comment, Range, Implementation, Group,
SchedulePoint, Language, SynchronizedGroups, StorageGroups,
Timelevels, VariableType, GridType, Visibility, Variables,
Implementations, Value, AllowedValues, UsedParameters, Description,
ExtendedParameters, NewParameters, Directory, Interface, Param,
Schedule, Sources, Makefile, Filename, Contents, ThornName,
BaseImplementation, EvolvedGFs, PrimitiveGFs, Groups, Calculation,
GridFunctions, Shorthands, Equations, Parameter, Value, UsesFunctions,
ArgString, Conditional, D1, D2, D3, D11, D22, D33, D21, D31, D32,
Textual, TriggerGroups, Include, RHSGroups};

{ExcisionGFs};

EndPackage[];

BeginPackage["Thorn`", "CodeGen`", "CalculationFunction`", "MapLookup`", "KrancGroups`", "Helpers`"];

(* These functions are externally visible, and comprise the public
   interface to this package. *)
CreateSchedule::usage = "Create the content of the schedule.ccl file.";
CreateMakefile::usage = "Create the content of the Cactus make.code.defn file.";
CreateInterface::usage = "Create the content of the interface.ccl file.";
CreateParam::usage = "Create the content of the param.ccl file.";
Quote::usage = ""; (* This should not be in this package *)
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

StartBAMProject::usage = "needs to be called before creating any BAM thorns";
CloseBAMProject::usage = "needs to be called after all BAM thorns have been created";

(* Ensure that we can refer to symbols without the `sym prefix *)
$ContextPath = Join[{"sym`"}, $ContextPath];

Begin["`Private`"];


(* ------------------------------------------------------------------------ 
   Miscellaneous definitions, could be moved elsewhere
   ------------------------------------------------------------------------ *)

(* date, user, etc. *)
date[] := ToString[Date[][[3]]] <> "/" <>
          ToString[Date[][[2]]] <> "/" <>
          ToString[Date[][[1]]]


dateLong[] := ToString[Date[][[3]]] <> "/" <>
              ToString[Date[][[2]]] <> "/" <>
              ToString[Date[][[1]]] <> "/" <> "   " <>
              ToString[Date[][[4]]] <> ":" <>
              ToString[Date[][[5]]] <> ":" <>
              ToString[Date[][[6]]];


user[] := ToString[<< "!whoami"];


whoWhen[lang_] := Module[{com1, com2},

com1 = "UNRECOGNIZED LANGUAGE";
com2 = "UNRECOGNIZED LANGUAGE";

If[(lang == "C" || lang == "c"),
   com1 = "/* ";  com2 = " */";
];

If[(lang == "Fortran" || lang == "FORTRAN" ||  lang == "F90"  || lang == "F95"),
   com1 = "!";  com2 = "";
];

If[(lang == "F77"),
   com1 = "C";  com2 = "";
];

If[(lang == "shell"),
   com1 = "#";  com2 = "";
];

{com1 <> " file produced by user " <> user[] <> ", " <> date[]       <> com2 <> "\n"  <>
 com1 <> " Produced with Mathematica Version " <> ToString[$Version] <> com2 <> "\n\n"<>
 com1 <> " Mathematica script written by Ian Hinder and Sascha Husa" <> com2 <> "\n\n"<>
 com1 <> " $Id" <> "$"                                               <> com2 <> "\n\n"}

];


whoWhenSeparator[lang_] := Module[{com1, com2},

com1 = "UNRECOGNIZED LANGUAGE";
com2 = "UNRECOGNIZED LANGUAGE";

If[(lang == "C" || lang == "c"),
    com1 = "/* ";  com2 = " */";
   ];

If[(lang == "Fortran" || lang == "FORTRAN" ||  lang == "F90"  || lang == "F95"),
    com1 = "!";  com2 = "";
  ];

If[(lang == "F77"),
    com1 = "C";  com2 = "";
  ];

If[(lang == "shell"),
    com1 = "#";  com2 = "";
  ];

{com1 <> " section produced by user " <> user[] <> ", " <> date[] <> com2 <> "\n\n"}
];

	       


(* ------------------------------------------------------------------------ 
   Makefile
   ------------------------------------------------------------------------ *)

(* Return a CodeGen block representing a makefile which refers to the
   list of filenames sourceFiles *)
CreateMakefile[sourceFiles_] :=
  {whoWhenSeparator["shell"],
   "SRCS = ", Map[{#, " "} &, sourceFiles]};

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

(* Return a CodeGen block which represents the quoted value of x *)
Quote[x_] := {"\"", x, "\""};

(* To be used for parameters; will quote it if it is a keyword *)
renderValue[type_, value_] :=
  If[type == "KEYWORD",
    Quote[value],
    value];

(* Return a block defining a parameter with the given
   parameterSpec (defined above).  This is used for defining new
   parameters, as well as extending existing ones. *)
parameterBlock[spec_] :=
  {"  AddPar(",
     lookup[spec, Name],           ", ",
     Quote@lookup[spec, Default],  ", ",
     Quote@lookup[spec, Description],
     ");\n"};

(* Given a parameterFileSpec structure, return a CodeGen block file *)
CreateParam[spec_] :=
  {Map[{parameterBlock[#]} &, lookupDefault[spec, NewParameters, {}]]};

(* ------------------------------------------------------------------------ 
   Interface file
   ------------------------------------------------------------------------ *)

(* The following "group" structure defines a Cactus group of variables
   to be included in an interface.ccl file.

  group:

  {Name -> "", VariableType -> "", Timelevels -> 2, GridType -> "GF",
   Comment -> "", Visibility -> "public"
   Variables -> {phi, h11, ...}} *)


bamTensorType[group_] := Module[{comps, indices, type},

    comps = Map[ToString,     group ] ;

    indices = Map[BreakTensorComponentName, comps];
    indices = Map[#[[2]] &, indices];

    type = "scalar"; (* the default *)

    If [Sort@indices == Sort@{"11", "21", "22", "31", "32", "33", "12", "13", "23"}, type = "ij"];
    If [Sort@indices == Sort@{"11", "21", "22", "31", "32", "33"},                   type = "ij+ji"];
    If [Sort@indices == Sort@{"33", "23", "22", "13", "12", "11"},                   type = "IJ+JI"];

    If [indices == {"1", "2", "3"}, type = "i"];
    If [indices == {"3", "2", "1"}, type = "I"];

    If[ValueQ@Global`UserSetTensorType[x], type = Global`UserSetTensorType[  Map[#[[1]] &, indices]  ] ];

    type
   ];
						


(* Given the specification of a group structure, return a CodeGen
   block for the interface.ccl file to define that group *)
interfaceGroupBlock[spec_] :=
  {"  AddVar(", lookup[spec, Name],              ", ",
   Quote@bamTensorType@lookup[spec, Variables],  ", ",
   Quote@lookup[spec, Comment], ");\n"};

CreateInterface[implementation_, inheritedImplementations_, includeFiles_, 
                groups_, opts___] :=
  {"/* Variable Parameter and Function declarations for ", implementation, " */\n",
   NewlineSeparated[Map[FlattenBlock[interfaceGroupBlock[#]] &, groups]]};
   
(* ------------------------------------------------------------------------ 
   Scheduling
   ------------------------------------------------------------------------ *)

(* storage group 

  (represents the fact that we want to allocate storage for a Cactus
  variable group with the given number of timelevels)

  {Group -> "admbase::metric", Timelevels -> 3, 
   Conditional -> {Parameter -> "", Value -> ""},
   Conditional -> {Textual -> "CCTK_EQUALS(name,value)"}}

   A "conditional" structure looks like this: {Parameter -> "", Value -> ""}

  scheduled function: (a function to be scheduled at a particular point)

  {Name -> "ADM_BSSN_CalcRHS_fn", SchedulePoint -> "in POSTINITIAL before ExternalLapse", 
   Language -> "C", Comment -> "", 
   (optional) SynchronizedGroups -> {ADM_BSSN_gamma, ...}, 
   (optional) Options -> {"meta", "level", ...},
   (optional) StorageGroups -> {Group -> "mygroup", Timelevels -> 1},
   (optional) Conditional -> {Parameter -> "", Value -> ""}}

  scheduled group:

  {... sameish}

*)

(* Given a function scheduling specification as defined above, return
   a CodeGen block to schedule the function for the schedule.ccl file *)
scheduleUnconditionalFunction[spec_] :=
  {"  AddFun(" <> lookup[spec,SchedulePoint] <> ", " <> lookup[spec, Name] <> ", " <> Quote[lookup[spec, Comment]], ");\n\n"};

(* Handle the aspect of scheduling the function conditionally *)
scheduleFunction[spec_] :=
  Module[{condition, parameter, value, u},

    u = scheduleUnconditionalFunction[spec];

       If[mapContains[spec, Conditional],

       (* Output the conditional structure *)
       condition = lookup[spec, Conditional];

       If[mapContains[condition, Textual],

         ConditionalOnParameterTextual[lookup[condition, Textual], u],
         
         If[mapContains[condition, Parameter],

            parameter = lookup[condition, Parameter];
            value     = lookup[condition, Value];
            ConditionalOnParameter[parameter, value, u],
              
            If[condition != {},
              Throw["Unrecognized conditional structure", condition],
              u]]],
        u]];

(* Taking a list of group storage specifications for global storage,
   and lists of scheduled function and scheduled group structures,
   return a CodeGen block representing a schdule.ccl file. *)
CreateSchedule[globalStorageGroups_, scheduledGroups_, scheduledFunctions_] :=
  {Map[SeparatedBlock[scheduleFunction[#]] &, scheduledFunctions]  };


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

calculationMacros[] :=
  CommentedBlock["Define macros used in calculations",
    Map[{"#define ", #, "\n"} &,
       {"INITVALUE  (42)",
        "INV(x) ((1.0) / (x))"   ,        
        "SQR(x) ((x) * (x))"   ,        
        "CUB(x) ((x) * (x) * (x))"   , 
        "QAD(x) ((x) * (x) * (x) * (x))"}]];

(* Given a list of Calculation structures as defined above, create a
   CodeGen representation of a source file that defines a function for
   each Calculation. *)

CreateSetterSource[calcs_, debug_, opts___] :=
  Module[{include},
  include = lookupDefault[{opts}, Include, {}];

  If[!MatchQ[include, _List],
    Throw["CreateSetterSource: Include should be a list but is in fact " <> ToString[include]]];

  {whoWhen[CodeGen`SOURCELANGUAGE],

   "#define KRANC_" <> ToUpperCase[CodeGen`SOURCELANGUAGE] <> "\n\n",

   If[CodeGen`SOURCELANGUAGE == "C",
         IncludeFile["math.h"],
         "\n"
      ],

   Map[IncludeFile, Join[{"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h",
                     "precomputations.h", "GenericFD.h", "Differencing.h"}, include]],
   calculationMacros[],

   (* For each function structure passed, create the function and
      insert it *)
   Map[CreateCalculationFunction[# , debug]& , 
       calcs]}];



(* ------------------------------------------------------------------------ 
   Symmetries Registration
   ------------------------------------------------------------------------ *)

(* Symmetries registration spec = {{FullName -> "impl::GFname", 
                                    Sym      -> {symX, symY, symZ}}, ...} *)

SymmetriesBlock[spec_] :=

  Module[{i, KrancDim},

  KrancDim = 3;

  sym = lookup[spec, Sym];

  {Table["sym[" <> ToString[i - 1]   <> "] = " <> 
                   ToString@sym[[i]] <> ";\n", {i, 1, KrancDim}],

  "SetCartSymVN(cctkGH, sym, \"" <> lookup[spec, FullName] <> "\");\n\n"
}
];


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


(* Given a symmetries registration structure as defined above, return a
   C CodeGen structure of a source file which will register the symmetries. *)
CreateSymmetriesRegistrationSource[thornName_, implementationName_, GFs_, debug_] :=

  Module[{spec, j, lang, tmp},

  If[debug,
      Print["Registering Symmetries for: ", GFs];
    ];

  lang = CodeGen`SOURCELANGUAGE;
  CodeGen`SOURCELANGUAGE = "C";

  spec = Table[{FullName -> implementationName <> "::" <> ToString@GFs[[j]],
                Sym      -> calcSymmetry[GFs[[j]] ]
               }, {j, 1, Length@GFs}];

  tmp = {whoWhen["C"],

   Map[IncludeFile, 
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h", "Symmetry.h"}],

   DefineCCTKFunction[ thornName <> "_RegisterSymmetries", "void", 
     {CommentedBlock["array holding symmetry definitions",

      "CCTK_INT sym[3];\n\n"],

      CommentedBlock["Register symmetries of grid functions",

      Map[SymmetriesBlock, spec]]}
]
  };

  CodeGen`SOURCELANGUAGE = lang;

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
	
    lang = CodeGen`SOURCELANGUAGE;
    CodeGen`SOURCELANGUAGE= "C";

    tmp = {whoWhen["C"],

    Map[IncludeFile, 
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],

    DefineCCTKFunction[lookup[spec,ThornName] <> "_RegisterVars", "CCTK_INT", 
      {DefineVariable["ierr", "CCTK_INT", "0"],

      CommentedBlock["Register all the evolved grid functions with MoL",

      Map[{"ierr += MoLRegisterEvolved(CCTK_VarIndex(\"", #, "\"),  CCTK_VarIndex(\"",
           lookup[spec,BaseImplementation], "::", unqualifiedGroupName[#], "rhs\"));\n"} &,
          lookup[spec, EvolvedGFs]]],

      CommentedBlock["Register all the primitive grid functions with MoL",
      Map[{"ierr += MoLRegisterConstrained(CCTK_VarIndex(\"", 
           lookup[spec,BaseImplementation], "::", #, "\"));\n"} &,
          lookup[spec, PrimitiveGFs]]],
	"return ierr;\n"}]};

      CodeGen`SOURCELANGUAGE = lang;

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

  Module[{gfs, groups, tmp, lang},

  gfs = lookup[spec, EvolvedGFs];
  groups = Map[unqualifiedGroupName, lookup[spec, Groups]];

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

    trivialBCGroup[group_] := Module[{boundpar, fullgroupname},
    (* boundary conditions that do not have parameters besides their name *)

    boundpar      = unqualifiedGroupName@group <> "_bound";
    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"none\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"static\") ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"flat\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"zero\"  ) ) \n",
     "{\n",

     "  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, -1, \n",
     "                    \"" <> fullgroupname <> "\", " <> boundpar <> ");\n",

     "  if (ierr < 0)\n",
     "     CCTK_WARN(-1, \"Failed to register "<>boundpar<>" BC for "<>fullgroupname<>"!\");\n",

     "}\n"}];


    trivialBCGF[gf_] := Module[{boundpar, fullgfname},
    (* boundary conditions that do not have parameters besides their name *)

    boundpar   = unqualifiedGroupName@ToString@gf <> "_bound";
    fullgfname = ToString@gf;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"none\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"static\") ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"flat\"  ) ||\n",
     "    CCTK_EQUALS(" <> boundpar <> ", \"zero\"  ) ) \n",
     "{\n",

     "  ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, -1, \n",
     "                    \"" <> fullgfname <> "\", " <> boundpar <> ");\n",

     "  if (ierr < 0)\n",
     "     CCTK_WARN(-1, \"Failed to register "<>boundpar<>" BC for "<>fullgfname<>"!\");\n",

     "}\n"}];

    radiationBCGroup[group_] := Module[{boundpar, fullgroupname, myhandle},
    (* a simple radiation boundary condition *)

    boundpar      = unqualifiedGroupName@ToString@group <> "_bound";
    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];

    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"radiative\"))\n",
     "{\n /* apply radiation boundary condition */\n  ",

      DefineVariable[myhandle, "CCTK_INT", "Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE)"],

      "  if ("<>myhandle<>" < 0) CCTK_WARN(-1, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" , "<> boundpar <>"_limit, \"LIMIT\") < 0)\n",
      "     CCTK_WARN(-1, \"could not set LIMIT value in table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_speed, \"SPEED\") < 0)\n",
      "     CCTK_WARN(-1, \"could not set SPEED value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgroupname <> "\", \"Radiation\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(-1, \"Failed to register Radiation BC for "<>fullgroupname<>"!\");\n",

      "\n}\n"}];


    radiationBCGF[gf_] := Module[{boundpar, fullgfname, myhandle},
    (* a simple radiation boundary condition *)

    boundpar   = unqualifiedGroupName@ToString@gf <> "_bound";
    fullgfname = ToString@gf;

    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"radiative\"))\n",
     "{\n /* apply radiation boundary condition */\n  ",

      DefineVariable[myhandle, "CCTK_INT", "Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE)"],

      "  if ("<>myhandle<>" < 0) CCTK_WARN(-1, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" , "<> boundpar <>"_limit, \"LIMIT\") < 0)\n",
      "     CCTK_WARN(-1, \"could not set LIMIT value in table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_speed, \"SPEED\") < 0)\n",
      "      CCTK_WARN(-1, \"could not set SPEED value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgfname <> "\", \"Radiation\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(-1, \"Failed to register Radiation BC for "<>fullgfname<>"!\");\n",

     "\n}\n"}];

    scalarBCGroup[group_] := Module[{boundpar, fullgroupnamei, myhandle},
    (* simple dirichlet boundary condition *)

    boundpar      = unqualifiedGroupName@group <> "_bound";
    fullgroupname = qualifyGroupName[ToString@group, lookup[spec, BaseImplementation]];
    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"scalar\"))\n",
     "{\n /* apply scalar boundary condition */\n  ",

      DefineVariable[myhandle, "CCTK_INT", "Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE)"],

      "  if ("<>myhandle<>" < 0) CCTK_WARN(-1, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_scalar, \"SCALAR\") < 0)\n",
      "      CCTK_WARN(-1, \"could not set SCALAR value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectGroupForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgroupname <> "\", \"scalar\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(-1, \"Failed to register Scalar BC for "<>fullgroupname<>"!\");\n",

      "\n}\n"}];


    scalarBCGF[gf_] := Module[{boundpar, fullgfname, myhandle},
    (* simple dirichlet boundary condition *)

    boundpar   = unqualifiedGroupName@ToString@gf <> "_bound";
    fullgfname = ToString@gf;
    myhandle = "handle_" <> boundpar;

    {"\n",
     "if (CCTK_EQUALS(" <> boundpar <> ", \"scalar\"))\n",
     "{\n /* apply scalar boundary condition */\n  ",

      DefineVariable[myhandle, "CCTK_INT", "Util_TableCreate(UTIL_TABLE_FLAGS_CASE_INSENSITIVE)"],

      "  if ("<>myhandle<>" < 0) CCTK_WARN(-1, \"could not create table!\");\n",

      "  if (Util_TableSetReal("<>myhandle<>" ," <> boundpar <> "_scalar, \"SCALAR\") < 0)\n",
      "    CCTK_WARN(-1, \"could not set SCALAR value in table!\");\n",

      "\n",
      "  ierr = Boundary_SelectVarForBC(cctkGH, CCTK_ALL_FACES, 1, "<>myhandle<>", \n",
      "                    \"" <> fullgfname <> "\", \"scalar\");\n\n",

      "  if (ierr < 0)\n",
      "     CCTK_WARN(-1, \"Error in registering Scalar BC for "<>fullgfname<>"!\");\n",

     "\n}\n"}];


   lang = CodeGen`SOURCELANGUAGE;
   CodeGen`SOURCELANGUAGE = "C";

  tmp = {whoWhen["C"],


   Map[IncludeFile,
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h", 
         "cctk_Faces.h", "util_Table.h"}],

   {"\n\n",
      "/* the boundary treatment is split into 3 steps:    */\n",
      "/* 1. excision                                      */\n",
      "/* 2. symmetries                                    */\n",
      "/* 3. \"other\" boundary conditions, e.g. radiative */\n\n",
      "/* to simplify scheduling and testing, the 3 steps  */\n",
      "/* are currently applied in separate functions      */\n\n"},


   cleanCPP@DefineCCTKFunction[lookup[spec,ThornName] <> "_CheckBoundaries",
   "CCTK_INT",
     {"return 0;\n"}],


   cleanCPP@DefineCCTKFunction[lookup[spec,ThornName] <> "_ApplyBoundConds", 
   "CCTK_INT",
     {DefineVariable["ierr",   "CCTK_INT", "0"],

       Map[trivialBCGroup,   groups],
       Map[trivialBCGF,      gfs],

       Map[radiationBCGroup, groups],
       Map[radiationBCGF,    gfs],

       Map[scalarBCGroup,    groups],
       Map[scalarBCGF,       gfs],

      "return ierr;\n"
     }],

     "\n\n\n",
     "/* template for entries in parameter file:\n",
      Map[listBCparfileEntry, groups],
      Map[listBCparfileEntry, gfs],
     "*/\n\n"
     };

	 CodeGen`SOURCELANGUAGE = lang;
tmp
];

CreateMoLExcisionSource[spec_] :=

  Module[{gfs, currentlang, body, excisionExtrap},

  gfs = lookup[spec, ExcisionGFs];
 
  Print["Applying excision to GFs: ", gfs];
 
  currentlang = CodeGen`SOURCELANGUAGE;
  CodeGen`SOURCELANGUAGE = "Fortran";

  excisionExtrap[gf_] :=  "  call ExcisionExtrapolate(ierr, "
    <> ToString@gf <> ", " <> ToString@gf
    <> "_p, emask, exnormx, exnormy, exnormz, nx, ny, nz, "<> ToString@gf  <> "_bound_limit)\n";

  body = {whoWhen["Fortran"],

   Map[IncludeFile,
        {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],

   {"\n\n",
      "! the boundary treatment is split into 3 steps:    \n",
      "! 1. excision                                      \n",
      "! 2. symmetries                                    \n",
      "! 3. \"other\" boundary conditions, e.g. radiative \n\n",
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

CodeGen`SOURCELANGUAGE = currentlang;

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
   argString =  "CCTK_POINTER_TO_CONST const cctkGH_,\n"       <>
        tab <>  "CCTK_INT const dir,\n"                        <>
        tab <>  "CCTK_INT const face,\n"                       <>
        tab <>  "CCTK_REAL const * restrict const base,\n"     <>
        tab <>  "CCTK_INT const * restrict const off,\n"       <>
        tab <>  "CCTK_INT const * restrict const len,\n"       <>
        tab <>  "CCTK_INT const rhs_flag,\n"                   <>
        tab <>  "CCTK_INT const num_modes,\n"                  <>
        tab <>  "CCTK_POINTER const * restrict const modes,\n" <>
        tab <>  "CCTK_POINTER const * restrict const speeds";

   headerComment1 = "/* translate from primary to characteristic variables           */\n";
   headerComment2 = "/* Output:                                                      */\n" <>
                    "/*       CCTK_POINTER ARRAY IN modes  ... array if char. vars   */\n" <>    
                    "/*       CCTK_POINTER ARRAY IN speeds ... array of char. speeds */\n\n";
];


If[type == "C2P", 

   funcName = lookup[spec,Name] <> "_MultiPatch_Char2Prim";
   argString =  "CCTK_POINTER_TO_CONST const cctkGH_,\n"       <>
        tab <>  "CCTK_INT const dir,\n"                        <>
        tab <>  "CCTK_INT const face,\n"                       <>
        tab <>  "CCTK_REAL const * restrict const base,\n"     <>
        tab <>  "CCTK_INT const * restrict const off,\n"       <>
        tab <>  "CCTK_INT const * restrict const len,\n"       <>
        tab <>  "CCTK_INT const rhs_flag,\n"                   <>
        tab <>  "CCTK_INT const num_modes,\n"                  <>
        tab <>  "CCTK_POINTER_TO_CONST const * restrict const modes";

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
"/*       CCTK_INT              rhs_flag ...              */\n",
"/*       CCTK_INT              num_modes...              */\n",
headerComment2,
"{\n",
"  cGH const * restrict const cctkGH = cctkGH_;\n",
"  DECLARE_CCTK_ARGUMENTS;\n",
"  DECLARE_CCTK_PARAMETERS;\n\n",

"  CCTK_REAL const * restrict prims["   <> ToString@numvars <> "];\n",
"  CCTK_REAL       * restrict chars["   <> ToString@numvars <> "];\n",
"  CCTK_REAL       * restrict cspeeds[" <> ToString@numvars <> "];\n",

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

  lang = CodeGen`SOURCELANGUAGE;
  CodeGen`SOURCELANGUAGE = "C";

  tmp = {whoWhen["C"],


   Map[IncludeFile,
        {"assert.h", "math.h", "cctk.h", "cctk_Arguments.h", "cctk_Parameters.h"}],

(* declare lapack function DGESV: compute solution to system of linear equations  E * X = B *)
{"\n/* declare lapack function DGESV for solving linear systems */\n",
"void CCTK_FCALL\n",
"CCTK_FNAME(dgesv) (int    const * n,\n",
"                   int    const * nrhs,\n",
"                   double       * a,\n",
"                   int    const * lda,\n",
"                   int          * ipiv,\n",
"                   double       * b,\n",
"                   int    const * ldb,\n",
"                   int          * info);\n\n\n"},

DefineFunction[lookup[spec,Name] <> "_MultiPatch_SystemDescription", "CCTK_INT", 
   "CCTK_POINTER_TO_CONST const cctkGH_, CCTK_INT const nvars,\n"     <>
   "    CCTK_INT * restrict const prim, CCTK_INT * restrict const rhs,\n" <>
   "    CCTK_REAL * restrict const sigma", 

{
"/* this function is called twice:                                            */\n",
"/* first to set the number of modes, then to set the rest of the information */\n",
"  cGH const * restrict const cctkGH = cctkGH_;\n",
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

CodeGen`SOURCELANGUAGE = lang;
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
  
  lang = CodeGen`SOURCELANGUAGE;
  CodeGen`SOURCELANGUAGE = "C";

  tmp = {whoWhen["C"],

   IncludeFile["cctk.h"],
   DefineFunction[thornName <> "_Startup", "int", "void",
     {DefineVariable["banner", "const char *", Quote[bannerText]],
      "CCTK_RegisterBanner(banner);\n",
      "return 0;\n"}]};

  CodeGen`SOURCELANGUAGE = lang;

  tmp
   ];

(* ------------------------------------------------------------------------ *)
   BAM 'Thorn' creation
(* ------------------------------------------------------------------------ *)

(* source = {Filename -> "MoLRegister.c", Contents -> "#include ..."} *)

(* thorn = {Name -> "ClassicADMMolEvolve", Directory -> "ClassicADM",
            Interface -> i, Schedule -> s, Param -> p, Makefile -> m, 
            Sources -> {s1, s2, ...} *)

(* Given a thorn specification structure as defined above, create a
   thorn.  Note that if you specify a path to the thorn, then you are
   responsible for making sure that the parent directory exists; this
   function does not automatically create any parent directories. *)
CreateThorn[thorn_] :=
  Module[{thornName, project, directory, bamC, bamH},


    lastDir[dir_] := StringDrop[dir, Last@Last@StringPosition[dir, "/"] ];

    thornName = lookup[thorn, Name];
    directory = lookup[thorn, Directory];
    project = lastDir@directory;

    Print["Creating thorns in directory ", directory];

    EnsureDirectory[directory];

    bamC = {"\n",
            whoWhenSeparator["C"],
            lookup[thorn, Interface], 
            lookup[thorn, Param], 
	    lookup[thorn, Schedule]};
   

    isFunEntry[x_]:= StringMatchQ[ToString@x, "*AddFun(*)*"];

    bamH = {"\n/* here we declare functions we have added with AddFun */\n"};
   
    AppendTo[bamH, Select[Flatten@bamC, isFunEntry]];

    AddToFile[directory <> "/bam_" <> project <> ".c", bamC];
    AddToFile[directory <> "/bam_" <> project <> ".h", bamH];
    
    Map[GenerateFile[directory <> "/" <> lookup[#, Filename], 
                                         lookup[#, Contents]] &,
                                         lookup[thorn, Sources]];

    AddToFile[directory <> "/Makefile", lookup[thorn, Makefile]]];


StartBAMProject[project_, directory_] :=
  Module[{bamC, bamH, bamM, cname, hname, mname, projectRootFunction},

      lastDir[dir_] := StringDrop[dir, Last@Last@StringPosition[dir, "/"] ];

      Print["Starting project " <> project <> " in directory ", directory];

      EnsureDirectory[directory];

      cname =  "bam_" <> project <> ".c";
      hname =  "bam_" <> project <> ".h";
      mname = "Makefile";
      
      projectRootFunction = "bam_" <> project;
      
      bamC = {"/* " <> cname  <> " */\n", 
              whoWhen["C"], 
	      "#include \"bam.h\"\n", 
	      "#include \"" <> hname <> "\"\n\n\n",
               "void " <> projectRootFunction <> "();\n{\n",
	       "   if (!Getv(\"physics\", \"" <> project <> "\")) return;\n",
	       "       printf(\"Adding " <> project <> "\");\n"
	       };

      bamH = {"/* " <> hname  <> " */\n", 
              whoWhen["C"], 
	      "void " <> projectRootFunction <> "();\n\n"};
     
     bamM = {"# " <> project <> "Makefile\n",
             whoWhen["SH"],
             "\n", 
             "NAME := " <> project,
             "\n", 
             "OBJS := bam_$(NAME).o", 
             "\n",
             "include $(TOP)/Makefile.subdirs"
             };

      GenerateFile[directory <> "/" <> cname, bamC];
      GenerateFile[directory <> "/" <> hname, bamH];
      GenerateFile[directory <> "/" <> mname, bamM];
    ];


CloseBAMProject[project_, directory_] :=
 Module[{bamC, bamH, bamM, cname, hname, mname, closingMsg},

      lastDir[dir_] := StringDrop[dir, Last@Last@StringPosition[dir, "/"] ];

      Print["Closing project in directory ", directory];
      EnsureDirectory[directory];

      cname =  "bam_" <> project <> ".c";
      hname =  "bam_" <> project <> ".h";
      mname = "Makefile";

      closingMsg = "File closed by CloseBAMProject";

      bamC = {"/* " <> closingMsg <> " */\n",
              "}\n"
             };

      bamH = {"\n/* " <> closingMsg  <> " */\n"};

      bamM = {"\n#" <> closingMsg <> "\n"};

      AddToFile[directory <> "/" <> cname, bamC];
      AddToFile[directory <> "/" <> hname, bamH];
      AddToFile[directory <> "/" <> mname, bamM];
    ];

End[];

EndPackage[];
