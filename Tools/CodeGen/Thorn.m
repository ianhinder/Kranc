
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
Textual, TriggerGroups};

{ExcisionGFs};

EndPackage[];

BeginPackage["Thorn`", "CodeGen`", "CalculationFunction`", "MapLookup`", "KrancGroups`"];

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
CreatePrecompMacros::usage = "";
CreateStartupFile::usage = "";

(* Ensure that we can refer to symbols without the `sym prefix *)
$ContextPath = Join[{"sym`"}, $ContextPath];

Begin["`Private`"];


(* ------------------------------------------------------------------------ 
   Miscellaneous definitions, could be moved elsewhere
   ------------------------------------------------------------------------ *)

(* Create a directory if it does not exist already *)
ensureDirectory[name_] :=
  If[FileType[name] == None,
     CreateDirectory[name]];


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

If[(lang == "shell" || lang == "CCL"),
   com1 = "#";  com2 = "";
];

{com1 <> " file produced by user " <> user[] <> ", " <> date[]       <> com2 <> "\n"  <>
 com1 <> " Produced with Mathematica Version " <> ToString[$Version] <> com2 <> "\n\n"<>
 com1 <> " Mathematica script written by Ian Hinder and Sascha Husa" <> com2 <> "\n\n"<>
 com1 <> " $Id" <> "$"                                               <> com2 <> "\n\n"}

];


(* ------------------------------------------------------------------------ 
   Makefile
   ------------------------------------------------------------------------ *)

(* Return a CodeGen block representing a makefile which refers to the
   list of filenames sourceFiles *)
CreateMakefile[sourceFiles_] :=
  {whoWhen["shell"],
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
  {lookup[spec, Type], " ", 
   lookup[spec, Name], " ", 
   Quote[lookup[spec, Description]], 

   If[mapContains[spec, AccumulatorBase],
      {" ACCUMULATOR-BASE=", lookup[spec, AccumulatorBase]},
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
  {whoWhen["CCL"],

    (* For each implementation defined in the spec, output a block
       which declares which parameters are used and extended by this
       implementation *)
    Map[parameterImplementationSection, 
        lookupDefault[spec, Implementations, {}]],

    (* For each new parameter being defined by this implementation,
       output a parameter block for it *)
    Map[{lookup[#, Visibility], ":\n", parameterBlock[#]} &, 
        lookupDefault[spec, NewParameters, {}]]};

(* ------------------------------------------------------------------------ 
   Interface file
   ------------------------------------------------------------------------ *)

(* The following "group" structure defines a Cactus group of variables
   to be included in an interface.ccl file.

  group:

  {Name -> "", VariableType -> "", Timelevels -> 2, GridType -> "GF",
   Comment -> "", Visibility -> "public"
   Variables -> {phi, h11, ...}} *)

(* Given the specification of a group structure, return a CodeGen
   block for the interface.ccl file to define that group *)
interfaceGroupBlock[spec_] :=
  {lookup[spec, Visibility], ":\n",
   lookup[spec, VariableType], " ", lookup[spec, Name], 
     " type=", lookup[spec,GridType], " ",
     "timelevels=", lookup[spec, Timelevels], "\n",
   SuffixedCBlock[{CommaNewlineSeparated[lookup[spec, Variables]],"\n"}, 
                  "\"" <> lookup[spec, Comment] <> "\""]};

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


(* Given the name of an implementation, a list of implementation names
   that we inherit from, a list of include files to mention, and a
   list of group structures as defined above, return a CodeGen block
   representing the interface.ccl file.  As an optional argument, one
   can specify Friends -> {list of implementations we want as
   friends}. Can also have UsesFunction -> {functions}*)
CreateInterface[implementation_, inheritedImplementations_, includeFiles_, 
                groups_, opts___] :=
  {whoWhen["CCL"],
   "implements: ", implementation, "\n\n",
   "inherits:   ", SpaceSeparated[inheritedImplementations], "\n\n",
   If[mapContains[{opts}, Friends],
     {"friend:     ", SpaceSeparated[lookup[{opts}, Friends]]},{}],
   "\n\n",
   Map[{"USES INCLUDE: ", #, "\n"} &, includeFiles],
   "\n",

   Map[usesFunction, lookupDefault[{opts}, UsesFunctions, {}]],


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
   (optional) StorageGroups -> {Group -> "mygroup", Timelevels -> 1},
   (optional) Conditional -> {Parameter -> "", Value -> ""}}

  scheduled group:

  {... sameish}

*)

(* Given a storage group structure defined above, return a CodeGen
   structure for inclusion in the schedule.ccl file to allocate
   storage for this group. *)
groupStorage[spec_] :=
  {"STORAGE: ", lookup[spec, Group], "[", lookup[spec, Timelevels], "]\n"}


(* Given a function scheduling specification as defined above, return
   a CodeGen block to schedule the function for the schedule.ccl file *)
scheduleUnconditionalFunction[spec_] :=
  {"schedule ", lookup[spec, Name], " ", lookup[spec,SchedulePoint], "\n",
   SuffixedCBlock[
     {If[lookup[spec, Language] == "None", "# no language specified\n",
                                   "LANG: " <> lookup[spec, Language] <> "\n\n"],

      (* Insert a SYNC line for each group we want to synchronize. *)
      Map[{"SYNC: ", #, "\n"} &, lookupDefault[spec, SynchronizedGroups, {}]],

      Map[{"TRIGGERS: ", #, "\n"} &, lookupDefault[spec, TriggerGroups, {}]],

      (* Insert a storage block for each group we want to allocate
         storage for *)
      Map[groupStorage, lookupDefault[spec, StorageGroups, {}]]},

     Quote[lookup[spec, Comment]]]};

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


(* Schedule a schedule group.  Use a slightly dirty trick; given that
   the structure is identical to that for a function except with the
   word "GROUP" added before the function name, just use the existing
   function. *)
scheduleGroup[spec_] :=
  scheduleFunction[mapReplace[spec, Name, "group " <> lookup[spec, Name]]];

(* Taking a list of group storage specifications for global storage,
   and lists of scheduled function and scheduled group structures,
   return a CodeGen block representing a schdule.ccl file. *)
CreateSchedule[globalStorageGroups_, scheduledGroups_, scheduledFunctions_] :=
  {whoWhen["CCL"],
   Map[SeparatedBlock[groupStorage[#]]     &, globalStorageGroups],
   Map[SeparatedBlock[scheduleFunction[#]] &, scheduledFunctions],
   Map[SeparatedBlock[scheduleGroup[#]]    &, scheduledGroups]};


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

CreateSetterSource[calcs_, debug_] :=
  {whoWhen[CodeGen`SOURCELANGUAGE],

   "#define KRANC_" <> ToUpperCase[CodeGen`SOURCELANGUAGE] <> "\n\n",

   If[CodeGen`SOURCELANGUAGE == "C",
         IncludeFile["math.h"],
         "\n"
      ],

   Map[IncludeFile, {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h",
                     "precomputations.h", "GenericFD.h", "Differencing.h"}],
   calculationMacros[],

   (* For each function structure passed, create the function and
      insert it *)
   Map[CreateCalculationFunction[# , debug]& , 
       calcs]};



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

      Map[{"ierr += MoLRegisterEvolved(CCTK_VarIndex(\"", 
           lookup[spec,BaseImplementation], "::", #, "\"), CCTK_VarIndex(\"",
           lookup[spec,BaseImplementation], "::", #, "rhs\"));\n"} &,
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
(* currently this only does periodic boundaries, i.e. provides
   a place for SYNCing evolution variables *)

(* boundaries spec = {Groups -> {trK, h11, ...},
                       BaseImplementation -> "ADMBase", ThornName -> "ADMMoL"} *)

(* the boundary treatment is split into 3 steps:
  1. excision                                      
  2. symmetries                                    
  3. "other" boundary conditions, e.g. flat, radiative   

To simplify scheduling, testing, etc. the 3 steps are currently applied in separate functions!*)

(* boundary conditions may have to be applied per GF or goup ; per group
should be more efficient, but sometimes there will be a GF-dependent parameter,
e.g. for radiation BC's *)

cleanCPP[x_] := Map[StringReplace[FlattenBlock[#],  "  #" -> "#"]&, x];

(* Given a BC registration structure as defined above, return a
   CodeGen structure of a source file which does nothing yet! *)
CreateMoLBoundariesSource[spec_] :=

  Module[{gfs, groups, tmp, lang},

  gfs = lookup[spec, EvolvedGFs];
  groups = Map[unqualifiedGroupName, lookup[spec, Groups]];

    listBCparfileEntry[gforgroup_] := Module[{prefix},
    (* include a comment block with template parameter file entries *)
    prefix =  "#$bound$#" <> lookup[spec, ThornImplementation] <> "::";
    {
     prefix <> ToString@gforgroup <> "_bound       = \"skip\"\n",
     prefix <> ToString@gforgroup <> "_bound_speed = 1.0\n",
     prefix <> ToString@gforgroup <> "_bound_limit = 0.0\n",
     prefix <> ToString@gforgroup <> "_bound_value = 0.0\n\n"
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

    boundpar = ToString@gf <> "_bound";
    fullgfname = lookup[spec, BaseImplementation] <> "::" <> ToString@gf;

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

    boundpar = ToString@gf <> "_bound";
    fullgfname = lookup[spec, BaseImplementation] <> "::" <> ToString@gf;
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

    boundpar = ToString@gf <> "_bound";
    fullgfname = lookup[spec, BaseImplementation] <> "::" <> ToString@gf;
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
     {"/* check whether we can use excision */\n\n",
      "#ifdef LEGOEXCISION_OFF\n",
      "CCTK_INFO(\"Do not compile code using LegoExcision\");\n",
      "#else\n",
      "CCTK_INFO(\"Compiling with code using LegoExcision\");\n",
      "#endif\n",
      "\n",
      "return 0;\n"}],


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

  excisionExtrap[gf_] :=  "  call excision_extrapolate(ierr, "
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


   cleanCPP@DefineCCTKSubroutine[lookup[spec,ThornName] <> "_ApplyExcision",
     {"#ifndef LEGOEXCISION_OFF\n",
      "! APPLY EXCISION\n\n",
      DefineVariable["ierr", "CCTK_INT :: ", "0"],
      "",

      "integer   :: nx, ny, nz\n\n",

      "! grid parameters\n",

      "nx = cctk_lsh(1)\n", 
      "ny = cctk_lsh(2)\n", 
      "nz = cctk_lsh(3)\n\n", 

      "if (excision) then\n",

      "  call CCTK_INFO(\"Applying LegoExcision\")\n\n",

      "  call excision_findboundary(ierr, emask, nx, ny, nz)\n",
      "  call excision_findnormals (ierr, emask, exnormx, exnormy, exnormz, nx, ny, nz)",
      "\n\n",

     Map[excisionExtrap, gfs],
     "endif\n",
      "#endif\n\n",
      "return\n"}]
};

CodeGen`SOURCELANGUAGE = currentlang;

body
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

(* ------------------------------------------------------------------------ 
   Thorn creation
   ------------------------------------------------------------------------ *)

(* source = {Filename -> "MoLRegister.c", Contents -> "#include ..."} *)

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

    ensureDirectory[thornDirectory];
    ensureDirectory[sourceDirectory];

    GenerateFile[thornDirectory <> "/interface.ccl", lookup[thorn, Interface]];
    GenerateFile[thornDirectory <> "/param.ccl",     lookup[thorn, Param]];
    GenerateFile[thornDirectory <> "/schedule.ccl",  lookup[thorn, Schedule]];
    
    Map[GenerateFile[sourceDirectory <> "/" <> lookup[#, Filename], 
                                               lookup[#, Contents]] &,
                                               lookup[thorn, Sources]];

    GenerateFile[sourceDirectory <> "/make.code.defn", lookup[thorn, Makefile]]];

End[];

EndPackage[];
