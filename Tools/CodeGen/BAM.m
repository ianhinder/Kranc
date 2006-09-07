
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
CreateMakefile[sourceFiles_] := Module[{srcFiles, objFiles},

c2o[name_] := StringDrop[name, -1] <> "o";

srcFiles = DeleteCases[sourceFiles, "Startup.c"]; (* BAM does not use Startup files *)
srcFiles = DeleteCases[srcFiles, "RegisterSymmetries.c"]; (* skip as well *)
(* srcFiles = DeleteCases[srcFiles, "*_CalcRHS.c"];  *)         (* all in one now *)
srcFiles = DeleteCases[srcFiles, "*_Boundaries.c"];       (* all in one now *)
srcFiles = DeleteCases[srcFiles, "*_RegisterVars.c"];     (* all in one now *)

srcFiles = Map[StringReplace[# , "_CalcRHS" -> ""]&, srcFiles];
srcFiles = Map[StringReplace[# , "_Boundaries" -> ""]&, srcFiles];
srcFiles = Map[StringReplace[# , "_RegisterVars" -> ""]&, srcFiles];

objFiles = Map[c2o, srcFiles];
objFiles = Union@objFiles; (* repetition makes no sense here *) 

  {whoWhenSeparator["shell"],
   "#SRCS += ", Map[{#, " "} &, srcFiles], "\n",
   "OBJS  += ", Map[{#, " "} &, objFiles]}
];

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


GetFun[type_]:= Switch[type,
                       "CCTK_INT",  "Geti",
                       "CCTK_REAL", "Getd",
                       "KEYWORD",   "Getv",
                       "BOOLEAN",   "Geti",
                       _, type <> "is not a valid type!"];

GetValue[type_, name_]:=  GetFun[type] <> "(\"" <> name <> "\")";

(* Return a block defining a parameter with the given
   parameterSpec (defined above).  This is used for defining new
   parameters, as well as extending existing ones. *)
parameterBlock[spec_] := 
If[StringMatchQ[lookup[spec, Name], "*_bound"],
  {"/* " <> lookup[spec, Name] <> " boundary type not assigned for BAM */\n"},
  {"  AddPar(",
     Quote@lookup[spec, Name],     ", ",
     Quote@ToExpression@lookup[spec, Default],  ", ",
     Quote@lookup[spec, Description],
     ");\n",
     "  ", DefineVariable[lookup[spec, Name], lookup[spec, Type], 
                    GetValue[lookup[spec, Type], lookup[spec, Name]]], "\n"}
];

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
  {"  AddVar(", Quote@lookup[spec, Name],        ", ",
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
  Module[{bin, name, comment, scheduleThis, entry},

bin     = lookup[spec,SchedulePoint];
name    = lookup[spec, Name];
comment = lookup[spec, Comment];

bin = StringReplace[bin, "at BASEGRID"     -> "PRE_INITIALDATA", IgnoreCase -> True];
bin = StringReplace[bin, "at INITIAL"      -> "INITIALDATA",     IgnoreCase -> True];
bin = StringReplace[bin, "in MoL_PostStep" -> "POST_EVOLVE",     IgnoreCase -> True];
bin = StringReplace[bin, "in MoL_Register" -> "POST_INITIALDATA",IgnoreCase -> True];


scheduleThis = True;

If[StringMatchQ[name, "*Startup"],            scheduleThis = False];
If[StringMatchQ[name, "*CheckBoundaries"],    scheduleThis = False];
If[StringMatchQ[name, "*ApplyBoundConds"],    scheduleThis = False];
If[StringMatchQ[name, "*RegisterSymmetries"], scheduleThis = False];
If[StringMatchQ[name, "*_CalcRHS"],           scheduleThis = False];


If[scheduleThis,
 entry = {"  AddFun(" <> bin <> ", " <> Quote@name <> ", " <> Quote[comment], ");\n\n"};,
 entry = {"/* " <> name <> " IS ONLY SCHEDULED FOR CACTUS, NOT FOR BAM */\n"};
];

entry
	];

(* Handle the aspect of scheduling the function conditionally *)
scheduleFunction[spec_] :=
  Module[{bamspec, condition, parameter, value, u},

    bamspec = spec;

    u = scheduleUnconditionalFunction[bamspec];

       If[mapContains[spec, Conditional],

       (* Output the conditional structure *)
       condition = lookup[bamspec, Conditional];

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
  Module[{include, rhsname},
  include = lookupDefault[{opts}, Include, {}];

  If[!MatchQ[include, _List],
    Throw["CreateSetterSource: Include should be a list but is in fact " <> ToString[include]]];

  {"\n",
   "#define KRANC_" <> ToUpperCase[CodeGen`SOURCELANGUAGE] <> "\n\n",

   If[CodeGen`SOURCELANGUAGE == "C",
         IncludeSystemFile["math.h"],
         "\n"
      ],

   "\n",
   "#define CCTK_INT int\n",
   "#define CCTK_REAL double\n",
   
   "\n",
   "#define index ccc\n"

   If[StringMatchQ[rhsname = lookup[First@calcs, Name], "*_CalcRHS"],
     Print["Coding MoL RHS function: ", rhsname];
     Print["FIXME: this hack only works if this thorn contains only MoL RHSs!"];
     "#define BAM_ARGUMENTS tVarList *unew, tVarList *upre, double c, tVarList *ucur\n",
     "#define BAM_ARGUMENTS tL *level\n"
   ],

   Map[IncludeFile,  include],
   calculationMacros[],


   

   (* For each function structure passed, create the function and
      insert it *)
   Map[CreateCalculationFunction[# , debug]& , 
       calcs]}];



(* ------------------------------------------------------------------------ 
   Symmetries Registration is not necessary in BAM
   ------------------------------------------------------------------------ *)

CreateSymmetriesRegistrationSource[thornName_, 
                         implementationName_, GFs_, debug_] := {};

(* ------------------------------------------------------------------------ 
   MoL Registration
   ------------------------------------------------------------------------ *)
(* 
  HOW TO DO THIS: consistently extract BAM-style names from GF-names with BreakComponentNames

CHECK WHETHER BOUNDARY INFO CAN BE SET BY GROUP!

MOVE ALL THE BOUNDARY STUFF INTO MOL REGISTRATION

WHEN CONVERTING TO BAM BINS: use this info to not register functions with AddFun that we do not need,
like startup etc.

DEFINE GLOBAL SWITCH: INDEX_ORDER = KEEPSIGN
                                    FORGETSIGN (= Cactus, BAM!)

OR: im makeSplitRules first generate two lists: one in the old
    format, and one in the new format, and parse this at list in groupStruct 
*)

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

    registerEntry[x_]:= "vlpush(varlist, Ind(\"" <> unqualifiedGroupName[x] <> "\"));\n";

    tmp = {
    "#define CCTK_INT int\n",
    "#define CCTK_REAL double\n\n",

    DefineFunction[lookup[spec,ThornName] <> "_RegisterVars", "CCTK_INT",
                   "tL *level",
      {DeclarePointer["varlist", "tVarList"],

        CommentedBlock["Register all the evolved grid functions",
	 {"varlist = vlalloc(level);\n\n",

          Map[registerEntry, lookup[spec, EvolvedGFs]],
          "\n",
          "evolve_vlregister(varlist);\n"}],

       CommentedBlock["register evolution routine",
	{"evolve_rhsregister(" <> lookup[spec,ThornName] <>  "_CalcRHS);\n"}],

       "return 0;\n"}]};

      CodeGen`SOURCELANGUAGE = lang;

tmp
];

(* ------------------------------------------------------------------------
   MoL Boundaries
   ------------------------------------------------------------------------ *)

(* boundaries spec = {Groups -> {trK, h11, ...},
                       BaseImplementation -> "ADMBase", ThornName -> "ADMMoL"} *)


cleanCPP[x_] := Map[StringReplace[FlattenBlock[#],  "  #" -> "#"]&, x];

(* Given a BC registration structure as defined above, return a
   CodeGen structure of a source file which does nothing yet! *)
CreateMoLBoundariesSource[spec_] :=

  Module[{gfs, tmp, lang},

  gfs = lookup[spec, EvolvedGFs];
  gfs = Map[unqualifiedGroupName, gfs];

  registerBoundaryEntry[gf_] := Module[{name},

     name = ToString@gf;

     ",\n"                                                   <>
     "                        " <> name <> "_bound_limit,  \n" <> 
     "                        " <> name <> "_bound_falloff,\n" <> 
     "                        " <> name <> "_bound_speed"
  ];

  listBCparfileEntry[gf_] := Module[{prefix, unqualName},
    prefix = "#$bound$#";
    unqualName = unqualifiedGroupName@ToString@gf;

    {
     prefix <> unqualName <> "_bound_limit   = 1.0\n",
     prefix <> unqualName <> "_bound_falloff = 0.0\n",
     prefix <> unqualName <> "_bound_speed   = 0.0\n\n"
  }];

    radiationBCGF[gf_] := Module[{boundpar},
       (* a simple radiation boundary condition *)

        boundpar = unqualifiedGroupName@ToString@gf <> "_bound";

        {"VarNameSetBoundaryInfo(" <> ToString@gf <> registerBoundaryEntry[gf]
          <> ");\n\n"}];

   lang = CodeGen`SOURCELANGUAGE;
   CodeGen`SOURCELANGUAGE = "C";

   tmp = {"\n\n",

   cleanCPP@DefineBAMFunction[lookup[spec,ThornName] <> "_ApplyBoundConds", 
   "CCTK_INT",
     {DefineVariable["ierr",   "CCTK_INT", "0"],

       Map[radiationBCGF, gfs],

      "return ierr;\n"
     }],

     "\n\n\n",
     "/* template for entries in parameter file:\n",
      Map[listBCparfileEntry, gfs],
     "*/\n\n"
     };

	 CodeGen`SOURCELANGUAGE = lang;
tmp
];

CreateMoLExcisionSource[spec_] := {};

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

CreateStartupFile[thornName_, bannerText_] := {};

(* ------------------------------------------------------------------------ *)
(*              BAM 'Thorn' creation                                        *)
(* ------------------------------------------------------------------------ *)

(* source = {Filename -> "MoLRegister.c", Contents -> "#include ..."} *)

(* thorn = {Name -> "ClassicADMMolEvolve", Directory -> "ClassicADM",
            Interface -> i, Schedule -> s, Param -> p, Makefile -> m, 
     Sources -> {s1, s2, ...}} *)

(* Given a thorn specification structure as defined above, create a
   thorn.  Note that if you specify a path to the thorn, then you are
   responsible for making sure that the parent directory exists; this
   function does not automatically create any parent directories. *)

CreateThorn[thorn_] :=
  Module[{thornName, project, directory, bamC, projH, 
          allSources, thisFileContent, i, includeBlock, funcDeclarations},

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
   

    isFunEntry[x_]:= StringMatchQ[ToString@x, "*AddFun*"];

    headerFunEntry[x_]:= "int " <> 
       StringReplace[
           StringReplace[x, ShortestMatch["AddFun(" ~~ __ ~~ ","]  -> ""], 
                         "," ~~ __ -> ""] <> "(tL *level);\n\n"; (* " *) 

    projH = {"\n/* declare functions we have added with AddFun for " <> thornName <> " */\n"};
  
    funcDeclarations = Union@Map[headerFunEntry, Select[Flatten@bamC, isFunEntry]];

    AppendTo[projH, funcDeclarations];

    AddToFile[directory <> "/bam_" <> project <> ".c", bamC];
    AddToFile[directory <> "/"     <> project <> ".h", projH];
  
    allSources = lookup[thorn, Sources]; 

    thisFileContent = Flatten@Table[ lookup[allSources[[i]], Contents], 
                        {i, 1, Length@allSources}];

     
    includeBlock = Map[IncludeFile, {"bam.h", "GenericFD_BAM.h", ToString@project <> ".h"}];


    thisFileContent = Flatten@{whoWhen["C"], includeBlock, thisFileContent};
 
    GenerateFile[directory <> "/" <> thornName <> ".c", thisFileContent];

    AddToFile[directory <> "/Makefile", lookup[thorn, Makefile]]];


StartBAMProject[project_, directory_] :=
  Module[{bamC, bamH, bamM, projH, cname, hname, mname, pname, projectRootFunction},

      lastDir[dir_] := StringDrop[dir, Last@Last@StringPosition[dir, "/"] ];

      Print["Starting project " <> project <> " in directory ", directory];

      EnsureDirectory[directory];

      cname = "bam_" <> project <> ".c";
      hname = "bam_" <> project <> ".h";
      mname = "Makefile";
      pname = project <> ".h";
      
      projectRootFunction = "bam_" <> project;
      
      bamC = {"/* " <> cname  <> " */\n", 
              whoWhen["C"], 
	      "#include \"bam.h\"\n", 
	      "#include \"" <> hname <> "\"\n\n\n",
              "#define BOOLEAN   int\n",
              "#define CCTK_INT  int\n",
              "#define CCTK_REAL double\n\n",

               "void " <> projectRootFunction <> "()\n{\n",
	       "   if (!Getv(\"physics\", \"" <> project <> "\")) return;\n",
	       "       printf(\"Adding " <> project <> "\");\n"
	       };

      bamH = {"/* " <> hname  <> " */\n", 
              whoWhen["C"], 
	      "void " <> projectRootFunction <> "();\n\n"};
     
      projH = {"/* " <> project <> ".h */\n",
              whoWhen["C"]};

      bamM = {"# " <> project <> "Makefile\n",
             whoWhen["shell"],
             "\n", 
             "NAME := " <> project,
             "\n", 
             "OBJS := bam_$(NAME).o", 
             "\n",
             "include $(TOP)/Makefile.subdirs "
             };

      GenerateFile[directory <> "/" <> cname, bamC];
      GenerateFile[directory <> "/" <> hname, bamH];
      GenerateFile[directory <> "/" <> mname, bamM];

      GenerateFile[directory <> "/" <> pname, projH];
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
