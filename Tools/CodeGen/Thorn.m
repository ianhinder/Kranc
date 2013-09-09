
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
CreateThorn::usage = "Create a general Cactus thorn from
a thorn specification structure";
CreateSetterSource::usage = "";
CreateMPCharSource::usage = "";
CreatePrecompMacros::usage = "";
CreateStartupFile::usage = "";

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
