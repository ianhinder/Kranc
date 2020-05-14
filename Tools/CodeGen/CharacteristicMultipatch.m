
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

BeginPackage[
  "CharacteristicMultipatch`",
  {"Errors`", "Helpers`", "Kranc`", "MapLookup`", "CodeGenC`", "KrancGroups`",
   "CodeGenC`"}];

CreateMPCharSource::usage = "";

Begin["`Private`"];

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
(* #ifdef only present to support older versions of the flesh *)
"  const cGH* restrict const cctkGH = cctkGH_;\n",
"#ifdef DECLARE_CCTK_ARGUMENTS_"<>name<>"\n"<>
"  DECLARE_CCTK_ARGUMENTS_CHECKED("<>name<>");\n"<>
"#else\n"<>
"  DECLARE_CCTK_ARGUMENTS;\n"<>
"#endif\n"
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

End[];

EndPackage[];
