
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

BeginPackage["OpenCL`", {"Errors`", "Helpers`", "Kranc`", "Calculation`", "CodeGen`",
                         "Vectorisation`"}];

OpenCLPrologue;
OpenCLEpilogue;
OpenCLProcessKernel;
OpenCLIncludeFiles;
OpenCLConfigurationCCL;
OpenCLProcessDifferencingHeader;

Begin["`Private`"];

DefFn[
  OpenCLPrologue[] :=
  "const char* const source =\n"];

DefFn[
  OpenCLEpilogue[cleancalc_List, imp_String, functionName_String] :=
  {
    ";\n\n",
    Module[
      {ignoreGroups, groupsNames, groupNameList},
      ignoreGroups = {"TmunuBase::stress_energy_scalar",
                      "TmunuBase::stress_energy_vector",
                      "TmunuBase::stress_energy_tensor"};
      groupNames = GroupsInCalculation[cleancalc, imp];
      groupNames = Select[groupNames, !MemberQ[ignoreGroups, #] &];
      {
        "const char* const groups[] = {\n  ",
        Riffle[Join[Map[Quote, groupNames], {"NULL"}], ",\n  "],
        "};\n\n"
      }
          ],
    "static struct OpenCLKernel *kernel = NULL;\n",
    "const char* const sources[] = {differencing, source, NULL};\n",
    "OpenCLRunTime_CallKernel(cctkGH, CCTK_THORNSTRING, \"" <> functionName <> "\",\n",
    "                         sources, groups, NULL, NULL, NULL, -1,\n",
    "                         imin, imax, &kernel);\n\n"
  }];

DefFn[
  OpenCLProcessKernel[code:CodeGenBlock] :=
  Stringify[code]];

DefFn[
  OpenCLLocalsToGridFunctions[gridNames_List, localNames_List] :=
  VectorisationLocalsToGridFunctions[gridNames, localNames, {"lc_imin", "lc_imax"}]];

DefFn[
  OpenCLIncludeFiles[] :=
  {"OpenCLRunTime.h"}];

DefFn[
  OpenCLConfigurationCCL[] :=
  "REQUIRES OpenCL OpenCLRunTime\n"];

DefFn[
  OpenCLProcessDifferencingHeader[diffHeader_] :=
  "static const char* const differencing =\n" <>
  Stringify[diffHeader] <>
  ";\n"];


End[];

EndPackage[];
