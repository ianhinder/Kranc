
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

BeginPackage[
  "CodeGenStartup`",
  {"Errors`", "Helpers`", "Kranc`", "CodeGenKranc`", "CodeGenC`", "CodeGen`"}];

CreateStartupFile::usage = "";

Begin["`Private`"];

(* ------------------------------------------------------------------------ 
   Startup file
   ------------------------------------------------------------------------ *)

CreateStartupFile[thornName_, bannerText_, useChemora_] :=
  Module[{tmp, lang, inclines, bodylines},
  
  lang = CodeGenC`SOURCELANGUAGE;
  CodeGenC`SOURCELANGUAGE = "C";

  inclines = If[ useChemora, IncludeFile["Chemora.h"], "" ];
  bodylines = If[ useChemora, "chemora_cg_thorn_startup();\n", "" ];

  tmp = {FileHeader["C"],

   NewlineSeparated[{IncludeFile["cctk.h"],
                     inclines,
   {"extern \"C\" ", DefineFunction[thornName <> "_Startup", "int", "void",
     {DefineVariable["banner", "const char*", Quote[bannerText]],
      "CCTK_RegisterBanner(banner);\n",
      bodylines,
      "return 0;\n"}]}}]};

  CodeGenC`SOURCELANGUAGE = lang;

  tmp
   ];

End[];

EndPackage[];
