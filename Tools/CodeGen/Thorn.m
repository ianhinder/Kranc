
(* $Id$ *)

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
  "Thorn`",
  {"CodeGen`", "CodeGenKranc`", "MapLookup`", "Helpers`", "Errors`", "Kranc`"}];

CreateThorn::usage = "Create a general Cactus thorn from a thorn specification structure";

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
  Module[{thornDirectory, sourceDirectory, generatedFiles},

    thornDirectory = lookup[thorn, Directory] <> "/" <> lookup[thorn, Name];
    sourceDirectory = thornDirectory <> "/src";

    Print["Creating thorns in directory ", thornDirectory];

    EnsureDirectory[thornDirectory];
    EnsureDirectory[sourceDirectory];

    generatedFiles = Flatten@Reap[

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

    Map[(EnsureDirectory[FileNameDrop[thornDirectory <> "/" <> lookup[#, Filename],-1]]; GenerateFile[thornDirectory <> "/" <> lookup[#, Filename], 
      lookup[#, Contents]]) &,
      lookup[thorn, Files]];

    GenerateFile[sourceDirectory <> "/make.code.defn", lookup[thorn, Makefile]],

      GenerateFile];

    mergeFiles[thorn, lookup[thorn,MergeFiles], thornDirectory, generatedFiles];

    (* Update thorn directory timestamp so that it can be used in makefiles *)
    GenerateFile[thornDirectory <> "/temp", {}];
    DeleteFile[thornDirectory <> "/temp"];

    Print["Thorn ", thornDirectory, " created successfully"];
];

mergeIgnorePattern =
  (__ ~~ "~") |
  (((__ ~~ "/")|(""))~~".#" ~~ __);

DefFn[mergeFiles[thorn_, from_, to_String, generatedFiles_List] :=
  If[from === None, 
    None,
    (* else *)
    Module[{allFiles, includedFiles},
      (* Make a list of all files in the |from| directory, relative to it *)
      (* The following test is commented out because some of the test thorns have problems with it *)
      (* If[FileType[from] =!= Directory, ThrowError["MergeFiles option should be a directory, but "<>ToString[from] <> " is not"]]; *)
      allFiles = FileNameDrop[#,FileNameDepth[from]]& /@ FileNames["*", from, Infinity];
      (* Copy or merge each file into the generated thorn directory *)
      includedFiles = Select[allFiles, (!StringMatchQ[#, mergeIgnorePattern]) &];
      Map[mergeFile[thorn, from, to, #, generatedFiles] &, includedFiles]]]];

DefFn[importText[fileName_String] :=
  Check[Import[fileName, "Text"],
    ThrowError["Unable to read file "<>fileName]]];

DefFn[mergeFile[thorn_,
                from_String, to_String, path_String, generatedFiles_List] :=
  Module[{thornName, fromPath, toPath},
    thornName = lookup[thorn, Name];
    fromPath = FileNameJoin[{from,path}];
    toPath = FileNameJoin[{to,path}];

    If[DirectoryQ[fromPath],
      If[!DirectoryQ[toPath],
        CreateDirectory[toPath, CreateIntermediateDirectories->True]],
      (* else *)
      If[MemberQ[generatedFiles, toPath],
        Module[{orig = importText[toPath], new = importText[fromPath]},
          new = StringReplace[new, "@THORN_NAME@" -> thornName];
          Export[toPath, orig<>"\n"<>new<>"\n", "Text"]],
        (* else *)
        If[FileExistsQ[toPath], DeleteFile[toPath]];
        (* CopyFile[fromPath,toPath] *)
        new = importText[fromPath];
        new = StringReplace[new, "@THORN_NAME@" -> thornName];
        Export[toPath, new<>"\n", "Text"]]]]];

End[];

EndPackage[];
