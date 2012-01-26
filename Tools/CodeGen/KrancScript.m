
(*  Copyright 2012 Ian Hinder

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

BeginPackage["KrancScript`", {"Errors`", "Helpers`", "Kranc`", "JLink`"}];

CreateThornFromKrancScript;

Begin["`Private`"];

InstallJava[];

(* The JRE does not share a current directory with the Mathematica
   kernel, so relative paths have to be converted to absolute paths.
   It is not possible to change the JRE working directory. *)

absPath[s_String] :=
  If[StringTake[s,1] === $PathnameSeparator, s, FileNameJoin[{Directory[],s}]];

fullKrancDir = absPath[KrancDirectory];

AddToClassPath[
 FileNameJoin[{fullKrancDir, "Tools","PirahaPEG","piraha.jar"}]];

DefFn[
  parseScript[filename_String] :=
  Module[
    {g,m,c,sw,dout,xmlString,xml},
    g = JavaNew["edu.lsu.cct.piraha.Grammar"];
    g@compileFile[JavaNew["java.io.File", FileNameJoin[{fullKrancDir, "Auxiliary", "Grammars","kranc2.peg"}]]];

    c = Grammar`readContents[JavaNew["java.io.File", absPath@filename]];

    m = g@matcher["thorn", c];

    If[!m@match[0], ThrowError["Failed to parse input file: ",m@near[]@toString[]]];

    sw = JavaNew["java.io.StringWriter"];
    dout = JavaNew["edu.lsu.cct.piraha.DebugOutput", JavaNew["java.io.PrintWriter", sw]];
    m@dumpMatchesXML[dout];
    dout@flush[];
    xmlString = sw@toString[];
    xml = ImportString[xmlString, "XML"];
    xml]];

DefFn[
  CreateThornFromKrancScript[filename_String] :=
  Module[
    {code},

    Print["Creating thorn from ",filename];
    
    code = parseScript[filename];
    Print["Parsed script:"];
    Print[code//InputForm]]];

End[];

EndPackage[];
