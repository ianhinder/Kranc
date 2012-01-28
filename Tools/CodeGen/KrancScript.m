
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

    code = code /. ("startIndex" -> _) :> Sequence[];
    code = code /. ("endIndex" -> _) :> Sequence[];

    (* Print["Parsed script:"]; *)
    (* Print[code//InputForm]; *)

    stringRules = XMLElement[s_,_,c_] :> s@@c;

    thorn = code[[2]] //. stringRules;

    Print[];
    Print["Parse tree:"];
    Print[thorn];
    Print[];

    processed = process[thorn];
    Print[processed];
]];

process[h_[args___]] :=
  Module[
    {},
    Print["No handler for ", h@@Map[ToString[Head[#]]&,{args}]];
    h[args]];

process[thorn:"thorn"[content___]] :=
  Module[
    {calcs, name,options},
    calcs = Cases[thorn, c:"calculation"[___]:>process[c]];
    name = Cases[thorn, "name"[n_]:>n][[1]];
    options = {Calculations -> calcs};
    CreateThornTTExpression[groups,parentDirectory,name,Sequence@@options]];

process[calc:"calculation"[content___]] :=
  Module[
    {name,eqs},
    name = Cases[calc, "uname"[n_]:>n][[1]];
    eqs = Cases[calc, "eqns"[es___]:>{es}][[1]];
    {Name -> name,
     Equations -> Map[process,eqs]}];

process["eqn"[lhs_,rhs_]] := Module[{name,eqs}, lhs -> process[rhs]];

process["tensor"["name"[k_],"indices"[]]] := k;
process["tensor"["name"[k_],inds_]] := Tensor[k,Sequence@@process[inds]];

process["dtensor"[inds_,tensor_]] := PD[process[tensor],Sequence@@process[inds]];
process["dtensor"["indices"["_t"],tensor_]] := dot[process[tensor]];

process["indices"[inds_]] :=
  Module[
    {lower,upper,is},

    lower[s_String] :=
    If[s === "", {},
       If[StringTake[s,1] === "^", upper[StringDrop[s,1]],
          If[StringTake[s,1] === "_", ThrowError["Repeated '_'"],
             Prepend[lower[StringDrop[s,1]], TensorIndex["l",StringTake[s,1]]]]]];

    upper[s_String] :=
    If[s === "", {},
       If[StringTake[s,1] === "_", lower[StringDrop[s,1]],
          If[StringTake[s,1] === "^", ThrowError["Repeated '^'"],
             Prepend[upper[StringDrop[s,1]], TensorIndex["u",StringTake[s,1]]]]]];

    is = Switch[StringTake[inds,1],
                "_", lower[StringDrop[inds,1]],
                "^", upper[StringDrop[inds,1]],
                _, ThrowError["Tensor indices must start with ^ or _"]];
    is];

process["func"["name"[name_],exprs__]] :=
  Module[
    {fns},
    fns = {"sin" -> Sin, "cos" -> Cos, "if" -> IfThen};
    If[MemberQ[First/@fns,name], (name/.fns)@@Map[process,{exprs}],
       ThrowError["Unrecognised function: ", name]]];

process["expr"[mul_]] := process[mul];
process["expr"[]] := 0;
process["expr"[a_, "addop"["-"], b_,cs___]] := process[a] - process[b] + process["expr"[cs]];
process["expr"[a_, "mulop"["+"], bs__]] := process[a] + process["expr"[bs]];

process["mul"[pow_]] := process[pow];
process["mul"[]] := 1;
process["mul"[a_, "mulop"["/"], b_,cs___]] := Times[process[a] / process[b],process["mul"[cs]]];
process["mul"[a_, "mulop"["*"], bs__]] := process[a] * process["mul"[bs]];

process["pow"[a_,b_]] := process[a]^process[b];
process["pow"[a_]] := process[a];

process["value"[a_]] := process[a];

process["number"[a_]] := ToExpression[a];

End[];

EndPackage[];
