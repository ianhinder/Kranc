
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
  "CodeGenSymmetries`",
  {"Errors`", "Helpers`", "Kranc`", "CodeGenCactus`", "MapLookup`", "CodeGenKranc`",
   "CodeGenC`", "CodeGen`"}];

CreateSymmetriesRegistrationSource::usage = "";

Begin["`Private`"];

(* ------------------------------------------------------------------------ 
   Symmetries Registration
   ------------------------------------------------------------------------ *)

(* Symmetries registration spec = {FullName -> "impl::GFname", 
                                    Sym      -> {symX, symY, symZ}} *)

DefFn[symmetriesBlock[spec_] :=
  Module[
    {i, dim},

    If[!MatchQ[spec, {FullName -> _String, Sym -> {_,_,_}}],
       ThrowError["symmetriesBlock: Expecting a symmetry registration spec but got ", spec]];

    sym = lookup[spec, Sym];
    dim = Length[sym];
    
    {Table[{"sym[", i - 1, "] = ", sym[[i]], ";\n"}, {i, 1, dim}],
     "SetCartSymVN(cctkGH, sym, ", Quote[lookup[spec, FullName]], ");\n\n"}]];

(* syms is a list of rules mapping gridfunctions to their symmetry structures *)
DefFn[
  calcSymmetry[gf_, syms_] :=
  Module[
    {},
    If[mapContains[syms, gf],
       Return[lookup[syms,gf]],
       (* FIXME: We are defaulting to scalar symmetries if no information is
          available.  This shouldn't happen, but I am bypassing this check
          temporarily. *)
       Print["WARNING: defaulting to symmetries of a scalar for "<>ToString[gf]];
       Return[{1,1,1}]]]];


(* This function guesses the symmetries based on component names as we
   have not been given them *)
DefFn[
  calcSymmetry[gf_] :=
  Module[
    {sym = {1,1,1} (* default *), q, string},
    string = ToString@gf;
    While[
      IntegerQ[q = ToExpression@StringTake[string, -1]],
      sym[[q]] = -sym[[q]];
      string = StringDrop[string, -1]];
    sym]];

(* Given a symmetries registration structure as defined above, return a
   C CodeGen structure of a source file which will register the symmetries. *)
DefFn[
  CreateSymmetriesRegistrationSource[thornName_, implementationName_, GFs_, reflectionSymmetries_, debug_] :=
  Module[
    {spec, j, lang, tmp},

    If[debug, Print["Registering Symmetries for: ", GFs]];

    lang = CodeGenC`SOURCELANGUAGE;
    CodeGenC`SOURCELANGUAGE = "C";

    spec = Table[{FullName -> implementationName <> "::" <> ToString@GFs[[j]],
                  Sym      -> If[reflectionSymmetries === False,
                                 calcSymmetry[GFs[[j]]],
                                 calcSymmetry[GFs[[j]], Union@reflectionSymmetries]]},
                 {j, 1, Length@GFs}];

    tmp = {FileHeader["C"],
           
           Map[IncludeFile, 
               {"cctk.h", "cctk_Arguments.h", "cctk_Parameters.h", "Symmetry.h"}],

           DefineCCTKFunction[
             thornName <> "_RegisterSymmetries", "void", 
             If[Length[spec] > 0,
                {CommentedBlock["array holding symmetry definitions",
                                "CCTK_INT sym[3];\n\n"],
                 CommentedBlock["Register symmetries of grid functions",
                                Map[symmetriesBlock, spec]]},
                {}]]};

    CodeGenC`SOURCELANGUAGE = lang;

    tmp]];

End[];

EndPackage[];
