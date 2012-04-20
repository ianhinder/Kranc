
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

BeginPackage["ScriptOutput`", {"Errors`", "Helpers`", "Kranc`", "CodeGen`", "MapLookup`", "TensorTools`"}];

WriteScript;

Begin["`Private`"];

(*************************************************************)
(* Script output *)
(*************************************************************)

Options[beginEndBlock] = {"Indent" -> False};

beginEndBlock[key_String, args_String, content_, OptionsPattern[]] :=
  {"begin ", key, " ", args, "\n",
   If[OptionValue[Indent], IndentBlock2[content], content], "\n",
   "end ", key, "\n"};

Options[WriteScript] = ThornOptions;

DefFn[
  WriteScript[groups_List, parentDirectory_String, thornName_String, OptionsPattern[]] :=
  Module[
    {script, docDir, scriptFile, derivs},
    Print["Writing script"];

    derivs = Head/@Map[First,OptionValue[PartialDerivatives]];

    Block[{$DerivativeNames = derivs},

    script = 
    {"# Expressions within @{...} are not yet supported by the script generator\n",
     beginEndBlock["thorn", thornName, 
                   {"\n",Riffle[Map[writeCalculation, OptionValue[Calculations]],"\n"],"\n"}]};
         ];

    docDir = FileNameJoin[{parentDirectory,thornName,"doc"}];
    EnsureDirectory[docDir];
    scriptFile = FileNameJoin[{docDir, thornName<>".kranc"}];
    GenerateFile[scriptFile, script];
    Print["Generated script "<> scriptFile]]];

DefFn[
  writeCalculation[calc_List] :=
  beginEndBlock["calculation", lookup[calc, Name],
                Riffle[Map[writeEquation, lookup[calc, Equations]],"\n"],
                Indent -> True]];

DefFn[writeEquation[lhs_ -> rhs_] :=
      {writeLHS[lhs], " = ", writeExpression[rhs]}];

DefFn[writeLHS[lhs_Tensor] :=
      writeTensor[lhs]];

DefFn[writeLHS[lhs:dot[t_]] :=
      {"D_t ",writeLHS[t]}];

DefFn[writeLHS[lhs_Symbol] :=
      ToString[lhs]];

DefFn[writeTensor[Tensor[T_, inds___]] :=
      {ToString[T], Map[writeIndex, {inds}]}];

DefFn[writeIndex[TensorIndex[sym_, "l"]] :=
      {"_", ToString[sym]}];

DefFn[writeIndex[TensorIndex[sym_, "u"]] :=
      {"^", ToString[sym]}];

writeExpression[lhs_] :=
      "@{"<>ToString[FullForm@lhs]<>"}";

writeExpression[t_Tensor] :=
  writeTensor[t];

writeExpression[n_Integer] :=
  ToString[n,InputForm];

writeExpression[n_Rational] :=
  ToString[n,InputForm];

writeExpression[n_Real] :=
  ToString[n,InputForm];

writeExpression[s_Symbol] :=
  ToString[s];

writeExpression[IfThen[a_,b_,c_]] :=
  {"(",paren@writeExpression[a], " ? ", paren@writeExpression[b], " : ", paren@writeExpression[c],")"};

writeExpression[KroneckerDelta[i1_TensorIndex,i2_TensorIndex]] :=
  {"delta", Map[writeIndex, {i1,i2}]};

paren[x_] := {"(",x,")"};

writeExpression[a_Plus] :=
  Riffle[Map[paren[writeExpression[#]] &,List@@a],"+"];

writeExpression[a_Times] :=
  Riffle[Map[paren[writeExpression[#]] &,List@@a],"*"];

writeExpression[Power[a_,b_]] :=
  {paren@writeExpression[a],"**",paren@writeExpression[b]};

writeExpression[Unequal[a_,b_]] :=
  {paren@writeExpression[a],"!=",paren@writeExpression[b]};

writeExpression[Log[a_]] :=
  {"log(",writeExpression[a],")"};

writeExpression[Sign[a_]] :=
  {"sign(",writeExpression[a],")"};

writeExpression[Abs[a_]] :=
  {"abs(",writeExpression[a],")"};

writeExpression[Max[a_,b_]] :=
  {"max(",writeExpression[a], ",", writeExpression[b],")"};

writeExpression[dot[a_]] :=
  {"D_t ",paren@writeExpression[a]};

writeExpression[d_?(MemberQ[$DerivativeNames,#]&)[var_,inds___]] :=
  {"D",Map[writeIndex,{inds}]," ",paren@writeExpression[var]};

writeExpression[MatrixInverse[Tensor[t_,i_,j_]]] :=
  {"inverse(",ToString@t,")",Map[writeIndex,{i,j}]};

(* Remaining tasks:

   * Eliminate repeated ^ and _ characters
   * Express derivatives by name
   * Implement covariant derivatives
   * Implement scheduling
   * Implement all additional calculation and thorn options
   * Implement remaining generated code in script parser

   Aesthetics:

   * Minimise parentheses
   * Wrap long lines
   * Align '=' signs

*)

(*

DefFn[write[] :=

     ];

*)

End[];

EndPackage[];
