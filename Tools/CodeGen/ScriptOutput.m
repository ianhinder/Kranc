
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
                writeExpression[lookup[calc, Equations]],
                Indent -> True]];

wrap[s_String, indent_Integer, chars_Integer:80] :=
  Module[
    {spaces = StringPosition[s," "],
     breakpoint, indentStr},
    indentStr = StringJoin@ConstantArray[" ", indent];
    If[spaces === {},Return[s]];

    breakpoint = TakeWhile[spaces,#[[1]]<chars &][[-1,1]];
    StringTake[s,breakpoint-1]<>"\n"<>indentStr<>wrap[StringDrop[s,breakpoint],indent,chars]];

writeExpression[eqs:List[___Rule]] :=
  Module[
    {lhss = FlattenBlock/@writeExpression/@First/@eqs,
     rhss = FlattenBlock/@writeExpression/@Last/@eqs,
     maxlhs,lhss2,rhss2},
    maxlhs = Max[StringLength /@ lhss];
    lhss2 = Map[#<>StringJoin[ConstantArray[" ",maxlhs-StringLength[#]]]
                <>" = "&, lhss];
    rhss2 = Map[wrap[#,maxlhs+3,80-(maxlhs+3)]&,rhss];
  Riffle[Thread[{lhss2,rhss2}],"\n"]];

writeExpression[Tensor[T_, inds___]] :=
  {ToString[T], writeExpression[{inds}]};

writeExpression[List[inds__TensorIndex]] :=
  Map[{#[[1,2]]/.{"u"->"^","l"->"_"},writeExpression/@#} &,
      SplitBy[{inds},#[[2]]&]];

writeExpression[TensorIndex[sym_, _]] :=
  ToString[sym];

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
  {"delta", Map[writeExpression, {i1,i2}]};

paren[x_] := {"(",x,")"};

writeExpression[a_Plus] :=
  Riffle[Map[paren[writeExpression[#]] &,List@@a]," + "];

writeExpression[a_Times] :=
  Riffle[Map[paren[writeExpression[#]] &,List@@a]," * "];

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
  {writeExpression[d],writeExpression[{inds}]," ",paren@writeExpression[var]};

writeExpression[MatrixInverse[Tensor[t_,i_,j_]]] :=
  {"inverse(",ToString@t,")",Map[writeExpression,{i,j}]};

(* Remaining tasks:

   * Implement covariant derivatives
   * Implement scheduling
   * Implement all additional calculation and thorn options
   * Implement remaining generated code in script parser

   Aesthetics:

   * Minimise parentheses

*)

(*

DefFn[write[] :=

     ];

*)

End[];

EndPackage[];
