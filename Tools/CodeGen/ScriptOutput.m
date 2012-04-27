
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

BeginPackage["ScriptOutput`", {"Errors`", "Helpers`", "Kranc`", "CodeGen`", "MapLookup`", "TensorTools`", "KrancGroups`"}];

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
    {script, docDir, scriptFile, derivs, allOptionNames, setOptions, 
     setOptionsWithValues},
    Print["Writing script"];

    derivs = Head/@Map[First,OptionValue[PartialDerivatives]];
    Block[{$DerivativeNames = derivs},

    allOptionNames = First/@ThornOptions;
    setOptions = Select[allOptionNames, OptionValue[#] =!= (#/.ThornOptions) &];
    setOptionsWithValues = Map[(#->OptionValue[#]) &,setOptions];

    setOptions = Complement[setOptions,
                            Join[{Calculations,
                                  DeclaredGroups,
                                  InheritedImplementations,
                                  PartialDerivatives}, Last/@ScriptFlags]];
    remainingOptionsWithValues = Map[(#->OptionValue[#]) &,setOptions];

    script = 
    {"# Expressions within @{...} are not yet supported by the script generator\n",
     beginEndBlock["thorn", thornName, 
                   {"\n",
                    Riffle[Map["# " <> ToString[#,InputForm] &,
                               remainingOptionsWithValues],"\n\n"],
                    "\n\n",

                    If[OptionValue[InheritedImplementations] =!= {},
                       {"inherit ", 
                        Riffle[Map[ToString,
                                   OptionValue[InheritedImplementations]],
                               " "], "\n\n"},{}],

                    writeFlags[setOptionsWithValues],
                    "\n",

                    writeDerivatives[OptionValue[PartialDerivatives]],
                    "\n",

                    writeVariables[Join@@(variablesInGroup[#,groups]&/@OptionValue[DeclaredGroups])],
                    "\n",
                    writeTemporaries[OptionValue[Calculations]],
                    "\n",
                    Riffle[Map[writeCalculation, OptionValue[Calculations]],"\n"],"\n"}]};
         ];

    docDir = FileNameJoin[{parentDirectory,thornName,"doc"}];
    EnsureDirectory[docDir];
    scriptFile = FileNameJoin[{docDir, thornName<>".kranc"}];
    GenerateFile[scriptFile, script];
    Print["Generated script "<> scriptFile]]];

DefFn[writeVariables[vars_List] :=
      beginEndBlock["variables","",
                    wrap[FlattenBlock[Riffle[Map[writeExpression,vars]," "]],0],Indent->True]];

DefFn[writeTemporaries[calcs_List] :=
      beginEndBlock[
        "temporaries","",
        wrap[FlattenBlock[Riffle[Map[writeExpression,Union@@Map[lookup[#,Shorthands,{}]&,calcs]]," "]],0],Indent->True]];

DefFn[writeFlags[options_List] :=
      Module[{rFlags, flagsSet},
        rFlags = Map[Reverse, ScriptFlags];
        flagsSet = Intersection[First/@options, Last/@ScriptFlags];
        Map[{If[#/.options,"use","disable"]," ", #/.rFlags, "\n"} &, flagsSet]]];

DefFn[
  writeCalculation[calc_List] :=
  beginEndBlock["calculation", lookup[calc, Name],
                writeExpression[lookup[calc, Equations]],
                Indent -> True]];

DefFn[
  writeDerivatives[pdefs_] :=
  beginEndBlock["derivatives", "",
                Map[{writeDerivative[#],"\n"} &, pdefs],
                Indent -> True]];

derivIndex[i_Integer] := i;
derivIndex[p_] := If[Head[p] === Pattern, p[[1]], ThrowError["Unknown derivative index "<>ToString[p,InputForm]]];

DefFn[
  writeDerivative[arg:(op_[inds___] -> def_)] :=
  {ToString[op], " u_", StringJoin[ToString/@derivIndex/@{inds}], " = ", writeDerivativeDefinition[def,{inds}],"\n"}];

DefFn[
  writeDerivativeDefinition[def_, indList_List] :=
  Module[{u = DerivVar["u", indList]},
  writeExpression[Simplify[Expand[u def] //. shiftRule]]]];

(* writeExpression[DerivVar[n_String, indList_List]] := *)
(*   n; *)

derivIndex[i_,p_/;p>0] :=
  {"(", ToString[i], "+", ToString[p], ")"};

derivIndex[i_,0] :=
  {"(", ToString[i], ")"};

derivIndex[i_,p_/;p<0] :=
  {"(", ToString[i], "-", ToString[-p], ")"};

derivIndex[i_,p_] :=
  {"(", ToString[i+writeExpression[p]], ")"};

shiftRule = Times[DerivVar[n_, indList_List],shifts:(((shift[_]^_)|shift[_])...),rest___] ->
  Times[DerivVar[n,indList, {shifts}], rest];



writeExpression[DerivVar[n_, indList_List, shifts_]] :=
  Module[
    {inds = Map[Replace[#,{shift[i_]^(p_) :> derivIndex[i,p],
                           shift[i_] :> derivIndex[i,0]}] &,shifts]},
    (* Print["{shifts} = ", {shifts}//FullForm]; *)
    (* Print["inds = ", inds]; *)
    FlattenBlock[{ToString[n], "_", inds}]];

writeExpression[spacing[i_]] :=
  "h";

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
  {"(",paren2[IfThen,a]@writeExpression[a], " ? ", paren2[IfThen,b]@writeExpression[b], " : ", paren2[IfThen,c]@writeExpression[c],")"};

writeExpression[KroneckerDelta[i1_TensorIndex,i2_TensorIndex]] :=
  {"delta", writeExpression[{i1,i2}]};

paren[x_] := {"(",x,")"};

(* paren2[_,_?NumberQ][x_] :=  *)
(*   x; *)

(* paren2[_,_Symbol][x_] :=  *)
(*   x; *)

(* paren2[_,_Tensor][x_] :=  *)
(*   x; *)

(* paren2[Times,_Power][x_] :=  *)
(*   x; *)

precedenceTable = {Rational, Tensor, Derivative, Power, Times, Plus, Equal, IfThen};

(* paren2[Plus,_Power][x_] :=  *)
(*   x; *)

paren2[where_,what_][x_] := 
  x;

paren2[where_,what_[___]][x_] := 
  If[!And@@Map[MemberQ[precedenceTable,#]&,{where,what}],
     paren[x],
     (* Print[where," ", what, " ", Position[precedenceTable,where], " ", Position[precedenceTable,where]]; *)
     If[Position[precedenceTable,where][[1,1]] < Position[precedenceTable,what][[1,1]],
        paren[x],
        x]];

    (* Print["where = ", where, ", what = ", what, ", positions = ", positions]; *)

(* paren2[_,_][x_] :=  *)
(*   paren[x]; *)

writeExpression[a_Plus] :=
  Riffle[Map[paren2[Plus,#][writeExpression[#]] &,List@@a]," + "];

writeExpression[a_Times] :=
  Riffle[Map[paren2[Times,#][writeExpression[#]] &,List@@a]," * "];

writeExpression[Power[a_,b_]] :=
  {paren2[Power,a]@writeExpression[a],"**",paren2[Power,b]@writeExpression[b]};

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
  {"D_t ",paren2[dot,a]@writeExpression[a]};

writeExpression[d_?(MemberQ[$DerivativeNames,#]&)[var_,inds___]] :=
  {writeExpression[d],writeExpression[{inds}]," ",paren2[Derivative,var]@writeExpression[var]};

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
