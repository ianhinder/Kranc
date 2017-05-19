
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

BeginPackage["KrancScript`", {"Errors`", "Helpers`", "Kranc`", "JLink`",
                              "KrancTensor`", "Piraha`", "MapLookup`"}];

CreateThornFromKrancScript;

If[$KrancTensorPackage === "TensorTools", Needs["TensorTools`"]];

Begin["`Private`"];

DefFn[
  CreateThornFromKrancScript[filename_String] :=
  Module[
    {code,stringRules,thorn,processed},

    Print["Creating thorn from ",filename];

    code = ParsePEG["kranc2.peg", "thorn", filename];

    stringRules = XMLElement[s_,{___},c_] :> s@@c;

    thorn = code[[2]] //. stringRules;

    processed = process[thorn]
]];

processAbbrev[h_[args___]] := h@@Map[ToString[Head[#]]&,{args}];
processAbbrev[h_] := ToString[h];
process[h__] :=
  Module[
    {},
    Print["No handler for ",Map[processAbbrev,{h}]];
    ThrowError["Failed to parse script"]];
process[h_] :=
  Module[
    {},
    Print[h];
    Print["No handler for ", processAbbrev[h]];
    (* Print["Full expression is: ", HoldForm[h[args]]]; *)
    ThrowError["Failed to parse script"]];

AddTmpCalcs[eqns_] := Module[{tmpeqns={}},
  eqns /. TmpEqn["TmpVar"[var_],"TmpCalc"[calc_],"Result"[res_]] :> AppendTo[tmpeqns,calc];
  Join[eqns,tmpeqns]
];

Global`KrancProjectionInfo[_] := False;
ZeroLength[x_String] := StringLength[x]===0;
process["projected_var"["name"[nm_], "x"[xv___], "y"[yv___], "z"[zv___]]] :=
  Module[{vsizes,var},
    vsizes = DeleteCases[{xv,yv,zv},_?ZeroLength];
    var = ToExpression[nm];
    Global`KrancProjectionInfo[var] = {Global`Sizes->vsizes};
    Return[var];
  ];

Clear[CombineInherited]
CombineInherited[{InheritedImplementations->{b_},c___},f_List,r_List] :=
    CombineInherited[{c},Join[f,{b}],r];
CombineInherited[{a_,c___},f_List,r_List] := CombineInherited[{c},f,Join[r,{a}]];
CombineInherited[{},f_,r_] := Join[{InheritedImplementations->f},r];

process[thorn:"thorn"[content___]] :=
  Module[
    {calcs = {}, name, parameters = {}, variables = {}, temporaries = {Global`boundx,Global`boundy,Global`boundz}, tensors, kernels,
     nonScalars, tensorRule, withInds, options = {}, operators = {}, bounds, iter, position, pos},
    
    Do[Switch[el,
              "calculation"[___], AppendTo[calcs,process[el]],
              "uname"[_], name = el[[1]],
              "parameters"["begin parameters end parameters"], Null,
              "variables"["begin variables end variables"], Null,
              "temporaries"["begin temporaries end temporaries"], Null,
              "parameters"[__], parameters = Join[parameters,List@@Map[process,el]],
              "variables"[__], variables = Join[variables,List@@Map[process,el]],
              "temporaries"[__], temporaries = Join[temporaries,List@@Map[process,el]],
              "operators"[__], operators = Join[operators,process[el]],
              "option"[__], options = Join[options, process[el]],
              _, ThrowError["Unrecognised element '"<>Head[el]<>"' in thorn"]],
       {el, {content}}];
    options=CombineInherited[options,{},{}];
    Map[GenOp,operators];

    bounds = {
      Global`boundx->"bound_x",
      Global`boundy->"bound_y",
      Global`boundz->"bound_z"
    };
    position = Map[First,Position[calcs,Where->Boundary]];
    Do[
      pos = position[[iter]];
      calcs[[pos]] = calcs[[pos]] /. (Equations -> eqn_) :> (Equations -> Join[bounds,eqn]);
    ,{iter,1,Length[position]}];

    (* Pull out implicitly defined temporary equations *)
    calcs = calcs /. (Equations->eqn_) :> (Equations->AddTmpCalcs[eqn]);
    (* Pull out implicitly defined temporary variables *)
    calcs = calcs /. TmpEqn["TmpVar"[var_],"TmpCalc"[calc_],"Result"[res_]] :> (
      AppendTo[temporaries,var];
      res);

    tensors = Join[variables,temporaries];
    kernels = Map[If[AtomQ[#],#,First[#]] &, tensors];
    Scan[DefineTensor, kernels];
    nonScalars = Cases[tensors, _tensor];
    tensorRule = tensor[k_,inds__] :> k[inds];

    withInds = nonScalars /. tensorRule; (* This should trigger the tensor character
                                            definitions *)

    SetEnhancedTimes[False];

    (* Need to do something about derives here *)
    derivatives = {};
    options = Join[{RealParameters -> parameters, Calculations -> calcs, Variables -> variables, Shorthands -> temporaries} /. tensorRule,
                   {PartialDerivatives -> derivatives},
                   options];

    (* Print["Creating thorn ", name, " with options:"]; *)
    (* Print[options]; *)
    
    CreateKrancThornTT2[name,Sequence@@options]];

process[calc:"calculation"[content___]] :=
  Module[
    {name,eqs,joinWord,where},
    name = Cases[calc, "uname"[n_]:>n][[1]];
    eqs = Cases[calc, "eqns"[es___]:>{es}][[1]];

    schedule = Cases[calc, "schedule"[___,"uname"[s_],___]:>s][[1]];
    onClause = Cases[calc, "schedule"[___,"uname"[s_],"on_clause"["uname"[f_]]]:>f];

    where = If[Length[onClause] > 0,
      Replace[ToLowerCase[onClause[[1]]],
        {"boundary" :> Boundary,
          _ :> Error["Unrecognised domain for calculation: " <> ToString[onClause[[1]]]]}],
      (* else *)
      Automatic];

    joinWord = If[StringMatchQ[schedule,"initial",IgnoreCase->True] ||
                  StringMatchQ[schedule,"analysis",IgnoreCase->True],
                  "at","in"];

    {Name -> name,
     Equations -> Map[process,eqs],
     Schedule -> {joinWord<>" "<>schedule},
     Where -> where}];

process["eqn"[lhs_,rhs_]] := Module[{name,eqs}, process[lhs] -> process[rhs]];
(*
process["tensor"["name"["Opast"],"indices"[ind___],follow___]] := Print["ind=",InputForm[ind]," follow=",InputForm[follow]];
process["tensor"["name"["Opast"],"indices"[ind___]],follow__] := Print["ind2=",InputForm[ind]," follow=",InputForm[follow]];
*)
process["Opast"["tensor"["name"[nm_], "indices"[]]]] := ToExpression[nm<>"Opast"];

process["tensor"["name"[k_],"indices"[]]] := ToExpression[If[Names[k] === {}, "Global`"<>k, k]];

process["tensor"["name"[nm_],"indices"[ind___],"bracket"[br1_,br2_,br3_]]] :=
  Module[{n1,n2,n3,var},
  n1 = process[br1];
  n2 = process[br2];
  n3 = process[br3];
  var = process["tensor"["name"[nm],"indices"[ind]]];
  I3D[var,n1,n2,n3]]

builtIns = {"true" -> True,
            "false" -> False,
            "PI" -> Pi};

Do[
  Module[
    {lhs = process["tensor"["name"[b[[1]]],"indices"[]]],
     rhs = b[[2]]},
    Evaluate[lhs] := rhs],
  {b,builtIns}];

process["tensor"["name"[k_],inds_]] :=
   tensor[ToExpression[If[Names[k] === {}, "Global`"<>k, k]],Sequence@@process[inds]];

fprint[x_] := (Print[x//InputForm]; x);

(* Simple operator rules *)
TempNum = 1
Clear[PDiv,MakeTemp,dtens];
MakeTemp[x1_,inds_] := Module[{tmpnm,inds2,tensorAST},
  tmpnm="tmp"<>ToString[TempNum++];
  inds2 = inds /.
    {
      TensorIndex[n1_,"l"] :> "lower_index"["index_symbol"[n1]],
      TensorIndex[n1_,"u"] :> "upper_index"["index_symbol"[n1]]
    } /. List->"indices";
  tensorAST="tensor"["name"[tmpnm],inds2];
  {tmpnm,tensorAST}];
MakeTemp[x1__] := 
  MakeTemp[x1,freesIn[x1]];

process["dtensor"["dname"[dname_],inds_,"tensor"[tensor__]]] := ToExpression[dname][process["tensor"[tensor]],Sequence@@process[inds]];

process["dtensor"["dname"["D"],inds_,"tensor"[tensor__]]] := PD[process["tensor"[tensor]],Sequence@@process[inds]];
(* dtensor[dname[Opast], tensor[name[pi], indices[]]] *)
process["dtensor"["dname"["Opast"],"tensor"["name"[name_],"indices"[]]]] := process["tensor"["name"[name<>"Opast"],"indices"[]]];
process["dtensor"["dname"["D"],inds_,"expr"[tensor__]]] := Module[{summed,tmp,tmpAST,procexpr,result},
  procexpr =process["expr"[tensor]];
  tmparray=MakeTemp[procexpr];
  tmpAST = tmparray[[2]];
  tmp = process[tmpAST];
  tmpnm = tmparray[[1]];
  summed=makeSumOverDummies[PD[tmp,Sequence@@process[inds]]];
  (* Construct the AST for the equation, re-use Kranc machinery *)
  precalc="eqn"[tmpAST,"expr"[tensor]];
  calc=process[precalc];
  result=TmpEqn["TmpVar"[tmp],"TmpCalc"[calc],"Result"[summed]];
  result]

process["dtensor"["dname"["D"], "indices"["lower_index"["index_symbol"["t"]]],"tensor"[tensor__]]] :=
  dot[process["tensor"[tensor]]];

process["indices"[inds___]] := Map[process, {inds}];

process[(pos:("lower_index"|"upper_index"))["index_symbol"[i_]]] :=
  If[StringMatchQ[i,DigitCharacter],
     ToExpression[i],
     TensorIndex[i, If[pos==="lower_index","l","u"]]];

process["func"["fname"[name_],exprs__]] :=
  Module[
    {fns},
    fns = {"sin" -> Sin, "cos" -> Cos,
           "tan" -> Tan, "cot" -> Cot,
           "sec" -> Sec, "csc" -> Csc,
           "atan" -> ArcTan, "asin" -> ArcSin, "acos" -> ArcCos,
           "sinh" -> Sinh, "cosh" -> Cosh, "tanh" -> Tanh,
           (* "if" -> IfThen, *) "max" -> Max, "min" -> Min,
           "copysign" -> copysign,
           "abs"->Abs,"sqrt" -> Sqrt, "exp" -> Exp};
    If[MemberQ[First/@fns,name], (name/.fns)@@Map[process,{exprs}],
      ThrowError["Function "<>name<>" is not yet supported"]]];

process["value"["neg"[_],arg_]] := -process[arg];
process["mexpr"[mul_]] := process[mul];
process["mexpr"[]] := 0;
process["ifexpr"[cond_, opt1_, opt2_]] :=
   IfThen[ process[cond], process[opt1], process[opt2] ];

process["coexpr"["cexpr"[cexpr__]]] := process["cexpr"[cexpr]];

process["cexpr"[Longest[v1__], "logop"["or"], v2__]] :=
   ChemoraOpOr[ process["cexpr"[v1]], process["cexpr"[v2]] ];

process["cexpr"[Longest[v1__], "logop"["and"], v2__]] :=
   ChemoraOpAnd[ process["cexpr"[v1]], process["cexpr"[v2]] ];

process["cexpr"[carg_]] := process[carg];


process["coexpr"[v1_, "cmpop"[cmpop_], v2_]] :=
   Module[
     { opMap = { "<"  -> Less,           "<=" -> LessEqual,
                 "==" -> ChemoraOpEqual, "!=" -> ChemoraOpNotEqual,
                 ">=" -> GreaterEqual,   ">"  -> Greater },
       mappedOp },
     mappedOp = cmpop /. opMap;
     If[ StringQ[mappedOp],
         ThrowError["Unrecognized comparison operator: " <> cmpop] ];
     mappedOp[ process[v1], process[v2] ] ];

(* Addition, subtraction, multiplication and division are all left-associative *)
process["mexpr"[a_,"addop"["+"],b_]] := process[a] + process[b];
process["mexpr"[a_,"addop"["-"],b_]] := process[a] - process[b];
process["mexpr"[cs___, a_, "addop"["+"], b_]] := process["mexpr"[cs,a]] + process[b];
process["mexpr"[cs___, a_, "addop"["-"], b_]] := process["mexpr"[cs,a]] - process[b];

process["expr"[m:"mexpr"[___]]] := process[m];
process["expr"["neg"[_],m:"mexpr"[___]]] := -process[m];

process["mul"[pow_]] := process[pow];
process["mul"[]] := 1;
process["mul"[cs___, a_, "mulop"["*"], b_]] := process["mul"[cs,a]] * process[b];
process["mul"[cs___, a_, "mulimp"[___], b_]] := process["mul"[cs,a]] * process[b];
process["mul"[cs___, a_, "mulop"["/"], b_]] := process["mul"[cs,a]] / process[b];
process["mexpr"["expr"[ex1_],"expr"[ex2_]]] := ThrowError[ToString[InputForm[ex1]]<> " :: "<>ToString[InputForm[ex2]]];

process["pow"["neg"[_],a_,b_]] := -process[a]^process[b];
process["pow"["neg"[_],a_]] := -process[a];
process["pow"[a_,b_]] := process[a]^process[b];
process["pow"[a_]] := process[a];

process["value"[a_]] := process[a];

process["number"[a_]] := ToExpression[a];
process["rawmath"["rawtext"[rm_]]] := ToExpression[rm];
process["rawc"["rawtext"[rm_]]] := Global`RawMath[rm];

process["uname"[n_]] := n;

process["option"["inherit"[imps__]]] :=
  {InheritedImplementations -> Map[process, {imps}]};

process["option"["implement"[imp_]]] :=
  {Implementation -> process[imp]};

process["leftenc"[sym_],"number"[num_]] := sym <> num;
process["number"[num_],"rightenc"[sym_]] := num <> sym;
process["infinity"[_]] := "*";

processRange[value_,minOrMax_,paramName_] :=
  If[StringQ[value] && value === "*",value,
    Module[{tmp},
      tmp = N[value];
      If[NumberQ[tmp],1,ThrowError[minOrMax<>" value for parameter "<>paramName<>" is not a number"]];
      Return[tmp]]]

process["parameter"["name"[nm_], "type"[ty_]]] :=
Module[{dtmp,lotmp,hitmp},
  dtmp = 1.0;
  lotmp = "*";
  hitmp = "*";
  {Name -> ToExpression[nm],
   VariableType -> ty,
   Description -> "Parameter " <> nm, (*StringTake[desc,{2,StringLength[desc]-1}],*)
   AllowedValues -> {{Value->ToString[lotmp] <> ":" <> ToString[hitmp]}},
   Default -> dtmp}]

process["parameter"["name"[nm_],"type"[ty_],"expr"[exprd_]]] :=
Module[{dtmp,lotmp,hitmp},
  dtmp = process[exprd];
  lotmp = "*";
  hitmp = "*";
  {Name -> ToExpression[nm],
   VariableType -> ty,
   Description -> "Parameter " <> nm, (*StringTake[desc,{2,StringLength[desc]-1}],*)
   AllowedValues -> {{Value->ToString[lotmp] <> ":" <> ToString[hitmp]}},
   Default -> dtmp}]

process["parameter"["name"[nm_], "type"[ty_], "quote"[desc_], "expr"[exprd_]]] :=
Module[{dtmp,lotmp,hitmp},
  dtmp = process[exprd];
  lotmp = "*";
  hitmp = "*";
  {Name -> ToExpression[nm],
   VariableType -> ty,
   Description -> StringTake[desc,{2,StringLength[desc]-1}],
   AllowedValues -> {{Value->ToString[lotmp] <> ":" <> ToString[hitmp]}},
   Default -> dtmp}]

process["parameter"["name"[nm_],"type"[ty_],"quote"[desc_]]] :=
Module[{dtmp,lotmp,hitmp},
  dtmp = 1.0;
  lotmp = "*";
  hitmp = "*";
  {Name -> ToExpression[nm],
   VariableType -> ty,
   Description -> StringTake[desc,{2,StringLength[desc]-1}],
   AllowedValues -> {{Value->ToString[lotmp] <> ":" <> ToString[hitmp]}},
   Default -> dtmp}]

process["parameter"["name"[nm_],"type"[ty_],"quote"[desc_],"expr"[def_],"parlo"[le__],"parhi"[re__]]] := 
Module[{dtmp,lotmp,hitmp},
  dtmp = N[process[def]];
  lotmp = processRange[process[le],"Minimum",nm];
  hitmp = processRange[process[re],"Maximum",nm];
  If[NumberQ[dtmp],1,ThrowError["Default value for parameter "<>nm<>" is not a number"]];
  {Name -> ToExpression[nm],
   VariableType -> ty,
   Description -> StringTake[desc,{2,StringLength[desc]-1}],
   AllowedValues -> {{Value->ToString[lotmp] <> ":" <> ToString[hitmp]}},
   Default -> dtmp}]

flags = ScriptFlags;

process["option"["use"[features__]]] :=
  Map[(lookup[flags,#] -> True) &,{features}/.(("feature"[n_]):>n)];

process["option"["disable"[features__]]] :=
  Map[(lookup[flags,#] -> False) &,{features}/.(("feature"[n_]):>n)];

(* Operators *)

(* deqn[doper, name[xdiv], expr] *)
process[h:"operators"["deqns"[eqs___]]] :=
  Map[process, {eqs}];

process[d:"deqn"[___]] :=
  Module[{},
    Print["(deqn) No handler for ", d];
    ThrowError["Failed to parse script"]];

replaceInd[indices_,vals_] := Module[{i},
  Table[Rule[indices[[i]],vals[[i]]],{i,1,Length[vals]}]]

(* If the programmer defines a custom operator and
   uses letters like i,j,k for the indices, then we
   need to generate a permutation of all mappings
   of these letters to unique dimensions, e.g.
   i,j = x,y, or i,j = x,z. *)
CreatePermutations[args_] := Module[{arr,n,perm,i},
  arr=DeleteDuplicates[args];
  n=Length[arr];
  perm=DeleteCases[Permutations[{"x","y","z"},n],_?(Length[#] != n&)];
  Table[StringReplace[args,
    replaceInd[arr,perm[[i]]]]
    ,{i,1,Length[perm]}]
  ];

(* Determine whether the arg is a possible index symbol. *)
IsIndex[arg_] := Length[StringCases[arg,RegularExpression["^[a-z]$"]]]===1;

(* Use two lists as a kind of map. If val is in position i of the
   first list, return the value at position i in the second list. *)
LookupTwo[val_,{},_] := Null;
LookupTwo[val_,_,{}] := Null;
LookupTwo[val_,{h1_,t1___},{h2_,t2___}] := If[val === h1,h2,LookupTwo[val,{t1},{t2}]];

(* Consumes output of NewBracket calls, combining them to form a 3-index
   input to the "bracket"[] symbol in the final, dimension specific version
   of a custom defined operator. *)
CombineBrackets[{dn_,n_,arg_}] := Module[{i},
  Apply["bracket",Table[
    If[n == i,arg,"number"["0"]],
    {i,1,3}]]];
CombineBrackets[{dn1_,n1_,arg1_},{dn2_,n2_,arg2_}] := Module[{i},
  If[n1 == n2,ThrowError["Two specifications for the same index in operator "<>dn]];
  Apply["bracket",Table[
    If[n1 == i,arg1,
      If[n2 == i,arg2,"number"["0"]]],
    {i,1,3}]]];
CombineBrackets[{dn1_,n1_,arg1_},{dn2_,n2_,arg2_},{dn3_,n3_,arg3_}] := Module[{i},
  If[n1 == n2,ThrowError["Two specifications for the same index in operator "<>dn]];
  If[n1 == n3,ThrowError["Two specifications for the same index in operator "<>dn]];
  If[n2 == n3,ThrowError["Two specifications for the same index in operator "<>dn]];
  Apply["bracket",Table[
    If[n1 == i,arg1,
      If[n2 == i,arg2,
        If[n3 == i,arg3,"number"["0"]]]],
    {i,1,3}]]];

(* Process a single argument to a custom defined bracket, determining
   which position the argument is at (indx), and what the contents at
   that position are (expt). The value dn is passed through for use in
   error messages. *)
NewBracket[dn_,expr_,arr_,perm_,pos_] :=
  Module[{indxLet,indx,br,res,i,expt},
    indxLet={};
    expt = (expr /.
      "name"[ind_?IsIndex] :> (AppendTo[indxLet,ind];"number"["0"]))
        /. "tensor"["number"["0"],"indices"[]] :> "number"["0"];
    indxLet=RemoveDuplicates[indxLet];
    If[Length[indxLet]>1,ThrowError["Multiple indices used together ("<>StringJoin[Riffle[indxLet,","]]<>") in definition ("<>dn<>")"]];
    If[Length[indxLet]==0,
      indx = pos,
      indxLet=indxLet[[1]];
      indx = LookupTwo[indxLet,arr,perm]];
    If[indx == Null,ThrowError["Undefined Index ("<>indx<>") used in definition ("<>dn<>")"]];
    res = {dn,indx,expt};
    Return[res];
  ];

(* Used to combine all 3 possible versions of arguments to "bracket"[].
   The result of this will be something of the form "bracket"[v1,v2,v3],
   where v1, v2, and v3 are expressions where the indices have been
   substituted with 0. *)
ApplyBrackets[dn_,uniqarr_,uniqperm_,br_] := CombineBrackets[
  NewBracket[dn,br,uniqarr,uniqperm,1]];
ApplyBrackets[dn_,uniqarr_,uniqperm_,br1_,br2_] := CombineBrackets[
  NewBracket[dn,br1,uniqarr,uniqperm,1],
  NewBracket[dn,br2,uniqarr,uniqperm,2]];
ApplyBrackets[dn_,uniqarr_,uniqperm_,br1_,br2_,br3_] := CombineBrackets[
  NewBracket[dn,br1,uniqarr,uniqperm,1],
  NewBracket[dn,br2,uniqarr,uniqperm,2],
  NewBracket[dn,br3,uniqarr,uniqperm,3]];

(* Create a set of rules for converting indexes to numbers, e.g. {i -> 1,j -> 2} *)
IndexSubstitutionRules[ar_,pm_] := Module[{i},
  Table[Rule["lower_index"["index_symbol"[ar[[i]]]],"number"[ ToString[pm[[i]]] ]],{i,1,Length[ar]}]]

(* Generate an array of indices from an index expression. *)
GenArr["indices"[ind__]] := Map[GenArr,{ind}];
GenArr["lower_index"[ind_]] := GenArr[ind];
GenArr["upper_index"[ind_]] := GenArr[ind];
GenArr["index_symbol"[letter_]] := letter;
GenArr[ind_] := ThrowError["GenArr "<>ToString[ind]];

(* Generate all the index specific versions of user defined operators.
   These definitions go into the global Mathematica variable space when
   ToExpression[] is evaluated. *)
GenOp[operator[dn_,ind_,nm_,ex_]] :=
  Module[{arr,perm,permno,str,uniqarr,uniqperm,ex2},
    arr=GenArr[ind];
    uniqarr=RemoveDuplicates[arr];
    perm=CreatePermutations[arr];
    For[i=1,i<=Length[perm],i++,
      uniqperm = Map[ToExpression,RemoveDuplicates[StringSplit[StringReplace[perm[[i]],{"x"->"1","y"->"2","z"->"3"}],""]]];

      ex2 = ex /. "bracket"[br__] :> ApplyBrackets[dn,uniqarr,uniqperm,br];
      ex2 = ex2 /. IndexSubstitutionRules[uniqarr,uniqperm];
      (* TODO: I feel like this transformation should happen elsewhere in the codebase. Substitute del[1]->x, etc. *)
      ex2 = ex2 /.
        "tensor"["name"["del"], "indices"["number"[nu_]]] :>
          "tensor"["name"[{"dx","dy","dz"}[[ToExpression[nu]]]],"indices"[]];

      permno=StringJoin[Riffle[StringCases[StringReplace[perm[[i]],{"x"->"1","y"->"2","z"->"3"}],RegularExpression["[1-3]"]],","]];
      ex2 = ToString[InputForm[process[ex2]]];
      (* TODO: This is kind of hacky *)
      str=dn<>"["<>nm<>"_,"<>permno<>"] := "<>ex2;

      ToExpression[str];
    ]];

process["tname"[tens_]] := process[tens]
process["deqn"["doper"["dname"[dn_],ind:"indices"[__]],"barename"[nm_],"expr"[ex_]]] := 
  Module[{},
    operator[dn,ind,nm,ex]]

process["deqn"[("dtensor"["dname"[dname_],
                          "tensor"["name"[tName_],
                                   "indices"[tinds___]]]),
               rhs:"expr"[___]]] :=
  Module[
    {gridInds = {tinds} /. "lower_index"["index_symbol"[i_]] :> Unique[ToExpression@i],
     gridIndSyms,ind,rhsEval},
    gridIndSyms = {tinds} /. "lower_index"["index_symbol"[i_]] :> i;

    If[Length[gridIndSyms] > 1,
       ThrowError["Kranc does not yet support explicit specification of finite difference operators of dimension higher than 1.  1D operators will be automatically generalised to act on grid functions of all dimensions."]];

    rhsEval = (process[rhs] /. {tensor[ToExpression[tName],__] :> 1,
                                "int"[n_]:>ToExpression[n],
                                Global`h :> spacing[1],
                                Global`h1 :> spacing[1],
                                Global`h2 :> spacing[2],
                                Global`h3 :> spacing[3],
                                dimof[i_] :> Position[gridIndSyms,i][[1,1]]});

    ind = Unique[i];

    ToExpression[dname][Pattern[Evaluate[ind],_]] -> (rhsEval/.{spacing[1]->spacing[ind],shift[1]->shift[ind]})];

(* Differencing operators *)

process["tensor"["name"[k_],
                 "indices"[a___,
                           pos_["index_expr"[sym:"index_symbol"[i_],
                                             "index_op"[op_], int_]],
                           b___]]] :=
  (shift[dimof[i]]^If[op==="+", int, -int] *
   process["tensor"["name"[k],
                    "indices"[a,pos[sym],b]]]);

(* TODO: support multi-dimensional grid function accesses, i.e. v_(i+1,j) *)

process["tensor"["name"[k_],
                 "indices"[a___,
                           pos_["index_expr"[sym:"index_symbol"[___]]],
                           b___]]] :=
   process["tensor"["name"[k],
                           "indices"[a,pos[sym],b]]];



End[];

EndPackage[];
