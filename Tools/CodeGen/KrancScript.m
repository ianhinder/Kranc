
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

BeginPackage["KrancScript`", {"Errors`", "Helpers`", "Kranc`", "JLink`", "TensorTools`",
                              "KrancTensor`", "Piraha`", "MapLookup`"}];

CreateThornFromKrancScript;

Begin["`Private`"];

DefFn[
  CreateThornFromKrancScript[filename_String] :=
  Module[
    {code,stringRules,thorn,processed},

    Print["Creating thorn from ",filename];

    code = Parse["kranc2.peg", "thorn", filename];

    code = code /. ("startIndex" -> _) :> Sequence[];
    code = code /. ("endIndex" -> _) :> Sequence[];

    stringRules = XMLElement[s_,_,c_] :> s@@c;

    thorn = code[[2]] //. stringRules;

    processed = process[thorn]
]];

process[h_[args___]] :=
  Module[
    {},
    Print["No handler for ", h@@Map[ToString[Head[#]]&,{args}]];
    ThrowError["Failed to parse script"]];

process[thorn:"thorn"[content___]] :=
  Module[
    {calcs = {}, name, variables = {}, temporaries = {}, tensors, kernels,
     nonScalars, tensorRule, withInds, options = {}},

    Do[Switch[el,
              "calculation"[___], AppendTo[calcs,process[el]],
              "name"[_], name = el[[1]],
              "variables"[__], variables = Join[variables,List@@Map[process,el]],
              "temporaries"[__], temporaries = Join[temporaries,List@@Map[process,el]],
              "option"[__], options = Join[options, process[el]],
              _, ThrowError["Unrecognised element '"<>Head[el]<>"' in thorn"]],
       {el, {content}}];

    tensors = Join[variables,temporaries];
    kernels = Map[If[AtomQ[#],#,First[#]] &, tensors];
    Scan[DefineTensor, kernels];
    nonScalars = Cases[tensors, _tensor];
    tensorRule = tensor[k_,inds__] :> k[inds];

    withInds = nonScalars /. tensorRule; (* This should trigger the tensor character
                                            definitions *)

    SetEnhancedTimes[False];

    options = Join[{Calculations -> calcs, Variables -> variables, Shorthands -> temporaries} /. tensorRule,
                   options];

    CreateKrancThornTT2[name,Sequence@@options]];

process[calc:"calculation"[content___]] :=
  Module[
    {name,eqs,joinWord},
    name = Cases[calc, "uname"[n_]:>n][[1]];
    eqs = Cases[calc, "eqns"[es___]:>{es}][[1]];

    schedule = Cases[calc, "schedule"[___,"uname"[s_],___]:>s][[1]];
    joinWord = If[StringMatchQ[schedule,"initial",IgnoreCase->True] ||
                  StringMatchQ[schedule,"analysis",IgnoreCase->True],
                  "at","in"];

    {Name -> name,
     Equations -> Map[process,eqs],
     Schedule -> {joinWord<>" "<>schedule},
     Where -> Automatic}];

process["eqn"[lhs_,rhs_]] := Module[{name,eqs}, process[lhs] -> process[rhs]];

process["tensor"["name"[k_],"indices"[]]] := ToExpression[If[Names[k] === {}, "Global`"<>k, k]];

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

process["dtensor"[inds_,tensor_]] := PD[process[tensor],Sequence@@process[inds]];
process["dtensor"["indices"["_t"],tensor_]] := dot[process[tensor]];

process["indices"[inds_]] :=
  Module[
    {lower,upper,is},

    lower[s_String] :=
    If[s === "", {},
       If[StringTake[s,1] === "^", upper[StringDrop[s,1]],
          If[StringTake[s,1] === "_", ThrowError["Repeated '_'"],
             Prepend[lower[StringDrop[s,1]], TensorIndex[StringTake[s,1],"l"]]]]];

    upper[s_String] :=
    If[s === "", {},
       If[StringTake[s,1] === "_", lower[StringDrop[s,1]],
          If[StringTake[s,1] === "^", ThrowError["Repeated '^'"],
             Prepend[upper[StringDrop[s,1]], TensorIndex[StringTake[s,1],"u"]]]]];

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
process["expr"[a_, "addop"["+"], bs__]] := process[a] + process["expr"[bs]];

process["mul"[pow_]] := process[pow];
process["mul"[]] := 1;
process["mul"[a_, "mulop"["/"], b_,cs___]] := Times[process[a] / process[b],process["mul"[cs]]];
process["mul"[a_, "mulop"["*"], bs__]] := process[a] * process["mul"[bs]];

process["pow"[a_,b_]] := process[a]^process[b];
process["pow"[a_]] := process[a];

process["value"[a_]] := process[a];

process["number"[a_]] := ToExpression[a];

process["uname"[n_]] := n;

process["option"["inherit"[imps__]]] :=
  {InheritedImplementations -> Map[process, {imps}]};

process["option"["implement"[imp_]]] :=
  {Implementation -> process[imp]};

flags = {"loopcontrol"->UseLoopControl,"vectors"->UseVectors,"opencl"->UseOpenCL,
         "jacobian"->UseJacobian, "cse" -> CSE};

process["option"["use"[features__]]] :=
  Map[(lookup[flags,#] -> True) &,{features}/.(("feature"[n_]):>n)];

process["option"["disable"[features__]]] :=
  Map[(lookup[flags,#] -> False) &,{features}/.(("feature"[n_]):>n)];


End[];

EndPackage[];
