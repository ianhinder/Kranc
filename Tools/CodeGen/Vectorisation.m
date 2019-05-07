
(*  Copyright 2013 Ian Hinder and Erik Schnetter

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

BeginPackage["Vectorisation`", {"Errors`", "Helpers`", "Kranc`", "CodeGenC`",
                                "CodeGen`"}];

VectoriseExpression;
VectorisationLocalsToGridFunctions;
VectorisationSimpleAssignEquationList;
VectorisationAssignVariableInLoop;
VectorisationLoadVariable;
VectorisationConfigurationCCL;
VectorisationType;
VectorisationIncludeFiles;

Begin["`Private`"];

DefFn[
  VectoriseExpression[exprp_] :=
  Module[
    {expr, vectoriseRules, scalarRules},
    expr = exprp;

    (* Vectorise *)
    vectoriseRules = {
      x_ToReal -> x,
      x_ConditionExpression -> x,

      GFOffset[var_,i_,j_,k_] :> GFOffset[var,i,j,k],
      IfThen[cond_, x_, y_] :> IfThen[cond,
                                      x//.vectoriseRules, y//.vectoriseRules],

      expr:kpow[x_,a_]  -> expr,
      expr:kpown[x_,a_] -> expr,
      pow[x_,a_]        :> kpow[x//.vectoriseRules, a],
      pown[x_,a_]       :> kpown[x//.vectoriseRules, a],

      expr:"vec_load"[___] -> expr,
      expr:CArray[id_, {args__}] -> "vec_load"[expr],
      CTileArray[id_, {args__}] -> "vec_load"["getelt"[id, args]],

      IfPositive[c_,x_,y_] -> kifthen[ksignbit[c],y,x],

      Parameter[x_] -> ToReal[x],

      x_Integer  -> ToReal[x],
      x_Rational -> ToReal[x],
      x_Real     -> ToReal[x],
      E          -> ToReal[E],
      Pi         -> ToReal[Pi],
      cctkLbnd1  -> ToReal[cctkLbnd1],
      cctkLbnd2  -> ToReal[cctkLbnd2],
      cctkLbnd3  -> ToReal[cctkLbnd3],

      -x_ -> kneg[x],

      x_ + y_ -> kadd[x,y],
      x_ - y_ -> ksub[x,y],

      x_ * y_ -> kmul[x,y],
      x_ / y_ -> kdiv[x,y],

      acos[x_]    -> kacos[x],
      acosh[x_]   -> kacosh[x],
      asin[x_]    -> kasin[x],
      asinh[x_]   -> kasinh[x],
      atan[x_]    -> katan[x],
      atanh[x_]   -> katanh[x],
      cos[x_]     -> kcos[x],
      cosh[x_]    -> kcosh[x],
      exp[x_]     -> kexp[x],
      fabs[x_]    -> kfabs[x],
      fmax[x_,y_] -> kfmax[x,y],
      fmin[x_,y_] -> kfmin[x,y],
      fmod[x_,y_] -> kfmod[x,y],
      isgn[x_]    -> kisgn[x],
      log[x_]     -> klog[x],
      sgn[x_]     -> ksgn[x],
      sin[x_]     -> ksin[x],
      sinh[x_]    -> ksinh[x],
      sqrt[x_]    -> ksqrt[x],
      tan[x_]     -> ktan[x],
      tanh[x_]    -> ktanh[x]};

    expr = expr //. vectoriseRules;

    (* Optimise *)

    optimiseRules = {
      kpow[x_,n_Integer] -> kpown[x,n],
      kpow[ToReal[x_],y_] -> ToReal[pow[x,y]],
      kpown[ToReal[x_],y_] -> ToReal[pown[x,y]],
      (* Handle division *)
      kpown[x_,n_Integer] /; n<0 :> kdiv[ToReal[1],kpown[x,-n]],
      (* Implement integer powers efficiently *)
      kpown[x_,0] -> 1,
      kpown[x_,1] -> x,
      kpown[x_,n_Integer] /; n>1 && Mod[n,2]==0 :> kmul[kpown[x,n/2],kpown[x,n/2]],
      kpown[x_,n_Integer] /; n>1 && Mod[n,2]==1 :> kmul[x,kpown[x,n-1]],

      (* kmul[x_,kpow[y_,ToReal[-1]]] -> kdiv[x,y], *)
      (* kmul[x_,kpow[y_,ToReal[-2]]] -> kdiv[x,kmul[y,y]], *)

      kpow[x_,a_] /; a==1/2  :> ksqrt[x],
      kpow[x_,a_] /; a==-1/2 :> kdiv[ToReal[1],ksqrt[x]],

      kneg[ToReal[a_]]      -> ToReal[-a],
      kmul[ToReal[-1],x_]   -> kneg[x],
      kmul[ToReal[-1.0],x_] -> kneg[x],
      kmul[x_,ToReal[-1]]   -> kneg[x],
      kmul[x_,ToReal[-1.0]] -> kneg[x],
      kneg[kneg[x_]]        -> x,

      (* expr:kadd[x_,y_] /; !OrderedQ[expr] :> Sort[expr], *)
      kadd[ToReal[0],x_]             -> x,
      kadd[ToReal[0.0],x_]           -> x,
      kadd[x_,ToReal[0]]             -> x,
      kadd[x_,ToReal[0.0]]           -> x,
      ksub[ToReal[0],x_]             -> kneg[x],
      ksub[ToReal[0.0],x_]           -> kneg[x],
      ksub[x_,ToReal[0]]             -> x,
      ksub[x_,ToReal[0.0]]           -> x,
      kadd[kneg[x_],y_]              -> ksub[y,x],
      ksub[kneg[x_],y_]              -> kneg[kadd[x,y]],
      kadd[x_,kneg[y_]]              -> ksub[x,y],
      ksub[x_,kneg[y_]]              -> kadd[x,y],
      kneg[ksub[x_,y_]]              -> ksub[y,x],
      kadd[x_,x_]                    -> kmul[ToReal[2],x],
      ksub[x_,x_]                    -> ToReal[0],
      kadd[ToReal[a_],ToReal[b_]]    -> ToReal[a+b],
      ksub[ToReal[a_],ToReal[b_]]    -> ToReal[a-b],
      kadd[x:Except[_ToReal],
           ToReal[a_]]               -> kadd[ToReal[a],x],
      kadd[kadd[ToReal[a_],x_],y_]   -> kadd[ToReal[a],kadd[x,y]],
      kadd[kadd[ToReal[a_],x_],
           kadd[ToReal[b_],y_]]      -> kadd[ToReal[a+b],kadd[x,y]],
      kadd[x:Except[_ToReal],
           kadd[ToReal[a_],y_]]      -> kadd[ToReal[a],kadd[x,y]],
      kadd[ToReal[a_],
           kadd[ToReal[b_],x_]]      -> kadd[ToReal[a+b],x],
      
      (* expr:kadd[x_,y_] /; !OrderedQ[expr] :> Sort[expr], *)
      kmul[ToReal[0],x_]             -> ToReal[0],
      kmul[ToReal[0.0],x_]           -> ToReal[0],
      kmul[x_,ToReal[0]]             -> ToReal[0],
      kmul[x_,ToReal[0.0]]           -> ToReal[0],
      kmul[ToReal[+1],x_]            -> x,
      kmul[ToReal[+1.0],x_]          -> x,
      kmul[x_,ToReal[+1]]            -> x,
      kmul[x_,ToReal[+1.0]]          -> x,
      kmul[ToReal[-1],x_]            -> kneg[x],
      kmul[ToReal[-1.0],x_]          -> kneg[x],
      kmul[x_,ToReal[-1]]            -> kneg[x],
      kmul[x_,ToReal[-1.0]]          -> kneg[x],
      kdiv[ToReal[0],x_]             -> ToReal[0],
      kdiv[ToReal[0.0],x_]           -> ToReal[0],
      (* kdiv[x_,ToReal[0]]           -> ToReal[nan], *)
      (* kdiv[x_,ToReal[0.0]]         -> ToReal[nan], *)
      kdiv[x_,ToReal[y_]]            -> kmul[x,ToReal[1/y]],
      kdiv[x_,kdiv[y_,z_]]           -> kdiv[kmul[x,z],y],
      kdiv[kdiv[x_,y_],z_]           -> kdiv[x,kmul[y,z]],
      kmul[x_,kdiv[y_,z_]]           -> kdiv[kmul[x,y],z],
      kmul[kdiv[x_,y_],z_]           -> kdiv[kmul[x,z],y],
      kmul[kneg[x_],y_]              -> kneg[kmul[x,y]],
      kmul[x_,kneg[y_]]              -> kneg[kmul[x,y]],
      kdiv[kneg[x_],y_]              -> kneg[kdiv[x,y]],
      kdiv[x_,kneg[y_]]              -> kneg[kdiv[x,y]],
      kdiv[x_,x_]                    -> ToReal[1],
      kmul[ToReal[a_],ToReal[b_]]    -> ToReal[a*b],
      kdiv[ToReal[a_],ToReal[b_]]    -> ToReal[a/b],
      kmul[x:Except[_ToReal],
           ToReal[a_]]               -> kmul[ToReal[a],x],
      kdiv[x:Except[_ToReal],
           ToReal[y_]]               -> kmul[ToReal[1/y],x],
      kmul[kmul[ToReal[a_],x_],y_]   -> kmul[ToReal[a],kmul[x,y]],
      kmul[kmul[ToReal[a_],x_],
           kmul[ToReal[b_],y_]]      -> kmul[ToReal[a*b],kmul[x,y]],
      kmul[x:Except[_ToReal],
           kmul[ToReal[a_],y_]]      -> kmul[ToReal[a],kmul[x,y]],
      kmul[ToReal[a_],
           kmul[ToReal[b_],x_]]      -> kmul[ToReal[a*b],x],
      
      kacos[ToReal[x_]]            -> ToReal[acos[x]],
      kacosh[ToReal[x_]]           -> ToReal[acosh[x]],
      kasin[ToReal[x_]]            -> ToReal[asin[x]],
      kasinh[ToReal[x_]]           -> ToReal[asinh[x]],
      katan[ToReal[x_]]            -> ToReal[atan[x]],
      katanh[ToReal[x_]]           -> ToReal[atanh[x]],
      kcos[ToReal[x_]]             -> ToReal[cos[x]],
      kcosh[ToReal[x_]]            -> ToReal[cosh[x]],
      kexp[ToReal[x_]]             -> ToReal[exp[x]],
      kfabs[ToReal[x_]]            -> ToReal[fabs[x]],
      kfmax[ToReal[x_],ToReal[y_]] -> ToReal[fmax[x,y]],
      kfmin[ToReal[x_],ToReal[y_]] -> ToReal[fmin[x,y]],
      kfmod[ToReal[x_],ToReal[y_]] -> ToReal[fmod[x,y]],
      kfnabs[ToReal[x_]]           -> ToReal[-fabs[x]],
      kisgn[ToReal[x_]]            -> ToReal[isgn[x]],
      klog[ToReal[x_]]             -> ToReal[log[x]],
      ksgn[ToReal[x_]]             -> ToReal[sgn[x]],
      ksin[ToReal[x_]]             -> ToReal[sin[x]],
      ksinh[ToReal[x_]]            -> ToReal[sinh[x]],
      ksqrt[ToReal[x_]]            -> ToReal[sqrt[x]],
      ktan[ToReal[x_]]             -> ToReal[tan[x]],
      ktanh[ToReal[x_]]            -> ToReal[tanh[x]],

      kasin[kneg[x_]]          -> kneg[kasin[x]],
      kasinh[kneg[x_]]         -> kneg[kasinh[x]],
      katan[kneg[x_]]          -> kneg[katan[x]],
      katanh[kneg[x_]]         -> kneg[katanh[x]],
      kcos[kneg[x_]]           -> kcos[x],
      kcosh[kneg[x_]]          -> kcosh[x],
      kfabs[kneg[x_]]          -> kfabs[x],
      kfmax[kneg[x_],kneg[y_]] -> kneg[kfmin[x,y]],
      kfmin[kneg[x_],kneg[y_]] -> kneg[kfmax[x,y]],
      kfnabs[kneg[x_]]         -> kfnabs[x],
      kneg[kfabs[x_]]          -> kfnabs[x],
      kneg[kfnabs[x_]]         -> kfabs[x],
      ksin[kneg[x_]]           -> kneg[ksin[x]],
      ksinh[kneg[x_]]          -> kneg[ksinh[x]],
      ktan[kneg[x_]]           -> kneg[ktan[x]],
      ktanh[kneg[x_]]          -> kneg[ktanh[x]]};

    (* Print["Optimising:"]; *)
    (* Module[{exprOld = Null}, *)
    (*   Print[expr]; *)
    (*   While[expr =!= exprOld, *)
    (*     exprOld = expr; *)
    (*     expr = expr /. optimiseRules; *)
    (*     Print[expr//InputForm]]; *)
    (*   Print["Done optimising"]]; *)

    expr = expr //. optimiseRules;

    (* FMA (fused multiply-add) *)
    (* kmadd (x,y,z) =   xy+z
       kmsub (x,y,z) =   xy-z
       knmadd(x,y,z) = -(xy+z)
       knmsub(x,y,z) = -(xy-z) *)
    expr = expr //. {
      kadd[kmul[x_,y_],z_] -> kmadd[x,y,z],
      kadd[z_,kmul[x_,y_]] -> kmadd[x,y,z],
      ksub[kmul[x_,y_],z_] -> kmsub[x,y,z],
      ksub[z_,kmul[x_,y_]] -> knmsub[x,y,z],
      kneg[kmadd [x_,y_,z_]] -> knmadd[x,y,z],
      kneg[kmsub [x_,y_,z_]] -> knmsub[x,y,z],
      kneg[knmadd[x_,y_,z_]] -> kmadd [x,y,z],
      kneg[knmsub[x_,y_,z_]] -> kmsub [x,y,z]
      (* we could match this and similar patterns
         kmul[x_, kadd[y_, ToReal[+1]]] -> kmadd[x, y, x],
         kmul[x_, kadd[y_, ToReal[-1]]] -> kmsub[x, y, x],
         *)};

    (* Apply some transformations to scalar expressions *)

    scalarRules = {
      (* ToReal[x_] :> ToReal[x//.scalarRules], *)
      (* IfThen[cond_, x_, y_] :> IfThen[cond//.scalarRules, x, y], *)
      (* kpow[x_,y_] :> kpow[x, y//.scalarRules], *)
      (* kpown[x_,y_] :> kpown[x, y//.scalarRules], *)

      (* Mod[ToReal[x_],ToReal[y_]] -> ToReal[Mod[x,y]], *)
      (* x_ ^ y_ :> pow[x//.scalarRules, y//.scalarRules], *)
      (* pow[x_,y_] :> pow[x//.scalarRules, y//.scalarRules], *)
      x_ ^ y_ -> pow[x,y],

      (* don't generate large integer constants *)
      x_Integer /; Abs[x]>=2^31 :> 1.0*x,
      (* generate sufficient precision *)
      x_Rational :> N[x,30],
      Pi -> N[Pi,30],
      E  -> N[E,30]};
    
    expr = expr //. scalarRules;

    If[Cases[expr, _Power, Infinity] =!= {},
      ThrowError["Power found in " <> ToString[expr,InputForm]]];

    Return[expr]]];

(* Code generation: The following functions are called when vectorising. *)

(* Return a block of code that assigns 'src' to 'dest' *)
DefFn[
  storeVariableInLoop[dest:(_String|_Symbol), src:(_String|_Symbol)] :=
  {"vec_store_nta(", dest, ",", src, ")", ";\n"}];

(* Return a block of code that defines some variables for a series of
   calls to StorePartialVariableInLoop *)
DefFn[
  prepareStorePartialVariableInLoop[i:(_String|_Symbol),
                                    ilo:(_String|_Symbol),
                                    ihi:(_String|_Symbol)] :=
  {"vec_store_partial_prepare(", i, ",", ilo, ",", ihi, ")", ";\n"}];

(* Return a block of code that assigns 'src' to 'dest' *)
DefFn[
  storePartialVariableInLoop[dest:(_String|_Symbol), src:(_String|_Symbol)] :=
  {"vec_store_nta_partial(", dest, ",", src, ")", ";\n"}];

DefFn[
  VectorisationLocalsToGridFunctions[gridNames_List, localNames_List] :=
  VectorisationLocalsToGridFunctions[gridNames, localNames, {"vecimin", "vecimax"}]];

DefFn[
  VectorisationLocalsToGridFunctions[gridNames_List, localNames_List,
                                     {minVar_String, maxVar_String}] :=
  {prepareStorePartialVariableInLoop["i", minVar, maxVar],
   MapThread[storePartialVariableInLoop, {gridNames, localNames}]}];

DefFn[
  VectorisationSimpleAssignEquationList[lhss_List, rhss_List] :=
  {prepareStorePartialVariableInLoop["i", "vecimin", "vecimax"],
   MapThread[storePartialVariableInLoop, {lhss, rhss}]}];

DefFn[
  VectorisationLoadVariable[x_] := 
  {"vec_load(", x, ")"}];

DefFn[
  VectorisationAssignVariableInLoop[dest:(_String|_Symbol), src:CodeGenBlock] :=
  {dest, " = ", VectorisationLoadVariable[src], ";\n"}];

DefFn[
  VectorisationConfigurationCCL[] :=
  "REQUIRES Vectors\n"];

DefFn[
  VectorisationType[] :=
  "CCTK_REAL_VEC"];

DefFn[
  VectorisationIncludeFiles[] :=
  {"vectors.h"}];

End[];

EndPackage[];
