
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

    expr = expr //. Power[x_,y_] -> pow[x,y];
    
    (* Vectorise *)
    vectoriseRules = {
      x_ToReal -> x,
      x_ConditionExpression -> x,
      IfThen[cond_, x_, y_] :> IfThen[cond, x//.vectoriseRules, y//.vectoriseRules],
      pow[x_,y_]  :> kpow[x//.vectoriseRules,y],
      kpow[x_,y_]  :> kpow[x,y],

      x:"vec_load"[___] :> x,
      x:CArray[id_, {args__}] :> "vec_load"[x],

      Parameter[x_] -> ToReal[x],

      x_Integer  -> ToReal[x],
      x_Rational -> ToReal[x],
      x_Real     -> ToReal[x],
      E          -> ToReal[E],
      Pi         -> ToReal[Pi],

      -x_ -> kneg[x],
      
      x_ + y_ -> kadd[x,y],
      x_ - y_ -> ksub[x,y],
      
      x_ * y_ -> kmul[x,y],
      x_ / y_ -> kdiv[x,y],
      
      acos[xx_]  -> kacos[xx],
      acosh[xx_] -> kacosh[xx],
      asin[xx_]  -> kasin[xx],
      asinh[xx_] -> kasinh[xx],
      atan[xx_]  -> katan[xx],
      atanh[xx_] -> katanh[xx],
      cos[xx_]   -> kcos[xx],
      cosh[xx_]  -> kcosh[xx],
      sin[xx_]   -> ksin[xx],
      sinh[xx_]  -> ksinh[xx],
      tan[xx_]   -> ktan[xx],
      tanh[xx_]  -> ktanh[xx],
      
      exp[x_]     -> kexp[x],
      fabs[x_]    -> kfabs[x],
      fmax[x_,y_] -> kfmax[x,y],
      fmin[x_,y_] -> kfmin[x,y],
      isgn[x_]    -> kisgn[x],
      log[x_]     -> klog[x],
      sgn[x_]     -> ksgn[x],
      sqrt[x_]    -> ksqrt[x]};

    expr = expr //. vectoriseRules;

    (* Optimise *)
    expr = expr //. {

      (* Handle division *)
      kpow[x_,n_Integer] /; n<0 :> kdiv[ToReal[1],kpow[x,-n]],

      (* Implement integer powers efficiently *)
      kpow[x_,0] -> 1,
      kpow[x_,1] -> x,
      kpow[x_,n_Integer] /; n>1 && Mod[n,2]==0 :> kmul[kpow[x,n/2],kpow[x,n/2]],
      kpow[x_,n_Integer] /; n>1 && Mod[n,2]==1 :> kmul[x,kpow[x,n-1]],

      kmul[x_,kpow[y_,-1]] -> kdiv[x,y],
      kmul[x_,kpow[y_,-2]] -> kdiv[x,kmul[y,y]],

      kpow[x_,(1/2)|0.5]     -> ksqrt[x],
      kpow[x_,(-1/2)|(-0.5)] -> kdiv[ToReal[1],ksqrt[x]],

      kneg[ToReal[a_]]      -> ToReal[-a],
      kmul[ToReal[-1],x_]   -> kneg[x],
      kmul[ToReal[-1.0],x_] -> kneg[x],
      kmul[x_,ToReal[-1]]   -> kneg[x],
      kmul[x_,ToReal[-1.0]] -> kneg[x],
      kneg[kneg[x_]]        -> x,
      
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
      kadd[x:Except[_ToReal],ToReal[a_]] -> kadd[ToReal[a],x],
      kadd[kadd[ToReal[a_],x_],y_]   -> kadd[ToReal[a],kadd[x,y]],
      kadd[kadd[ToReal[a_],x_],
           kadd[ToReal[b_],y_]]      -> kadd[ToReal[kadd[a,b]],kadd[x,y]],
      kadd[x:Except[_ToReal],
           kadd[ToReal[a_],y_]]      -> kadd[ToReal[a],kadd[x,y]],
      
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
      kmul[ToReal[a_],ToReal[b_]]    -> ToReal[a b],
      (* kdiv[ToReal[a_],ToReal[b_]]    -> ToReal[a/b], *)
      kmul[x:Except[_ToReal],ToReal[a_]] -> kmul[ToReal[a],x],
      kdiv[x:Except[_ToReal],ToReal[y_]] -> kmul[ToReal[1/y],x],
      kmul[kmul[ToReal[a_],x_],y_]   -> kmul[ToReal[a],kmul[x,y]],
      kmul[kmul[ToReal[a_],x_],
           kmul[ToReal[b_],y_]]      -> kmul[ToReal[a*b],kmul[x,y]],
      kmul[x:Except[_ToReal],
           kmul[ToReal[a_],y_]]      -> kmul[ToReal[a],kmul[x,y]],
      
      kasin[kneg[xx_]]           -> kneg[kasin[xx]],
      kasinh[kneg[xx_]]          -> kneg[kasinh[xx]],
      katan[kneg[xx_]]           -> kneg[katan[xx]],
      katanh[kneg[xx_]]          -> kneg[katanh[xx]],
      kcos[kneg[xx_]]            -> kcos[xx],
      kcosh[kneg[xx_]]           -> kcosh[xx],
      ksin[kneg[xx_]]            -> kneg[ksin[xx]],
      ksinh[kneg[xx_]]           -> kneg[ksinh[xx]],
      ktan[kneg[xx_]]            -> kneg[ktan[xx]],
      ktanh[kneg[xx_]]           -> kneg[ktanh[xx]],
      kfmax[kneg[xx_],kneg[yy_]] -> kneg[kfmin[xx,yy]],
      kfmin[kneg[xx_],kneg[yy_]] -> kneg[kfmax[xx,yy]],
      kfabs[kneg[xx_]]           -> kfabs[xx],
      kfnabs[kneg[xx_]]          -> kfnabs[xx],
      kneg[kfabs[xx_]]           -> kfnabs[xx],
      kneg[kfnabs[xx_]]          -> kfabs[xx]};


    (* The above optimisations introduce divisions and multiplications
       of unvectorised quantities which Mathematica sometimes converts
       into Power.  Hence, we need to convert these back to pow. *)
    expr = expr //. {x:ToReal[_] :> (x /. {Power[y_, power_] :> pow[y,power]})};

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
      ToReal[x_] :> ToReal[x//.scalarRules],
      IfThen[cond_, x_, y_] :> IfThen[cond//.scalarRules, x, y],
      
      (* don't generate large integer constants *)
      x_Integer /; Abs[x]>10^10 :> 1.0*x,
      (* generate sufficient precision *)
      x_Rational :> N[x,30],
      Pi -> N[Pi,30],
      E  -> N[E,30]};
    
    expr = expr //. scalarRules;
    
    expr = expr /. {
      x:(kmul|kadd)[a_,b_] /; !OrderedQ[x] :> Sort[x]};

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
