
(*  Copyright 2013 Ian Hinder

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

BeginPackage["Object`", {"Errors`", "Helpers`", "Kranc`"}];

NewObject;
SetObjectField;
GetObjectField;
AppendObjectField;
JoinObjectField;
ApplyToObjectField;

Begin["`Private`"];

(* Low-level functions *)

DefFn[
  NewObject[class_Symbol, initList_List] :=
  Apply[class, initList]];

DefFn[
  keys[obj_] :=
  Sort[Map[First, obj]]];

DefFn[
  hasValueQ[obj_, field_] :=
  MemberQ[keys[obj], field]];

DefFn[
  SetObjectField[obj_, field_String, value_] :=
  If[Length[Cases[obj, HoldPattern[field -> _]]] > 0,
     Replace[obj, (field -> _) :> field -> value, {1}],
     Append[obj, field -> value]]];

DefFn[
  GetObjectField[obj_, field_String] :=
  Cases[obj, (field -> value_) :> value][[1]]];

(* High level functions *)

DefFn[
  AppendObjectField[obj_, field_String, value_] :=
  SetObjectField[obj, field, Append[GetObjectField[obj, field], value]]];

DefFn[
  JoinObjectField[obj_, field_String, values_List] :=
  SetObjectField[obj, field, Join[GetObjectField[obj, field], values]]];

DefFn[
  ApplyToObjectField[obj_, field_String, f_] :=
  SetObjectField[obj, field, f[GetObjectField[obj, field]]]];
  

End[];

EndPackage[];
