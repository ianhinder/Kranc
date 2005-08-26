
BeginPackage["MapLookup`", {"Errors`"}];

lookup::usage = "";
mapContains::usage = "";
mapReplace::usage = "";
mapValueMap::usage = "";
lookupDefault::usage = "";
mapValueMapMultiple::usage = "";
mapAdd::usage = "";
mapEnsureKey::usage = "";
mapQuery::usage = "";

Begin["`Private`"];

(* -------------------------------------------------------------------------- 
   Map lookup
   -------------------------------------------------------------------------- *)

lookup[map_, key_] :=
  Module[{values},
    If[! ListQ[map],
      ThrowError["lookup failure: map", map, " is not a list"]];

    values = Select[map, First[#] === key &];
    If[values == {},
       ThrowError["lookup failure: key " <> ToString[key] <> " not found in map " <> ToString[map]]];
    If[Length[values] > 1,
       ThrowError["lookup failure: key ", key, " found multiple times in map", map]];

    First[values][[2]]];

mapContains[map_, key_] :=
  Length[Select[map, First[#] === key &]] >= 1;

lookupDefault[map_, key_, default_] :=
  If[mapContains[map, key],
    lookup[map, key],
    default];

mapReplace[map_, key_, value_] :=
  Map[If[First[#] === key, key -> value, #] &, map];

mapAdd[map_, key_, value_] :=
  Join[map, {key -> value}];

mapEnsureKey[map_, key_, defaultValue_] :=
  If[mapContains[map, key],
     map,
     mapAdd[map, key, defaultValue]];

mapValueMap[map_, key_, f_] :=
  If[mapContains[map, key],
     mapReplace[map, key, f[lookup[map,key]]],
     map];

(* defs are rules of the form key -> function.  m is a map.  The result is
   a version of m which the functions applied to the corresponding keys
   at the top level only. This function needs to be renamed! *)
mapValueMapMultiple[m_, defs_] :=
  Fold[mapValueMap[#1, #2[[1]], #2[[2]]] &,
       m, defs];

mapQuery[ms_, key_, value_] :=
  Select[Select[ms, mapContains[#, key] &], lookup[#, key] === value &];

End[];


EndPackage[];

