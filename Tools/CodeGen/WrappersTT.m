
(*  Copyright 2004 Sascha Husa, Ian Hinder, Christiane Lechner

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
    along with Foobar; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

BeginPackage["WrappersTT`", {"TensorTools`", "Arrangement`", "MapLookup`" ,
                              "KrancThorns`"}];

$ContextPath = Join[{"sym`"}, $ContextPath];

CreateArrangementTT::usage = "";
CreateGroupFromTensor::usage = "";
CreateNamedGroupFromTensor::usage = "";
CreateGroupNameFromTensor::usage = "";

CreateMoLThornTT::usage = "";
CreateBaseThornTT::usage = "";
CreateSetterThornTT::usage = "";
CreateEvaluatorThornTT::usage = "";
CreateTranslatorThornTT::usage = "";

Begin["`Private`"];

CreateArrangementTT[spec_] :=
  CreateArrangement[mapValueMapMultiple[spec,
    {Groups -> (Map[makeGroupExplicit, #] &),
     EvolutionSystems -> (Map[makeEvolutionSystemExplicit, #] &),
     TranslatorInCalculation -> makeCalculationExplicit,
     TranslatorOutCalculation -> makeCalculationExplicit,
     TranslatorGroups -> (Map[makeGroupExplicit, #] &),
     Evaluators -> (Map[makeEvaluatorExplicit, #] &),
     Setters -> (Map[makeSetterExplicit, #] &)}]];

makeCalculationExplicit[calc_] :=
  mapValueMapMultiple[calc, 
    {Shorthands -> MakeExplicit, 
(*     CollectList -> makeExplicit, *)
     Equations -> (Map[MakeExplicit, #] &)}];

makeGroupExplicit[g_] :=
  {g[[1]], RemoveDuplicates[MakeExplicit[g[[2]]]]};

makeGroupExplicit[g_] :=
  {g[[1]], RemoveDuplicates[MakeExplicit[g[[2]]]]};

makeEvolutionSystemExplicit[spec_] :=
  mapValueMapMultiple[spec, 
    {Calculation -> makeCalculationExplicit,
     PrimitiveGroups -> (Map[CreateGroupNameFromTensor, #] &)}];

makeEvaluatorExplicit[spec_] :=
  mapValueMapMultiple[spec,
    {Groups -> (Map[makeGroupExplicit, #] &),
     GroupCalculations -> (Map[makeGroupCalculationExplicit, #] &)}];
     
makeGroupCalculationExplicit[gc_] :=
  {gc[[1]], makeCalculationExplicit[gc[[2]]]};

makeSetterExplicit[spec_] :=
  mapValueMapMultiple[spec, 
    {Calculation -> makeCalculationExplicit,
     Groups -> (Map[makeGroupExplicit, #] &)}];

CreateGroupFromTensor[T:Tensor[k_, is__]] :=
  {ToString[k], T};

CreateGroupNameFromTensor[T:Tensor[k_, is__]] :=
  ToString[k];

CreateGroupNameFromTensor[x_] :=
  ToString[x];

CreateNamedGroupFromTensor[name_, T:Tensor[k_, is__]] :=
  {name, T};

CreateGroupFromTensor[x_] :=
  {ToString[x], {x}};

CreateNamedGroupFromTensor[name_, x_] :=
    {name, {x}};

CheckEquationTensors[eq_] :=
  Module[{},
    CheckTensors[eq]];

CheckCalculationTensors[calc_] :=
  Module[{eqs},
    eqs = lookup[calc, Equations];
    Map[CheckEquationTensors, eqs]];



(* Wrappers for the KrancThorns functions *)

CreateMoLThornTT[calc_, groups_, optArgs___] :=
  Module[{calc2, groups2},
    CheckCalculationTensors[calc];
    calc2 = makeCalculationExplicit[calc];
    groups2 = Map[makeGroupExplicit, groups];
    CreateMoLThorn[calc2, groups2, optArgs]];

CreateBaseThornTT[groups_, evolvedGroupNames_, primitiveGroupNames_, optArgs___] :=
  Module[{groups2},
    groups2 = Map[makeGroupExplicit, groups];
    CreateBaseThorn[groups2, evolvedGroupNames, primitiveGroupNames, optArgs]];

CreateSetterThornTT[calc_, groups_, optArgs___] :=
  Module[{calc2, groups2},
    CheckCalculationTensors[calc];
    calc2 = makeCalculationExplicit[calc];
    groups2 = Map[makeGroupExplicit, groups];
    CreateSetterThorn[calc2, groups2, optArgs]];

CreateEvaluatorThornTT[unqualifiedEvaluationDefinitions_, unqualifiedGroups_, optArgs___] :=
  Module[{newDefs, groups2},
    Map[CheckCalculationTensors[Last[#]] &, unqualifiedEvaluationDefinitions];
    groups2 = Map[makeGroupExplicit, unqualifiedGroups];
    newDefs = Map[makeGroupCalculationExplicit, unqualifiedEvaluationDefinitions];
    CreateEvaluatorThorn[newDefs, groups2, optArgs]];

(* Obsolete; no tensor checking *)
CreateTranslatorThornTT[groups_, optArgs___] :=
  Module[{optArgs2, groups2},
    groups2 = Map[makeGroupExplicit, groups];
    optArgs2 = mapValueMapMultiple[{optArgs}, 
      {TranslatorInCalculation -> makeCalculationExplicit, 
       TranslatorOutCalculation -> makeCalculationExplicit}];
    CreateTranslatorThorn[groups2, Apply[Sequence,optArgs2]]];

End[];

EndPackage[];
