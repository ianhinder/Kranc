
(* $Id$ *)

BeginPackage["sym`"]

{Name, Calculation, GroupCalculations, Groups, Initial, AfterEvolve,
 Arrangement, Directory, Description, ArrangementGroups,
 EvolutionSystems, TranslatorInCalculation, TranslatorOutCalculation,
 Evaluators, Setters, EvolvedGroups, PrimitiveGroups,
 SetTime, Initial, AfterEvolve,
 InitialAndAfterEvolve, SetterName, ThornList, NeededThorns}

EndPackage[];

BeginPackage["Arrangement`", {"TensorTools`", "MapLookup`", "KrancThorns`", "sym`" (* ,"CodeGen`" *) }];

CreateArrangement::usage = "";

Begin["`Private`"];

(* This probably wants to go in a general utilities package *)
ensureDirectory[name_] :=
  If[FileType[name] == None,
     CreateDirectory[name]];

createSetterThornAndThornList[arrangementSpec_, setterSpec_] :=
  {SetterName -> lookup[setterSpec, Name],
   ThornList -> 
     CreateSetterThorn[lookup[setterSpec, Calculation], 
                       lookup[arrangementSpec, Groups],
        DeBug -> True,
        RealBaseParameters -> lookupDefault[arrangementSpec, RealParameters, {}],
        IntBaseParameters -> lookupDefault[arrangementSpec, IntParameters, {}],
        SystemName -> lookup[arrangementSpec, Name],
        ThornName -> lookup[arrangementSpec, Name] <> lookup[setterSpec, Name],
        SystemDescription -> lookup[arrangementSpec, Description],

        SetTime -> Switch[lookup[setterSpec, SetTime],
                          Initial, "initial_only",
                          AfterEvolve, "poststep_only",
                          InitialAndAfterEvolve, "initial_and_poststep"]]};



(* evolutionSystem -> {Name -> "EvolveMain", Calculation -> calc,
                       PrimitiveGroups -> groups},

   group calculation -> {groupName, calculation}

   evaluator -> {Name -> "EvalConstraints", GroupCalculations -> group calcs,
                 Groups -> groupsToDefine}

   setter -> {Name -> "SetHarmonicLapse", Calculation -> calc, 
              Groups -> {list of group names being set}
              Initial -> True, AfterEvolve -> True}

   arrangement = {Name -> "classicADM", Directory -> ".", Description -> desc,
                  RealParameters -> {alpha, sigma},
                  IntParameters -> {m},
                  Groups -> groups (list of group structures),
                  EvolvedGroups -> groups (list of group names),
                  PrimitiveGroups -> groups (list of group names),
                  EvolutionSystems -> evolutionSystems,
                  TranslatorInCalculation -> calc,
                  TranslatorOutCalculation -> calc,
                  Evaluators -> evaluators,
                  Setters -> setters} *)

CreateArrangement[spec_] :=
  Module[{arrDir = lookup[spec, Directory], 
          translatorThornList, evolutionThornList,setterNamedThornList},

(* This is not the right place for this! *)

    SetEnhancedTimes[False];
    
    ensureDirectory[arrDir <> "/" <> lookup[spec, Name]];
    Put[spec, arrDir <> "/" <> lookup[spec, Name] <> "/spec.m"];

    (* Create the base thorn *)
    baseThornList = 
      CreateBaseThorn[
        lookup[spec, Groups],
        lookup[spec, EvolvedGroups],
        lookup[spec, PrimitiveGroups],

        DeBug -> True,
        RealBaseParameters -> lookupDefault[spec, RealParameters, {}],
        IntBaseParameters -> lookupDefault[spec, RealParameters, {}],
        SystemName -> lookup[spec, Name],
        ThornName -> lookup[spec, Name] <> "Base",
        SystemDescription -> lookup[spec, Description]];

    (* Create the translator thorn *)
    translatorThornList = {};

    If[mapContains[spec, TranslatorInCalculation] || mapContains[spec, TranslatorOutCalculation] ,

    translatorThornList = 
      CreateTranslatorThorn[lookup[spec, Groups],
        TranslatorInCalculation -> lookup[spec, TranslatorInCalculation],
        TranslatorOutCalculation -> lookup[spec, TranslatorOutCalculation],

        DeBug -> True,
        RealBaseParameters -> lookupDefault[spec, RealParameters, {}],
        IntBaseParameters -> lookupDefault[spec, IntParameters, {}],
        SystemName -> lookup[spec, Name],
        ThornName -> lookup[spec, Name] <> "Translator",
        SystemDescription -> lookup[spec, Description]]];


    (* Create the evolution thorns *)
    evolutionThornLists = 
      Map[CreateMoLThorn[
            lookup[#, Calculation], lookup[spec, Groups],
            PrimitiveGroups -> lookupDefault[#, PrimitiveGroups, {}],

            DeBug -> True,
            RealBaseParameters -> lookupDefault[spec, RealParameters, {}],
            IntBaseParameters -> lookupDefault[spec, IntParameters, {}],
            SystemName -> lookup[spec, Name],
            ThornName -> lookup[spec, Name] <> lookup[#, Name],
            SystemDescription -> lookup[spec, Description]] &,

          lookup[spec, EvolutionSystems]];

    (* Create the setter thorns *)
    setterNamedThornLists =
      Map[createSetterThornAndThornList[spec, #] &,
        lookup[spec, Setters]];


    (* Create the evaluator thorns *)
    evaluatorThornLists = Map[CreateEvaluatorThorn[
          lookup[#, GroupCalculations], 
          lookup[spec, Groups],

            DeBug -> True,
            RealBaseParameters -> lookupDefault[spec, RealParameters, {}],
            IntBaseParameters -> lookupDefault[spec, IntParameters, {}],
            SystemName -> lookup[spec, Name],
            ThornName -> lookup[spec, Name] <> lookup[#, Name],
            SystemDescription -> lookup[spec, Description]] &,

        lookupDefault[spec, Evaluators, {}]];

    neededThorns = Join[baseThornList, translatorThornList, 
                        Flatten[evolutionThornLists,1], 
                        Flatten[evaluatorThornLists,1]];

    Put[{NeededThorns -> neededThorns, Setters -> setterNamedThornLists},
        arrDir <> "/" <> lookup[spec, Name] <> "/thorns.m"];


];

End[];
EndPackage[];
