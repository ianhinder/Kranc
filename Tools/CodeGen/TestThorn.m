
(* $Id$ *)

$Path = Flatten[{"../../Tools/CodeGen", "../../Tools/MathematicaMisc", "../../Tools/External", ".", $Path}];


BeginPackage["TestThorn`", "Thorn`", "MapLookup`"]

{Bx, By, Bz, gInv11, gInv12, g11rhs, g11, K11}

Begin["Private`"];

$ContextPath = Join[{"sym`"}, $ContextPath];

(* Interface *)

group1 = 
  {Name -> "SHIFT", 
   VariableType -> "real", 
   Timelevels -> 3, 
   GridType -> "GF",
   Comment -> "Time derivatives of the shift", 
   Visibility -> "public",
   Variables -> {Bx, By, Bz}};


if = CreateInterface[
       "evolveGR", {admbase , admmacros , boundary, coordgauge, spacemask, 
                    staticconformal},
        {"one.h", "two.h"}, {group1}, Friends -> {"admcoupling"}];

(* Schedule *)

sch = 
  CreateSchedule[
    {{Group -> "admbase::metric", Timelevels -> 3}},
    {},
    {{Name -> "ADM_BSSN_CalcRHSfn", 
        SchedulePoint -> "in POSTINITIAL before ExternalLapse",
        Language -> "C", 
        SynchronizedGroups -> {"ADM_BSSN_gamma"},
        TriggerGroups -> {"trig"},
        StorageGroups -> {{Group -> "ADM_BSSN_gamma", Timelevels -> 3}},
        Conditional -> {Parameter -> "my_param", Value -> "something"},
        Comment -> "a function"}}];

(* Makefile *)

mf =
  CreateMakefile[{"one.c", "two.c", "three.c"}];

(* Parameter file *)

par = 
  CreateParam[
    {Implementations ->
       {{Name -> "grid",
         UsedParameters -> {{Name -> "domain", Type -> "KEYWORD"},
                            {Name -> "quadrant_direction", Type -> "KEYWORD"},
                            {Name -> "bitant_plane", Type -> "KEYWORD"}},
         ExtendedParameters -> {{Name -> "lapse_evolution_method", Type -> "KEYWORD",
                                 Default -> "", Description -> "Which lapse method to use",
                                 AllowedValues -> {{Value -> "geodesic", Description -> "Geodesic slicing"},
                                                   {Value -> "harmonic", Description -> "Harmonic slicing"},
                                                   {Value -> "1+log", Description -> "Generalized 1+log slicing"}}}}}},
     NewParameters -> {{Name -> "fisheye_scale", Type -> "REAL", Description -> "Asymptotic coordinate scale (fisheye factor)",
                        Default -> 1.0, Visibility -> "private",
                        AllowedValues -> {{Value -> 0.0, Description -> "Positive please"}}}}}];


(* Precompmacros source file *)

precomp =
  CreatePrecompMacros[{h11,h22,h33}];

(* MoL Source file *)

molreg = 
  CreateMoLRegistrationSource[
    {EvolvedGFs -> {h11, h12}, PrimitiveGFs -> {trK}, BaseImplementation -> "ADMBase", ThornName -> "MyMoL"}];

(* Startup file *)

startup = CreateStartupFile["MyTestThorn", "MyTestThorn: A test thorn for Kranc"];

(* Setter file *)

debug = False;

setter = CreateSetterSource[
 {{Name -> "MyMoL_ImportVariables",
   Calculation -> 
     {Shorthands -> {gInv11, gInv12},
      GridFunctions -> {g11rhs, g11, K11},
      Equations -> {{g11rhs -> Log[g11] + D11[K11] - D22[K22] + gInv11 + gInv12}, {}}}}}, debug];

srcs = {{Filename -> "MoLRegister.c", Contents -> molreg},
        {Filename -> "Setter.c", Contents -> setter},
        {Filename -> "precompMacros.h", Contents -> precomp},
        {Filename -> "Startup.c", Contents -> startup}};

th = {Name -> "MyTestThorn", Directory -> ".", Interface -> if,
      Schedule -> sch, Param -> par, Makefile -> mf,
      Sources -> srcs};

CreateThorn[th];

End[];
EndPackage[];
