
(* $Id$ *)


(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
BeginPackage["sym`"];
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

{Type, Resolution, StencilWidth, ActiveThorns, GaugeThorns,
 GaugeParameterSettings, IOSpec, outDir, outFormat,
 ioEvery, ioInfoGFs, io0dGFs, io1dGFs, io2dGFs,
 out1D nx, ny, nz, initialTime, itLast, Name, Directory}

{hamGF, lapseGF, trKGF};

{Amplitude, NoiseAmp, NoiseGroups};

{IDSpec,  IDSettings};
{EvolutionSpec,  EvolutionSettings};
{EvaluationSpec, EvaluationSettings};

{NumDifferentiations};

EndPackage[];


(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
BeginPackage["MexicoTests`", {"sym`", "MapLookup`", "KrancThorns`", "Helpers`"}];
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

MexicoTests = {"robust", "robust2D",
               "GaugeWave", "GaugeWave2D", 
               "LinearWave", "LinearWave2D", 
               "PolarizedGowdyExpansion", "PolarizedGowdyCollapse"};


ExactTypes = {"GaugeWave", "GaugeWave2D", 
              "LinearWave", "LinearWave2D", 
              "PolarizedGowdyExpansion", "PolarizedGowdyCollapse"};


xbaseRes["robust"] := 50;
ybaseRes["robust"] := 3;
zbaseRes["robust"] := 3;

xbaseRes["robust2D"] := 50;
ybaseRes["robust2D"] := 50;
zbaseRes["robust2D"] := 3;

xbaseRes["GaugeWave"] := 50;
ybaseRes["GaugeWave"] := 1;
zbaseRes["GaugeWave"] := 1;

xbaseRes["GaugeWave2D"] := 50;
ybaseRes["GaugeWave2D"] := 50;
zbaseRes["GaugeWave2D"] := 1;


xbaseRes["LinearWave"] := 50 ;
ybaseRes["LinearWave"] := 1;
zbaseRes["LinearWave"] := 1;

xbaseRes["LinearWave2D"] := 50 ;
ybaseRes["LinearWave2D"] := 50;
zbaseRes["LinearWave2D"] := 1;


xbaseRes["PolarizedGowdyExpansion"] := 1;
ybaseRes["PolarizedGowdyExpansion"] := 1;
zbaseRes["PolarizedGowdyExpansion"] := 50;


xbaseRes["PolarizedGowdyCollapse"] := xbaseRes["PolarizedGowdyExpansion"];
ybaseRes["PolarizedGowdyCollapse"] := ybaseRes["PolarizedGowdyExpansion"];
zbaseRes["PolarizedGowdyCollapse"] := zbaseRes["PolarizedGowdyExpansion"];


xbaseRes[type_] := 50;
ybaseRes[type_] := 50;
zbaseRes[type_] := 50;


dtFactor["robust"]   := 0.1;
dtFactor["robust2D"] := 0.1;
dtFactor["PolarizedGowdyCollapse"] := -0.25;
dtFactor[type_]    := 0.25;


XTimes["robust"] := 1000;
XTimes["robust2D"] := 1000;
XTimes[type_]    := 1000;


InitialTime["PolarizedGowdyCollapse"]  = 9.8753205829098;
InitialTime["PolarizedGowdyExpansion"] = 1.0;
InitialTime[type_]                     = 0.0;


ExactEntry["LinearWave", amp_] :=

{
 "ActiveThorns = \"MexicoLinearWave\"\n",
 "MexicoLinearWave::wave_A                = " <> ToString@CForm@amp,
 "# MexicoLinearWave::wave_d                = <currently fixed to default>"
}

ExactEntry["LinearWave2D", amp_] :=

{
 "ActiveThorns = \"MexicoLinearWave\"\n",
 "MexicoLinearWave::wave_A                = " <> ToString@CForm@amp,
 "# MexicoLinearWave::wave_d                = <currently fixed to default>"
}


ExactEntry["PolarizedGowdyCollapse", amp_] := 

{"Exact::exact_model                = \"Gowdy-wave\"",
 "Exact::Gowdy_wave__amplitude      = 1.0"
}

ExactEntry["PolarizedGowdyExpansion", amp_] := 
ExactEntry["PolarizedGowdyCollapse",  amp]


ExactEntry["GaugeWave", amp_] :=
{
"Exact::exact_model                 = \"Minkowski/gauge wave\"",
"",
"Exact::Minkowski_gauge_wave__what_fn      = \"sin\"",
"Exact::Minkowski_gauge_wave__amplitude    = " <> ToString@amp,
"Exact::Minkowski_gauge_wave__diagonal     = \"no\"",
"Exact::Minkowski_gauge_wave__omega        = 1.0",
"Exact::Minkowski_gauge_wave__phase        = 0.0",
"",
"##################################################################",
"# GW_del = (global_x - 2*ghosts)*dxyz to get periodic boundaries",
"# and .../sqrt(2) for diagonal = \"yes\"",
"##################################################################",
"",
"Exact::Minkowski_gauge_wave__lambda                     = 1.0",
""
}


ExactEntry["GaugeWave2D", amp_] :=
{
"Exact::exact_model                 = \"Minkowski/gauge wave\"",
"",
"Exact::Minkowski_gauge_wave__what_fn      = \"sin\"",
"Exact::Minkowski_gauge_wave__amplitude    = " <> ToString@amp,
"Exact::Minkowski_gauge_wave__diagonal     = \"yes\"",
"Exact::Minkowski_gauge_wave__omega        = 1.0",
"Exact::Minkowski_gauge_wave__phase        = 0.0",
"",
"##################################################################",
"# GW_del = (global_x - 2*ghosts)*dxyz to get periodic boundaries",
"# and .../sqrt(2) for diagonal = \"yes\"",
"##################################################################",
"",
"Exact::Minkowski_gauge_wave__lambda                     = 1.0",
""
}


EndPackage[];




(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)
BeginPackage["Parfiles`", {"CodeGen`", "sym`", "MapLookup`",
                           "KrancThorns`", "MexicoTests`"}];
(* XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *)

CreateParfile::usage       = "create a `generic` Cactus par file";

CreateRobustTest::usage    = "create a Cactus par file for the robust stability test";
CreateRobust2DTest::usage    = "create a Cactus par file for the robust 2D stability test";
CreatePolarizedGowdyTest::usage = "create a Cactus par file for the Gowdy test";
CreateGaugeWaveTest::usage = "create a Cactus par file for the GaugeWave test";
CreateLinearWaveTest::usage= "create a Cactus par file for the LinearWave test";


Begin["`Private`"];

(* -------------------------------------------------------------------------- 
   General Utility Functions
   -------------------------------------------------------------------------- *)

inner[list_] := Table[{i}, {i, 2, Length@list}];

spacePad[stringList_] := StringJoin[ Insert[stringList, " ", inner@stringList]  ];

date[] := ToString[Date[][[3]]] <> "/" <>
          ToString[Date[][[2]]] <> "/" <>
          ToString[Date[][[1]]];

user[] := ToString[<< "!whoami"];

whoWhen[] := Module[{},

{"# file created by user " <> user[] <> ", " <> date[] <> "\n"  <>
 "# using Parfiles.m written by Sascha Husa"            <> "\n"  <>
 "# $Id" <> "$"                                         <> "\n\n"}

];

(* -------------------------------------------------------------------------- 
   Parameter Block specific functions
   -------------------------------------------------------------------------- *)

initialDataBlock[idSpec_] :=

Module[{},

{
"# initial data thorns\n",

"ActiveThorns = \"" <> spacePad[lookup[idSpec, ActiveThorns]] <> "\"\n",

lookup[idSpec, IDSettings],
"\n"
}
] 


evolutionBlock[evolutionSpec_] :=

Module[{},

{
"# base and evolution thorns\n",

"ActiveThorns = \"" <> spacePad[lookup[evolutionSpec, ActiveThorns]] <> "\"\n",
lookup[evolutionSpec, EvolutionSettings],
"\n"
}
] 



evaluationBlock[evaluationSpec_] :=

Module[{},

{
"# evaluation thorns\n",

"ActiveThorns = \"" <> spacePad[lookup[evaluationSpec, ActiveThorns]] <> "\"\n",

lookup[evaluationSpec, EvaluationSettings],
"\n"
}
] 

(* gridSpec = {nx -> 50, ny -> 7, nz -> 7, dxzy -> 0.1, 
   initialTime -> 0, itLast -> 1000, dtfac -> 0.25, ghostSize -> 2} *)

periodicGridBlock[gridSpec_] :=

Module[{ActiveThorns},

stringValue[key_] := ToString@lookup[gridSpec, key];
value[key_]       := lookup[gridSpec, key];

ActiveThorns = "SymBase Boundary CoordBase CartGrid3D Time PUGH PUGHReduce PUGHSlab NaNChecker";

{
"# Grid\n",
"ActiveThorns = \"" <> ActiveThorns <> "\"\n",

"grid::domain               = \"full\"",
"grid::type                 = \"byspacing\"\n",

"driver::global_nx          = " <> ToString[value@nx + 2 value@ghostSize],
"driver::global_ny          = " <> ToString[value@ny + 2 value@ghostSize],
"driver::global_nz          = " <> ToString[value@nz + 2 value@ghostSize],
"",

"grid::dxyz                 = " <> stringValue@dxyz,
"",

"cactus::cctk_initial_time  = " <> stringValue@initialTime,
"cactus::cctk_itlast        = " <> stringValue@itLast,
"",

"time::dtfac                = " <> stringValue@dtfac,
"",

"pugh::ghost_size           = " <> stringValue@ghostSize,
"",

"# Boundary",
"pugh::periodic             = \"yes\"",
""
} 
] 

noiseBlock[amp_, groups_] :=

Module[{},

Print["Adding noise to Cactus groups: ", spacePad@groups];

If[amp > 0,
{
"# add noise to initial data\n",

"ActiveThorns = \"noise\"\n",

"noise::apply_id_noise = \"true\"", 
"noise::id_vars   = \"" <> spacePad[groups] <> "\"",
"noise::amplitude = "   <> ToString@CForm@amp,
"\n"
},
{""}]
] 


ADMcouplingGridBlock[] :=

Module[{},

{
"# Coupling to ADMBase\n",
"ActiveThorns         = \"SpaceMask ADMBase ADMCoupling\"",
"admbase::metric_type = \"physical\"",
"\n"
} 
] 


numericalGridBlock[sw_] :=

Module[{ActiveThorns},

stringValue[key_] := ToString@lookup[gridSpec, key];
value[key_]       := lookup[gridSpec, key];

ActiveThorns = "MoL GenericFD";

{
"# numerics\n",
"ActiveThorns = \"" <> ActiveThorns <> "\"\n",

"GenericFD::stencil_width = " <> ToString@sw <> "\n",

"methodoflines::ode_method    = \"icn\"",
"# methodoflines::mol_intermediate_steps = 3",
"\n",

""} 
] 


exactGridBlock[Type_, amp_] :=

Module[{},

If[MemberQ[ExactTypes, Type],
Print["Exact type:", Type];
{
"# data from exact solution \n",
"ActiveThorns = \"exact coordgauge staticconformal\"\n",

"admbase::initial_data             = \"exact\"",
"admbase::initial_lapse            = \"exact\"",

ExactEntry[Type, amp]
},
Print["Not an exact type:", Type];
{"\n"}]
] 


gaugeGridBlock[gaugeSpec_] :=

Module[{},

{
"# Gauges\n",
"ActiveThorns = \"" <> spacePad[lookup[gaugeSpec, ActiveThorns]] <> "\"",

lookup[gaugeSpec, GaugeParameterSettings],
"\n"
} 
] 


ioGridBlock[ioSpec_] :=

Module[{},

{
"# Data Output\n",
"ActiveThorns = \"IOUtil IOBasic IOASCII\"\n",

"IO::out_dir = \"" <> ToString@lookup[ioSpec,outDir] <> "\"",
"",

"IOBasic::outScalar_reductions = \"minimum maximum norm1 norm2 norm_inf\"",

"IOBasic::outInfo_every   = "   <> ToString[lookup[ioSpec, ioEvery][[1]]],
"IOBasic::outInfo_vars    = \"" <> spacePad[lookup[ioSpec, ioInfoGFs]] <> "\"",
"",

"IOBasic::outScalar_every = "   <> ToString[lookup[ioSpec, ioEvery][[2]]],
"IOBasic::outScalar_vars  = \"" <> spacePad[lookup[ioSpec, io0dGFs]] <> "\"",
"",

"IOASCII::out1D_every     = "   <> ToString[lookup[ioSpec, ioEvery][[3]]],

"IOASCII::out1D_x         = "   <> lookup[ioSpec, out1D][[1]],
"IOASCII::out1D_y         = "   <> lookup[ioSpec, out1D][[2]],
"IOASCII::out1D_z         = "   <> lookup[ioSpec, out1D][[3]],

"IOASCII::out1D_vars      = \"" <> spacePad[lookup[ioSpec, io1dGFs]] <> "\"",
"",

"IOASCII::out_format      = " <> lookup[ioSpec, outFormat],
"",
"IOBasic::out_format      = " <> lookup[ioSpec, outFormat],
""
}
]


performanceBlock[] :=

Module[{},

{
"# Performance\n",

"cactus::cctk_timer_output          = \"full\"",
"pugh::timer_output                 = \"yes\"",
"io::print_timing_info              = \"yes\"",

"\n"
}
]
 

(* -------------------------------------------------------------------------- 
   Default options
   -------------------------------------------------------------------------- *)



Options[CreateParfile] = {Name                    -> "MyTest",
                          Directory               -> "par",
                          Type                    -> "robust",
                          Amplitude               -> 0.0,
                          NoiseAmp                -> 0.0,
                          NoiseGroups             -> {},
                          Resolution              -> 1, 
                          StencilWidth            -> 1, 
                          NumDifferentiations    -> 2,
                          GaugeThorns             -> {},
                          GaugeParameterSettings  -> {},
                          IDSpec                  -> 
                                 {ActiveThorns -> {},
                                  IDSettings   -> {"# no parameters"}},
                          EvolutionSpec           -> 
                                 {ActiveThorns      -> {},
                                  EvolutionSettings -> {"# no parameters"}},
                          EvaluationSpec          -> 
                                 {ActiveThorns       -> {},
                                  EvaluationSettings -> {"# no parameters"}},
                          IOSpec    -> {outDir -> "TEST", outFormat -> ".16e",
                          ioEvery   -> {1, 1, 10, 100}, 
                          ioInfoGFs -> {""} , io0dGFs -> {""}, io1dGFs -> {""}, 
                          io2dGFs   -> {""},
                          out1D     -> {"yes", "no", "no"} }
};


Options[CreateRobustTest] = {Name                   -> "MyFormulation",
                             Directory              -> "par",
                             NoiseAmp               -> 1.0 * 10^(-10),
                             NoiseGroups            -> {},
                             Resolution             -> 1,
                             GaugeThorns            -> {},
                             GaugeParameterSettings -> {},
                             IDSpec                 -> {ActiveThorns -> {},
                                                        IDSettings   -> {}},
                             EvolutionSpec          -> {ActiveThorns -> {},
                                                        EvolutionSettings -> {}},
                             EvaluationSpec         -> {ActiveThorns -> {},
                                                        EvaluationSettings -> {}},
                             StencilWidth           -> 1,
                             NumDifferentiations    -> 2,
                             hamGF                  -> "ADMBase::ham",
                             lapseGF                -> "ADMBase::alp",
                             ioInfoGFs              -> {}, 
                             io0dGFs                -> {}, 
                             io1dGFs                -> {}, 
                             io2dGFs                -> {}
};

Options[CreateRobust2DTest] = {Name                   -> "MyFormulation",
                             Directory              -> "par",
                             NoiseAmp               -> 1.0 * 10^(-10),
                             NoiseGroups            -> {},
                             Resolution             -> 1,
                             GaugeThorns            -> {},
                             GaugeParameterSettings -> {},
                             IDSpec                 -> {ActiveThorns -> {},
                                                        IDSettings   -> {}},
                             EvolutionSpec          -> {ActiveThorns -> {},
                                                        EvolutionSettings -> {}},
                             EvaluationSpec         -> {ActiveThorns -> {},
                                                        EvaluationSettings -> {}},
                             StencilWidth           -> 1,
                             NumDifferentiations    -> 2,
                             hamGF                  -> "ADMBase::ham",
                             lapseGF                -> "ADMBase::alp",
                             ioInfoGFs              -> {}, 
                             io0dGFs                -> {}, 
                             io1dGFs                -> {}, 
                             io2dGFs                -> {}
};


Options[CreateGaugeWaveTest] = {Name                -> "MyFormulation",
                             Directory              -> "par",
                             Type                   -> "GaugeWave",
                             Amplitude              -> 0.01,
                             NoiseAmp               -> 0.0 * 10^(-10),
                             NoiseGroups            -> {},
                             Resolution             -> 1,
                             GaugeThorns            -> {},
                             GaugeParameterSettings -> {},
                             IDSpec                 -> {ActiveThorns -> {},
                                                        IDSettings   -> {}},
                             EvolutionSpec          -> {ActiveThorns -> {},
                                                        EvolutionSettings -> {}},
                             EvaluationSpec         -> {ActiveThorns -> {},
                                                        EvaluationSettings -> {}},
                             StencilWidth           -> 1,
                             NumDifferentiations    -> 2,
                             hamGF                  -> "ADMBase::ham",
                             lapseGF                -> "ADMBase::alp",
                             trKGF                  -> "ADMBase::trK",
                             ioInfoGFs              -> {}, 
                             io0dGFs                -> {}, 
                             io1dGFs                -> {}, 
                             io2dGFs                -> {}
};


Options[CreateLinearWaveTest] = {Name                -> "MyFormulation",
                             Directory              -> "par",
                             Type                   -> "LinearWave",
                             Amplitude              -> 10^(-8),
                             NoiseAmp               -> 0.0,
                             NoiseGroups            -> {},
                             Resolution             -> 1,
                             GaugeThorns            -> {},
                             GaugeParameterSettings -> {},
                             IDSpec                 -> {ActiveThorns -> {},
                                                        IDSettings   -> {}},
                             EvolutionSpec          -> {ActiveThorns -> {},
                                                        EvolutionSettings -> {}},
                             EvaluationSpec         -> {ActiveThorns -> {},
                                                        EvaluationSettings -> {}},
                             StencilWidth           -> 1,
                             NumDifferentiations    -> 2,
                             hamGF                  -> "ADMBase::ham",
                             lapseGF                -> "ADMBase::alp",
                             trKGF                  -> "ADMBase::trK",
                             ioInfoGFs              -> {},
                             io0dGFs                -> {},
                             io1dGFs                -> {},
                             io2dGFs                -> {}
};


Options[CreatePolarizedGowdyTest] = {Name           -> "MyFormulation",
                             Directory               -> "par",
                             Type                   -> "PolarizedGowdyCollapse",
                             NoiseAmp               -> 0.0 * 10^(-10),
                             NoiseGroups            -> {},
                             Resolution             -> 1,
                             GaugeThorns            -> {},
                             GaugeParameterSettings -> {},
                             IDSpec                 -> {ActiveThorns -> {},
                                                        IDSettings   -> {}},
                             EvolutionSpec          -> {ActiveThorns -> {},
                                                        EvolutionSettings -> {}},
                             EvaluationSpec         -> {ActiveThorns -> {},
                                                        EvaluationSettings -> {}},
                             StencilWidth           -> 1,
                             NumDifferentiations    -> 2,
                             hamGF                  -> "ADMBase::ham",
                             lapseGF                -> "ADMBase::alp",
                             trKGF                  -> "ADMBase::trK",
                             ioInfoGFs              -> {}, 
                             io0dGFs                -> {}, 
                             io1dGFs                -> {}, 
                             io2dGFs                -> {}
};

(* -------------------------------------------------------------------------- 
   generic parameter file generator
   -------------------------------------------------------------------------- *)



CreateParfile[optArgs___]:=

Module[{opts, $Type, $Resolution, $StencilWidth, $GaugeThorns, 
        $GaugeParameterSettings, $amp,
        $nx, $ny, $nz, $dxyz, $ghostSize, DomainSize, par, $name,
        ioSpec, idSpec, evolutionSpec, evaluationSpec, paramdir},

opts = GetOptions[CreateParfile, {optArgs}];


$Type                   = lookup[opts, Type];
$amp                    = lookup[opts, Amplitude];
$NoiseAmp               = lookup[opts, NoiseAmp];
$NoiseGroups            = Map[ToString, Flatten@lookup[opts, NoiseGroups]];
$Resolution             = lookup[opts, Resolution];
$StencilWidth           = lookup[opts, StencilWidth];
$GaugeThorns            = lookup[opts, GaugeThorns];
$GaugeParameterSettings = lookup[opts, GaugeParameterSettings];
ioSpec                  = lookup[opts, IOSpec];
idSpec                  = lookup[opts, IDSpec];
evolutionSpec           = lookup[opts, EvolutionSpec];
evaluationSpec          = lookup[opts, EvaluationSpec];
$name                   = lookup[opts, Name] <> ".par";
paramdir                = lookup[opts, Directory];

EnsureDirectory[paramdir];


DomainSize = 1;


(*$ghostSize = lookup[opts, NumDifferentiations] * $StencilWidth;*)
(* as loops are set up properly now*)
$ghostSize = $StencilWidth;

$nx = xbaseRes[$Type] * (2 ^ $Resolution[[1]]);
$ny = ybaseRes[$Type] * (2 ^ $Resolution[[2]]);
$nz = zbaseRes[$Type] * (2 ^ $Resolution[[3]]);

$dxyz = N[DomainSize / Max[$nx, $ny, $nz], 16];

gridSpec = {nx          ->  $nx,
            ny          ->  $ny,
            nz          ->  $nz,
            dxyz        ->  $dxyz,
            dtfac       ->  dtFactor[$Type],
            initialTime ->  InitialTime[$Type], 
            itLast      ->  Abs@IntegerPart[
                              (DomainSize/$dxyz) * (XTimes[$Type]/dtFactor[$Type])], 
            ghostSize   ->  $ghostSize};

gaugeSpec = {ActiveThorns           -> $GaugeThorns, 
             GaugeParameterSettings -> $GaugeParameterSettings};

par = Flatten@{whoWhen[],
       periodicGridBlock[gridSpec],
       noiseBlock[$NoiseAmp, $NoiseGroups],
       ADMcouplingGridBlock[],
       numericalGridBlock[$StencilWidth],
       exactGridBlock[$Type, $amp],
       initialDataBlock[idSpec],
       evolutionBlock[evolutionSpec],
       evaluationBlock[evaluationSpec],
       gaugeGridBlock[gaugeSpec],
       ioGridBlock[ioSpec],
       performanceBlock[]
};

$name = paramdir <> "/"  <> $name;
Print["Writing file ", $name];
GenerateFile[$name, NewlineSeparated@par];

par
]



(* -------------------------------------------------------------------------- 
   Mexico Test generators
   -------------------------------------------------------------------------- *)



CreateRobustTest[optArgs___]:= Module[
{opts, par, noiseBasic, crossingSteps, name, hamgf, lapsegf},

opts = GetOptions[CreateRobustTest, {optArgs}];


Print["Create robust stability test parameter file"];

noiseBasic = 10^(-10);

crossingSteps = 
 Abs@IntegerPart[(xbaseRes["robust"]*(2 ^ lookup[opts, Resolution]))*(1./dtFactor["robust"])];

Print["time steps for one crossing time: ", crossingSteps];

Print["Creating parameter file for robust stability test."];

name = lookup[opts, Name] <> "Robust"  <> "_rho" <>ToString[2^lookup[opts, Resolution]];



hamgf   = ToString@lookup[opts, hamGF]; 
lapsegf = ToString@lookup[opts, lapseGF]; 

CreateParfile[Name                   -> name,
              Type                   -> "robust",
              Directory              -> lookup[opts, Directory],
              NoiseAmp               -> noiseBasic /  (2 ^ lookup[opts, Resolution])^2,
              NoiseGroups            -> lookup[opts, NoiseGroups],
              Resolution             -> {lookup[opts, Resolution],0,0}, 
              StencilWidth           -> lookup[opts, StencilWidth],
              NumDifferentiations    -> lookup[opts, NumDifferentiations],
              GaugeThorns            -> lookup[opts, GaugeThorns],
              GaugeParameterSettings -> lookup[opts, GaugeParameterSettings],
              IDSpec                 -> {ActiveThorns -> {}, IDSettings
                   -> {"admbase::initial_data = \"Cartesian Minkowski\""}},
              EvolutionSpec          -> lookup[opts, EvolutionSpec],
              EvaluationSpec         -> lookup[opts, EvaluationSpec],
              IOSpec -> {outDir -> name,
              outFormat -> ".16e",
              ioEvery -> {100, 
                crossingSteps, crossingSteps * 10, -1*(2 ^ lookup[opts, Resolution])},
              ioInfoGFs -> Union@Flatten@{hamgf, lapsegf, lookup[opts, ioInfoGFs]}, 
              io0dGFs   -> Union@Flatten@{hamgf, lapsegf, lookup[opts, NoiseGroups],
                                                         lookup[opts, io0dGFs]}, 
              io1dGFs   -> Union@Flatten@{hamgf, lapsegf, lookup[opts, io1dGFs]}, 
              io2dGFs   -> Union@Flatten@{lookup[opts, io2dGFs]},

              out1D -> {"yes", "no", "no"} }];
]

CreateRobust2DTest[optArgs___]:= Module[
{opts, par, noiseBasic, crossingSteps, name, hamgf, lapsegf},

opts = GetOptions[CreateRobust2DTest, {optArgs}];


Print["Create robust 2D stability test parameter file"];

noiseBasic = 10^(-10);

crossingSteps = 
 Abs@IntegerPart[(xbaseRes["robust2D"]*(2 ^ lookup[opts, Resolution]))*(1./dtFactor["robust2D"])];

Print["time steps for one crossing time: ", crossingSteps];

Print["Creating parameter file for robust 2D stability test."];

name = lookup[opts, Name] <> "Robust2D"  <> "_rho" <>ToString[2^lookup[opts, Resolution]];



hamgf   = ToString@lookup[opts, hamGF]; 
lapsegf = ToString@lookup[opts, lapseGF]; 

CreateParfile[Name                   -> name,
              Type                   -> "robust2D",
              Directory              -> lookup[opts, Directory],
              NoiseAmp               -> noiseBasic /  (2 ^ lookup[opts, Resolution])^2,
              NoiseGroups            -> lookup[opts, NoiseGroups],
              Resolution             -> {lookup[opts, Resolution],lookup[opts, Resolution],0}, 
              StencilWidth           -> lookup[opts, StencilWidth],
              NumDifferentiations    -> lookup[opts, NumDifferentiations],
              GaugeThorns            -> lookup[opts, GaugeThorns],
              GaugeParameterSettings -> lookup[opts, GaugeParameterSettings],
              IDSpec                 -> {ActiveThorns -> {}, IDSettings
                   -> {"admbase::initial_data = \"Cartesian Minkowski\""}},
              EvolutionSpec          -> lookup[opts, EvolutionSpec],
              EvaluationSpec         -> lookup[opts, EvaluationSpec],
              IOSpec -> {outDir -> name,
              outFormat -> ".16e",
              ioEvery -> {100, 
                crossingSteps, crossingSteps * 10, -1*(2 ^ lookup[opts, Resolution])},
              ioInfoGFs -> Union@Flatten@{hamgf, lapsegf, lookup[opts, ioInfoGFs]}, 
              io0dGFs   -> Union@Flatten@{hamgf, lapsegf, lookup[opts, NoiseGroups],
                                                         lookup[opts, io0dGFs]}, 
              io1dGFs   -> Union@Flatten@{hamgf, lapsegf, lookup[opts, io1dGFs]}, 
              io2dGFs   -> Union@Flatten@{lookup[opts, io2dGFs]},

              out1D -> {"yes", "no", "no"} }];
]



CreateGaugeWaveTest[optArgs___]:= Module[
{opts, par, noiseBasic, crossingSteps, name, hamgf, lapsegf, trKgf},

opts = GetOptions[CreateGaugeWaveTest, {optArgs}];


If[MemberQ[{"GaugeWave", "GaugeWave2D"}, lookup[opts, Type]],

Print["Creating parameter file for " <>  lookup[opts, Type] <> " test."],
Throw["invalid type"]
];

noiseBasic = 0.0 * 10^(-10);

crossingSteps = 
 Abs@IntegerPart[(xbaseRes[lookup[opts, Type]]*(2 ^ lookup[opts, Resolution]))
            * (1./dtFactor[lookup[opts, Type]])];

Print["time steps for one crossing time: ", crossingSteps];

name = lookup[opts, Name] <> lookup[opts, Type] <> "_rho" <>ToString[2^lookup[opts, Resolution]];


hamgf   = ToString@lookup[opts, hamGF]; 
lapsegf = ToString@lookup[opts, lapseGF]; 
trKgf   = ToString@lookup[opts, trKGF]; 

CreateParfile[Name                   -> name,
              Directory              -> lookup[opts, Directory],
              Type                   -> lookup[opts, Type],
              Amplitude              -> lookup[opts, Amplitude],
              NoiseAmp               -> noiseBasic /  (2 ^ lookup[opts, Resolution])^2,
              NoiseGroups            -> lookup[opts, NoiseGroups],
              Resolution             -> {lookup[opts, Resolution],If[StringMatchQ["GaugeWave2D",lookup[opts, Type]],lookup[opts, Resolution],0],0}, 
              StencilWidth           -> lookup[opts, StencilWidth],
              NumDifferentiations    -> lookup[opts, NumDifferentiations],
              GaugeThorns            -> lookup[opts, GaugeThorns],
              GaugeParameterSettings -> lookup[opts, GaugeParameterSettings],
              IDSpec                 -> {ActiveThorns -> {}, IDSettings -> {}},
              EvolutionSpec          -> lookup[opts, EvolutionSpec],
              EvaluationSpec         -> lookup[opts, EvaluationSpec],
              IOSpec -> {outDir -> name,
              outFormat -> ".16e",
              ioEvery -> {100, 
                crossingSteps, crossingSteps * 10, -1* (2 ^ lookup[opts, Resolution])},
              ioInfoGFs -> Union@Flatten@{hamgf, lapsegf, trKgf, 
                                          lookup[opts, ioInfoGFs]}, 
              io0dGFs   -> Union@Flatten@{hamgf, lapsegf, trKgf,
                                          lookup[opts, NoiseGroups],
                                          lookup[opts, io0dGFs]}, 
              io1dGFs   -> Union@Flatten@{hamgf, lapsegf, trKgf, 
                                          lookup[opts, io1dGFs]}, 
              io2dGFs   -> Union@Flatten@{lookup[opts, io2dGFs]},

              out1D -> {"yes", "no", "no"} }];
]



CreateLinearWaveTest[optArgs___]:= Module[
{opts, par, noiseBasic, crossingSteps, name, hamgf, lapsegf, trKgf},

opts = GetOptions[CreateLinearWaveTest, {optArgs}];


If[MemberQ[{"LinearWave", "LinearWave2D"}, lookup[opts, Type]],

Print["Creating parameter file for " <>  lookup[opts, Type] <> " test."],
Throw["invalid type"]
];

noiseBasic = 0.0;

crossingSteps = 
 Abs@IntegerPart[(xbaseRes[lookup[opts, Type]]*(2 ^ lookup[opts, Resolution]))
            * (1./dtFactor[lookup[opts, Type]])];

Print["time steps for one crossing time: ", crossingSteps];

name = lookup[opts, Name] <> lookup[opts, Type] <> "_rho" <>ToString[2^lookup[opts, Resolution]];


hamgf   = ToString@lookup[opts, hamGF]; 
lapsegf = ToString@lookup[opts, lapseGF]; 
trKgf   = ToString@lookup[opts, trKGF]; 

CreateParfile[Name                   -> name,
              Directory              -> lookup[opts, Directory],
              Type                   -> lookup[opts, Type],
              Amplitude              -> lookup[opts, Amplitude],
              NoiseAmp               -> noiseBasic /  (2 ^ lookup[opts, Resolution])^2,
              NoiseGroups            -> lookup[opts, NoiseGroups],
              Resolution             -> {lookup[opts, Resolution],If[StringMatchQ["LinearWave2D",lookup[opts, Type]],lookup[opts, Resolution],0],0}, 
              StencilWidth           -> lookup[opts, StencilWidth],
              NumDifferentiations    -> lookup[opts, NumDifferentiations],
              GaugeThorns            -> lookup[opts, GaugeThorns],
              GaugeParameterSettings -> lookup[opts, GaugeParameterSettings],
              IDSpec                 -> {ActiveThorns -> {}, IDSettings -> {}},
              EvolutionSpec          -> lookup[opts, EvolutionSpec],
              EvaluationSpec         -> lookup[opts, EvaluationSpec],
              IOSpec -> {outDir -> name,
              outFormat -> ".16e",
              ioEvery -> {100, 
                crossingSteps, crossingSteps * 10, -1* (2 ^ lookup[opts, Resolution])},
              ioInfoGFs -> Union@Flatten@{hamgf, lapsegf, trKgf, 
                                          lookup[opts, ioInfoGFs]}, 
              io0dGFs   -> Union@Flatten@{hamgf, lapsegf, trKgf,
                                          lookup[opts, NoiseGroups],
                                          lookup[opts, io0dGFs]}, 
              io1dGFs   -> Union@Flatten@{hamgf, lapsegf, trKgf, 
                                          lookup[opts, io1dGFs]}, 
              io2dGFs   -> Union@Flatten@{lookup[opts, io2dGFs]},

              out1D -> {"yes", "no", "no"} }];
]



CreatePolarizedGowdyTest[optArgs___]:= Module[
{opts, par, noiseBasic, crossingSteps, name, hamgf, lapsegf, trKgf},

opts = GetOptions[CreatePolarizedGowdyTest, {optArgs}];


If[MemberQ[{"PolarizedGowdyCollapse", "PolarizedGowdyExpansion"}, lookup[opts, Type]],

Print["Creating parameter file for " <>  lookup[opts, Type] <> " test."],
Throw["invalid type"]
];

noiseBasic = 0.0 * 10^(-10);

crossingSteps = 
  Abs@IntegerPart[(zbaseRes[lookup[opts, Type]] * (2 ^ lookup[opts, Resolution]))
                * (1./dtFactor[lookup[opts, Type]])];

Print["time steps for one crossing time: ", crossingSteps];

name = lookup[opts, Name] <> lookup[opts, Type] <> "_rho" <>ToString[2^lookup[opts, Resolution]];


hamgf   = ToString@lookup[opts, hamGF]; 
lapsegf = ToString@lookup[opts, lapseGF]; 
trKgf   = ToString@lookup[opts, trKGF]; 

CreateParfile[Name                   -> name,
              Directory              -> lookup[opts, Directory],
              Type                   -> lookup[opts, Type],
              NoiseAmp               -> noiseBasic /  (2 ^ lookup[opts, Resolution])^2,
              NoiseGroups            -> lookup[opts, NoiseGroups],
              Resolution             -> {0,0,lookup[opts, Resolution]}, 
              StencilWidth           -> lookup[opts, StencilWidth],
              NumDifferentiations    -> lookup[opts, NumDifferentiations],
              GaugeThorns            -> lookup[opts, GaugeThorns],
              GaugeParameterSettings -> lookup[opts, GaugeParameterSettings],
              IDSpec                 -> {ActiveThorns -> {}, IDSettings -> {}},
              EvolutionSpec          -> lookup[opts, EvolutionSpec],
              EvaluationSpec         -> lookup[opts, EvaluationSpec],
              IOSpec -> {outDir -> name,
              outFormat -> ".16e",
              ioEvery -> {100,
                crossingSteps, crossingSteps * 10, -1* (2 ^ lookup[opts, Resolution])},
              ioInfoGFs -> Union@Flatten@{hamgf, lapsegf, trKgf, 
                                          lookup[opts, ioInfoGFs]}, 
              io0dGFs   -> Union@Flatten@{hamgf, lapsegf, trKgf,
                                          lookup[opts, NoiseGroups],
                                          lookup[opts, io0dGFs]}, 
              io1dGFs   -> Union@Flatten@{hamgf, lapsegf, trKgf, 
                                          lookup[opts, io1dGFs]}, 
              io2dGFs   -> Union@Flatten@{lookup[opts, io2dGFs]},

              out1D -> {"no", "no", "yes"} }];
]


End[];

EndPackage[];

