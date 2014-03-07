
Get["KrancThorn`"];
SetEnhancedTimes[False];

(********************************************************)
(* Derivatives *)
(********************************************************)

derivatives =
{
  PDstandard[i_] ->
    StandardCenteredDifferenceOperator[1,fdOrder/2,i],
  PDstandard[i_, i_] ->
    StandardCenteredDifferenceOperator[2,fdOrder/2,i],
  PDstandard[i_, j_] ->
    StandardCenteredDifferenceOperator[1,fdOrder/2,i] StandardCenteredDifferenceOperator[1,fdOrder/2,j],

  PDonesided2nd[1] -> dir[1] (-shift[1]^(2 dir[1]) + 4 shift[1]^dir[1] - 3 )/(2 spacing[1]),
  PDonesided2nd[2] -> dir[2] (-shift[2]^(2 dir[2]) + 4 shift[2]^dir[2] - 3 )/(2 spacing[2]),
  PDonesided2nd[3] -> dir[3] (-shift[3]^(2 dir[3]) + 4 shift[3]^dir[3] - 3 )/(2 spacing[3]),

  Diss[] -> 
    Switch[fdOrder,
      2, - diss Sum[spacing[i]^3 (DPlus[i] DMinus[i])^2, {i, 1, 3}],
      4,   diss Sum[spacing[i]^5 (DPlus[i] DMinus[i])^3, {i, 1, 3}]],

  PDzero[i_] -> DZero[i],
  PDzero[i_, j_] -> DZero[i] DZero[j],

  PDplus[i_] -> DPlus[i]
};

Map[DefineTensor, {norm[ui], dir[li]}];

zerodims = {};

DefineDerivative[pd, PDstandard];
DefineDerivative[pdo, PDonesided2nd];
DefineDerivative[PDnorm, PDplus];

evolvedGroup = {"evolved", {phi, pi}, Timelevels -> 3};
normGroup    = {"norms",   {VL2, VDP, EL2}, Timelevels -> 3};
exactGroup   = {"exact",   {phiExact, piExact}, Timelevels -> 3};
errorGroup   = {"errors",  {phiError, piError}, Timelevels -> 3};

groups = {evolvedGroup, normGroup, exactGroup, errorGroup};

wp = {Name -> periodicity, Default -> 1};
wa = {Name -> amplitude, Default -> 1};

nNorm = Sqrt[n1^2+n2^2+n3^2];
nX = n1 x + n2 y + n3 z;

(* Sine wave exact solution *)

exactSineCalc =
{
  Name -> "wave_exact_sine",
  Before -> {"import_exact"},
  ConditionalOnKeyword -> {"initial_data", "sine"},
  Schedule -> {"AT INITIAL before import_exact","AT POSTSTEP before calc_errors"},
  Shorthands -> {piconst},
  Equations -> 
  {
    piconst -> N[Pi,20],
    phiExact -> amplitude Sin[2 piconst / periodicity (nX - nNorm t)],
    piExact -> -2 piconst / periodicity nNorm  amplitude Cos[2 piconst / periodicity (nX - nNorm t)]
  }
}

(* Radial profile of exact Gaussian solution *)
f[x_] := x^3 Exp[-x^2/nSigma^2];

exactGaussianCalc =
{
  Name -> "wave_exact_gaussian",
  Before -> {"import_exact"},
  ConditionalOnKeyword -> {"initial_data", "gaussian"},
  Schedule -> {"AT INITIAL before import_exact","AT POSTSTEP before calc_errors"},
  Shorthands -> {piconst,rEps},
  Equations -> 
  {
      rEps -> (r^4+(10^(-6))^4)^(1/4),

      phiExact -> (f[t+t0+r] - f[t+t0-r]) / rEps,
      piExact -> (D[f[t+t0+r],t] - D[f[t+t0-r],t]) / rEps
  }
}

(* Import the exact solution as the initial data *)

importerEquations =
{
  phi -> phiExact,
  pi -> piExact
};

importerCalc = 
{
  Name -> "wave_import_exact",
  Schedule -> {"at INITIAL as import_exact"},
  Equations -> importerEquations
}

(* The evolution equations *)

evolveEquations =
{
  dot[phi] -> pi,
  dot[pi]  -> pd[phi,li,lj] Euc[ui,uj]
}

evolveCalc = 
{
  Name -> "wave_evolve",
  Schedule -> {"in MoL_CalcRHS as evolve"},
  Where -> Interior,
  Equations -> evolveEquations
}

(* Evaluate the errors *)

errorEquations =
{
  phiError -> phi - phiExact,
  piError -> pi - piExact
}

errorCalc = 
{
  Name -> "wave_calc_errors",
  Schedule -> {"at ANALYSIS as calc_errors"},
  Equations -> errorEquations
}

(* Evaluate the norms *)

normEquations =
{
  VL2squared -> phi^2 + pi^2,
  VL2 -> Sqrt[VL2squared],
  VDPsquared -> pi^2 + Euc[ui,uj] PDnorm[phi,li] PDnorm[phi,lj],
  VDP -> Sqrt[VDPsquared],
  EL2squared -> phiError^2 + piError^2,
  EL2 -> Sqrt[EL2squared]
}

normCalc = 
{
  Name -> "wave_calc_norm",
  Where -> Interior,
  Schedule -> {"at ANALYSIS as calc_norm"},
  Shorthands -> {VL2squared, VDPsquared, EL2squared},
  Equations -> normEquations
}

(**************************************************************************************)
(* Boundary conditions *)
(**************************************************************************************)

boundaryParam = 
{
  Name -> "boundary_condition",
  Default -> "radiative",
  AllowedValues -> {"none", "radiative"}
};

boundaryCalc =
{
  Name -> "wave_boundary",
  Schedule -> {"in MoL_RHSBoundaries"},
  ConditionalOnKeyword -> {"boundary_condition", "radiative"},
  Where -> Boundary,
  Shorthands -> {norm[ui], dir[li]},
  Equations ->
  {
    norm1 -> -x/r, 
    norm2 -> -y/r,
    norm3 -> -z/r,

    dir[li] -> EucDD[li, lj] Sign[norm[uj]],

    (* \partial_t u = - (u - u_0) / r - \partial_r u
       for u_0 some background solution. In this case, Minkowski in Cartesian 
       coordinates. *)
    
    dot[phi] -> -(phi - 0) / r + norm[ui] pdo[phi, li],
    dot[pi] -> -(pi - 0) / r + norm[ui] pdo[pi, li]
  }
};

(* Construct the thorn *)

calculations = {exactSineCalc, exactGaussianCalc, importerCalc, evolveCalc, errorCalc, normCalc, boundaryCalc};

declaredGroups = Map[groupName, {evolvedGroup, exactGroup, errorGroup, 
  normGroup}];

idParam = 
{
  Name -> "initial_data",
  Default -> "gaussian",
  AllowedValues -> {"gaussian", "sine"}
};

keywordParameters = 
{
  idParam,
  boundaryParam
};

CreateKrancThornTT[groups, ".", "Wave", 
  Calculations -> calculations,
  DeclaredGroups -> declaredGroups,
  PartialDerivatives -> derivatives,
  ZeroDimensions -> zerodims,
  KeywordParameters -> keywordParameters,
  RealParameters -> {wp, wa, {Name -> n1, Default -> 1}, n2, n3, nSigma, r0, t0, x0, diss},
  IntParameters -> {{Name -> fdOrder, Default -> 2, AllowedValues -> {2, 4}}}]
