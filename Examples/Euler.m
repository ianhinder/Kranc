
Get["KrancThorn`"];

SetEnhancedTimes[False];

(**************************************************************************************)
(* Derivatives *)
(**************************************************************************************)

derivatives =
{
  PDstandard2nd[i_] -> StandardCenteredDifferenceOperator[1,1,i],
  PDstandard2nd[i_, i_] -> StandardCenteredDifferenceOperator[2,1,i],
  PDstandard2nd[i_, j_] -> StandardCenteredDifferenceOperator[1,1,i] *
    StandardCenteredDifferenceOperator[1,1,j],

  PDstandard4th[i_] -> StandardCenteredDifferenceOperator[1,2,i],
  PDstandard4th[i_, i_] -> StandardCenteredDifferenceOperator[2,2,i],
  PDstandard4th[i_, j_] -> StandardCenteredDifferenceOperator[1,2,i] *
    StandardCenteredDifferenceOperator[1,2,j]
};

PD = PDstandard2nd;

(**************************************************************************************)
(* Tensors *)
(**************************************************************************************)

(* Register the tensor quantities with the TensorTools package *)
Map[DefineTensor, {w, Frho, Fw, FEn, rho, En, p}];

(**************************************************************************************)
(* Groups *)
(**************************************************************************************)

evolvedGroups = Map[CreateGroupFromTensor, {rho, w[uj], En}];
nonevolvedGroups = Map[CreateGroupFromTensor, {Frho[ui], Fw[ui,uj], FEn[ui], p}];

declaredGroups = Join[evolvedGroups, nonevolvedGroups];
declaredGroupNames = Map[First, declaredGroups];

groups = Join[declaredGroups];

(**************************************************************************************)
(* Initial data *)
(**************************************************************************************)

initialCalc =
{
  Name -> "euler_initial",
  Schedule -> {"at CCTK_INITIAL"},
  Equations ->
  {
    rho -> 1 + 0.1 Sin[2 Pi x],
    w1 -> 1,
    w2 -> 0,
    w3 -> 0,
    En -> 0
  }
}

(**************************************************************************************)
(* Evolution equations *)
(**************************************************************************************)

evolCalc =
{
  Name -> "euler_evol",
  Schedule -> {"in MoL_CalcRHS"},
  Equations -> 
  {
    dot[rho] -> PD[Frho[ui],li],
    dot[w[uj]] -> PD[Fw[uj, ui],li],
    dot[En] -> PD[FEn[ui],li]
  }
}

fluxCalc =
{
  Name -> "euler_flux",
  Schedule -> {"in MoL_PostStep"},
  Equations -> 
  {
    Frho[ui] -> w[ui],
    Fw[uj,ui] -> w[ui] w[uj] / rho + p Euc[ui,uj],
    FEn[ui] -> w[ui]/rho (En + p)
  }
}

pressureCalc =
{
  Name -> "euler_pressure",
  Schedule -> {"in MoL_PostStep before euler_flux"},
  Equations -> 
  {
    p -> 0
  }
}

realParameters = {sigma};

(**************************************************************************************)
(* Construct the thorn *)
(**************************************************************************************)

calculations = 
{
  initialCalc,
  evolCalc,
  fluxCalc,
  pressureCalc
};

CreateKrancThornTT[groups, ".", "Euler", 
  Calculations -> calculations,
  DeclaredGroups -> declaredGroupNames,
  PartialDerivatives -> derivatives,
  RealParameters -> realParameters];
