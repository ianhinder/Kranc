
SetEnhancedTimes[False];

groups = {{"phi_group", {phi}}};

derivatives =
{
  PDstandard[i_] ->
    StandardCenteredDifferenceOperator[1,fdOrder/2,i],
  PDstandard[i_, i_] ->
    StandardCenteredDifferenceOperator[2,fdOrder/2,i],
  PDstandard[i_, j_] ->
    StandardCenteredDifferenceOperator[1,fdOrder/2,i] StandardCenteredDifferenceOperator[1,fdOrder/2,j]
};

PD = PDstandard;

initialCalc = 
{
  Name -> "Laplace_initial",
  Schedule -> {"AT INITIAL"},
  Where -> Interior,
  Equations -> 
  {
    phi -> phi0 Sum[4/(Pi n) Sin[n Pi x/Lx] Sinh[n Pi y/Lx]/Sinh[n Pi Ly/Lx], {n, 1, 1, 2}]
  }
};

initialBoundaryCalc = 
{
  Name -> "Laplace_initial_boundary",
  Schedule -> {"AT INITIAL after Laplace_initial"},
  Where -> Boundary,
  Equations -> 
  {
    phi -> IfThen[Abs[y-Ly]<10^-10, phi0, 0]
  }
};

evolveCalc = 
{
  Name -> "Laplace_relax",
  Schedule -> {"in MoL_CalcRHS", "AT ANALYSIS"},
  Where -> Interior,
  Equations ->
  {
    dot[phi] -> mu Euc[ui,uj] PD[phi,li,lj]
  }
};

boundaryCalc = 
{
  Name -> "Laplace_boundary",
  Schedule -> {"in MoL_RHSBoundaries", "AT ANALYSIS"},
  Where -> Boundary,
  Equations ->
  {
    dot[phi] -> 0
  }
};


CreateKrancThornTT[
  groups, ".", 
  "Laplace", 
  Calculations -> {initialCalc, initialBoundaryCalc, evolveCalc, boundaryCalc},
  PartialDerivatives -> derivatives,
  ZeroDimensions -> {3},
  RealParameters -> {Lx,Ly,phi0,mu},
  IntParameters -> {{Name -> fdOrder, Default -> 2, AllowedValues -> {2, 4}}},
  DeclaredGroups -> {"phi_group"}];
