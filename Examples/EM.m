
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
Map[DefineTensor, {El,B}];

(**************************************************************************************)
(* Groups *)
(**************************************************************************************)

(* NB This will give B the symmetries of a VECTOR, which is not correct *)
evolvedGroups = Map[CreateGroupFromTensor, {El[la], B[la]}];
evaluatedGroups = 
 {{"constraints", {CEl, CB}},
  {"endens",{rho}}};

declaredGroups = Join[evolvedGroups, evaluatedGroups];
declaredGroupNames = Map[First, declaredGroups];

groups = Join[declaredGroups];

(**************************************************************************************)
(* Initial data *)
(**************************************************************************************)

initialCalc =
{
  Name -> "EM_initial",
  Schedule -> {"at CCTK_INITIAL"},
  Equations -> 
  {
    El1 -> sigma*Cos[2 Pi (x + y)] ,
    El2 -> - (1 - sigma)*Cos[2 Pi x] - sigma*Cos[2 Pi (x + y)],
    El3 -> 0,
    B1  -> 0,
    B2  -> 0,
    B3  -> (1 - sigma)*Cos[2 Pi x] + sigma*Cos[2 Pi (x + y)]
  }
}

(**************************************************************************************)
(* Evolution equations *)
(**************************************************************************************)

evolCalc =
{
  Name -> "EM_evol",
  Schedule -> {"in MoL_CalcRHS"},
  Where -> Interior,
  Equations -> 
  {
    dot[El[la]] -> (Eps[la,lb,lc] Euc[ub,ue] Euc[uc,uf] PD[B[lf],le]),
    dot[B[la]]  -> -(Eps[la,lb,lc] Euc[ub,ue] Euc[uc,uf] PD[El[lf],le])
  }
}

(**************************************************************************************)
(* Constraint equations *)
(**************************************************************************************)

constraintsCalc =
{
  Name -> "EM_constraints",
  Where -> Interior,
  Equations -> 
  {
    CEl -> PD[El[la],lb] Euc[ua,ub], CB -> PD[B[la],lb] Euc[ua,ub]
  }
}

(**************************************************************************************)
(* Energy equation *)
(**************************************************************************************)

energyCalc =
{
  Name -> "EM_energy",
  Equations -> 
  {
    rho -> El[la] El[ua]/2 + B[la] B[ua]/2
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
  constraintsCalc, 
  energyCalc
};

CreateKrancThornTT[groups, ".", "EM", 
  Calculations -> calculations,
  DeclaredGroups -> declaredGroupNames,
  PartialDerivatives -> derivatives,
  RealParameters -> realParameters];
