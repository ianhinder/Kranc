(* ::Package:: *)

(*  Copyright 2004 Sascha Husa, Ian Hinder, Christiane Lechner, Barry Wardell

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

(**************************************************************************************)
(* Tensors *)
(**************************************************************************************)

DefineTensor[El[-a]];
DefineTensor[B[-a]];

DefineTensor[eps[-a, b, c]];
SetComponents[eps[-a, b, c], Array[Signature[{##}] &, {3, 3, 3}]];

DefineTensor[gInv[a, b]];
SetComponents[gInv[a, b], IdentityMatrix[{3,3}]];

(**************************************************************************************)
(* Groups *)
(**************************************************************************************)

(* NB This will give B the symmetries of a VECTOR, which is not correct *)
evolvedGroups = Map[CreateGroupFromTensor, {El[-a], B[-a]}];
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
};

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
    dot[El[-a]] -> (eps[-a,e,f] pd[B[-f], -e]),
    dot[B[-a]] -> -(eps[-a,e,f] pd[El[-f], -e])
  }
};

(**************************************************************************************)
(* Constraint equations *)
(**************************************************************************************)

constraintsCalc =
{
  Name -> "EM_constraints",
  Where -> Interior,
  Equations ->
  {
    CEl -> gInv[a, b] pd[El[-a], -b],
    CB -> gInv[a, b] pd[B[-a], -b]
  }
};

(**************************************************************************************)
(* Energy equation *)
(**************************************************************************************)

energyCalc =
{
  Name -> "EM_energy",
  Equations ->
  {
    rho -> gInv[a, b] El[-a] El[-b] / 2 + gInv[a, b] B[-a] B[-b] / 2
  }
};

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

CreateKrancThornTT[groups, "xTensor", "EM",
  Calculations -> calculations,
  DeclaredGroups -> declaredGroupNames,
  PartialDerivatives -> derivatives,
  RealParameters -> realParameters];
