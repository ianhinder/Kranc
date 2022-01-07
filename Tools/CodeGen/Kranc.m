
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
    along with Kranc; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

BeginPackage["Kranc`"];

(* CodeGen.m *)

{INV, SQR, CUB, QAD, ScalarINV, ScalarSQR, ScalarCUB, ScalarQAD,
 IfThen, Parenthesis, ToReal,
 sqrt, exp, log, pow, atan2, cos, sin, tan, acos, asin, atan,
 cosh, sinh, tanh, acosh, asinh, atanh, fmax, fmin, fabs, isgn, sgn,
 kmadd, kmsub, knmadd, knmsub, kpos, kneg, kadd, ksub, kmul, kdiv,
 kacos, kacosh, kasin, kasinh, katan, katanh, kcopysign, kcos, kcosh, kfabs,
 kfmax, kfmin, kisgn, ksqrt, kexp, klog, kpow, ksgn, ksin, ksinh, ktan, ktanh,
 dir1, dir2, dir3, t, cctkOriginSpace1, cctkOriginSpace2, cctkOriginSpace3,
 dt, dx, dy, dz, cctkLbnd1, cctkLbnd2, cctkLbnd3, usejacobian,
 khalf, kthird, ktwothird, kfourthird, keightthird};

(* Helpers.m *)

dummy;

(* CodeGenCalculation.m *)

{GridFunctions, Shorthands, Equations, t, DeclarationIncludes,
LoopPreIncludes, GroupImplementations, PartialDerivatives, NoSimplify,
Boundary, Interior, InteriorNoSync, Where, AddToStencilWidth,
Everywhere, normal1, normal2, normal3, INV, SQR, CUB, QAD, dot, pow,
exp, t, cctkOriginSpace1, cctkOriginSpace2, cctkOriginSpace3,
dt, dx, dy, dz, idx, idy, idz, cctkLbnd1, cctkLbnd2, cctkLbnd3,
MinMod, VanLeer, BodyFunction,
CallerFunction, LoopFunction, GFAccessFunction, InitFDVariables,
MacroPointer, CachedVariables, SplitBy, SeparatedDerivatives,
SeparatedDerivatives2,GFOffset,GFLocal}

{ConditionalOnKeyword, ConditionalOnKeywords, CollectList, Interior,
InteriorNoSync, Boundary, BoundaryNoSync, BoundaryWithGhosts, Where, PreDefinitions,
AllowedSymbols, Parameters, ConditionalOnTextuals, ApplyBCs,
SimpleCode};

(* MoL.m *)
MoLEvolved;
MoLRHS;

(* Differencing.m *)

{Name, Definitions, shift, spacing, SBPDerivative};

(* Interface.m *)

{InterfaceTimelevels};

(* Schedule.m *)
ScheduleTimelevels;

(* KrancThorn.m *)

(* provide a ValueQ function the way we expect *)
SetAttributes[ValueQLegacy, HoldAll]
ValueQLegacy[x_] := If[$VersionNumber >= 12.2, ValueQ[x, Method -> "Legacy"], ValueQ[x]]

(* Work around bug in Mathematica 10.0.0 where z is a symbol in the global context *)
If[NameQ["Global`z"] && !ValueQLegacy[Global`z], Remove[Global`z]];

{x, y, z, r};

ThornOptions =
 {Calculations -> {},
  ConservationCalculations -> {},
  DeclaredGroups -> {},
  ODEGroups -> {},
  Implementation -> None,
  InheritedImplementations -> {},
  EvolutionTimelevels -> 3,
  DefaultEvolutionTimelevels -> None,
  RealParameters -> {},
  IntParameters -> {},
  KeywordParameters -> {},
  InheritedRealParameters -> {},
  InheritedIntParameters -> {},
  InheritedKeywordParameters -> {},
  ExtendedRealParameters -> {},
  ExtendedIntParameters -> {},
  ExtendedKeywordParameters -> {},
  PartialDerivatives -> {},
  ReflectionSymmetries -> {},
  ZeroDimensions -> {},
  UseLoopControl -> True,
  UseDGFE -> False,
  Tile -> False,
  UseOpenCL -> False,
  UseVectors -> False,
  ProhibitAssignmentToGridFunctionsRead -> False,
  IncludeFiles -> {},
  CSE -> False,
  UseJacobian -> False,
  UseCaKernel -> False,
  GenerateHostCode -> False,
  TileSize -> {8,8,8},
  LoopName -> "" (* internal hack - do not use *),
  Variables -> Automatic,
  Shorthands -> {},
  ParentDirectory -> ".",
  GenerateScript -> False,
  ParameterConditions -> {},
  MergeFiles -> None,
  CountOperations -> False};

ExecuteOn;
Device;
Host;
LocalGroups;

(* Thorn.m *)

{AccumulatorBase, ThornImplementation, Name, Type, Extend, Default,
Comment, Range, Implementation, KrancGroup, SchedulePoint, Language,
RequiredGroups, RequiredRegion, ProvidedGroups, ProvidedRegion,
SynchronizedGroups, StorageGroups, Timelevels, TimelevelsParameter,
VariableType, GridType, Dim, Size,
Visibility, Variables, Implementations, Value, AllowedValues,
UsedParameters, Description, ExtendedParameters, NewParameters,
Directory, Configuration, Interface, Param, Schedule, Sources, Makefile,
Filename,
Contents, ThornName, BaseImplementation, EvolvedGFs, EvolvedArrays, PrimitiveGFs,
Groups, Calculation, GridFunctions, Shorthands, Equations, Parameter,
Value, UsesFunctions, ArgString, Conditional, Conditionals, NewConditional, D1, D2, D3, D11, D22,
D33, D21, D31, D32, Textual, TriggerGroups, Include, RHSGroups, Tags, 
Steerable, Never, Always, Recover, Primitives, CaKernel, ScheduleGroups, Files};

{ExcisionGFs};

(* CaKernel.m *)
I3D;

(* TensorTools.m *)

{D1, D2, D3, D11, D22, D33, D21, D31, D32, D12, D13, D23, dot, Eps, Zero3, StepFunction}

(* ConservationCalculation.m *)
{flux, slopeL, slopeR, slope};

(* KrancScript.m *)
ScriptFlags =
  {"loopcontrol" -> UseLoopControl,
   "vectors" -> UseVectors,
   "opencl" -> UseOpenCL,
   "jacobian"-> UseJacobian,
   "cse" -> CSE,
   "cakernel" -> UseCaKernel};

(* CodeGenKranc.m *)
CArray;
ConditionExpression;

EndPackage[];
