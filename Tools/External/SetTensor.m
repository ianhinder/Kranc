Print["Ignore the following warning message: Lower::shdw"];

Needs["MathTensor`","MathTensor.m"];
Needs["Format`","Format.m"];
Needs["Optimize`","Optimize.m"];
BeginPackage["SetTensor`",{"MathTensor`","Format`","Optimize`"}];
Print["Turning off AssignFunction::undef"];
Off[AssignFunction::undef];

AddReplaceList::usage = "AddReplaceList is a place to store a
list of replacement rules";
FortranInt::usage = "FortranInt[x1_Integer] formats an integer
for output by the fwri function.";

AddReplace::usage = "fwri[...,AddReplace->{from_String,to_String}]";

MTEval[x1_] := EvalMT[RicciToAffine[x1]]
MTEval[x1_,x2_] := EvalMT[RicciToAffine[x1],x2]

SetSqrtDetg::usage = "Define this quantity to aid evaluation where
the determinant of the metric is required.";

(* something to make SqrtDetg work right *)
prot=Unprotect[Power,Abs];
Power[Power[sDetgVal,2],1/2] := sDetgVal
Abs[sDetgVal^2] := sDetgVal^2
DetgVal := sDetgVal^2
Protect /@ prot;

SetTensor::usage =
"SetTensor[name[ua,ub],{{n11,n12,n13,...},{n12,n22,...},...}]
SetTensor[name[ua,ub],MyFunction[ua,ub]]
The first form given above for SetTensor will
assign the components of the array on the right hand side to the
tensor on the left hand side.  The second form of this function
is like SetComponents, except that it does far more than merely
summing dummy indices.  Most importantly, it does not evaluate
MyFunction until a numerical value has been assigned to the index.
Secondly, before assigning it calls EvalMT to resolve ordinary
derivatives, sums, etc. If you wish to see a message as each
component is evaluated, you can do that by typing On[EvalMT::PrintIndex]
before calling SetTensor.";

EvalMT::PrintIndex = "If this flag is set by the On[] command, then
EvalMT prints its second argument when it runs (if it was called
with two arguments.  This means that SetTensor will tell you the
indices of each component of the tensor it is assigning values to."

EvalMT::usage = "EvalMT[expression], EvalMT[name[la,lb],{la->-1,lb->-1}]
In the first form, this function lowers all covariant derivative
indices, expands all sums, and evaluates all ordinary derivatives,
and Affine connection terms.  In the second form, the values of the
indices in the expression are given in list. In order for this command
to work properly, you must first use the command InitializeMetric.";

InitializeMetricg = InitializeMetric;
InitalizeMetric::usage = "InitializeMetric[array],
InitializeMetric[array,MySimplify] This command
takes the array metdn, assigns its components to the lower indexed
Metricg, assigns Inverse[metdn] to the upper index Metricg, and then
evaluates all AffineG components.  An optional function to simplify
these various stages is provided.";

EvalOD::usage = "EvalOD[expression] Evaluates ordinary derivatives
within an expression.";

MetricgVal::usage = "This object contains the component values for the
Metricg tensor."

AffineGVal::usage = "This object contains the component values for the
AffineG tensor."

SubFun::usage = "SubFun[expression,pattern,value] operates like
the normal substitution rule ->, except that if pattern is
a function it will also substitute for derivatives of that
function.  Furthermore, if pattern is an expression to a power,
it will match the negative power, Thus SubFun[Sqrt[x]+1/Sqrt[x],Sqrt[x],y]
becomes y+1/y.";

(* The user can over-ride this function *)
Clear[MakeDeriv];
MakeDeriv[x1_,x2_,x3_] := ToExpression["d"<>x1<>x2<>x3]
MakeDeriv::usage = "MakeDeriv[derivs,function,args] a user definable
function to make the representation of derivatives more suitable for
printing.";

fwri::usage = "fwri[OuputStream,lhs,rhs] an interface to the FortranAssign
command provided through MathSource.  It is an unnecessary but user-
friendly piece of syntatic sugar. Obsolete.  See new function: FortranWrite";

$FortranOptimize::usage = "Set to True or False.  Determines whether Fortran
output will be optimized automatically."
$FortranOptimize = True;

$FortranReplace::usage = "This is an array of rules of the form one might
hand to StringReplace.  It will be used to process the output of FortranWrite.";

$FortranArrayList::usage = "The optimize routine eliminates multiple calls to
subroutines within a function by assigning the output of the function to a
variable.  Since it is hard to distinguish arrays from functions syntactically
in fortran, you can use this variable to avoid the unnecessary creation of
excess scalar variables."

FortranReplaceDefault::usage = "Calling FortranReplaceDefault[] resets the value of
$FortranReplace to its initial value."

FortranOpen::usage = "A call to OpenWrite that makes sure the file is closed first.  Aside
from this, it is the same as OpenWrite."
FortranWrite::usage = "A call to WriteString and FortranAssign, but wrapped together
and buffered. Output is only actually done when FortranFlush or FortranClose are called.
The purpose of the buffering is to allow the FortranAssign optimizer to re-use common
subexpressions in the equations.";

FortranFlush::usage = "FortranFlush[fd] : Causes the output of the last several
FortranWrite's to be done.";

FortranClose::usage = "Calls FortranFlush and then closes the file.";

Iter::usage = "Iter[Tensor Object,Code] is a type of looping mechanism
similar to multiple nested For[] loops.  The looping is over the number
of unique index values to the tensor object.";

DerivRule::usage = "A rule to implement the MakeDeriv[] function.";

AddStrs::usage = "AddStrs[Append/Prepend,arg] adds a string to the
front/end of each element of a
list composed of lists and/or strings (which may have
a minus sign) or zero's.";

AddIndex::usage = "AddIndex[Append/Prepend,arg] adds an index to a
list composed of lists and/or strings (which may have
a minus sign) or zero's.";

AddSym2::usage = "AddSym2[Append/Prepend,arg] adds two symmetric
indicies to a
list composed of lists and/or strings (which may have
a minus sign) or zero's.";

AddAsym2::usage = "AddAsym2[Append/Prepend,arg] adds two
anti-symmetric indices to a
list composed of lists and/or strings (which may have
a minus sign) or zero's.";

AddDeps::usage = "AddDeps[arg] adds the default dependency list
to the end of all strings in its arg, then runs Clear and ToExpression
on each symbol name."

DepList::usage = "DepList[leftbracket,rightbracket] provides a string
object that represents the default list.  The arguments are strings
which provide the left and right brackets you wish to use, (), [], or {}";

FortranOutputOfDepList::usage = "FortranOutputOfDepList is a variable
which is set to the string value you want your dependcy list to map
to when Fortran output is produced.  It is null by default.";
FortranOutputOfDepList = "";

Set1Component::usage = "Set1Component[tensor element,value] sets one
element of a tensor.  It takes into account the symmetries of the
tensor when performing its operation.";

DefaultDepString::usage = "Set the value of this string to the default
dependency of your functions (without the brackets).  For examples \"x,y,z\"."; 

NeedsIt::usage = "NeedsIt[func] Returns true if the function has
been in an expression handed to AddtoNeedsIt[]";

AddToNeedsIt::usage = "AddToNeedsIt[expr] finds all functions in
expr of the form f_Symbol[a__] and makes NeedsIt[f[a]] return
True.";

ClearNeedsIt::usage = "ClearNeedsIt[] causes NeedsIt[_] to return
False.";

Begin["Private`"];

EvalMT::DimensionNotSet = "This function will not evaluate
properly until you set the variable Dimension";
DimNotSet[] := If[Not[MatchQ[Dimension,_Integer]],
	Message[EvalMT::DimensionNotSet];
	True,False];

(* flag setting... *)
prot = Unprotect[On,Off];
On[EvalMT::PrintIndex] := (Flag[EvalMT::PrintIndex] = True);
Off[EvalMT::PrintIndex] := (Flag[EvalMT::PrintIndex] = False);
Protect /@ prot;
On[EvalMT::PrintIndex]

(* some helper functions *)
(* just some shorthand *)
sti[x1_] := ToString[InputForm[x1]]

(* make a string that represents a comma joined list *)
CommaJoin[{x1_}] := x1
CommaJoin[{x1_,x2_}] := x1<>","<>x2;
CommaJoin[{x1_,x2__}] := x1<>","<>CommaJoin[{x2}];

(* NoSimp is a simplification method that does nothing,
 * PostSimp is a mechanism for applying the simplification
 * method (its first arg) after the evaluation of EvalMT.
 *)
Clear[PostSimp,NoSimp];
NoSimp[x1_] := x1
PostSimp[si_,fun_] := si[fun] /; FreeQ[fun,EvalMT];
PostSimp[si_,x1_List] := PostSimp[si,#]& /@ x1;

(* This section handles the form of SetTensor that
 * takes a list as an argument.
 *)
Clear[SetTensor,FunctionArg,ArrayArg]
FunctionArg[x1_?LowerIndexQ] := "l"<>ToString[x1]
FunctionArg[x1_?UpperIndexQ] := "u"<>ToString[x1]
ArrayArg[x1_?LowerIndexQ] := "-l"<>ToString[x1]
ArrayArg[x1_?UpperIndexQ] := "u"<>ToString[x1]

SetTensor[x1_,x2_] := SetTensor[x1,x2,NoSimp];

SetTensor[x1_[x2___],x3_List,si_] := Module[{ftmp,atmp,res},
	If[DimNotSet[],Return[]];
	ftmp = "["<>CommaJoin[FunctionArg /@ {x2}]<>"]";
	atmp = "[["<>CommaJoin[ArrayArg /@ {x2}]<>"]]";
	res="Iter["<>sti[x1]<>ftmp<>",Set1Component["<>
	sti[x1]<>ftmp<>","<>sti[PostSimp[si,x3]]<>atmp<>"]]";
	ToExpression[res];
	]

(* FakeIndex will not fool MathTensor, but it can
 * be used for SetTensor, because SetTensor only
 * looks at the Upper/LowerIndexQ functions.
 *)
Clear[FakeIndex];
FakeIndex[la_?LowerIndexQ] := Module[{tmp},
	tmp = Unique[la];
	LowerIndexQ[tmp] ^:= True;
	tmp
	]
FakeIndex[la_?UpperIndexQ] := Module[{tmp},
	tmp = Unique[la];
	UpperIndexQ[tmp] ^:= True;
	tmp
	]

(* This form of SetTensor uses a function to set
 * values. *)
SetTensor[x1_[x2__],x3_?NoList,si_] := Module[{rule,v1},
	If[DimNotSet[],Return[]];
	rule = {x2} /. v1_?IndexQ -> (v1->FakeIndex[v1]);
	tmp="Private`AssignVals["<>
	ToString[InputForm[x1[x2] /. rule]]<>",Private`PostSimp["<>
	sti[si]<>",EvalMT["<>
	ToString[InputForm[x3]]<>","<>
	ToString[InputForm[rule]]<>"]]];";
	ToExpression[tmp];
]

Print["Turning off MetricgFlag"];
Off[MetricgFlag];

(* evaluate ordinary derivatives *)
EvalOD[x1_] := Module[{ODtmp},
	On[EvaluateODFlag];
	ODtmp = x1;
	Off[EvaluateODFlag];
	ODtmp];

(* rule for lowering covariant derivative indicies *)
RuleUnique[
	CDdown,
	CD[x3_,ua_],
	CD[x3,lb] Metricg[ub,ua],
	UpperIndexQ[ua]
	];

(* fix a bug that exists in some versions of Mathematica *)
(* this is a kludge *)
DiagonalQ[x1_List] := Module[{id},
	id = IdentityMatrix[Length[x1]];
	If[x1 - (x1*id) === 0*id,True,False]];
Unprotect[Inverse];
Inverse[x1_?DiagonalQ] := Module[{i1,i2,ln},
	ln = Length[x1];
	Table[If[i1 == i2,1/x1[[i1,i2]],0],
		{i1,1,ln},{i2,1,ln}]
];
Protect[Inverse];
(* End bug fix *)

(* MathTensor Evaluator *)
EvalMT[x1_] := EvalMT[x1,{}];
EvalMT[x1_,x2_] := Module[{tmp},
	If[DimNotSet[],Return[]];
	tmp=Dum[x1];
	While[Not[FreeQ[tmp,LieD]],
		tmp=LieDtoCD[tmp];
	]; (* expand LieD's *)
	tmp=ApplyRulesRepeated[tmp,{CDdown}];
		(* we can only evaluate down indicies *)
	While[Not[FreeQ[tmp,CD]],
		tmp=CDtoOD[tmp];
	]; (* expand CD's *)
	tmp = SubVals[tmp];
	tmp = MakeSum[tmp];
	If[x2 =!= {},
		(* If[FreeQ[Messages[EvalMT],
		  Literal[EvalMT::PrintIndex] :> $Off[___]], *)
		If[Flag[EvalMT::PrintIndex],
		  Print[sti[x2]] ]; (* print messages if PrintIndex is On *)
		tmp = tmp //. x2; (* substitute index values *)
	];
	tmp = SubVals[tmp]; (* needed for Epsilon *)
	tmp = EvalOD[tmp];
	tmp
	];

SetSqrtDetg[x2_,x1_] := Module[{},signDetg=x2;SqrtDetg = x1]

SubVals[x1_] := (x1 /. {
	Metricg -> MetricgVal,
	AffineG -> AffineGVal,
	Sqrt[Abs[Detg]] -> SqrtDetg
	}) /. Detg -> signDetg SqrtDetg^2;

(* Sets things up so that EvalMT can evaluate rapidly *)
NoList[_List] := False
NoList[_] := True
InitializeMetric[metdn_List] := InitializeMetric[metdn,NoSimp];
InitializeMetric[metdn_List,SimpOp_?NoList] := Module[{},
	metup = SimpOp[Inverse[metdn]];
	InitializeMetric[metdn,metup,SimpOp]];
InitializeMetric[metdn_List,metup_List] := 
	InitializeMetric[metdn,metup,NoSimp];
InitializeMetric[metdn_List,metup_List,SimpOp_] := Module[{},
	Clear[MetricgVal];
	DefineTensor[MetricgVal,"gv",{{2,1},1}];
	SetTensor[MetricgVal[la,lb],metdn,SimpOp];
	SetTensor[MetricgVal[ua,ub],metup,SimpOp];
	SetTensor[MetricgVal[ua,lb],IdentityMatrix[Dimension]];

	Print["Evaluating Affine Connections"];

	Clear[AffineGVal];
	DefineTensor[AffineGVal,"Gv",{{1,3,2},1}];
	SetTensor[AffineGVal[ua,lb,lc],AffineToMetric[
		AffineG[ua,lb,lc]
		],SimpOp];
	
	Print["Done"];
];


(* Set Zero's: this looks at the symmetries of a tensor
 * and assigns the value 0 to components which must have
 * it by symmetry, i.e. RiemannR[-1,-1,-2,-3].
 *)

(* In reality, this functionality should be provided by
 * DefineTensor, and probably will be eventually.
 *)

(* consider the example:
 * DefineTensor[foo,{{2,1},-1}];
 * consider what SetZeros[foo[la,lb]] does...
 * AllSymmetries returns {{1, 2}, 1, {2, 1}, -1}
 *)
SetZeros[foo_[x1___]] := SetZeros[foo,{x1},AllSymmetries[foo[x1]] ];
SetZeros[foo_,x1_List,sym_List] := Module[{zer,i1,i2},
	For[i1=1,i1<=Length[sym],i1 += 2,
		If[sym[[i1+1]] == -1,
			(* We collect indices not in their proper order, and
			 * stick them in zer.  If sym[[i1]] were {1,3,2}
			 * zer would become {2,3}. *)
			(* When we get here, in our example,
			 * i1==3, sym[[i1]]=={2,1} *)
			zer={};
			For[i2=1,i2<=Length[sym[[i1]]],i2++,
				If[i2 != sym[[i1,i2]],zer=AppendTo[zer,i2]];
			];
			(* We make a copy of the non-symmetric index list,
			 * in our example {la,lb}, and use the next loop
			 * to fill it in so that it becomes {la,la} *)
			list2 = x1;
			For[i2=2,i2<=Length[zer],i2++,
				list2[[ zer[[i2]] ]] = list2[[ zer[[1]] ]];
			];
			(* Now, in our example, we call AssignVals with
			 * as follows AssignVals[foo[la,la],0] *)
			AssignVals[list2 /. List->foo,0];
		];
	];
]


(* EatIndex takes a list of objects (indices) and an object to be removed
 * from the list, and returns the list with all instances of object
 * rmoved.  For example:  EatIndex[{a,b,b,c},{b}] returns {a,c}
 *)
EatIndex[x1_List,x2_] := EatIndex[x1,{},x2];
EatIndex[{x1_,x2___},{x3___},x1_] := EatIndex[{x2},{x3},x1];
EatIndex[{x1_,x2___},{x3___},x4_] := EatIndex[{x2},{x3,x1},x4];
EatIndex[{},x2_List,x3_] := x2

(* just a shorthand *)
AssignVals[x1_[xa__],x2_] := Module[{tmp,tmp2,res},
	IterNoZeros[x1[xa],Set1Component[x1[xa],x2]];
];
	
SetAttributes[AssignVals,HoldRest];

Iter[x1_,x2_] := Module[{},
	If[DimNotSet[],Return[]];
	SetZeros[x1];
	IterNoZeros[x1,x2]
]
IterNoZeros[foov_[v1___],thingv_] := Module[{IterInternal},
	Clear[IterInternal,IterSet];
	IterSet[0] := 0;
	IterInternal[{x1_?LowerIndexQ,x2___},fo_,foo_,thing_,ru_List] :=
		Module[{ix,tmp},
		For[ix=-Dimension,ix<0,ix++,
			tmp=ru;
			IterInternal[EatIndex[{x2},x1],fo,foo /. x1->ix,thing,
			AppendTo[tmp,x1->ix]]
		]
	];
	IterInternal[{x1_?UpperIndexQ,x2___},fo_,foo_,thing_,ru_List] :=
		Module[{ix,tmp},
		For[ix=1,ix<=Dimension,ix++,
			tmp=ru;
			IterInternal[EatIndex[{x2},x1],fo,foo /.
				x1->ix,thing,AppendTo[tmp,x1->ix]]
		]
	];
	IterInternal[{},foo_,IterSet[foo_[x1__]],
		thing_,ru_List] := (Evaluate[Release[thing /.
		ru]];IterSet[foo[x1]]=1);
	IterInternal[{},fo_,x1_,x2_,ru_] := Null;
	IterInternal[{v1},foov,IterSet[foov[v1]],Hold[thingv],{}];
	];
SetAttributes[Iter,HoldRest];
SetAttributes[IterNoZeros,HoldRest];

(* SubFun is part of a larger idea for a more general substitution
 * function.  This version only does functions and powers
 *)
Clear[SubFun,unList,mkdiv];
SubFun[x1_,Power[x2_,x3_],x4_] :=
	x1 /. {
		Power[x2,x3]->x4,
		Power[x2,-x3]->1/x4
		};

unList[List[x1__]] := x1;
mkdiv[val_,nums_,args_] := Module[{tmp,ii},
	tmp=Table[{args[[ii]],nums[[ii]]},{ii,1,Length[args]}];
	D[val,Evaluate[unList[tmp]] ]
	];

SubFun[x1_,fun_[args__],val_] :=
	x1 /. {
		fun[args]:>val,
		Derivative[nums__][fun][args]:>mkdiv[val,
		List[nums],List[args]]
		};

SubFun[x1_,x2_,x3_] :> x1 /. x2 -> x3
(* End of SubFun *)

(* This section is for generating values for tensors *)
Clear[IsAppPrep];
IsAppPrep[Prepend] := True;
IsAppPrep[Append] := True;
IsAppPrep[_] := False;

Clear[AddStrs]
AddStrs[Append,x1_String,x2_String] := Module[{},
	If[DimNotSet[],Return[]];x1<>x2];
AddStrs[Prepend,x1_String,x2_String] := x2<>x1;
AddStrs[pa_?IsAppPrep,x1_String,x2_List] :=
	AddStrs[pa,x1,#]& /@ x2;
AddStrs[x1_String,x2_] := AddStrs[Append,x1,x2];
AddStrs[pa_,-x1_,x2_] := -AddStrs[pa,x1,x2];
AddStrs[pa_,x1_,-x2_] := -AddStrs[pa,x1,x2];
AddStrs[pa_,-x1_,-x2_] := AddStrs[pa,x1,x2];
AddStrs[pa_,0,x2_] := 0
AddStrs[pa_,x1_,0] := 0
AddStrs[pa_,x1_List,x2_] := AddStrs[pa,#,x2]& /@ x1;
AddStrs[x1_List,x2_] := AddStrs[Append,x1,x2];

Clear[AddIndex];
AddIndex[pa_?IsAppPrep,x1_String] := Module[{},
	If[DimNotSet[],Return[]];
	AddStrs[pa,x1,#]& /@
	Table[ToString[x[i]],{i,1,Dimension}]];
AddIndex[pa_?IsAppPrep,x1_List] := AddIndex[pa,#]& /@ x1;
AddIndex[x1_] := AddIndex[Append,x1];
AddIndex[pa_?IsAppPrep,-x1_] := -AddIndex[x1]
AddIndex[pa_?IsAppPrep,0] := Module[{i},Table[0,{i,1,Dimension}]]

Clear[AddSym2,Sym2];
Sym2[x1_Integer,x2_Integer] := Module[{tmp},
	If[DimNotSet[],Return[]];
	tmp = {x1,x2};
	tmp = Abs /@ tmp;
	tmp = Sort[tmp];
	tmp = x /@ tmp;
	tmp = ToString /@ tmp;
	tmp = tmp /. List->StringJoin
	];
Sym2[] := Module[{i,j},Table[Sym2[i,j],
	{i,1,Dimension},{j,1,MathTensor`Dimension}] ];
AddSym2[pa_?IsAppPrep,x1_String] := AddStrs[pa,x1,Sym2[]];
AddSym2[pa_?IsAppPrep,x1_List] := AddSym2[pa,#]& /@ x1;
AddSym2[x1_] := AddSym2[Append,x1];
AddSym2[pa_?IsAppPrep,0] := Module[{i,j},
	Table[0,{i,1,Dimension},{j,1,MathTensor`Dimension}]
	];
AddSym2[pa_?IsAppPrep,-x1_] := -AddSym2[pa,x1];

Clear[AddAsym2,Asym2];
Asym2[x1_Integer,x2_Integer] := Module[{tmp,sgn},
	If[DimNotSet[],Return[]];
	If[x1 === x2,Return[0]];
	tmp = {x1,x2};
	tmp = Abs /@ tmp;
	tmp = Sort[tmp];
	sgn = If[tmp === {x1,x2},1,-1];
	tmp = x /@ tmp;
	tmp = ToString /@ tmp;
	tmp = tmp /. List->StringJoin;
	tmp sgn
	];
Asym2[] := Module[{i,j},Table[Asym2[i,j],
	{i,1,Dimension},{j,1,MathTensor`Dimension}] ];
AddAsym2[pa_?IsAppPrep,x1_String] := AddStrs[pa,x1,Asym2[]];
AddAsym2[pa_?IsAppPrep,x1_List] := AddAsym2[pa,#]& /@ x1;
AddAsym2[x1_] := AddAsym2[Append,x1];
AddAsym2[pa_?IsAppPrep,0] := Module[{i,j},
	Table[0,{i,1,Dimension},{j,1,MathTensor`Dimension}]
	];
AddAsym2[pa_?IsAppPrep,-x1_] := -AddAsym2[pa,x1];

ClearStrs[x1_List] := ClearStrs /@ x1;
ClearStrs[x1_String] := Clear[x1];
ClearStrs[-x1_String] := Clear[x1];

Clear[ExtendedToExpression];
ExtendedToExpression[0] := 0;
ExtendedToExpression[-x1_String] := -ToExpression[x1];
ExtendedToExpression[x1_String] := ToExpression[x1];
ExtendedToExpression[x1_List] := ExtendedToExpression /@ x1;
AddDeps[x1_] := Module[{},
	If[DimNotSet[],Return[]];
	ClearStrs[x1];
	ExtendedToExpression[ AddStrs[Prepend,DepList["[","]"],x1] ]
	];

(* Note that this does not do the same thing as x1 = x2
 * by itself.  MathTensor expressions order themselves
 * according to symmetry, and this puts that action first
 * in the order of evaluations.
 *)
Clear[Set1Component];
Set1Component[-x1_,x2_] := x1 = -x2;
Set1Component[x1_,x2_] := x1 = x2;

(* Derivative conversion *)
Clear[DerivList,DerivWithRespect2,DerivOf,DerivDeps,StringRepeat];

(* I keep thinking that there must be some
 * utility for this already, but I don't know
 * what it is.
 *)
StringRepeat[x1_,0] := "";
StringRepeat[x1_,1] := x1;
StringRepeat[x1_String,n1_Integer] := 
	x1<>StringRepeat[x1,n1-1];

DerivList[n_,args_,n1_] := StringRepeat[ToString[args[[n]]],n1];
DerivList[n_,args_,n1_,n2__] := DerivList[n,args,n1]<>
	DerivList[n+1,args,n2];
DerivWithRespect2[Derivative[n___][x2_][x3___]] :=
	DerivList[1,{x3},n];
DerivOf[Derivative[x1___][x2_][x3___]] := ToString[x2]
DerivDeps[Derivative[x1___][x2_][x3___]] := "["<>
	CommaJoin[ToString /@ {x3}]<>"]";

Clear[DerivRule];
DerivRule := Derivative[x1___][x2_][x3___] :>
	MakeDeriv[DerivWithRespect2[Derivative[x1][x2][x3]],
			    DerivOf[Derivative[x1][x2][x3]],
			  DerivDeps[Derivative[x1][x2][x3]] ];
	
Clear[DepList]
DepList[s1_String,s2_String] := Module[{i},
	If[DimNotSet[],Return[]];
	If[MatchQ[DefaultDepString,_String],Return[s1<>
		DefaultDepString<>s2]];
	s1<>CommaJoin[Table[ToString[x[i]],{i,1,Dimension}]]<>s2
	]

Clear[NeedsIt,AddToNeedsIt];
NeedsIt[_] := False
ClearNeedsIt[] := (
	Clear[NeedsIt];
	NeedsIt[_] := False;
	)
AddToNeedsIt[x1_] := Module[{tmp,x2,srule,dep},
	tmp=x1 /. DerivRule;
	dep=DepList["[","]"];
	srule = sti[x2]<>"_Symbol"<>dep<>
	" :> (NeedsIt["<>
	sti[x2]<>dep<>"] = True; "<>sti[x2]<>dep<>")";
	tmp /. ToExpression[srule];
	];

(* Here is how I make fwri, so I made a short symbol name for once.
 * Once in a while we should save on typing..
 *)
Clear[fwri,GetAddReplaceRule,IsAddReplace,FortranAssignArray,IsCall]
GetAddReplaceRule[AddReplace->{x1_}] := x1;
IsAddReplace[AddReplace->_] := True
IsAddReplace[_] := False
FortranAssignArray[{x1___}] := FortranAssign[x1];
IsCall[x1_] := Module[{tmp},
	tmp = ToString[x1];
	StringMatchQ[tmp,"call"] ||
	StringMatchQ[tmp,"Call"] ||
	StringMatchQ[tmp,"CALL"]]

Clear[FortranInt]
FortranInt[x1_?PosIntegerQ] := ToExpression["EraseMe"<>ToString[x1]]
FortranInt[x1_?NegIntegerQ] := ToExpression["MinusSign"<>ToString[-x1]]

Options[fwri] := {AddReplace->{x1_String->x2_String}};
fwri[fd_,v1_,v2_,v3___] := Module[{tmp,i,faArgs,li},
	If[MatchQ[Dimension,_Integer],
		tmp={DepList["(",")"]->FortranOutputOfDepList},
		tmp={}
	];
	tmp=AppendTo[tmp,"EraseMe"->""];
	tmp=AppendTo[tmp,"UND"->"_"];
	tmp=AppendTo[tmp,"MinusSign"->"-"];
	If[IsCall[v1],tmp=AppendTo[tmp,"="->""]];
	faArgs={}; li={v3};
	If[MatchQ[AddReplaceList,_List],
		li=Join[li,AddReplaceList]]; 
	For[i=1,i<=Length[li],i++, 
		If[IsAddReplace[li[[i]]], 
			tmp=AppendTo[tmp,GetAddReplaceRule[li[[i]]] ], 
			faArgs=AppendTo[faArgs,li[[i]] ] 
		]; 
	];
	faArgs = AppendTo[faArgs,AssignReplace->tmp];
	faArgs = PrependTo[faArgs,v2 /. DerivRule];
	faArgs = PrependTo[faArgs,v1];
	Write[fd,FortranAssignArray[faArgs]];
];

Clear[FortranOpen,FortranWrite,FortranClose,FortranSimp,
$FortranArrayList,$FortranReplace,DefaultFortranReplace,FortranFlush,FortranLHS,FortranRHS];
DefaultFortranReplace[] :=
	$FortranReplace = {
		"(r,q)"->"(i,j)",
		"UND"->"_",
		"sin(q)"->"sint(j)",
		"cos(q)"->"cost(j)",
		"ric"->"r",
		"phi"->"p",
		"tan(q)"->"tant(j)",
		"EraseMe"->"",
		"MinusSign"->"-"
	};
DefaultFortranReplace[];
$FortranArrayList = {};

FortranOpen[file_] := FortranOpen[file,NoSimp];
FortranOpen[file_,Simp_] := Module[{fd},
	Off[General::openx];
	Close[file];
	Print["Opening: ",file];
	fd = OpenWrite[file];
	FortranLHS[fd] = {};
	FortranRHS[fd] = {};
	FortranSimp[fd][x1_] := Simp[x1];
	Return[fd];
	];
FortranWrite[fd_,from_,to_,etc___] := Module[{tmpto},
	from /. xxx_[args___] :> ($FortranArrayList=Union[$FortranArrayList,{xxx}];xxx[i,j]);
	tmpto = FortranSimp[fd][to /. DerivRule];
	FortranLHS[fd] = AppendTo[FortranLHS[fd],from];
	FortranRHS[fd] = AppendTo[FortranRHS[fd],tmpto];
	];
fprep[x1_] := Flatten[x1];
fprep[{x1_}] := x1;
FortranFlush[fd_] := Module[{},
	If[Length[FortranLHS[fd]]==0,Return[]];
	If[$FortranOptimize,
	  WriteString[fd,FortranAssign[Evaluate[fprep[FortranLHS[fd]]],
	    Evaluate[fprep[FortranRHS[fd]]],
	  AssignOptimize->True,OptimizeNull->$FortranArrayList,OptimizePower->Binary,
	  AssignReplace->$FortranReplace],"\n"];
	  ,
	  WriteString[fd,FortranAssign[Evaluate[fprep[FortranLHS[fd]]],
	    Evaluate[fprep[FortranRHS[fd]]],
	  AssignReplace->$FortranReplace],"\n"];
	];
	FortranLHS[fd]={};
	FortranRHS[fd]={};
];
FortranClose[_] := Print["Bad call to FortranClose -- not opened with FortranOpen"];
FortranClose[OutputStream[s1_String, n1_Integer]] := Module[{fd},
	fd = OutputStream[s1, n1];
	FortranFlush[fd];
	Print["Closing: ",s1 ];Close[fd];
];


Protect[SetTensor,EvalOD,EvalMT,InitializeMetric,SetSqrtDetg,
	Iter,AddToNeedsIt,ClearNeedsIt,fwri,DerivRule,
	Set1Component];

End[];
EndPackage[];
