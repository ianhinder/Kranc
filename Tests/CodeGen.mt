
(* Mathematica Test File *)

SetAttributes[withVectorisation, HoldFirst];
withVectorisation[expr_] :=
  Block[{CodeGenKranc`Private`dataType = "CCTK_REAL_VEC",
    $CodeGenTarget = NewObject[TargetC, {"UseVectors" -> True}]},
    expr];

Test[
  CodeBlockContents[CodeBlock["text"]]
  ,
  "text"
  ,
  TestID->"CodeBlockContentsk"
]

Test[
  VectoriseExpression[a+b]
  ,
  kadd[a,b]
  ,
  TestID->"ProcessExpression-vec-plus"
]

Test[
  VectoriseExpression[a+3]
  ,
  kadd[a,ToReal[3]]
  ,
  TestID->"ProcessExpression-vec-plus-int"
]

Test[
  VectoriseExpression[a+ToReal[3]]
  ,
  kadd[a,ToReal[3]]
  ,
  TestID->"ProcessExpression-vec-plus-torealint"
]

Test[
  VectoriseExpression[a+b c]
  ,
  kmadd[b,c,a]
  ,
  TestID->"ProcessExpression-vec-plus-kmadd"
]

Test[
  VectoriseExpression[a+ToReal[3] c]
  ,
  kmadd[ToReal[3],c,a]
  ,
  TestID->"ProcessExpression-vec-plus-kmadd-toreal"
]

Test[
  VectoriseExpression[a+ToReal[b] c]
  ,
  kmadd[ToReal[b],c,a]
  ,
  TestID->"ProcessExpression-vec-plus-kmadd-toreal-sym"
]

Test[
  VectoriseExpression[ToReal[b] + ToReal[c]]
  ,
  ToReal[b+c]
  ,
  TestID->"ProcessExpression-vec-plus-kmadd-toreal-sym2"
]

Test[
  VectoriseExpression[IfThen[1, 2, 3]]
  ,
  IfThen[1,ToReal[2],ToReal[3]]
  ,
  TestID->"ProcessExpression-vec-IfThenConsts"
]

Test[
  VectoriseExpression[1./1024/dx]
  ,
  kdiv[ToReal[Evaluate[1./1024]],dx]
  ,
  TestID->"ProcessExpression-vec-kdivrealvec"
]

Test[
  VectoriseExpression[dx^-1]
  ,
  kdiv[ToReal[1],dx]
  ,
  TestID->"ProcessExpression-vec-kpowdiv"
]

Test[
  VectoriseExpression[fmin[1,exp[Subtract[1,rL*pow[ToReal[SpatialShiftGammaCoeffRadius],-1]]]]]
  ,
  kfmin[ToReal[1], kexp[knmsub[ToReal[SpatialShiftGammaCoeffRadius^(-1)], rL, ToReal[1]]]]
  ,
  TestID->"ProcessExpression-vec-param"
]

Test[
    Block[{CodeGenKranc`Private`dataType = "CCTK_REAL",
    $CodeGenTarget = NewObject[TargetC, {"UseVectors" -> False}]},
      FlattenBlock@GenerateCodeFromExpression[GFLocal[u],False]]
  ,
  "u[index]"
  ,
  TestID->"ProcessExpression-GFLocal"
]

Test[
    Block[{CodeGenKranc`Private`dataType = "CCTK_REAL_VEC",
    $CodeGenTarget = NewObject[TargetC, {"UseVectors" -> True}]},
      FlattenBlock@GenerateCodeFromExpression[GFLocal[u],True]]
  ,
  "vec_load(u[index])"
  ,
  TestID->"ProcessExpression-GFLocal-vec"
]



