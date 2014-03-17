
(* Mathematica Test File *)

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
  kmadd[c,ToReal[3],a]
  ,
  TestID->"ProcessExpression-vec-plus-kmadd-toreal"
]

Test[
  VectoriseExpression[a+ToReal[b] c]
  ,
  kmadd[c,ToReal[b],a]
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


(*

(kmul(p1o12dy,kmul(dir2,kadd(KRANC_GFOFFSET3D(u,0,3,0),kmadd(KRANC_GFOFFSET3D(u,0,0,0),-10,kmadd(KRANC_GFOFFSET3D(u,0,2,0),-6,kmadd(KRANC_GFOFFSET3D(u,0,-1,0),-3,kmul(KRANC_GFOFFSET3D(u,0,1,0),18))))))))

*)