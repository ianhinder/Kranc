
(* Mathematica Test File *)


Test[ProcessExpression[1/x,False],pow[x,-1],TestID->"INV"]

Test[ProcessExpression[x^2,False],pow[x,2],TestID->"SQR"]

Test[ProcessExpression[x^3,False],pow[x,3],TestID->"CUB"]

Test[ProcessExpression[x^4,False],pow[x,4],TestID->"QAD"]
