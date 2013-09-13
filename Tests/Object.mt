
(* Mathematica Test File *)


(****************************************************************)
(* NewObject *)
(****************************************************************)

Test[
  NewObject[Obj, {}]
  ,
  Obj[]
  ,
  TestID->"NewObject-Empty"
]

Test[
  NewObject[Obj, {"Key" -> 1}]
  ,
  Obj["Key" -> 1]
  ,
  TestID->"NewObject-OneKey"
]

Test[
  NewObject[Obj, {"Key1" -> 1, "Key2" -> 2}]
  ,
  Obj["Key1" -> 1, "Key2" -> 2]
  ,
  TestID->"NewObject-TwoKeys"
]

Test[
  SetObjectField[NewObject[Obj, {"Key1" -> 1, "Key2" -> 2}], "Key1", 3]
  ,
  Obj["Key1" -> 3, "Key2" -> 2]
  ,
  TestID->"SetObjectField-Existing"
]

Test[
  SetObjectField[NewObject[Obj, {"Key1" -> 1, "Key2" -> 2}], "Key3", 3]
  ,
  Obj["Key1" -> 1, "Key2" -> 2, "Key3" -> 3]
  ,
  TestID->"SetObjectField-New"
]

Test[
  GetObjectField[NewObject[Obj, {"Key1" -> 1, "Key2" -> 2}], "Key1"]
  ,
  1
  ,
  TestID->"GetObjectField-Exists"
]

Test[
  AppendObjectField[NewObject[Obj, {"Key1" -> {}, "Key2" -> 2}], "Key1", 1]
  ,
  Obj["Key1" -> {1}, "Key2" -> 2]
  ,
  TestID->"AppendObjectField-Exists"
]

Test[
  JoinObjectField[NewObject[Obj, {"Key1" -> {}, "Key2" -> 2}], "Key1", {1}]
  ,
  Obj["Key1" -> {1}, "Key2" -> 2]
  ,
  TestID->"JoinObjectField-Exists"
]
