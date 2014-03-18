
(* Mathematica Test File *)

Test[
  Block[{CodeGenKranc`Private`dataType = "CCTK_REAL",
    $CodeGenTarget = NewObject[TargetC, {"UseVectors" -> False}]},
    FlattenBlock@CodeGenCalculation`Private`assignLocalFunctions[{u,v}, False, False, ReadGridFunctionInLoop]]
  ,
  "\nCCTK_REAL uL CCTK_ATTRIBUTE_UNUSED = u[index];\nCCTK_REAL vL CCTK_ATTRIBUTE_UNUSED = v[index];\n\n"
  ,
  TestID->"assignLocalFunctions"
]

Test[
  Block[{CodeGenKranc`Private`dataType = "CCTK_REAL",
    $CodeGenTarget = NewObject[TargetC, {"UseVectors" -> True}]},
    FlattenBlock@CodeGenCalculation`Private`assignLocalFunctions[{u,v}, True, False, ReadGridFunctionInLoop]]
  ,
  "\nCCTK_REAL uL CCTK_ATTRIBUTE_UNUSED = vec_load(u[index]);\nCCTK_REAL vL CCTK_ATTRIBUTE_UNUSED = vec_load(v[index]);\n\n"
  ,
  TestID->"assignLocalFunctions-vec"
]

Test[
  Block[{CodeGenKranc`Private`dataType = "CCTK_REAL",
    $CodeGenTarget = NewObject[TargetC, {"UseVectors" -> False}]},
    FlattenBlock@CodeGenCalculation`Private`assignLocalFunctions[{eT11}, False, False, ReadGridFunctionInLoop]]
  ,
  "\n\nCCTK_REAL eT11L CCTK_ATTRIBUTE_UNUSED ;\n\nif (assume_stress_energy_state>=0 ? assume_stress_energy_state : *stress_energy_state)\n{\n  eT11L = eT11[index];\n}\nelse\n{\n  eT11L = ToReal(0.0);\n}\n"
  ,
  TestID->"assignLocalFunctions-eT"
]

Test[
  Block[{CodeGenKranc`Private`dataType = "CCTK_REAL",
    $CodeGenTarget = NewObject[TargetC, {"UseVectors" -> True}]},
    FlattenBlock@CodeGenCalculation`Private`assignLocalFunctions[{eT11}, True, False, ReadGridFunctionInLoop]]
  ,
  "\n\nCCTK_REAL eT11L CCTK_ATTRIBUTE_UNUSED ;\n\nif (assume_stress_energy_state>=0 ? assume_stress_energy_state : *stress_energy_state)\n{\n  eT11L = vec_load(eT11[index]);\n}\nelse\n{\n  eT11L = ToReal(0.0);\n}\n"
  ,
  TestID->"assignLocalFunctions-eT-vec"
]

