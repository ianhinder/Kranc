
(*  Copyright 2013 Erik Schnetter

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

BeginPackage["DGFE`", {"Errors`", "Helpers`", "Kranc`", "MapLookup`"}];

DGFEDefinitions;
DGFEInitialise;
DGFECall;
DGFEConfigurationCCL;

Begin["`Private`"];

DefFn[
  DGFEDefinitions[cleancalc_, eqs_, gfs_] :=
  Module[
    {thorn, name, lhss, gfsInLHS, vars},
    InfoMessage[InfoFull, "Generating DGFE boilerplate"];
    thorn = "CCTK_THORN"; (* TODO: get actual thorn name *)
    name = lookup[cleancalc, Name];
    lhss = Map[#[[1]] &, eqs];
    gfsInLHS = Union[Cases[lhss, _ ? (MemberQ[gfs,#] &), Infinity]];
    InfoMessage[InfoFull, "gfsInLHS:" <> Map[" "<>ToString[#] &, gfsInLHS]];
    (* TODO: do this in a better way, don't examine the variable names *)
    vars = Select[gfsInLHS,
                  StringMatchQ[ToString[#],
                               RegularExpression[".*rhs.*"]] &];
    vars = Map[Symbol[StringReplace[ToString[#], "rhs"->""]] &, vars];
    InfoMessage[InfoFull, "DGFE variables:" <> Map[" "<>ToString[#] &, vars]];
    {
      "/* DGFE Definitions */",
      "",
      "#define config_sdg_order      4", (* TODO: make this a parameter *)
      "#define config_riemann_solver hrscc::LaxFriedrichsRS<DGFE_"<>name<>", false>",
      "",
      "/* Export definitions */",
      "#define "<>name<>"_sdg_grid   hrscc::GNIGrid<hrscc::GLLElement<config_sdg_order> >",
      "#define "<>name<>"_sdg_method hrscc::SDGMethod<DGFE_"<>name<>", "<>name<>"_sdg_grid, config_riemann_solver>",
      "",
      "/*** Numerical scheme ***/",
      "",
      "/* Configuration */",
      "#define config_method "<>name<>"_sdg_method",
      "",
      "/* Export definitions */",
      "#define "<>name<>"_method config_method",
      "#define "<>name<>"_solver hrscc::CLawSolver<DGFE_"<>name<>", config_method>",
      "",
      "",
      "",
      "class DGFE_"<>name<>";",
      "",
      "} // namespace "<>thorn,
      "namespace hrscc {",
      "  using namespace "<>thorn<>";",
      "  template<>",
      "  struct traits<DGFE_"<>name<>"> {",
      "    // All state vector variables",
      "    enum state_t {" <> Map["i"<>ToString[#]<>", " &, vars] <> "nvars};",
      "    enum {nequations = nvars};",
      "    enum {nexternal = 3*nvars};",
      "    enum {nbitmasks = 0};",
      "    static const bool pure = false;",
      "  };",
      "} // namespace hrscc",
      "namespace "<>thorn<>" {",
      "",
      "",
      "",
      "class DGFE_"<>name<>": public hrscc::CLaw<DGFE_"<>name<>"> {",
      "public:",
      "  typedef hrscc::CLaw<DGFE_"<>name<>"> claw;",
      "  typedef hrscc::traits<DGFE_"<>name<>">::state_t state_t;",
      "  typedef hrscc::traits<DGFE_"<>name<>"> variables_t;",
      "  static const int nvars = variables_t::nvars;",
      "  ",
      "  DGFE_"<>name<>"();",
      "  ",
      "  inline void prim_to_all(hrscc::Observer<claw> & observer) const",
      "  {",
      "  }",
      "  ",
      "  template<hrscc::policy::direction_t dir>",
      "  inline void fluxes(hrscc::Observer<claw> & observer) const",
      "  {",
      "    ",
      Map["    CCTK_REAL flux"<>ToString[#]<>"L;" &, vars],
      "    ",
      "    switch (dir) {",
      Table[{
        "    case hrscc::policy::" <> {"x", "y", "z"}[[dir]] <> ": {",
        Map["      flux"<>ToString[#]<>"L = observer.field[variables_t::i"<>ToString[#]<>" + "<>ToString[dir-1]<>"*DGFE_"<>name<>"::nvars];" &, vars],
        "      break;",
        "    }"},
            {dir, 1, 3}],
      "    default:",
      "      CCTK_BUILTIN_UNREACHABLE();",
      "    }",
      "    ",
      Map["    observer.flux[dir][variables_t::i"<>ToString[#]<>"] = flux"<>ToString[#]<>"L;" &, vars],
      "  }",
      "  ",
      "  template<hrscc::policy::direction_t dir>",
      "  inline void eigenvalues(hrscc::Observer<claw> & observer) const",
      "  {",
      "    assert(0);",
      "  }",
      "  ",
      "  template<hrscc::policy::direction_t dir>",
      "  inline void eig(hrscc::Observer<claw> & observer) const",
      "  {",
      "    assert(0);",
      "  }",
      "};",
      "",
      "",
      "",
      "} // namespace "<>thorn,
      "namespace hrscc {",
      "  using namespace "<>thorn<>";",
      "  template<> int CLaw<DGFE_"<>name<>">::conserved_idx[DGFE_"<>name<>"::nvars] = {};",
      "  template<> int CLaw<DGFE_"<>name<>">::primitive_idx[DGFE_"<>name<>"::nvars] = {};",
      "  template<> int CLaw<DGFE_"<>name<>">::rhs_idx[DGFE_"<>name<>"::nvars] = {};",
      "  template<> int CLaw<DGFE_"<>name<>">::field_idx[3*DGFE_"<>name<>"::nvars] = {};",
      "  template<> int CLaw<DGFE_"<>name<>">::bitmask_idx[0] = {};",
      "} // namespace hrscc",
      "namespace "<>thorn<>" {",
      "",
      "",
      "",
      "namespace {",
      "  int varindex(const char* const varname)",
      "  {",
      "    const int vi = CCTK_VarIndex(varname);",
      "    if (vi<0) CCTK_ERROR(\"Internal error\");",
      "    return vi;",
      "  }",
      "}",
      "",
      "DGFE_"<>name<>"::DGFE_"<>name<>"()",
      "{",
      "  using namespace hrscc;",
      "",
      Map["  CLaw<DGFE_"<>name<>">::conserved_idx[variables_t::i"<>ToString[#]<>"] = varindex(CCTK_THORNSTRING \"::"<>ToString[#]<>"\");" &, vars],
      Map["  CLaw<DGFE_"<>name<>">::primitive_idx[variables_t::i"<>ToString[#]<>"] = varindex(CCTK_THORNSTRING \"::"<>ToString[#]<>"\");" &, vars],
      "",
      Map["  CLaw<DGFE_"<>name<>">::field_idx[variables_t::i"<>ToString[#]<>" + 0*DGFE_"<>name<>"::nvars] = varindex(CCTK_THORNSTRING \"::flux"<>ToString[#]<>"1\");" &, vars],
      Map["  CLaw<DGFE_"<>name<>">::field_idx[variables_t::i"<>ToString[#]<>" + 1*DGFE_"<>name<>"::nvars] = varindex(CCTK_THORNSTRING \"::flux"<>ToString[#]<>"2\");" &, vars],
      Map["  CLaw<DGFE_"<>name<>">::field_idx[variables_t::i"<>ToString[#]<>" + 2*DGFE_"<>name<>"::nvars] = varindex(CCTK_THORNSTRING \"::flux"<>ToString[#]<>"3\");" &, vars],
      "",
      Map["  CLaw<DGFE_"<>name<>">::rhs_idx[variables_t::i"<>ToString[#]<>"] = varindex(CCTK_THORNSTRING \"::"<>ToString[#]<>"rhs\");" &, vars],
      "}",
      "",
      "",
      "",
      "/* A solver, DGFE's equivalent of cctkGH */",
      "static "<>name<>"_solver *solver = NULL;",
      "",
      "",
      "",
      "/* Call the pointwise DGFE derivative operator */",
      "#undef PDstandardNth1",
      "#undef PDstandardNth2",
      "#undef PDstandardNth3",
      "#define PDstandardNth1(u) (solver->wdiff<hrscc::policy::x>(&(u)[-index], i,j,k))",
      "#define PDstandardNth2(u) (solver->wdiff<hrscc::policy::y>(&(u)[-index], i,j,k))",
      "#define PDstandardNth3(u) (solver->wdiff<hrscc::policy::z>(&(u)[-index], i,j,k))",
      "",
      "",
      ""
    } // Flatten // Map[# <> "\n" &, #] &]
     ];

DefFn[
  DGFEInitialise[cleancalc_] :=
  Module[
    {name},
    name = lookup[cleancalc, Name];
    {
      "",
      "if (not solver) solver = new "<>name<>"_method(cctkGH);"
    } // Flatten // Map[# <> "\n" &, #] &]];

DefFn[
  DGFECall[cleancalc_] :=
  Module[
    {name},
    name = lookup[cleancalc, Name];
    {
      "",
      "/* Add the flux terms to the RHS */",
      "solver->compute_rhs();",
      "",
      "delete solver;",
      "solver = NULL;"
    } // Flatten // Map[# <> "\n" &, #] &]];

DefFn[
  DGFEConfigurationCCL[] :=
  "REQUIRES Boost CPPUtils FDCore HRSCCore\n"];


End[];

EndPackage[];
