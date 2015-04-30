#include "chemora_cg_kranc_startup.h"

static void
chemora_cg_thorn_startup()
{
  CHEMORA_CG_KRANC_INIT_THORN_START("SimpleWaveScriptCaKernel");
 set_differencing_op("PDstandardfdOrder21", "Times:2;Plus:1;", "(-KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1o2dx", "p1o2dx", NULL, 2,-1,0,0,1,0,0);
 set_differencing_op("PDstandardfdOrder22", "Times:2;Plus:1;", "(-KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1o2dy", "p1o2dy", NULL, 2,0,-1,0,0,1,0);
 set_differencing_op("PDstandardfdOrder23", "Times:2;Plus:1;", "(-KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1o2dz", "p1o2dz", NULL, 2,0,0,-1,0,0,1);
 set_differencing_op("PDstandardfdOrder41", "Times:4;Plus:3;", "(-8*KRANC_GFOFFSET3D(u,-1,0,0) + 8*KRANC_GFOFFSET3D(u,1,0,0) + KRANC_GFOFFSET3D(u,-2,0,0) - KRANC_GFOFFSET3D(u,2,0,0))*p1o12dx", "p1o12dx", NULL, 4,-2,0,0,-1,0,0,1,0,0,2,0,0);
 set_differencing_op("PDstandardfdOrder42", "Times:4;Plus:3;", "(-8*KRANC_GFOFFSET3D(u,0,-1,0) + 8*KRANC_GFOFFSET3D(u,0,1,0) + KRANC_GFOFFSET3D(u,0,-2,0) - KRANC_GFOFFSET3D(u,0,2,0))*p1o12dy", "p1o12dy", NULL, 4,0,-2,0,0,-1,0,0,1,0,0,2,0);
 set_differencing_op("PDstandardfdOrder43", "Times:4;Plus:3;", "(-8*KRANC_GFOFFSET3D(u,0,0,-1) + 8*KRANC_GFOFFSET3D(u,0,0,1) + KRANC_GFOFFSET3D(u,0,0,-2) - KRANC_GFOFFSET3D(u,0,0,2))*p1o12dz", "p1o12dz", NULL, 4,0,0,-2,0,0,-1,0,0,1,0,0,2);
 set_differencing_op("PDstandardfdOrder211", "Times:2;Plus:2;", "(-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0))*p1odx2", "p1odx2", NULL, 3,-1,0,0,0,0,0,1,0,0);
 set_differencing_op("PDstandardfdOrder222", "Times:2;Plus:2;", "(-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0))*p1ody2", "p1ody2", NULL, 3,0,-1,0,0,0,0,0,1,0);
 set_differencing_op("PDstandardfdOrder233", "Times:2;Plus:2;", "(-2*KRANC_GFOFFSET3D(u,0,0,0) + KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1))*p1odz2", "p1odz2", NULL, 3,0,0,-1,0,0,0,0,0,1);
 set_differencing_op("PDstandardfdOrder411", "Times:3;Plus:4;", "(30*KRANC_GFOFFSET3D(u,0,0,0) - 16*(KRANC_GFOFFSET3D(u,-1,0,0) + KRANC_GFOFFSET3D(u,1,0,0)) + KRANC_GFOFFSET3D(u,-2,0,0) + KRANC_GFOFFSET3D(u,2,0,0))*pm1o12dx2", "pm1o12dx2", NULL, 5,-2,0,0,-1,0,0,0,0,0,1,0,0,2,0,0);
 set_differencing_op("PDstandardfdOrder422", "Times:3;Plus:4;", "(30*KRANC_GFOFFSET3D(u,0,0,0) - 16*(KRANC_GFOFFSET3D(u,0,-1,0) + KRANC_GFOFFSET3D(u,0,1,0)) + KRANC_GFOFFSET3D(u,0,-2,0) + KRANC_GFOFFSET3D(u,0,2,0))*pm1o12dy2", "pm1o12dy2", NULL, 5,0,-2,0,0,-1,0,0,0,0,0,1,0,0,2,0);
 set_differencing_op("PDstandardfdOrder433", "Times:3;Plus:4;", "(30*KRANC_GFOFFSET3D(u,0,0,0) - 16*(KRANC_GFOFFSET3D(u,0,0,-1) + KRANC_GFOFFSET3D(u,0,0,1)) + KRANC_GFOFFSET3D(u,0,0,-2) + KRANC_GFOFFSET3D(u,0,0,2))*pm1o12dz2", "pm1o12dz2", NULL, 5,0,0,-2,0,0,-1,0,0,0,0,0,1,0,0,2);
 set_differencing_op("PDstandardfdOrder212", "Times:3;Plus:3;", "(KRANC_GFOFFSET3D(u,-1,-1,0) - KRANC_GFOFFSET3D(u,-1,1,0) - KRANC_GFOFFSET3D(u,1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy", "p1o4dxdy", NULL, 4,-1,-1,0,-1,1,0,1,-1,0,1,1,0);
 set_differencing_op("PDstandardfdOrder213", "Times:3;Plus:3;", "(KRANC_GFOFFSET3D(u,-1,0,-1) - KRANC_GFOFFSET3D(u,-1,0,1) - KRANC_GFOFFSET3D(u,1,0,-1) + KRANC_GFOFFSET3D(u,1,0,1))*p1o4dxdz", "p1o4dxdz", NULL, 4,-1,0,-1,-1,0,1,1,0,-1,1,0,1);
 set_differencing_op("PDstandardfdOrder221", "Times:3;Plus:3;", "(KRANC_GFOFFSET3D(u,-1,-1,0) - KRANC_GFOFFSET3D(u,-1,1,0) - KRANC_GFOFFSET3D(u,1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0))*p1o4dxdy", "p1o4dxdy", NULL, 4,-1,-1,0,-1,1,0,1,-1,0,1,1,0);
 set_differencing_op("PDstandardfdOrder223", "Times:3;Plus:3;", "(KRANC_GFOFFSET3D(u,0,-1,-1) - KRANC_GFOFFSET3D(u,0,-1,1) - KRANC_GFOFFSET3D(u,0,1,-1) + KRANC_GFOFFSET3D(u,0,1,1))*p1o4dydz", "p1o4dydz", NULL, 4,0,-1,-1,0,-1,1,0,1,-1,0,1,1);
 set_differencing_op("PDstandardfdOrder231", "Times:3;Plus:3;", "(KRANC_GFOFFSET3D(u,-1,0,-1) - KRANC_GFOFFSET3D(u,-1,0,1) - KRANC_GFOFFSET3D(u,1,0,-1) + KRANC_GFOFFSET3D(u,1,0,1))*p1o4dxdz", "p1o4dxdz", NULL, 4,-1,0,-1,-1,0,1,1,0,-1,1,0,1);
 set_differencing_op("PDstandardfdOrder232", "Times:3;Plus:3;", "(KRANC_GFOFFSET3D(u,0,-1,-1) - KRANC_GFOFFSET3D(u,0,-1,1) - KRANC_GFOFFSET3D(u,0,1,-1) + KRANC_GFOFFSET3D(u,0,1,1))*p1o4dydz", "p1o4dydz", NULL, 4,0,-1,-1,0,-1,1,0,1,-1,0,1,1);
 set_differencing_op("PDstandardfdOrder412", "Times:7;Plus:15;", "(-64*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - KRANC_GFOFFSET3D(u,-2,2,0) - KRANC_GFOFFSET3D(u,2,-2,0) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy", "p1o144dxdy", NULL, 16,-2,-2,0,-2,-1,0,-2,1,0,-2,2,0,-1,-2,0,-1,-1,0,-1,1,0,-1,2,0,1,-2,0,1,-1,0,1,1,0,1,2,0,2,-2,0,2,-1,0,2,1,0,2,2,0);
 set_differencing_op("PDstandardfdOrder413", "Times:7;Plus:15;", "(-64*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + 64*(KRANC_GFOFFSET3D(u,-1,0,-1) + KRANC_GFOFFSET3D(u,1,0,1)) + 8*(KRANC_GFOFFSET3D(u,-1,0,2) + KRANC_GFOFFSET3D(u,1,0,-2) + KRANC_GFOFFSET3D(u,-2,0,1) + KRANC_GFOFFSET3D(u,2,0,-1)) - 8*(KRANC_GFOFFSET3D(u,-1,0,-2) + KRANC_GFOFFSET3D(u,1,0,2) + KRANC_GFOFFSET3D(u,-2,0,-1) + KRANC_GFOFFSET3D(u,2,0,1)) + KRANC_GFOFFSET3D(u,-2,0,-2) - KRANC_GFOFFSET3D(u,-2,0,2) - KRANC_GFOFFSET3D(u,2,0,-2) + KRANC_GFOFFSET3D(u,2,0,2))*p1o144dxdz", "p1o144dxdz", NULL, 16,-2,0,-2,-2,0,-1,-2,0,1,-2,0,2,-1,0,-2,-1,0,-1,-1,0,1,-1,0,2,1,0,-2,1,0,-1,1,0,1,1,0,2,2,0,-2,2,0,-1,2,0,1,2,0,2);
 set_differencing_op("PDstandardfdOrder421", "Times:7;Plus:15;", "(-64*(KRANC_GFOFFSET3D(u,-1,1,0) + KRANC_GFOFFSET3D(u,1,-1,0)) + 64*(KRANC_GFOFFSET3D(u,-1,-1,0) + KRANC_GFOFFSET3D(u,1,1,0)) + 8*(KRANC_GFOFFSET3D(u,-1,2,0) + KRANC_GFOFFSET3D(u,1,-2,0) + KRANC_GFOFFSET3D(u,-2,1,0) + KRANC_GFOFFSET3D(u,2,-1,0)) - 8*(KRANC_GFOFFSET3D(u,-1,-2,0) + KRANC_GFOFFSET3D(u,1,2,0) + KRANC_GFOFFSET3D(u,-2,-1,0) + KRANC_GFOFFSET3D(u,2,1,0)) + KRANC_GFOFFSET3D(u,-2,-2,0) - KRANC_GFOFFSET3D(u,-2,2,0) - KRANC_GFOFFSET3D(u,2,-2,0) + KRANC_GFOFFSET3D(u,2,2,0))*p1o144dxdy", "p1o144dxdy", NULL, 16,-2,-2,0,-2,-1,0,-2,1,0,-2,2,0,-1,-2,0,-1,-1,0,-1,1,0,-1,2,0,1,-2,0,1,-1,0,1,1,0,1,2,0,2,-2,0,2,-1,0,2,1,0,2,2,0);
 set_differencing_op("PDstandardfdOrder423", "Times:7;Plus:15;", "(-64*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + 64*(KRANC_GFOFFSET3D(u,0,-1,-1) + KRANC_GFOFFSET3D(u,0,1,1)) + 8*(KRANC_GFOFFSET3D(u,0,-1,2) + KRANC_GFOFFSET3D(u,0,1,-2) + KRANC_GFOFFSET3D(u,0,-2,1) + KRANC_GFOFFSET3D(u,0,2,-1)) - 8*(KRANC_GFOFFSET3D(u,0,-1,-2) + KRANC_GFOFFSET3D(u,0,1,2) + KRANC_GFOFFSET3D(u,0,-2,-1) + KRANC_GFOFFSET3D(u,0,2,1)) + KRANC_GFOFFSET3D(u,0,-2,-2) - KRANC_GFOFFSET3D(u,0,-2,2) - KRANC_GFOFFSET3D(u,0,2,-2) + KRANC_GFOFFSET3D(u,0,2,2))*p1o144dydz", "p1o144dydz", NULL, 16,0,-2,-2,0,-2,-1,0,-2,1,0,-2,2,0,-1,-2,0,-1,-1,0,-1,1,0,-1,2,0,1,-2,0,1,-1,0,1,1,0,1,2,0,2,-2,0,2,-1,0,2,1,0,2,2);
 set_differencing_op("PDstandardfdOrder431", "Times:7;Plus:15;", "(-64*(KRANC_GFOFFSET3D(u,-1,0,1) + KRANC_GFOFFSET3D(u,1,0,-1)) + 64*(KRANC_GFOFFSET3D(u,-1,0,-1) + KRANC_GFOFFSET3D(u,1,0,1)) + 8*(KRANC_GFOFFSET3D(u,-1,0,2) + KRANC_GFOFFSET3D(u,1,0,-2) + KRANC_GFOFFSET3D(u,-2,0,1) + KRANC_GFOFFSET3D(u,2,0,-1)) - 8*(KRANC_GFOFFSET3D(u,-1,0,-2) + KRANC_GFOFFSET3D(u,1,0,2) + KRANC_GFOFFSET3D(u,-2,0,-1) + KRANC_GFOFFSET3D(u,2,0,1)) + KRANC_GFOFFSET3D(u,-2,0,-2) - KRANC_GFOFFSET3D(u,-2,0,2) - KRANC_GFOFFSET3D(u,2,0,-2) + KRANC_GFOFFSET3D(u,2,0,2))*p1o144dxdz", "p1o144dxdz", NULL, 16,-2,0,-2,-2,0,-1,-2,0,1,-2,0,2,-1,0,-2,-1,0,-1,-1,0,1,-1,0,2,1,0,-2,1,0,-1,1,0,1,1,0,2,2,0,-2,2,0,-1,2,0,1,2,0,2);
 set_differencing_op("PDstandardfdOrder432", "Times:7;Plus:15;", "(-64*(KRANC_GFOFFSET3D(u,0,-1,1) + KRANC_GFOFFSET3D(u,0,1,-1)) + 64*(KRANC_GFOFFSET3D(u,0,-1,-1) + KRANC_GFOFFSET3D(u,0,1,1)) + 8*(KRANC_GFOFFSET3D(u,0,-1,2) + KRANC_GFOFFSET3D(u,0,1,-2) + KRANC_GFOFFSET3D(u,0,-2,1) + KRANC_GFOFFSET3D(u,0,2,-1)) - 8*(KRANC_GFOFFSET3D(u,0,-1,-2) + KRANC_GFOFFSET3D(u,0,1,2) + KRANC_GFOFFSET3D(u,0,-2,-1) + KRANC_GFOFFSET3D(u,0,2,1)) + KRANC_GFOFFSET3D(u,0,-2,-2) - KRANC_GFOFFSET3D(u,0,-2,2) - KRANC_GFOFFSET3D(u,0,2,-2) + KRANC_GFOFFSET3D(u,0,2,2))*p1o144dydz", "p1o144dydz", NULL, 16,0,-2,-2,0,-2,-1,0,-2,1,0,-2,2,0,-1,-2,0,-1,-1,0,-1,1,0,-1,2,0,1,-2,0,1,-1,0,1,1,0,1,2,0,2,-2,0,2,-1,0,2,1,0,2,2);


  CHEMORA_CG_KRANC_INIT_THORN_KERNEL("initial_sine_calc");
 set_parameter("fdOrder", "CCTK_INT");
 set_parameter("tile_size", "CCTK_INT");
 set_parameter("amp", "CCTK_REAL");
 set_parameter("kfac", "CCTK_REAL");
// Start of calculation.
// PreDefinitions:
 assign_from_expr("p1o12dx", "Times:1;Power:1;", "0.0833333333333333333333333333333*pow(dx,-1)", "dx", NULL );
 assign_from_expr("p1o12dy", "Times:1;Power:1;", "0.0833333333333333333333333333333*pow(dy,-1)", "dy", NULL );
 assign_from_expr("p1o12dz", "Times:1;Power:1;", "0.0833333333333333333333333333333*pow(dz,-1)", "dz", NULL );
 assign_from_expr("p1o144dxdy", "Times:2;Power:2;", "0.00694444444444444444444444444444*pow(dx,-1)*pow(dy,-1)", "dx", "dy", NULL );
 assign_from_expr("p1o144dxdz", "Times:2;Power:2;", "0.00694444444444444444444444444444*pow(dx,-1)*pow(dz,-1)", "dx", "dz", NULL );
 assign_from_expr("p1o144dydz", "Times:2;Power:2;", "0.00694444444444444444444444444444*pow(dy,-1)*pow(dz,-1)", "dy", "dz", NULL );
 assign_from_expr("p1o2dx", "Times:1;Power:1;", "0.5*pow(dx,-1)", "dx", NULL );
 assign_from_expr("p1o2dy", "Times:1;Power:1;", "0.5*pow(dy,-1)", "dy", NULL );
 assign_from_expr("p1o2dz", "Times:1;Power:1;", "0.5*pow(dz,-1)", "dz", NULL );
 assign_from_expr("p1o4dxdy", "Times:2;Power:2;", "0.25*pow(dx,-1)*pow(dy,-1)", "dx", "dy", NULL );
 assign_from_expr("p1o4dxdz", "Times:2;Power:2;", "0.25*pow(dx,-1)*pow(dz,-1)", "dx", "dz", NULL );
 assign_from_expr("p1o4dydz", "Times:2;Power:2;", "0.25*pow(dy,-1)*pow(dz,-1)", "dy", "dz", NULL );
 assign_from_expr("p1odx2", "Power:1;", "pow(dx,-2)", "dx", NULL );
 assign_from_expr("p1ody2", "Power:1;", "pow(dy,-2)", "dy", NULL );
 assign_from_expr("p1odz2", "Power:1;", "pow(dz,-2)", "dz", NULL );
 assign_from_expr("pm1o12dx2", "Times:1;Power:1;", "-0.0833333333333333333333333333333*pow(dx,-2)", "dx", NULL );
 assign_from_expr("pm1o12dy2", "Times:1;Power:1;", "-0.0833333333333333333333333333333*pow(dy,-2)", "dy", NULL );
 assign_from_expr("pm1o12dz2", "Times:1;Power:1;", "-0.0833333333333333333333333333333*pow(dz,-2)", "dz", NULL );
// Generating code using equationLoop.
//phi = amp*sin(kfac*x);
//pi = 0;

// Replaced:
//phiL = amp*sin(xL*kfac);
//piL = 0;
// gfsInRHS:
 assign_from_gf_load("xL", "x", NULL);
// About to show dfs:?:
// gfds: 
 assign_from_expr("phiL", "Times:2;Trig:1;", "amp*sin(xL*kfac)", "xL", "amp", "kfac", NULL );
 assign_from_expr("piL", "", "0", NULL );
  gf_store("phi", "phiL");
  gf_store("pi", "piL");


  CHEMORA_CG_KRANC_INIT_THORN_KERNEL("calc_rhs");
 set_parameter("fdOrder", "CCTK_INT");
 set_parameter("tile_size", "CCTK_INT");
 set_parameter("amp", "CCTK_REAL");
 set_parameter("kfac", "CCTK_REAL");
// Start of calculation.
// PreDefinitions:
 assign_from_expr("p1o12dx", "Times:1;Power:1;", "0.0833333333333333333333333333333*pow(dx,-1)", "dx", NULL );
 assign_from_expr("p1o12dy", "Times:1;Power:1;", "0.0833333333333333333333333333333*pow(dy,-1)", "dy", NULL );
 assign_from_expr("p1o12dz", "Times:1;Power:1;", "0.0833333333333333333333333333333*pow(dz,-1)", "dz", NULL );
 assign_from_expr("p1o144dxdy", "Times:2;Power:2;", "0.00694444444444444444444444444444*pow(dx,-1)*pow(dy,-1)", "dx", "dy", NULL );
 assign_from_expr("p1o144dxdz", "Times:2;Power:2;", "0.00694444444444444444444444444444*pow(dx,-1)*pow(dz,-1)", "dx", "dz", NULL );
 assign_from_expr("p1o144dydz", "Times:2;Power:2;", "0.00694444444444444444444444444444*pow(dy,-1)*pow(dz,-1)", "dy", "dz", NULL );
 assign_from_expr("p1o2dx", "Times:1;Power:1;", "0.5*pow(dx,-1)", "dx", NULL );
 assign_from_expr("p1o2dy", "Times:1;Power:1;", "0.5*pow(dy,-1)", "dy", NULL );
 assign_from_expr("p1o2dz", "Times:1;Power:1;", "0.5*pow(dz,-1)", "dz", NULL );
 assign_from_expr("p1o4dxdy", "Times:2;Power:2;", "0.25*pow(dx,-1)*pow(dy,-1)", "dx", "dy", NULL );
 assign_from_expr("p1o4dxdz", "Times:2;Power:2;", "0.25*pow(dx,-1)*pow(dz,-1)", "dx", "dz", NULL );
 assign_from_expr("p1o4dydz", "Times:2;Power:2;", "0.25*pow(dy,-1)*pow(dz,-1)", "dy", "dz", NULL );
 assign_from_expr("p1odx2", "Power:1;", "pow(dx,-2)", "dx", NULL );
 assign_from_expr("p1ody2", "Power:1;", "pow(dy,-2)", "dy", NULL );
 assign_from_expr("p1odz2", "Power:1;", "pow(dz,-2)", "dz", NULL );
 assign_from_expr("pm1o12dx2", "Times:1;Power:1;", "-0.0833333333333333333333333333333*pow(dx,-2)", "dx", NULL );
 assign_from_expr("pm1o12dy2", "Times:1;Power:1;", "-0.0833333333333333333333333333333*pow(dy,-2)", "dy", NULL );
 assign_from_expr("pm1o12dz2", "Times:1;Power:1;", "-0.0833333333333333333333333333333*pow(dz,-2)", "dz", NULL );
// Generating code using equationLoop.
//phirhs = pi;
//pirhs = PDstandard(phi,1,1) + PDstandard(phi,2,2) + PDstandard(phi,3,3);

// Replaced:
//phirhsL = piL;
//pirhsL = PDstandard11phi + PDstandard22phi + PDstandard33phi;
// gfsInRHS:
 assign_from_gf_load("phiL", "phi", NULL);
 assign_from_gf_load("piL", "pi", NULL);
// About to show dfs:?:
// gfds: PDstandard[phi, 1, 1], PDstandard[phi, 2, 2], PDstandard[phi, 3, 3], 
// PDstandard11phi = PDstandard11{ phi}
// PDstandard22phi = PDstandard22{ phi}
// PDstandard33phi = PDstandard33{ phi}
 assign_from_gf_load_switch("PDstandard11phi", "phi", "fdOrder", 2, "PDstandardfdOrder211", 4, "PDstandardfdOrder411", 0, NULL);
 assign_from_gf_load_switch("PDstandard22phi", "phi", "fdOrder", 2, "PDstandardfdOrder222", 4, "PDstandardfdOrder422", 0, NULL);
 assign_from_gf_load_switch("PDstandard33phi", "phi", "fdOrder", 2, "PDstandardfdOrder233", 4, "PDstandardfdOrder433", 0, NULL);
 assign_from_expr("phirhsL", "", "piL", "piL", NULL );
 assign_from_expr("pirhsLxxx1", "", "PDstandard11phi", "PDstandard11phi", NULL );
 assign_from_expr("pirhsLxxx2", "Plus:1;", "PDstandard22phi + PDstandard33phi", "PDstandard22phi", "PDstandard33phi", NULL );
 assign_from_expr("pirhsL", "Plus:1;", "pirhsLxxx1 + pirhsLxxx2", "pirhsLxxx1", "pirhsLxxx2", NULL );
  gf_store("phirhs", "phirhsL");
  gf_store("pirhs", "pirhsL");


  CHEMORA_CG_KRANC_INIT_THORN_KERNEL("calc_bound_rhs");
 set_parameter("fdOrder", "CCTK_INT");
 set_parameter("tile_size", "CCTK_INT");
 set_parameter("amp", "CCTK_REAL");
 set_parameter("kfac", "CCTK_REAL");
// Start of calculation.
// PreDefinitions:
 assign_from_expr("p1o12dx", "Times:1;Power:1;", "0.0833333333333333333333333333333*pow(dx,-1)", "dx", NULL );
 assign_from_expr("p1o12dy", "Times:1;Power:1;", "0.0833333333333333333333333333333*pow(dy,-1)", "dy", NULL );
 assign_from_expr("p1o12dz", "Times:1;Power:1;", "0.0833333333333333333333333333333*pow(dz,-1)", "dz", NULL );
 assign_from_expr("p1o144dxdy", "Times:2;Power:2;", "0.00694444444444444444444444444444*pow(dx,-1)*pow(dy,-1)", "dx", "dy", NULL );
 assign_from_expr("p1o144dxdz", "Times:2;Power:2;", "0.00694444444444444444444444444444*pow(dx,-1)*pow(dz,-1)", "dx", "dz", NULL );
 assign_from_expr("p1o144dydz", "Times:2;Power:2;", "0.00694444444444444444444444444444*pow(dy,-1)*pow(dz,-1)", "dy", "dz", NULL );
 assign_from_expr("p1o2dx", "Times:1;Power:1;", "0.5*pow(dx,-1)", "dx", NULL );
 assign_from_expr("p1o2dy", "Times:1;Power:1;", "0.5*pow(dy,-1)", "dy", NULL );
 assign_from_expr("p1o2dz", "Times:1;Power:1;", "0.5*pow(dz,-1)", "dz", NULL );
 assign_from_expr("p1o4dxdy", "Times:2;Power:2;", "0.25*pow(dx,-1)*pow(dy,-1)", "dx", "dy", NULL );
 assign_from_expr("p1o4dxdz", "Times:2;Power:2;", "0.25*pow(dx,-1)*pow(dz,-1)", "dx", "dz", NULL );
 assign_from_expr("p1o4dydz", "Times:2;Power:2;", "0.25*pow(dy,-1)*pow(dz,-1)", "dy", "dz", NULL );
 assign_from_expr("p1odx2", "Power:1;", "pow(dx,-2)", "dx", NULL );
 assign_from_expr("p1ody2", "Power:1;", "pow(dy,-2)", "dy", NULL );
 assign_from_expr("p1odz2", "Power:1;", "pow(dz,-2)", "dz", NULL );
 assign_from_expr("pm1o12dx2", "Times:1;Power:1;", "-0.0833333333333333333333333333333*pow(dx,-2)", "dx", NULL );
 assign_from_expr("pm1o12dy2", "Times:1;Power:1;", "-0.0833333333333333333333333333333*pow(dy,-2)", "dy", NULL );
 assign_from_expr("pm1o12dz2", "Times:1;Power:1;", "-0.0833333333333333333333333333333*pow(dz,-2)", "dz", NULL );
// Generating code using equationLoop.
//phirhs = pi;
//pirhs = -(phi*pow(kfac,2));

// Replaced:
//phirhsL = piL;
//pirhsL = -(phiL*pow(kfac,2));
// gfsInRHS:
 assign_from_gf_load("phiL", "phi", NULL);
 assign_from_gf_load("piL", "pi", NULL);
// About to show dfs:?:
// gfds: 
 assign_from_expr("phirhsL", "", "piL", "piL", NULL );
 assign_from_expr("pirhsL", "Times:2;Power:1;", "-(phiL*pow(kfac,2))", "phiL", "kfac", NULL );
  gf_store("phirhs", "phirhsL");
  gf_store("pirhs", "pirhsL");
  CHEMORA_CG_KRANC_INIT_THORN_END;
}
