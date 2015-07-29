#include "chemora_cg_kranc_startup.h"

static void
chemora_cg_thorn_startup()
{
  CHEMORA_CG_KRANC_INIT_THORN_START("SimpleWaveScriptCaKernel");


  CHEMORA_CG_KRANC_INIT_THORN_KERNEL("initial_sine_calc");
 set_parameter("fdOrder", "CCTK_INT");
 set_parameter("tile_size", "CCTK_INT");
 set_parameter("amp", "CCTK_REAL");
 set_parameter("kfac", "CCTK_REAL");
// Start of calculation.
// PreDefinitions:
 assign_from_func("p1o12dxxpowx1", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o12dx", "Times", "0.0833333333333333333333333333333*p1o12dxxpowx1", "=0.0833333333333333333333333333333", "p1o12dxxpowx1", NULL );
 assign_from_func("p1o12dyxpowx2", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o12dy", "Times", "0.0833333333333333333333333333333*p1o12dyxpowx2", "=0.0833333333333333333333333333333", "p1o12dyxpowx2", NULL );
 assign_from_func("p1o12dzxpowx3", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o12dz", "Times", "0.0833333333333333333333333333333*p1o12dzxpowx3", "=0.0833333333333333333333333333333", "p1o12dzxpowx3", NULL );
 assign_from_func("p1o144dxdyxpowx4", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o144dxdyxpowx16", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o144dxdyyTimesy22", "Times", "0.00694444444444444444444444444444*p1o144dxdyxpowx16", "=0.00694444444444444444444444444444", "p1o144dxdyxpowx16", NULL );
 assign_from_func("p1o144dxdy", "Times", "p1o144dxdyxpowx4*p1o144dxdyyTimesy22", "p1o144dxdyxpowx4", "p1o144dxdyyTimesy22", NULL );
 assign_from_func("p1o144dxdzxpowx5", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o144dxdzxpowx17", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o144dxdzyTimesy23", "Times", "0.00694444444444444444444444444444*p1o144dxdzxpowx17", "=0.00694444444444444444444444444444", "p1o144dxdzxpowx17", NULL );
 assign_from_func("p1o144dxdz", "Times", "p1o144dxdzxpowx5*p1o144dxdzyTimesy23", "p1o144dxdzxpowx5", "p1o144dxdzyTimesy23", NULL );
 assign_from_func("p1o144dydzxpowx6", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o144dydzxpowx18", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o144dydzyTimesy24", "Times", "0.00694444444444444444444444444444*p1o144dydzxpowx18", "=0.00694444444444444444444444444444", "p1o144dydzxpowx18", NULL );
 assign_from_func("p1o144dydz", "Times", "p1o144dydzxpowx6*p1o144dydzyTimesy24", "p1o144dydzxpowx6", "p1o144dydzyTimesy24", NULL );
 assign_from_func("p1o2dxxpowx7", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o2dx", "Times", "0.5*p1o2dxxpowx7", "=0.5", "p1o2dxxpowx7", NULL );
 assign_from_func("p1o2dyxpowx8", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o2dy", "Times", "0.5*p1o2dyxpowx8", "=0.5", "p1o2dyxpowx8", NULL );
 assign_from_func("p1o2dzxpowx9", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o2dz", "Times", "0.5*p1o2dzxpowx9", "=0.5", "p1o2dzxpowx9", NULL );
 assign_from_func("p1o4dxdyxpowx10", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o4dxdyxpowx19", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o4dxdyyTimesy25", "Times", "0.25*p1o4dxdyxpowx10", "=0.25", "p1o4dxdyxpowx10", NULL );
 assign_from_func("p1o4dxdy", "Times", "p1o4dxdyxpowx19*p1o4dxdyyTimesy25", "p1o4dxdyxpowx19", "p1o4dxdyyTimesy25", NULL );
 assign_from_func("p1o4dxdzxpowx11", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o4dxdzxpowx20", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o4dxdzyTimesy26", "Times", "0.25*p1o4dxdzxpowx11", "=0.25", "p1o4dxdzxpowx11", NULL );
 assign_from_func("p1o4dxdz", "Times", "p1o4dxdzxpowx20*p1o4dxdzyTimesy26", "p1o4dxdzxpowx20", "p1o4dxdzyTimesy26", NULL );
 assign_from_func("p1o4dydzxpowx12", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o4dydzxpowx21", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o4dydzyTimesy27", "Times", "0.25*p1o4dydzxpowx12", "=0.25", "p1o4dydzxpowx12", NULL );
 assign_from_func("p1o4dydz", "Times", "p1o4dydzxpowx21*p1o4dydzyTimesy27", "p1o4dydzxpowx21", "p1o4dydzyTimesy27", NULL );
 assign_from_func("p1odx2", "pow", "pow(dx,-2)", "dx", "=-2", NULL );
 assign_from_func("p1ody2", "pow", "pow(dy,-2)", "dy", "=-2", NULL );
 assign_from_func("p1odz2", "pow", "pow(dz,-2)", "dz", "=-2", NULL );
 assign_from_func("pm1o12dx2xpowx13", "pow", "pow(dx,-2)", "dx", "=-2", NULL );
 assign_from_func("pm1o12dx2", "Times", "-0.0833333333333333333333333333333*pm1o12dx2xpowx13", "=-0.0833333333333333333333333333333", "pm1o12dx2xpowx13", NULL );
 assign_from_func("pm1o12dy2xpowx14", "pow", "pow(dy,-2)", "dy", "=-2", NULL );
 assign_from_func("pm1o12dy2", "Times", "-0.0833333333333333333333333333333*pm1o12dy2xpowx14", "=-0.0833333333333333333333333333333", "pm1o12dy2xpowx14", NULL );
 assign_from_func("pm1o12dz2xpowx15", "pow", "pow(dz,-2)", "dz", "=-2", NULL );
 assign_from_func("pm1o12dz2", "Times", "-0.0833333333333333333333333333333*pm1o12dz2xpowx15", "=-0.0833333333333333333333333333333", "pm1o12dz2xpowx15", NULL );
// Generating code using equationLoop.
//phi = amp*sin(kfac*x);
//pi = 0;

// Replaced:
//phiL = amp*sin(xL*kfac);
//piL = 0;
// gfsInRHS:
 assign_from_gf_load("xL", "x");
// About to show dfs:?:
// gfds: 
 assign_from_func("phiLxSinx28xTimesx29", "Times", "xL*kfac", "xL", "kfac", NULL );
 assign_from_func("phiLxSinx28", "Sin", "sin(phiLxSinx28xTimesx29)", "phiLxSinx28xTimesx29", NULL );
 assign_from_func("phiL", "Times", "amp*phiLxSinx28", "phiLxSinx28", "amp", NULL );
 assign_from_expr("piL", "Literal", "0", "=0", NULL );
  gf_store("phi", "phiL");
  gf_store("pi", "piL");


  CHEMORA_CG_KRANC_INIT_THORN_KERNEL("calc_rhs");
 set_parameter("fdOrder", "CCTK_INT");
 set_parameter("tile_size", "CCTK_INT");
 set_parameter("amp", "CCTK_REAL");
 set_parameter("kfac", "CCTK_REAL");
// Start of calculation.
// PreDefinitions:
 assign_from_func("p1o12dxxpowx30", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o12dx", "Times", "0.0833333333333333333333333333333*p1o12dxxpowx30", "=0.0833333333333333333333333333333", "p1o12dxxpowx30", NULL );
 assign_from_func("p1o12dyxpowx31", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o12dy", "Times", "0.0833333333333333333333333333333*p1o12dyxpowx31", "=0.0833333333333333333333333333333", "p1o12dyxpowx31", NULL );
 assign_from_func("p1o12dzxpowx32", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o12dz", "Times", "0.0833333333333333333333333333333*p1o12dzxpowx32", "=0.0833333333333333333333333333333", "p1o12dzxpowx32", NULL );
 assign_from_func("p1o144dxdyxpowx33", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o144dxdyxpowx45", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o144dxdyyTimesy51", "Times", "0.00694444444444444444444444444444*p1o144dxdyxpowx33", "=0.00694444444444444444444444444444", "p1o144dxdyxpowx33", NULL );
 assign_from_func("p1o144dxdy", "Times", "p1o144dxdyxpowx45*p1o144dxdyyTimesy51", "p1o144dxdyxpowx45", "p1o144dxdyyTimesy51", NULL );
 assign_from_func("p1o144dxdzxpowx34", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o144dxdzxpowx46", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o144dxdzyTimesy52", "Times", "0.00694444444444444444444444444444*p1o144dxdzxpowx34", "=0.00694444444444444444444444444444", "p1o144dxdzxpowx34", NULL );
 assign_from_func("p1o144dxdz", "Times", "p1o144dxdzxpowx46*p1o144dxdzyTimesy52", "p1o144dxdzxpowx46", "p1o144dxdzyTimesy52", NULL );
 assign_from_func("p1o144dydzxpowx35", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o144dydzxpowx47", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o144dydzyTimesy53", "Times", "0.00694444444444444444444444444444*p1o144dydzxpowx35", "=0.00694444444444444444444444444444", "p1o144dydzxpowx35", NULL );
 assign_from_func("p1o144dydz", "Times", "p1o144dydzxpowx47*p1o144dydzyTimesy53", "p1o144dydzxpowx47", "p1o144dydzyTimesy53", NULL );
 assign_from_func("p1o2dxxpowx36", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o2dx", "Times", "0.5*p1o2dxxpowx36", "=0.5", "p1o2dxxpowx36", NULL );
 assign_from_func("p1o2dyxpowx37", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o2dy", "Times", "0.5*p1o2dyxpowx37", "=0.5", "p1o2dyxpowx37", NULL );
 assign_from_func("p1o2dzxpowx38", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o2dz", "Times", "0.5*p1o2dzxpowx38", "=0.5", "p1o2dzxpowx38", NULL );
 assign_from_func("p1o4dxdyxpowx39", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o4dxdyxpowx48", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o4dxdyyTimesy54", "Times", "0.25*p1o4dxdyxpowx39", "=0.25", "p1o4dxdyxpowx39", NULL );
 assign_from_func("p1o4dxdy", "Times", "p1o4dxdyxpowx48*p1o4dxdyyTimesy54", "p1o4dxdyxpowx48", "p1o4dxdyyTimesy54", NULL );
 assign_from_func("p1o4dxdzxpowx40", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o4dxdzxpowx49", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o4dxdzyTimesy55", "Times", "0.25*p1o4dxdzxpowx40", "=0.25", "p1o4dxdzxpowx40", NULL );
 assign_from_func("p1o4dxdz", "Times", "p1o4dxdzxpowx49*p1o4dxdzyTimesy55", "p1o4dxdzxpowx49", "p1o4dxdzyTimesy55", NULL );
 assign_from_func("p1o4dydzxpowx41", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o4dydzxpowx50", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o4dydzyTimesy56", "Times", "0.25*p1o4dydzxpowx41", "=0.25", "p1o4dydzxpowx41", NULL );
 assign_from_func("p1o4dydz", "Times", "p1o4dydzxpowx50*p1o4dydzyTimesy56", "p1o4dydzxpowx50", "p1o4dydzyTimesy56", NULL );
 assign_from_func("p1odx2", "pow", "pow(dx,-2)", "dx", "=-2", NULL );
 assign_from_func("p1ody2", "pow", "pow(dy,-2)", "dy", "=-2", NULL );
 assign_from_func("p1odz2", "pow", "pow(dz,-2)", "dz", "=-2", NULL );
 assign_from_func("pm1o12dx2xpowx42", "pow", "pow(dx,-2)", "dx", "=-2", NULL );
 assign_from_func("pm1o12dx2", "Times", "-0.0833333333333333333333333333333*pm1o12dx2xpowx42", "=-0.0833333333333333333333333333333", "pm1o12dx2xpowx42", NULL );
 assign_from_func("pm1o12dy2xpowx43", "pow", "pow(dy,-2)", "dy", "=-2", NULL );
 assign_from_func("pm1o12dy2", "Times", "-0.0833333333333333333333333333333*pm1o12dy2xpowx43", "=-0.0833333333333333333333333333333", "pm1o12dy2xpowx43", NULL );
 assign_from_func("pm1o12dz2xpowx44", "pow", "pow(dz,-2)", "dz", "=-2", NULL );
 assign_from_func("pm1o12dz2", "Times", "-0.0833333333333333333333333333333*pm1o12dz2xpowx44", "=-0.0833333333333333333333333333333", "pm1o12dz2xpowx44", NULL );
// Generating code using equationLoop.
//phirhs = pi;
//pirhs = PDstandard(phi,1,1) + PDstandard(phi,2,2) + PDstandard(phi,3,3);

// Replaced:
//phirhsL = piL;
//pirhsL = PDstandard11phi + PDstandard22phi + PDstandard33phi;
// gfsInRHS:
 assign_from_gf_load("phiL", "phi");
 assign_from_gf_load("piL", "pi");
// About to show dfs:?:
// gfds: PDstandard[phi, 1, 1], PDstandard[phi, 2, 2], PDstandard[phi, 3, 3], 
// PDstandard11phi = PDstandard11{ phi}
// PDstandard22phi = PDstandard22{ phi}
// PDstandard33phi = PDstandard33{ phi}
 assign_from_offset_load("PDstandardfdOrder211phixPlusx57xTimesx63xChemoraNOffsetx69", "phi", "0", "0", "0");
 assign_from_func("PDstandardfdOrder211phixPlusx57xTimesx63", "Times", "-2*PDstandardfdOrder211phixPlusx57xTimesx63xChemoraNOffsetx69", "=-2", "PDstandardfdOrder211phixPlusx57xTimesx63xChemoraNOffsetx69", NULL );
 assign_from_offset_load("PDstandardfdOrder211phixPlusx57xChemoraNOffsetx70", "phi", "-1", "0", "0");
 assign_from_offset_load("PDstandardfdOrder211phixPlusx57xChemoraNOffsetx81", "phi", "1", "0", "0");
 assign_from_func("PDstandardfdOrder211phixPlusx57yPlusy90", "Plus", "PDstandardfdOrder211phixPlusx57xChemoraNOffsetx70 + PDstandardfdOrder211phixPlusx57xChemoraNOffsetx81", "PDstandardfdOrder211phixPlusx57xChemoraNOffsetx70", "PDstandardfdOrder211phixPlusx57xChemoraNOffsetx81", NULL );
 assign_from_func("PDstandardfdOrder211phixPlusx57", "Plus", "PDstandardfdOrder211phixPlusx57xTimesx63 + PDstandardfdOrder211phixPlusx57yPlusy90", "PDstandardfdOrder211phixPlusx57xTimesx63", "PDstandardfdOrder211phixPlusx57yPlusy90", NULL );
 assign_from_func("PDstandardfdOrder211phi", "Times", "p1odx2*PDstandardfdOrder211phixPlusx57", "p1odx2", "PDstandardfdOrder211phixPlusx57", NULL );
 assign_from_offset_load("PDstandardfdOrder411phixPlusx58xTimesx64xChemoraNOffsetx71", "phi", "0", "0", "0");
 assign_from_func("PDstandardfdOrder411phixPlusx58xTimesx64", "Times", "30*PDstandardfdOrder411phixPlusx58xTimesx64xChemoraNOffsetx71", "=30", "PDstandardfdOrder411phixPlusx58xTimesx64xChemoraNOffsetx71", NULL );
 assign_from_offset_load("PDstandardfdOrder411phixPlusx58xTimesx72xPlusx82xChemoraNOffsetx91", "phi", "-1", "0", "0");
 assign_from_offset_load("PDstandardfdOrder411phixPlusx58xTimesx72xPlusx82xChemoraNOffsetx99", "phi", "1", "0", "0");
 assign_from_func("PDstandardfdOrder411phixPlusx58xTimesx72xPlusx82", "Plus", "PDstandardfdOrder411phixPlusx58xTimesx72xPlusx82xChemoraNOffsetx91 + PDstandardfdOrder411phixPlusx58xTimesx72xPlusx82xChemoraNOffsetx99", "PDstandardfdOrder411phixPlusx58xTimesx72xPlusx82xChemoraNOffsetx91", "PDstandardfdOrder411phixPlusx58xTimesx72xPlusx82xChemoraNOffsetx99", NULL );
 assign_from_func("PDstandardfdOrder411phixPlusx58xTimesx72", "Times", "-16*PDstandardfdOrder411phixPlusx58xTimesx72xPlusx82", "=-16", "PDstandardfdOrder411phixPlusx58xTimesx72xPlusx82", NULL );
 assign_from_offset_load("PDstandardfdOrder411phixPlusx58xChemoraNOffsetx83", "phi", "-2", "0", "0");
 assign_from_offset_load("PDstandardfdOrder411phixPlusx58xChemoraNOffsetx92", "phi", "2", "0", "0");
 assign_from_func("PDstandardfdOrder411phixPlusx58yPlusy100", "Plus", "PDstandardfdOrder411phixPlusx58xChemoraNOffsetx83 + PDstandardfdOrder411phixPlusx58xChemoraNOffsetx92", "PDstandardfdOrder411phixPlusx58xChemoraNOffsetx83", "PDstandardfdOrder411phixPlusx58xChemoraNOffsetx92", NULL );
 assign_from_func("PDstandardfdOrder411phixPlusx58yPlusy105", "Plus", "PDstandardfdOrder411phixPlusx58xTimesx64 + PDstandardfdOrder411phixPlusx58xTimesx72", "PDstandardfdOrder411phixPlusx58xTimesx64", "PDstandardfdOrder411phixPlusx58xTimesx72", NULL );
 assign_from_func("PDstandardfdOrder411phixPlusx58", "Plus", "PDstandardfdOrder411phixPlusx58yPlusy100 + PDstandardfdOrder411phixPlusx58yPlusy105", "PDstandardfdOrder411phixPlusx58yPlusy100", "PDstandardfdOrder411phixPlusx58yPlusy105", NULL );
 assign_from_func("PDstandardfdOrder411phi", "Times", "pm1o12dx2*PDstandardfdOrder411phixPlusx58", "pm1o12dx2", "PDstandardfdOrder411phixPlusx58", NULL );
 assign_from_offset_load("PDstandardfdOrder222phixPlusx59xTimesx65xChemoraNOffsetx73", "phi", "0", "0", "0");
 assign_from_func("PDstandardfdOrder222phixPlusx59xTimesx65", "Times", "-2*PDstandardfdOrder222phixPlusx59xTimesx65xChemoraNOffsetx73", "=-2", "PDstandardfdOrder222phixPlusx59xTimesx65xChemoraNOffsetx73", NULL );
 assign_from_offset_load("PDstandardfdOrder222phixPlusx59xChemoraNOffsetx74", "phi", "0", "-1", "0");
 assign_from_offset_load("PDstandardfdOrder222phixPlusx59xChemoraNOffsetx84", "phi", "0", "1", "0");
 assign_from_func("PDstandardfdOrder222phixPlusx59yPlusy93", "Plus", "PDstandardfdOrder222phixPlusx59xChemoraNOffsetx74 + PDstandardfdOrder222phixPlusx59xChemoraNOffsetx84", "PDstandardfdOrder222phixPlusx59xChemoraNOffsetx74", "PDstandardfdOrder222phixPlusx59xChemoraNOffsetx84", NULL );
 assign_from_func("PDstandardfdOrder222phixPlusx59", "Plus", "PDstandardfdOrder222phixPlusx59xTimesx65 + PDstandardfdOrder222phixPlusx59yPlusy93", "PDstandardfdOrder222phixPlusx59xTimesx65", "PDstandardfdOrder222phixPlusx59yPlusy93", NULL );
 assign_from_func("PDstandardfdOrder222phi", "Times", "p1ody2*PDstandardfdOrder222phixPlusx59", "p1ody2", "PDstandardfdOrder222phixPlusx59", NULL );
 assign_from_offset_load("PDstandardfdOrder422phixPlusx60xTimesx66xChemoraNOffsetx75", "phi", "0", "0", "0");
 assign_from_func("PDstandardfdOrder422phixPlusx60xTimesx66", "Times", "30*PDstandardfdOrder422phixPlusx60xTimesx66xChemoraNOffsetx75", "=30", "PDstandardfdOrder422phixPlusx60xTimesx66xChemoraNOffsetx75", NULL );
 assign_from_offset_load("PDstandardfdOrder422phixPlusx60xTimesx76xPlusx85xChemoraNOffsetx94", "phi", "0", "-1", "0");
 assign_from_offset_load("PDstandardfdOrder422phixPlusx60xTimesx76xPlusx85xChemoraNOffsetx101", "phi", "0", "1", "0");
 assign_from_func("PDstandardfdOrder422phixPlusx60xTimesx76xPlusx85", "Plus", "PDstandardfdOrder422phixPlusx60xTimesx76xPlusx85xChemoraNOffsetx101 + PDstandardfdOrder422phixPlusx60xTimesx76xPlusx85xChemoraNOffsetx94", "PDstandardfdOrder422phixPlusx60xTimesx76xPlusx85xChemoraNOffsetx101", "PDstandardfdOrder422phixPlusx60xTimesx76xPlusx85xChemoraNOffsetx94", NULL );
 assign_from_func("PDstandardfdOrder422phixPlusx60xTimesx76", "Times", "-16*PDstandardfdOrder422phixPlusx60xTimesx76xPlusx85", "=-16", "PDstandardfdOrder422phixPlusx60xTimesx76xPlusx85", NULL );
 assign_from_offset_load("PDstandardfdOrder422phixPlusx60xChemoraNOffsetx86", "phi", "0", "-2", "0");
 assign_from_offset_load("PDstandardfdOrder422phixPlusx60xChemoraNOffsetx95", "phi", "0", "2", "0");
 assign_from_func("PDstandardfdOrder422phixPlusx60yPlusy102", "Plus", "PDstandardfdOrder422phixPlusx60xChemoraNOffsetx86 + PDstandardfdOrder422phixPlusx60xChemoraNOffsetx95", "PDstandardfdOrder422phixPlusx60xChemoraNOffsetx86", "PDstandardfdOrder422phixPlusx60xChemoraNOffsetx95", NULL );
 assign_from_func("PDstandardfdOrder422phixPlusx60yPlusy106", "Plus", "PDstandardfdOrder422phixPlusx60xTimesx66 + PDstandardfdOrder422phixPlusx60xTimesx76", "PDstandardfdOrder422phixPlusx60xTimesx66", "PDstandardfdOrder422phixPlusx60xTimesx76", NULL );
 assign_from_func("PDstandardfdOrder422phixPlusx60", "Plus", "PDstandardfdOrder422phixPlusx60yPlusy102 + PDstandardfdOrder422phixPlusx60yPlusy106", "PDstandardfdOrder422phixPlusx60yPlusy102", "PDstandardfdOrder422phixPlusx60yPlusy106", NULL );
 assign_from_func("PDstandardfdOrder422phi", "Times", "pm1o12dy2*PDstandardfdOrder422phixPlusx60", "pm1o12dy2", "PDstandardfdOrder422phixPlusx60", NULL );
 assign_from_offset_load("PDstandardfdOrder233phixPlusx61xTimesx67xChemoraNOffsetx77", "phi", "0", "0", "0");
 assign_from_func("PDstandardfdOrder233phixPlusx61xTimesx67", "Times", "-2*PDstandardfdOrder233phixPlusx61xTimesx67xChemoraNOffsetx77", "=-2", "PDstandardfdOrder233phixPlusx61xTimesx67xChemoraNOffsetx77", NULL );
 assign_from_offset_load("PDstandardfdOrder233phixPlusx61xChemoraNOffsetx78", "phi", "0", "0", "-1");
 assign_from_offset_load("PDstandardfdOrder233phixPlusx61xChemoraNOffsetx87", "phi", "0", "0", "1");
 assign_from_func("PDstandardfdOrder233phixPlusx61yPlusy96", "Plus", "PDstandardfdOrder233phixPlusx61xChemoraNOffsetx78 + PDstandardfdOrder233phixPlusx61xChemoraNOffsetx87", "PDstandardfdOrder233phixPlusx61xChemoraNOffsetx78", "PDstandardfdOrder233phixPlusx61xChemoraNOffsetx87", NULL );
 assign_from_func("PDstandardfdOrder233phixPlusx61", "Plus", "PDstandardfdOrder233phixPlusx61xTimesx67 + PDstandardfdOrder233phixPlusx61yPlusy96", "PDstandardfdOrder233phixPlusx61xTimesx67", "PDstandardfdOrder233phixPlusx61yPlusy96", NULL );
 assign_from_func("PDstandardfdOrder233phi", "Times", "p1odz2*PDstandardfdOrder233phixPlusx61", "p1odz2", "PDstandardfdOrder233phixPlusx61", NULL );
 assign_from_offset_load("PDstandardfdOrder433phixPlusx62xTimesx68xChemoraNOffsetx79", "phi", "0", "0", "0");
 assign_from_func("PDstandardfdOrder433phixPlusx62xTimesx68", "Times", "30*PDstandardfdOrder433phixPlusx62xTimesx68xChemoraNOffsetx79", "=30", "PDstandardfdOrder433phixPlusx62xTimesx68xChemoraNOffsetx79", NULL );
 assign_from_offset_load("PDstandardfdOrder433phixPlusx62xTimesx80xPlusx88xChemoraNOffsetx97", "phi", "0", "0", "-1");
 assign_from_offset_load("PDstandardfdOrder433phixPlusx62xTimesx80xPlusx88xChemoraNOffsetx103", "phi", "0", "0", "1");
 assign_from_func("PDstandardfdOrder433phixPlusx62xTimesx80xPlusx88", "Plus", "PDstandardfdOrder433phixPlusx62xTimesx80xPlusx88xChemoraNOffsetx103 + PDstandardfdOrder433phixPlusx62xTimesx80xPlusx88xChemoraNOffsetx97", "PDstandardfdOrder433phixPlusx62xTimesx80xPlusx88xChemoraNOffsetx103", "PDstandardfdOrder433phixPlusx62xTimesx80xPlusx88xChemoraNOffsetx97", NULL );
 assign_from_func("PDstandardfdOrder433phixPlusx62xTimesx80", "Times", "-16*PDstandardfdOrder433phixPlusx62xTimesx80xPlusx88", "=-16", "PDstandardfdOrder433phixPlusx62xTimesx80xPlusx88", NULL );
 assign_from_offset_load("PDstandardfdOrder433phixPlusx62xChemoraNOffsetx89", "phi", "0", "0", "-2");
 assign_from_offset_load("PDstandardfdOrder433phixPlusx62xChemoraNOffsetx98", "phi", "0", "0", "2");
 assign_from_func("PDstandardfdOrder433phixPlusx62yPlusy104", "Plus", "PDstandardfdOrder433phixPlusx62xChemoraNOffsetx89 + PDstandardfdOrder433phixPlusx62xChemoraNOffsetx98", "PDstandardfdOrder433phixPlusx62xChemoraNOffsetx89", "PDstandardfdOrder433phixPlusx62xChemoraNOffsetx98", NULL );
 assign_from_func("PDstandardfdOrder433phixPlusx62yPlusy107", "Plus", "PDstandardfdOrder433phixPlusx62xTimesx68 + PDstandardfdOrder433phixPlusx62xTimesx80", "PDstandardfdOrder433phixPlusx62xTimesx68", "PDstandardfdOrder433phixPlusx62xTimesx80", NULL );
 assign_from_func("PDstandardfdOrder433phixPlusx62", "Plus", "PDstandardfdOrder433phixPlusx62yPlusy104 + PDstandardfdOrder433phixPlusx62yPlusy107", "PDstandardfdOrder433phixPlusx62yPlusy104", "PDstandardfdOrder433phixPlusx62yPlusy107", NULL );
 assign_from_func("PDstandardfdOrder433phi", "Times", "pm1o12dz2*PDstandardfdOrder433phixPlusx62", "pm1o12dz2", "PDstandardfdOrder433phixPlusx62", NULL );
 assign_from_gf_load_switch("PDstandard11phi", "phi", "fdOrder", 2, "PDstandardfdOrder211phi", 4, "PDstandardfdOrder411phi", 0, NULL);
 assign_from_gf_load_switch("PDstandard22phi", "phi", "fdOrder", 2, "PDstandardfdOrder222phi", 4, "PDstandardfdOrder422phi", 0, NULL);
 assign_from_gf_load_switch("PDstandard33phi", "phi", "fdOrder", 2, "PDstandardfdOrder233phi", 4, "PDstandardfdOrder433phi", 0, NULL);
 assign_from_expr("phirhsL", "Identity", "piL", "piL", NULL );
 assign_from_func("pirhsLyPlusy108", "Plus", "PDstandard11phi + PDstandard22phi", "PDstandard11phi", "PDstandard22phi", NULL );
 assign_from_func("pirhsL", "Plus", "PDstandard33phi + pirhsLyPlusy108", "PDstandard33phi", "pirhsLyPlusy108", NULL );
  gf_store("phirhs", "phirhsL");
  gf_store("pirhs", "pirhsL");


  CHEMORA_CG_KRANC_INIT_THORN_KERNEL("calc_bound_rhs");
 set_parameter("fdOrder", "CCTK_INT");
 set_parameter("tile_size", "CCTK_INT");
 set_parameter("amp", "CCTK_REAL");
 set_parameter("kfac", "CCTK_REAL");
// Start of calculation.
// PreDefinitions:
 assign_from_func("p1o12dxxpowx109", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o12dx", "Times", "0.0833333333333333333333333333333*p1o12dxxpowx109", "=0.0833333333333333333333333333333", "p1o12dxxpowx109", NULL );
 assign_from_func("p1o12dyxpowx110", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o12dy", "Times", "0.0833333333333333333333333333333*p1o12dyxpowx110", "=0.0833333333333333333333333333333", "p1o12dyxpowx110", NULL );
 assign_from_func("p1o12dzxpowx111", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o12dz", "Times", "0.0833333333333333333333333333333*p1o12dzxpowx111", "=0.0833333333333333333333333333333", "p1o12dzxpowx111", NULL );
 assign_from_func("p1o144dxdyxpowx112", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o144dxdyxpowx124", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o144dxdyyTimesy130", "Times", "0.00694444444444444444444444444444*p1o144dxdyxpowx112", "=0.00694444444444444444444444444444", "p1o144dxdyxpowx112", NULL );
 assign_from_func("p1o144dxdy", "Times", "p1o144dxdyxpowx124*p1o144dxdyyTimesy130", "p1o144dxdyxpowx124", "p1o144dxdyyTimesy130", NULL );
 assign_from_func("p1o144dxdzxpowx113", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o144dxdzxpowx125", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o144dxdzyTimesy131", "Times", "0.00694444444444444444444444444444*p1o144dxdzxpowx113", "=0.00694444444444444444444444444444", "p1o144dxdzxpowx113", NULL );
 assign_from_func("p1o144dxdz", "Times", "p1o144dxdzxpowx125*p1o144dxdzyTimesy131", "p1o144dxdzxpowx125", "p1o144dxdzyTimesy131", NULL );
 assign_from_func("p1o144dydzxpowx114", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o144dydzxpowx126", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o144dydzyTimesy132", "Times", "0.00694444444444444444444444444444*p1o144dydzxpowx114", "=0.00694444444444444444444444444444", "p1o144dydzxpowx114", NULL );
 assign_from_func("p1o144dydz", "Times", "p1o144dydzxpowx126*p1o144dydzyTimesy132", "p1o144dydzxpowx126", "p1o144dydzyTimesy132", NULL );
 assign_from_func("p1o2dxxpowx115", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o2dx", "Times", "0.5*p1o2dxxpowx115", "=0.5", "p1o2dxxpowx115", NULL );
 assign_from_func("p1o2dyxpowx116", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o2dy", "Times", "0.5*p1o2dyxpowx116", "=0.5", "p1o2dyxpowx116", NULL );
 assign_from_func("p1o2dzxpowx117", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o2dz", "Times", "0.5*p1o2dzxpowx117", "=0.5", "p1o2dzxpowx117", NULL );
 assign_from_func("p1o4dxdyxpowx118", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o4dxdyxpowx127", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o4dxdyyTimesy133", "Times", "0.25*p1o4dxdyxpowx118", "=0.25", "p1o4dxdyxpowx118", NULL );
 assign_from_func("p1o4dxdy", "Times", "p1o4dxdyxpowx127*p1o4dxdyyTimesy133", "p1o4dxdyxpowx127", "p1o4dxdyyTimesy133", NULL );
 assign_from_func("p1o4dxdzxpowx119", "pow", "pow(dx,-1)", "dx", "=-1", NULL );
 assign_from_func("p1o4dxdzxpowx128", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o4dxdzyTimesy134", "Times", "0.25*p1o4dxdzxpowx119", "=0.25", "p1o4dxdzxpowx119", NULL );
 assign_from_func("p1o4dxdz", "Times", "p1o4dxdzxpowx128*p1o4dxdzyTimesy134", "p1o4dxdzxpowx128", "p1o4dxdzyTimesy134", NULL );
 assign_from_func("p1o4dydzxpowx120", "pow", "pow(dy,-1)", "dy", "=-1", NULL );
 assign_from_func("p1o4dydzxpowx129", "pow", "pow(dz,-1)", "dz", "=-1", NULL );
 assign_from_func("p1o4dydzyTimesy135", "Times", "0.25*p1o4dydzxpowx120", "=0.25", "p1o4dydzxpowx120", NULL );
 assign_from_func("p1o4dydz", "Times", "p1o4dydzxpowx129*p1o4dydzyTimesy135", "p1o4dydzxpowx129", "p1o4dydzyTimesy135", NULL );
 assign_from_func("p1odx2", "pow", "pow(dx,-2)", "dx", "=-2", NULL );
 assign_from_func("p1ody2", "pow", "pow(dy,-2)", "dy", "=-2", NULL );
 assign_from_func("p1odz2", "pow", "pow(dz,-2)", "dz", "=-2", NULL );
 assign_from_func("pm1o12dx2xpowx121", "pow", "pow(dx,-2)", "dx", "=-2", NULL );
 assign_from_func("pm1o12dx2", "Times", "-0.0833333333333333333333333333333*pm1o12dx2xpowx121", "=-0.0833333333333333333333333333333", "pm1o12dx2xpowx121", NULL );
 assign_from_func("pm1o12dy2xpowx122", "pow", "pow(dy,-2)", "dy", "=-2", NULL );
 assign_from_func("pm1o12dy2", "Times", "-0.0833333333333333333333333333333*pm1o12dy2xpowx122", "=-0.0833333333333333333333333333333", "pm1o12dy2xpowx122", NULL );
 assign_from_func("pm1o12dz2xpowx123", "pow", "pow(dz,-2)", "dz", "=-2", NULL );
 assign_from_func("pm1o12dz2", "Times", "-0.0833333333333333333333333333333*pm1o12dz2xpowx123", "=-0.0833333333333333333333333333333", "pm1o12dz2xpowx123", NULL );
// Generating code using equationLoop.
//phirhs = pi;
//pirhs = -(phi*pow(kfac,2));

// Replaced:
//phirhsL = piL;
//pirhsL = -(phiL*pow(kfac,2));
// gfsInRHS:
 assign_from_gf_load("phiL", "phi");
 assign_from_gf_load("piL", "pi");
// About to show dfs:?:
// gfds: 
 assign_from_expr("phirhsL", "Identity", "piL", "piL", NULL );
 assign_from_func("pirhsLxPowerx136", "Power", "pow(kfac,2)", "kfac", "=2", NULL );
 assign_from_func("pirhsLyTimesy137", "Times", "-phiL", "=-1", "phiL", NULL );
 assign_from_func("pirhsL", "Times", "pirhsLxPowerx136*pirhsLyTimesy137", "pirhsLxPowerx136", "pirhsLyTimesy137", NULL );
  gf_store("phirhs", "phirhsL");
  gf_store("pirhs", "pirhsL");
  CHEMORA_CG_KRANC_INIT_THORN_END;
}
