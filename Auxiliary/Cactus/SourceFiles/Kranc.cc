
/*  Copyright 2014 Ian Hinder

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
*/
                                              
#include "cctk.h"                             
#include "cctk_Arguments.h"                   
#include "cctk_Parameters.h"                  
// #include "util_Table.h"
// #include <assert.h>
// #include <stdlib.h>
// #include <math.h>
// #include <stdio.h>
            
#define KRANC_C
                                                
#include "GenericFD.h"
#include "Kranc.hh"

void @THORN_NAME@_TiledLoopOverInterior(
  cGH const * restrict const cctkGH,
  void (calc)(const cGH* restrict const cctkGH, const KrancData &kd))
{
  DECLARE_CCTK_ARGUMENTS;
  DECLARE_CCTK_PARAMETERS;

  int   is_symbnd[6], is_physbnd[6], is_ipbnd[6];

  KrancData  kd;

  kd.dir = 0;
  kd.face = 0;
  // TODO: initialise the rest, or use a constructor

  GenericFD_GetBoundaryInfo(cctkGH, cctk_ash, cctk_lsh, cctk_bbox,
                            cctk_nghostzones, 
                            kd.imin, kd.imax, is_symbnd, is_physbnd, is_ipbnd);

  int tile_size_l[3];
  int tile_n[3];

  for (int d = 0; d < 3; d++)
  {
    tile_size_l[d] = tile_size == -1 ? (kd.imax[d]-kd.imin[d]) : tile_size;
    assert((kd.imax[d]-kd.imin[d]) % tile_size_l[d] == 0);
    tile_n[d] = (kd.imax[d]-kd.imin[d])/tile_size_l[d];
  }

  for (int ti = 0; ti < tile_n[0]; ti++)
  {
    for (int tj = 0; tj < tile_n[1]; tj++)
    {
      for (int tk = 0; tk < tile_n[2]; tk++)
      {
        kd.tile_imin[0] = ti*tile_size_l[0];
        kd.tile_imax[0] = (ti+1)*tile_size_l[0];
        kd.tile_imin[1] = tj*tile_size_l[1];
        kd.tile_imax[1] = (tj+1)*tile_size_l[1];
        kd.tile_imin[2] = tk*tile_size_l[2];
        kd.tile_imax[2] = (tk+1)*tile_size_l[2];
        
        calc(cctkGH, kd);
      }
    }
  }
  
  return;
}
