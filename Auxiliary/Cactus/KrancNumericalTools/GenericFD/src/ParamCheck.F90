/*@@                                         
   @file      GenericFD/src/ParamCheck.F90
   @date      October 20 2004
   @author    S. Husa                           
   @desc                                        
   Check consistency of parameters associated with stencil widths

   $Header$                                  

   @enddesc                                     
 @@*/                                           
                                                
/*  Copyright 2004 Sascha Husa, Ian Hinder, Christiane Lechner

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

#define KRANC_FORTRAN
                                           
#include "cctk.h"         
#include "cctk_Arguments.h"
#include "cctk_Parameters.h"
#include "GenericFD.h"

/*@@                                         
   @routine   GenericFD_ParamCheck 
   @date      October 20 2004
   @author    S. Husa                           
   @desc                     
   Check consistency of parameters associated with stencil widths                   
   
   @enddesc                                     
   @calls                                       
   @calledby                                    
   @history                                     
   @endhistory                                  
 @@*/                                           
                                                
                                              

SUBROUTINE GenericFD_ParamCheck(CCTK_ARGUMENTS)

implicit none

DECLARE_CCTK_ARGUMENTS
DECLARE_CCTK_PARAMETERS

if (stencil_width < 0) then
  call CCTK_WARN(0, "stencil_width < 0  - set GenericFD::stencil_width > 0 in par file!")
endif

if ((stencil_width_x < 0).AND.(stencil_width < 0)) then
  call CCTK_WARN(0, "stencil_width_x < 0, set GenericFD::stencil_width_x (or stencil_width) > 0!")
endif

if ((stencil_width_y < 0).AND.(stencil_width < 0)) then
  call CCTK_WARN(0, "stencil_width_y < 0, set GenericFD::stencil_width_x (or stencil_width) > 0!")
endif

if ((stencil_width_z < 0).AND.(stencil_width < 0)) then
  call CCTK_WARN(0, "stencil_width_z < 0, set GenericFD::stencil_width_x (or stencil_width) > 0!")
endif


if (stencil_width > maxval(cctk_nghostzones)) then
 call CCTK_WARN(0, "stencil_width is larger than max(cctk_nghostzones)!")
endif

if (stencil_width_x > cctk_nghostzones(1)) then
 call CCTK_WARN(0, "stencil_width is smaller than cctk_nghostzones(1)!")
endif

if (stencil_width_y > cctk_nghostzones(2)) then
 call CCTK_WARN(0, "stencil_width is smaller than cctk_nghostzones(2)!")
endif

if (stencil_width_z > cctk_nghostzones(3)) then
 call CCTK_WARN(0, "stencil_width is smaller than cctk_nghostzones(3)!")
endif


END SUBROUTINE GenericFD_ParamCheck
