/*@@                                         
   @file      GenericFD/src/Startup.c
   @date      June 16 2002
   @author    S. Husa                           
   @desc                                        
   Register Banner - straight copy of WaveToy   

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
    along with Foobar; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
                                           
#include "cctk.h"                           
#include "GenericFD.h"

int GenericFD_Startup(void);

/*@@                                         
   @routine   GenericFD_Startup 
   @date      June 16 2002
   @author    S. Husa                           
   @desc                                        
   
   @enddesc                                     
   @calls                                       
   @calledby                                    
   @history                                     
   @endhistory                                  
 @@*/                                           
                                                
                                              

int GenericFD_Startup(void)
{                                          
    const char *banner = "GenericFD: generic finite differencing";
    CCTK_RegisterBanner(banner);              


    CCTK_INFO(FD_METHOD_DESC);
    return 0; 
 }
