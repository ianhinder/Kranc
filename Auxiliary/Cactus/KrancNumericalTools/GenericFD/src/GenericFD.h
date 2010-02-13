/*@@                                         
   @file      GenericFD/src/GenericFD.h
   @date      June 16 2002
   @author    S. Husa                           
   @desc

   $Id$                                  
   
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
                         
#ifndef NOPRECOMPUTE
#define PRECOMPUTE
#endif

#if defined(FD_C0) || defined(FD_C2) || defined(FD_C4) || defined(FD_C2C4)
#define FD_SET_BY_USER
#endif
#ifndef FD_SET_BY_USER
#define FD_C2
#endif	


#if defined(FD_C0)
#define FD_METHOD_DESC  "FD method: replace derivatives by zero"
#endif

#if defined(FD_C2)
#define FD_METHOD_DESC  "FD method: second order centered finite differences"
#endif

#if defined(FD_C4)
#define FD_METHOD_DESC  "FD method: fourth order centered finite differences"
#endif

#if defined(FD_C2C4)
#define FD_METHOD_DESC  "FD method: weighted 2nd/4th order centered finite differences"
#endif



/* utility functions  */
#if defined(KRANC_C)
#define string(d,f) d ## f
#else
#define string(d,f) d/**/f
#endif

#if defined(KRANC_C)
#define IMAX(int1, int2) ((int1) > (int2) ? (int1) : (int2))
#endif


#include "MathematicaCompat.h"

/* finite differencing macros */

/*                                   */
/* add method argument to shorthands */
/*                                   */

/* third derivatives */
#define D111x(gf)  string(D111,gf)
#define D211x(gf)  string(D211,gf)
#define D311x(gf)  string(D311,gf)
#define D221x(gf)  string(D221,gf)
#define D321x(gf)  string(D321,gf)
#define D331x(gf)  string(D331,gf)
#define D222x(gf)  string(D222,gf)
#define D322x(gf)  string(D322,gf)
#define D332x(gf)  string(D332,gf)
#define D333x(gf)  string(D333,gf)

/* second derivatives */
#define D11x(gf)  string(D11,gf)
#define D22x(gf)  string(D22,gf)
#define D33x(gf)  string(D33,gf)
#define D21x(gf)  string(D21,gf)
#define D32x(gf)  string(D32,gf)
#define D31x(gf)  string(D31,gf)

/* first derivatives                */
#define D1x(gf)   string(D1,gf)
#define D2x(gf)   string(D2,gf)
#define D3x(gf)   string(D3,gf)

#ifdef PRECOMPUTE

/* third derivatives */
#define D111(gf)  string(D111,gf)
#define D211(gf)  string(D211,gf)
#define D311(gf)  string(D311,gf)
#define D221(gf)  string(D221,gf)
#define D321(gf)  string(D321,gf)
#define D331(gf)  string(D331,gf)
#define D222(gf)  string(D222,gf)
#define D322(gf)  string(D322,gf)
#define D332(gf)  string(D332,gf)
#define D333(gf)  string(D333,gf)

/* second derivatives */
#define D11(gf,i,j,k)  string(D11,gf)
#define D22(gf,i,j,k)  string(D22,gf)
#define D33(gf,i,j,k)  string(D33,gf)
#define D21(gf,i,j,k)  string(D21,gf)
#define D32(gf,i,j,k)  string(D32,gf)
#define D31(gf,i,j,k)  string(D31,gf) 

/* first derivatives                */
#define D1(gf,i,j,k)   string(D1,gf)
#define D2(gf,i,j,k)   string(D2,gf) 
#define D3(gf,i,j,k)   string(D3,gf) 

#else

/* third derivatives */
#define D111(gf)  D111gf(gf,i,j,k)
#define D211(gf)  D211gf(gf,i,j,k)
#define D311(gf)  D311gf(gf,i,j,k)
#define D221(gf)  D221gf(gf,i,j,k)
#define D321(gf)  D321gf(gf,i,j,k)
#define D331(gf)  D331gf(gf,i,j,k)
#define D222(gf)  D222gf(gf,i,j,k)
#define D322(gf)  D322gf(gf,i,j,k)
#define D332(gf)  D332gf(gf,i,j,k)
#define D333(gf)  D333gf(gf,i,j,k)

/* second derivatives */
#define D11(gf,i,j,k)  D11gf(gf,i,j,k)
#define D22(gf,i,j,k)  D22gf(gf,i,j,k)
#define D33(gf,i,j,k)  D33gf(gf,i,j,k)
#define D21(gf,i,j,k)  D21gf(gf,i,j,k)
#define D32(gf,i,j,k)  D32gf(gf,i,j,k)
#define D31(gf,i,j,k)  D31gf(gf,i,j,k)

/* first derivatives                */
#define D1(gf,i,j,k)   D1gf(gf, i,j,k)
#define D2(gf,i,j,k)   D2gf(gf, i,j,k)
#define D3(gf,i,j,k)   D3gf(gf, i,j,k)

#endif


#ifdef FD_C0
/* third derivatives */
#define D111gf(gf,i,j,k)  D111_c0(gf,i,j,k)
#define D211gf(gf,i,j,k)  D211_c0(gf,i,j,k)
#define D311gf(gf,i,j,k)  D311_c0(gf,i,j,k)
#define D221gf(gf,i,j,k)  D221_c0(gf,i,j,k)
#define D321gf(gf,i,j,k)  D321_c0(gf,i,j,k)
#define D331gf(gf,i,j,k)  D331_c0(gf,i,j,k)
#define D222gf(gf,i,j,k)  D222_c0(gf,i,j,k)
#define D322gf(gf,i,j,k)  D322_c0(gf,i,j,k)
#define D332gf(gf,i,j,k)  D332_c0(gf,i,j,k)
#define D333gf(gf,i,j,k)  D333_c0(gf,i,j,k)

/* second derivatives */
#define D11gf(gf,i,j,k)  D11_c0(gf,i,j,k)
#define D22gf(gf,i,j,k)  D22_c0(gf,i,j,k)
#define D33gf(gf,i,j,k)  D33_c0(gf,i,j,k)
#define D21gf(gf,i,j,k)  D21_c0(gf,i,j,k)
#define D32gf(gf,i,j,k)  D32_c0(gf,i,j,k)
#define D31gf(gf,i,j,k)  D31_c0(gf,i,j,k)

/* first derivatives                */
#define D1gf(gf,i,j,k)   D1_c0(gf, i,j,k)
#define D2gf(gf,i,j,k)   D2_c0(gf, i,j,k)
#define D3gf(gf,i,j,k)   D3_c0(gf, i,j,k)
#endif



#ifdef FD_C2
/* third derivatives */
#define D111gf(gf,i,j,k)  D111_c2(gf,i,j,k)
#define D211gf(gf,i,j,k)  D211_c2(gf,i,j,k)
#define D311gf(gf,i,j,k)  D311_c2(gf,i,j,k)
#define D221gf(gf,i,j,k)  D221_c2(gf,i,j,k)
#define D321gf(gf,i,j,k)  D321_c2(gf,i,j,k)
#define D331gf(gf,i,j,k)  D331_c2(gf,i,j,k)
#define D222gf(gf,i,j,k)  D222_c2(gf,i,j,k)
#define D322gf(gf,i,j,k)  D322_c2(gf,i,j,k)
#define D332gf(gf,i,j,k)  D332_c2(gf,i,j,k)
#define D333gf(gf,i,j,k)  D333_c2(gf,i,j,k)

/* second derivatives */
#define D11gf(gf,i,j,k)  D11_c2(gf,i,j,k)
#define D22gf(gf,i,j,k)  D22_c2(gf,i,j,k)
#define D33gf(gf,i,j,k)  D33_c2(gf,i,j,k)
#define D21gf(gf,i,j,k)  D21_c2(gf,i,j,k)
#define D32gf(gf,i,j,k)  D32_c2(gf,i,j,k)
#define D31gf(gf,i,j,k)  D31_c2(gf,i,j,k)

/* first derivatives                */
#define D1gf(gf,i,j,k)   D1_c2(gf, i,j,k)
#define D2gf(gf,i,j,k)   D2_c2(gf, i,j,k)
#define D3gf(gf,i,j,k)   D3_c2(gf, i,j,k)
#endif



#ifdef FD_C4
/* third derivatives */
#define D111gf(gf,i,j,k)  D111_c4(gf,i,j,k)
#define D211gf(gf,i,j,k)  D211_c4(gf,i,j,k)
#define D311gf(gf,i,j,k)  D311_c4(gf,i,j,k)
#define D221gf(gf,i,j,k)  D221_c4(gf,i,j,k)
#define D321gf(gf,i,j,k)  D321_c4(gf,i,j,k)
#define D331gf(gf,i,j,k)  D331_c4(gf,i,j,k)
#define D222gf(gf,i,j,k)  D222_c4(gf,i,j,k)
#define D322gf(gf,i,j,k)  D322_c4(gf,i,j,k)
#define D332gf(gf,i,j,k)  D332_c4(gf,i,j,k)
#define D333gf(gf,i,j,k)  D333_c4(gf,i,j,k)

/* second derivatives */
#define D11gf(gf,i,j,k)  D11_c4(gf,i,j,k) 
#define D22gf(gf,i,j,k)  D22_c4(gf,i,j,k)
#define D33gf(gf,i,j,k)  D33_c4(gf,i,j,k)
#define D21gf(gf,i,j,k)  D21_c4(gf,i,j,k) 
#define D32gf(gf,i,j,k)  D32_c4(gf,i,j,k) 
#define D31gf(gf,i,j,k)  D31_c4(gf,i,j,k)

/* first derivatives                */
#define D1gf(gf,i,j,k)   D1_c4(gf, i,j,k) 
#define D2gf(gf,i,j,k)   D2_c4(gf, i,j,k)
#define D3gf(gf,i,j,k)   D3_c4(gf, i,j,k)
#endif


#ifdef FD_C2C4
/* third derivatives */
#define D111gf(gf,i,j,k)  D111_c2c4(gf,i,j,k)
#define D211gf(gf,i,j,k)  D211_c2c4(gf,i,j,k)
#define D311gf(gf,i,j,k)  D311_c2c4(gf,i,j,k)
#define D221gf(gf,i,j,k)  D221_c2c4(gf,i,j,k)
#define D321gf(gf,i,j,k)  D321_c2c4(gf,i,j,k)
#define D331gf(gf,i,j,k)  D331_c2c4(gf,i,j,k)
#define D222gf(gf,i,j,k)  D222_c2c4(gf,i,j,k)
#define D322gf(gf,i,j,k)  D322_c2c4(gf,i,j,k)
#define D332gf(gf,i,j,k)  D332_c2c4(gf,i,j,k)
#define D333gf(gf,i,j,k)  D333_c2c4(gf,i,j,k)

/* second derivatives */
#define D11gf(gf,i,j,k)  D11_c2c4(gf,i,j,k)
#define D22gf(gf,i,j,k)  D22_c2c4(gf,i,j,k)
#define D33gf(gf,i,j,k)  D33_c2c4(gf,i,j,k)
#define D21gf(gf,i,j,k)  D21_c2c4(gf,i,j,k)
#define D32gf(gf,i,j,k)  D32_c2c4(gf,i,j,k)
#define D31gf(gf,i,j,k)  D31_c2c4(gf,i,j,k)

/* first derivatives                */
#define D1gf(gf,i,j,k)   D1_c2c4(gf, i,j,k)
#define D2gf(gf,i,j,k)   D2_c2c4(gf, i,j,k)
#define D3gf(gf,i,j,k)   D3_c2c4(gf, i,j,k)
#endif




/*****************************************************/
/*                                                   */
/*             METHODS                               */
/*                                                   */
/*****************************************************/

/*  c0                              */

/*  set all derivatives = 0         */
/*  for debugging and benchmarking  */

/* third derivatives */

#define D111_c0(gf,i,j,k)   0.
#define D211_c0(gf,i,j,k)   0.
#define D311_c0(gf,i,j,k)   0.
#define D221_c0(gf,i,j,k)   0.
#define D321_c0(gf,i,j,k)   0.
#define D331_c0(gf,i,j,k)   0.
#define D222_c0(gf,i,j,k)   0.
#define D322_c0(gf,i,j,k)   0.
#define D332_c0(gf,i,j,k)   0.
#define D333_c0(gf,i,j,k)   0.

/*  second derivatives              */

#define D11_c0(gf,i,j,k)   0.
#define D22_c0(gf,i,j,k)   0.
#define D33_c0(gf,i,j,k)   0.
#define D21_c0(gf,i,j,k)   0.
#define D32_c0(gf,i,j,k)   0.
#define D31_c0(gf,i,j,k)   0.

/* first derivatives                */

#define D1_c0(gf,i,j,k)    0.
#define D2_c0(gf,i,j,k)    0.
#define D3_c0(gf,i,j,k)    0.



#ifndef KRANC_C

/*  c2                  */
/*                      */
/*  2nd order centered  */
/*                      */

/* third derivatives, centered, 2nd order */

#define D111_c2(gf,i,j,k)   ((- gf(i+2,j,k) + 2*gf(i+1,j,k) - 2*gf(i-1,j,k) + gf(i-2,j,k)) * dxi*dxi*dxi * (1.0/2.0))
#define D211_c2(gf,i,j,k)   ((gf(i+1,j+1,k) - 2*gf(i,j+1,k) + gf(i-1,j+1,k) - gf(i+1,j-1,k) + 2*gf(i,j-1,k) - gf(i-1,j-1,k)) * dxi*dxi*dyi * (1.0/2.0))
#define D311_c2(gf,i,j,k)   ((gf(i+1,j,k+1) - 2*gf(i,j,k+1) + gf(i-1,j,k+1) - gf(i+1,j,k-1) + 2*gf(i,j,k-1) - gf(i-1,j,k-1)) * dxi*dxi*dzi * (1.0/2.0))
#define D221_c2(gf,i,j,k)   ((gf(i+1,j+1,k) - 2*gf(i+1,j,k) + gf(i+1,j-1,k) - gf(i-1,j+1,k) + 2*gf(i-1,j,k) - gf(i-1,j-1,k)) * dxi*dyi*dyi * (1.0/2.0))
#define D321_c2(gf,i,j,k)   ((gf(i+1,j+1,k+1) - gf(i-1,j+1,k+1) - gf(i+1,j-1,k+1) + gf(i-1,j-1,k+1) - gf(i+1,j+1,k-1) + gf(i-1,j+1,k-1) + gf(i+1,j-1,k-1) - gf(i-1,j-1,k-1)) * dxi*dyi*dzi * (1.0/8.0))
#define D331_c2(gf,i,j,k)   ((gf(i+1,j,k+1) - 2*gf(i+1,j,k) + gf(i+1,j,k-1) - gf(i-1,j,k+1) + 2*gf(i-1,j,k) - gf(i-1,j,k-1)) * dxi*dzi*dzi * (1.0/2.0))
#define D222_c2(gf,i,j,k)   ((- gf(i,j+2,k) + 2*gf(i,j+1,k) - 2*gf(i,j-1,k) + gf(i,j-2,k)) * dyi*dyi*dyi * (1.0/2.0))
#define D322_c2(gf,i,j,k)   ((gf(i,j+1,k+1) - 2*gf(i,j,k+1) + gf(i,j-1,k+1) - gf(i,j+1,k-1) + 2*gf(i,j,k-1) - gf(i,j-1,k-1)) * dyi*dyi*dzi * (1.0/2.0))
#define D332_c2(gf,i,j,k)   ((gf(i,j+1,k+1) - 2*gf(i,j+1,k) + gf(i,j+1,k-1) - gf(i,j-1,k+1) + 2*gf(i,j-1,k) - gf(i,j-1,k-1)) * dyi*dzi*dzi * (1.0/2.0))
#define D333_c2(gf,i,j,k)   ((- gf(i,j,k+2) + 2*gf(i,j,k+1) - 2*gf(i,j,k-1) + gf(i,j,k-2)) * dzi*dzi*dzi * (1.0/2.0))

/* second derivatives, centered, 2nd order */

#define D11_c2(gf,i,j,k)                         \
	 ((   gf(i+1,j,k) \
	 - 2.*gf(i,  j,k) \
	 +    gf(i-1,j,k)) * dxi * dxi )

#define D22_c2(gf,i,j,k)                         \
	 ((   gf(i,j+1,k) \
	 - 2.*gf(i,j,  k) \
	 +    gf(i,j-1,k)) * dyi * dyi )

#define D33_c2(gf,i,j,k)                         \
	 ((   gf(i,j,k+1) \
	 - 2.*gf(i,j,k  ) \
	 +    gf(i,j,k-1)) * dzi * dzi )

#define D21_c2(gf,i,j,k)                        \
	 ((gf(i+1,j+1,k) \
	 + gf(i-1,j-1,k) \
	 - gf(i+1,j-1,k) \
	 - gf(i-1,j+1,k)) * hdxi * hdyi )

#define D32_c2(gf,i,j,k)                        \
	 ((gf(i,j+1,k+1) \
	 + gf(i,j-1,k-1) \
	 - gf(i,j+1,k-1) \
	 - gf(i,j-1,k+1)) * hdyi * hdzi )

#define D31_c2(gf,i,j,k)                        \
	 ((gf(i+1,j,k+1) \
	 + gf(i-1,j,k-1) \
	 - gf(i+1,j,k-1) \
	 - gf(i-1,j,k+1)) * hdxi * hdzi )

/* first derivatives, centered, 2nd order */

#define D1_c2(gf,i,j,k)                       \
	 ((gf(i+1,j,k) \
	 - gf(i-1,j,k)) * hdxi)

#define D2_c2(gf,i,j,k)                       \
	 ((gf(i,j+1,k) \
	 - gf(i,j-1,k)) * hdyi)

#define D3_c2(gf,i,j,k)                       \
	 ((gf(i,j,k+1) \
	 - gf(i,j,k-1)) * hdzi)
	
#else


#define D11_c2(gf,i,j,k)                         \
	 ((   gf[CCTK_GFINDEX3D(cctkGH,i+1,j,k)] \
	 - 2.*gf[CCTK_GFINDEX3D(cctkGH,i,  j,k)] \
	 +    gf[CCTK_GFINDEX3D(cctkGH,i-1,j,k)]) * dxi * dxi )

#define D22_c2(gf,i,j,k)                         \
	 ((   gf[CCTK_GFINDEX3D(cctkGH,i,j+1,k)] \
	 - 2.*gf[CCTK_GFINDEX3D(cctkGH,i,j,  k)] \
	 +    gf[CCTK_GFINDEX3D(cctkGH,i,j-1,k)]) * dyi * dyi )

#define D33_c2(gf,i,j,k)                         \
	 ((   gf[CCTK_GFINDEX3D(cctkGH,i,j,k+1)] \
	 - 2.*gf[CCTK_GFINDEX3D(cctkGH,i,j,k  )] \
	 +    gf[CCTK_GFINDEX3D(cctkGH,i,j,k-1)]) * dzi * dzi )

#define D21_c2(gf,i,j,k)                        \
	 ((gf[CCTK_GFINDEX3D(cctkGH,i+1,j+1,k)] \
	 + gf[CCTK_GFINDEX3D(cctkGH,i-1,j-1,k)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i+1,j-1,k)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i-1,j+1,k)]) * hdxi * hdyi )

#define D32_c2(gf,i,j,k)                        \
	 ((gf[CCTK_GFINDEX3D(cctkGH,i,j+1,k+1)] \
	 + gf[CCTK_GFINDEX3D(cctkGH,i,j-1,k-1)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i,j+1,k-1)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i,j-1,k+1)]) * hdyi * hdzi )

#define D31_c2(gf,i,j,k)                        \
	 ((gf[CCTK_GFINDEX3D(cctkGH,i+1,j,k+1)] \
	 + gf[CCTK_GFINDEX3D(cctkGH,i-1,j,k-1)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i+1,j,k-1)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i-1,j,k+1)]) * hdxi * hdzi )

/* first derivatives, centered, 2nd order */

#define D1_c2(gf,i,j,k)                       \
	 ((gf[CCTK_GFINDEX3D(cctkGH,i+1,j,k)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i-1,j,k)]) * hdxi)

#define D2_c2(gf,i,j,k)                       \
	 ((gf[CCTK_GFINDEX3D(cctkGH,i,j+1,k)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i,j-1,k)]) * hdyi)

#define D3_c2(gf,i,j,k)                       \
	 ((gf[CCTK_GFINDEX3D(cctkGH,i,j,k+1)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i,j,k-1)]) * hdzi)

#endif	

	
/*  c4                  */
/*                      */
/*  4th order centered  */
/*                      */

/* second derivatives, centered, 4th order */

#ifndef KRANC_C

#define D11_c4(gf,i,j,k)                    \
  ((-       gf(i+2,j,k)			      \
	   + 16. * gf(i+1,j,k) \
	   - 30. * gf(i,  j,k) \
	   + 16. * gf(i-1,j,k) \
	   -       gf(i-2,j,k)) * dxi * dxi / 12.)


#define D22_c4(gf,i,j,k)                    \
	 ((-       gf(i,j+2,k) \
	   + 16. * gf(i,j+1,k) \
	   - 30. * gf(i,j,  k) \
	   + 16. * gf(i,j-1,k) \
	   -       gf(i,j-2,k)) * dyi * dyi / 12.)


#define D33_c4(gf,i,j,k)                              \
	 ((-       gf(i,j,k+2) \
	   + 16. * gf(i,j,k+1) \
	   - 30. * gf(i,j,k  ) \
	   + 16. * gf(i,j,k-1) \
	   -       gf(i,j,k-2)) * dzi * dzi / 12.)

#define D21_c4(gf,i,j,k)                                    \
	     ((-       gf(i+2,j+2,k) \
               +       gf(i+2,j-2,k) \
	       +       gf(i-2,j+2,k) \
               -       gf(i-2,j-2,k) \
               + 16. * gf(i+1,j+1,k) \
    	       - 16. * gf(i+1,j-1,k) \
	       - 16. * gf(i-1,j+1,k) \
	       + 16. * gf(i-1,j-1,k)) * dxi * dyi / 48.)

#define D31_c4(gf,i,j,k)                                    \
             ((-       gf(i+2,j,k+2) \
               +       gf(i+2,j,k-2) \
               +       gf(i-2,j,k+2) \
               -       gf(i-2,j,k-2) \
               + 16. * gf(i+1,j,k+1) \
               - 16. * gf(i+1,j,k-1) \
               - 16. * gf(i-1,j,k+1) \
               + 16. * gf(i-1,j,k-1)) * dxi * dzi / 48.)

	
#define D32_c4(gf,i,j,k)                                    \
             ((-       gf(i,j+2,k+2) \
               +       gf(i,j+2,k-2) \
               +       gf(i,j-2,k+2) \
               -       gf(i,j-2,k-2) \
               + 16. * gf(i,j+1,k+1) \
               - 16. * gf(i,j+1,k-1) \
               - 16. * gf(i,j-1,k+1) \
               + 16. * gf(i,j-1,k-1)) * dzi * dyi / 48.)


/* first derivatives, centered, 4th order */

#define D1_c4(gf,i,j,k)                            \
       ((-      gf(i+2,j,k) \
         + 8. * gf(i+1,j,k) \
         - 8. * gf(i-1,j,k) \
	 +      gf(i-2,j,k)) * (dxi / 12.))

#define D2_c4(gf,i,j,k)                            \
       ((-      gf(i,j+2,k) \
         + 8. * gf(i,j+1,k) \
         - 8. * gf(i,j-1,k) \
	 +      gf(i,j-2,k)) * (dyi / 12.))

#define D3_c4(gf,i,j,k)                            \
       ((-      gf(i,j,k+2) \
         + 8. * gf(i,j,k+1) \
         - 8. * gf(i,j,k-1) \
	 +      gf(i,j,k-2)) * (dzi / 12.))


#else

#define D11_c4(gf,i,j,k)                    \
	 ((-       gf[CCTK_GFINDEX3D(cctkGH,i+2,j,k)] \
	   + 16. * gf[CCTK_GFINDEX3D(cctkGH,i+1,j,k)] \
	   - 30. * gf[CCTK_GFINDEX3D(cctkGH,i,  j,k)] \
	   + 16. * gf[CCTK_GFINDEX3D(cctkGH,i-1,j,k)] \
	   -       gf[CCTK_GFINDEX3D(cctkGH,i-2,j,k)]) * dxi * dxi / 12.)


#define D22_c4(gf,i,j,k)                    \
	 ((-       gf[CCTK_GFINDEX3D(cctkGH,i,j+2,k)] \
	   + 16. * gf[CCTK_GFINDEX3D(cctkGH,i,j+1,k)] \
	   - 30. * gf[CCTK_GFINDEX3D(cctkGH,i,j,  k)] \
	   + 16. * gf[CCTK_GFINDEX3D(cctkGH,i,j-1,k)] \
	   -       gf[CCTK_GFINDEX3D(cctkGH,i,j-2,k)]) * dyi * dyi / 12.)


#define D33_c4(gf,i,j,k)                              \
	 ((-       gf[CCTK_GFINDEX3D(cctkGH,i,j,k+2)] \
	   + 16. * gf[CCTK_GFINDEX3D(cctkGH,i,j,k+1)] \
	   - 30. * gf[CCTK_GFINDEX3D(cctkGH,i,j,k  )] \
	   + 16. * gf[CCTK_GFINDEX3D(cctkGH,i,j,k-1)] \
	   -       gf[CCTK_GFINDEX3D(cctkGH,i,j,k-2)]) * dzi * dzi / 12.)

#define D21_c4(gf,i,j,k)                                    \
	     ((-       gf[CCTK_GFINDEX3D(cctkGH,i+2,j+2,k)] \
               +       gf[CCTK_GFINDEX3D(cctkGH,i+2,j-2,k)] \
	       +       gf[CCTK_GFINDEX3D(cctkGH,i-2,j+2,k)] \
               -       gf[CCTK_GFINDEX3D(cctkGH,i-2,j-2,k)] \
               + 16. * gf[CCTK_GFINDEX3D(cctkGH,i+1,j+1,k)] \
    	       - 16. * gf[CCTK_GFINDEX3D(cctkGH,i+1,j-1,k)] \
	       - 16. * gf[CCTK_GFINDEX3D(cctkGH,i-1,j+1,k)] \
	       + 16. * gf[CCTK_GFINDEX3D(cctkGH,i-1,j-1,k)]) * dxi * dyi / 48.)

#define D31_c4(gf,i,j,k)                                    \
             ((-       gf[CCTK_GFINDEX3D(cctkGH,i+2,j,k+2)] \
               +       gf[CCTK_GFINDEX3D(cctkGH,i+2,j,k-2)] \
               +       gf[CCTK_GFINDEX3D(cctkGH,i-2,j,k+2)] \
               -       gf[CCTK_GFINDEX3D(cctkGH,i-2,j,k-2)] \
               + 16. * gf[CCTK_GFINDEX3D(cctkGH,i+1,j,k+1)] \
               - 16. * gf[CCTK_GFINDEX3D(cctkGH,i+1,j,k-1)] \
               - 16. * gf[CCTK_GFINDEX3D(cctkGH,i-1,j,k+1)] \
               + 16. * gf[CCTK_GFINDEX3D(cctkGH,i-1,j,k-1)]) * dxi * dzi / 48.)

	
#define D32_c4(gf,i,j,k)                                    \
             ((-       gf[CCTK_GFINDEX3D(cctkGH,i,j+2,k+2)] \
               +       gf[CCTK_GFINDEX3D(cctkGH,i,j+2,k-2)] \
               +       gf[CCTK_GFINDEX3D(cctkGH,i,j-2,k+2)] \
               -       gf[CCTK_GFINDEX3D(cctkGH,i,j-2,k-2)] \
               + 16. * gf[CCTK_GFINDEX3D(cctkGH,i,j+1,k+1)] \
               - 16. * gf[CCTK_GFINDEX3D(cctkGH,i,j+1,k-1)] \
               - 16. * gf[CCTK_GFINDEX3D(cctkGH,i,j-1,k+1)] \
               + 16. * gf[CCTK_GFINDEX3D(cctkGH,i,j-1,k-1)]) * dzi * dyi / 48.)


/* first derivatives, centered, 4th order */

#define D1_c4(gf,i,j,k)                            \
       ((-      gf[CCTK_GFINDEX3D(cctkGH,i+2,j,k)] \
         + 8. * gf[CCTK_GFINDEX3D(cctkGH,i+1,j,k)] \
         - 8. * gf[CCTK_GFINDEX3D(cctkGH,i-1,j,k)] \
	 +      gf[CCTK_GFINDEX3D(cctkGH,i-2,j,k)]) * (dxi / 12.))

#define D2_c4(gf,i,j,k)                            \
       ((-      gf[CCTK_GFINDEX3D(cctkGH,i,j+2,k)] \
         + 8. * gf[CCTK_GFINDEX3D(cctkGH,i,j+1,k)] \
         - 8. * gf[CCTK_GFINDEX3D(cctkGH,i,j-1,k)] \
	 +      gf[CCTK_GFINDEX3D(cctkGH,i,j-2,k)]) * (dyi / 12.))

#define D3_c4(gf,i,j,k)                            \
       ((-      gf[CCTK_GFINDEX3D(cctkGH,i,j,k+2)] \
         + 8. * gf[CCTK_GFINDEX3D(cctkGH,i,j,k+1)] \
         - 8. * gf[CCTK_GFINDEX3D(cctkGH,i,j,k-1)] \
	 +      gf[CCTK_GFINDEX3D(cctkGH,i,j,k-2)]) * (dzi / 12.))

#endif

/*****************************************************/
/*                                                    */
/*         DERIVED METHODS                            */
/*                                                    */
/******************************************************/


/* blend c2 and c4 */
/* second derivatives */
#define D11_c2c4(gf,i,j,k) (fdweight_c2*D11_c2(gf,i,j,k) + fdweight_c4*D11_c4(gf,i,j,k))
#define D22_c2c4(gf,i,j,k) (fdweight_c2*D22_c2(gf,i,j,k) + fdweight_c4*D22_c4(gf,i,j,k))
#define D33_c2c4(gf,i,j,k) (fdweight_c2*D33_c2(gf,i,j,k) + fdweight_c4*D33_c4(gf,i,j,k))
#define D21_c2c4(gf,i,j,k) (fdweight_c2*D21_c2(gf,i,j,k) + fdweight_c4*D21_c4(gf,i,j,k))
#define D32_c2c4(gf,i,j,k) (fdweight_c2*D32_c2(gf,i,j,k) + fdweight_c4*D32_c4(gf,i,j,k))
#define D31_c2c4(gf,i,j,k) (fdweight_c2*D31_c2(gf,i,j,k) + fdweight_c4*D31_c4(gf,i,j,k))

/* first derivatives */
#define D1_c2c4(gf,i,j,k)  (fdweight_c2*D1_c2(gf, i,j,k) + fdweight_c4*D1_c4(gf,i,j,k))
#define D2_c2c4(gf,i,j,k)  (fdweight_c2*D2_c2(gf, i,j,k) + fdweight_c4*D2_c4(gf,i,j,k))
#define D3_c2c4(gf,i,j,k)  (fdweight_c2*D3_c2(gf, i,j,k) + fdweight_c4*D3_c4(gf,i,j,k))


/*****************************************************/
/*                                                    */
/*  Poor man's one-sided derivatives                  */
/*                                                    */
/******************************************************/


#define Dplus1(gf,i,j,k)   string(Dplus1,gf)
#define Dplus2(gf,i,j,k)   string(Dplus2,gf)
#define Dplus3(gf,i,j,k)   string(Dplus3,gf)


#define Dplus1gf(gf,i,j,k)   Dplus1x(gf, i,j,k)
#define Dplus2gf(gf,i,j,k)   Dplus2x(gf, i,j,k)
#define Dplus3gf(gf,i,j,k)   Dplus3x(gf, i,j,k)

#ifdef KRANC_C

#define Dplus1x(gf,i,j,k)                     \
	 ((gf[CCTK_GFINDEX3D(cctkGH,i+1,j,k)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i,j,k)]) * dxi)

#define Dplus2x(gf,i,j,k)                     \
	 ((gf[CCTK_GFINDEX3D(cctkGH,i,j+1,k)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i,j,k)]) * dyi)

#define Dplus3x(gf,i,j,k)                     \
	 ((gf[CCTK_GFINDEX3D(cctkGH,i,j,k+1)] \
	 - gf[CCTK_GFINDEX3D(cctkGH,i,j,k)]) * dzi)

#else

#define Dplus1x(gf,i,j,k)  ( ( gf(i+1, j,     k)     - gf(i,j,k) ) * dxi )
#define Dplus2x(gf,i,j,k)  ( ( gf(i,   j + 1, k)     - gf(i,j,k) ) * dxi )
#define Dplus3x(gf,i,j,k)  ( ( gf(i,   j,     k + 1) - gf(i,j,k) ) * dxi )

#endif

#ifdef KRANC_C
int sgn(CCTK_REAL x);

#define Dupwind1(gf,dir,i,j,k) ((dir * gf[CCTK_GFINDEX3D(cctkGH,i+dir,j,k)] \
	 - dir * gf[CCTK_GFINDEX3D(cctkGH,i,j,k)]) * dxi)
#define Dupwind2(gf,dir,i,j,k) ((dir * gf[CCTK_GFINDEX3D(cctkGH,i,j+dir,k)] \
	 - dir * gf[CCTK_GFINDEX3D(cctkGH,i,j,k)]) * dxi)
#define Dupwind3(gf,dir,i,j,k) ((dir * gf[CCTK_GFINDEX3D(cctkGH,i,j,k+dir)] \
	 - dir * gf[CCTK_GFINDEX3D(cctkGH,i,j,k)]) * dxi)

void GenericFD_GetBoundaryInfo(cGH const * restrict cctkGH,
                               int const * restrict cctk_lsh,
                               int const * restrict cctk_lssh,
                               int const * restrict cctk_bbox,
			       int const * restrict cctk_nghostzones,
                               int * restrict imin, 
			       int * restrict imax,
                               int * restrict is_symbnd, 
			       int * restrict is_physbnd,
                               int * restrict is_ipbnd);

#if 0
/* Finite differencing near boundaries */

/* The array var is to be accessed at the location
   [i+ioff,j+joff,k+koff].  idir,jdir,kdir specify whether there is a
   lower (dir<0), upper (dir>0), or no boundary nearby.  If a boundary
   is in the way, the value 0 is returned instead of the array
   content.  */
#define CCTK_GFACCESS3D(cctkGH, var, i,j,k, ioff,joff,koff, idir,jdir,kdir) \
  (((idir)<0 && (ioff)<0) ||                                            \
   ((jdir)<0 && (joff)<0) ||                                            \
   ((kdir)<0 && (koff)<0) ||                                            \
   ((idir)>0 && (ioff)>0) ||                                            \
   ((jdir)>0 && (joff)>0) ||                                            \
   ((kdir)>0 && (koff)>0) ||                                            \
   ? 0                                                                  \
   : (var)[CCTK_GFINDEX3D((cctkGH), (i)+(ioff),(j)+(joff),(k)+(koff))])
#endif

/* Summation by parts */

static inline CCTK_REAL sbp_deriv_x(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
  CCTK_ATTRIBUTE_PURE;
static inline CCTK_REAL sbp_deriv_x(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
{
  CCTK_REAL dvarl = 0;
  int ni = cctkGH->cctk_lsh[0];
  for (int ii=min[i]-1; ii<=max[i]-1; ++ii) {
    dvarl += q[ii+ni*i]*var[CCTK_GFINDEX3D (cctkGH, ii, j, k)];
  }
  dvarl /= d;
  return dvarl;
}

static inline CCTK_REAL sbp_deriv_y(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
  CCTK_ATTRIBUTE_PURE;
static inline CCTK_REAL sbp_deriv_y(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
{
  CCTK_REAL dvarl = 0;
  int nj = cctkGH->cctk_lsh[1];
  for (int jj=min[j]-1; jj<=max[j]-1; ++jj) {
    dvarl += q[jj+nj*j]*var[CCTK_GFINDEX3D (cctkGH, i, jj, k)];
  }
  dvarl /= d;
  return dvarl;
}

static inline CCTK_REAL sbp_deriv_z(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
  CCTK_ATTRIBUTE_PURE;
static inline CCTK_REAL sbp_deriv_z(int i, int j, int k, 
                                    const int min[], const int max[], 
                                    CCTK_REAL d,
                                    const CCTK_REAL *var, const CCTK_REAL *q,
                                    const cGH *cctkGH)
{
  CCTK_REAL dvarl = 0;
  int nk = cctkGH->cctk_lsh[2];
  for (int kk=min[k]-1; kk<=max[k]-1; ++kk) {
    dvarl += q[kk+nk*k]*var[CCTK_GFINDEX3D (cctkGH, i, j, kk)];
  }
  dvarl /= d;
  return dvarl;
}

/* New calculation format */

typedef void(*Kranc_Calculation)(cGH const * restrict cctkGH,
                                 int eir,
                                 int face,
                                 CCTK_REAL const normal[3],
                                 CCTK_REAL const tangentA[3],
                                 CCTK_REAL const tangentB[3],
                                 int const min[3],
                                 int const max[3], 
                                 int n_subblock_gfs, 
                                 CCTK_REAL * restrict const subblock_gfs[]);

void GenericFD_LoopOverEverything(cGH const * restrict cctkGH, Kranc_Calculation calc);
void GenericFD_LoopOverBoundary(cGH const * restrict cctkGH, Kranc_Calculation calc);
void GenericFD_LoopOverBoundaryWithGhosts(cGH const * restrict cctkGH, Kranc_Calculation calc);
void GenericFD_LoopOverInterior(cGH const * restrict cctkGH, Kranc_Calculation calc);



/* Vectorisation of memory accesses  */

#include <stdlib.h>
#include <cctk.h>

#if defined(__SSE2__) && defined(CCTK_REAL_PRECISION_8)

#include <emmintrin.h>

/* A vector type corresponding to CCTK_REAL */
typedef __m128d CCTK_REAL_VEC;

/* Really only SSE is required, but there doesn't seem to be a
   preprocessing flag to check for this */
#elif defined(__SSE2__) && defined(CCTK_REAL_PRECISION_4)

#include <emmintrin.h>

/* A vector type corresponding to CCTK_REAL */
typedef __m128 CCTK_REAL_VEC;

#else

/* There is no vector type corresponding to CCTK_REAL */
typedef CCTK_REAL CCTK_REAL_VEC;

#endif

/* The number of vector elements in a CCTK_REAL_VEC */
static
size_t const CCTK_REAL_VEC_SIZE = sizeof(CCTK_REAL_VEC) / sizeof(CCTK_REAL);



#endif
