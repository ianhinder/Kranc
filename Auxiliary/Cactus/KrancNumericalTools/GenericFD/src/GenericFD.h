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
#define IMAX(int1, int2) (int1 > int2) ? int1 : int2
#endif


#include "MathematicaCompat.h"

/* finite differencing macros */

/*                                   */
/* add method argument to shorthands */
/*                                   */

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


/*  c2                  */
/*                      */
/*  2nd order centered  */
/*                      */
/* second derivatives, centered, 2nd order */

#ifndef KRANC_C
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
	 +      gf(i,j,k-2)) * (dxi / 12.))


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
	 +      gf[CCTK_GFINDEX3D(cctkGH,i,j,k-2)]) * (dxi / 12.))

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
CCTK_INT sgn(CCTK_REAL x);
#endif
