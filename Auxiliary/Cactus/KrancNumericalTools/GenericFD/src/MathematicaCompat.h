 
#define Power(x, y)   (pow(x,y))
#define Sqrt(x)       (sqrt(x))


#ifdef KRANC_C
#  define Abs(x)        (fabs(x))
#  define Min(x, y)     (fmin(x,y))
#  define Max(x, y)     (fmax(x,y))
#  define IfThen(x,y,z) ((x) ? (y) : (z))
#else
#  define Abs(x)        (abs(x))
#  define Min(x, y)     (min(x,y))
#  define Max(x, y)     (max(x,y))
/* IfThen cannot be expressed in Fortran */
#endif

#define Exp(x)        (exp(x))
#define Log(x)        (log(x))

#define Sin(x)        (sin(x))
#define Cos(x)        (cos(x))
#define Tan(x)        (tan(x))

#define ArcSin(x)     (asin(x))
#define ArcCos(x)     (acos(x))
#define ArcTan(x)     (atan(x))

#define Sinh(x)       (sinh(x))
#define Cosh(x)       (cosh(x))
#define Tanh(x)       (tanh(x))

#ifdef KRANC_C
#  define Sign(x)     ((x)<0?-1:+1)
#  define ToReal(x)   ((CCTK_REAL)(x))
#else
#  define Sign(x)     (sgn(x))
#  define ToReal(x)   (real((x),kind(khalf)))
#endif

/* TODO: use fma(x,y,z) to implement fmadd and friends?  Note that fma
   may be unsupported, or may be slow.  */

/* #define fmadd(x,y,z)  ((x)*(y)+(z)) */
/* #define fmsub(x,y,z)  ((x)*(y)-(z)) */
/* #define fnmadd(x,y,z) (-(z)-(x)*(y)) */
/* #define fnmsub(x,y,z) (+(z)-(x)*(y)) */

#define fneg(x)   (-(x))
#define fmul(x,y) ((x)*(y))
#define fdiv(x,y) ((x)/(y))
#define fadd(x,y) ((x)+(y))
#define fsub(x,y) ((x)-(y))

#define fmadd(x,y,z)  (fadd(fmul(x,y),z))
#define fmsub(x,y,z)  (fsub(fmul(x,y),z))
#define fnmadd(x,y,z) (fsub(fneg(z),fmul(x,y)))
#define fnmsub(x,y,z) (fsub(z,fmul(x,y)))

#define kexp(x)    (exp(x))
#define kfabs(x)   (fabs(x))
#define kfmax(x,y) (fmax(x,y))
#define kfmin(x,y) (fmin(x,y))
#define klog(x)    (log(x))
#define kpow(x,y)  (pow(x,y))
#define ksqrt(x)   (sqrt(x))

#ifdef KRANC_C
#  define E           M_E
#  define Pi          M_PI
#else
#  define E           2.71828182845904523536029d0
#  define Pi          3.14159265358979323846264d0
#endif

#define UnitStep(x)   ((x)>0)
