#ifdef KRANC_C
#  define IfThen(x,y,z) ((x) ? (y) : (z))
#else
#  define Abs(x)        (abs(x))
#  define IntAbs(x)     (abs(x))
#  define Min(x, y)     (min(x,y))
#  define Max(x, y)     (max(x,y))
#  define Sqrt(x)       (sqrt(x))
#  define IfThen(x,y,z) ((x)*(y) + (1-(x))*(z))
#endif

#ifdef KRANC_C
#  define Sign(x)     (copysign( (CCTK_REAL) 1.0,(CCTK_REAL) (x)))
#  define ToReal(x)   ((CCTK_REAL)(x))
#else
#  define Sign(x)     (sgn(x))
#  define ToReal(x)   (real((x),kind(khalf)))
#endif

#define MinMod(x, y)  ((x) * (y) < 0 ? 0 : (fabs((x)) < fabs((y)) ? (x) : (y)))

#define VanLeer(x, y)  ((x) * (y) < 0 ? 0 : (Min3(2*fabs(x),2*fabs(y),0.5*(fabs(x)+fabs(y)))*Sign((x)+(y))))

#ifdef KRANC_C
#  define E           M_E
#  define Pi          M_PI

#else
#  define E           2.71828182845904523536029d0
#  define Pi          3.14159265358979323846264d0
#endif

#define StepFunction(x) ((x)>0)
