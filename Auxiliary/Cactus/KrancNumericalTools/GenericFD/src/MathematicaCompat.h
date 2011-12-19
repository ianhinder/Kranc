#ifdef KRANC_C
#  define pown(x,y)     pow(x,y)
#  define IfThen(x,y,z) ((x) ? (y) : (z))
#else
#  define Abs(x)        (abs(x))
#  define Min(x, y)     (min(x,y))
#  define Max(x, y)     (max(x,y))
#  define Sqrt(x)       (sqrt(x))
#  define IfThen(x,y,z) ((x)*(y) + (1-(x))*(z))
#endif

#ifdef KRANC_C
#  define Sign(x)     (copysign(1.0,(x)))
#  define ToReal(x)   ((CCTK_REAL)(x))
#else
#  define Sign(x)     (sgn(x))
#  define ToReal(x)   (real((x),kind(khalf)))
#endif

#ifdef KRANC_C
#  define E           M_E
#  define Pi          M_PI
#else
#  define E           2.71828182845904523536029d0
#  define Pi          3.14159265358979323846264d0
#endif

#define UnitStep(x)   ((x)>0)
