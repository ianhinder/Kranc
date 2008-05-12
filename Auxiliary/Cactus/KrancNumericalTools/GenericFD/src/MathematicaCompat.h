 
#define Power(x, y)   (pow((x), (y)))
#define Sqrt(x)       (sqrt(x))


#ifdef KRANC_C
#define Abs(x)        (fabs(x))
#define Min(x, y)     (fmin((x), (y)))
#define Max(x, y)     (fmax((x), (y)))
#define IfThen(x,y,z) ((x) ? (y) : (z))
#else
#define Abs(x)        (abs(x))
#define Min(x, y)     (min((x), (y)))
#define Max(x, y)     (max((x), (y)))
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

#define Sign(x)       (sgn(x))

#define E               2.71828182845904523536029
#define Pi              3.14159265358979323846264

#define UnitStep(x) ( (x) > 0 ? 1 : 0 )
