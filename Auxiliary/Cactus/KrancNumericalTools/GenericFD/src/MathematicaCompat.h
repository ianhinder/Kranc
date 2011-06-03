 
#define Power(x, y)   (pow((x), (y)))
#define Sqrt(x)       (sqrt(x))


#ifdef KRANC_C
#define Abs(x)        (fabs(x))
#define Min(x, y)     (fmin((x), (y)))
#define Min3(x, y, z)     (fmin(fmin((x), (y)), (z)))
#define Max(x, y)     (fmax((x), (y)))
#define IfThen(x,y,z) ((x) ? (y) : (z))
#else
#define Abs(x)        (abs(x))
#define Min(x, y)     (min((x), (y)))
#define Max(x, y)     (max((x), (y)))
/* IfThen cannot be expressed in Fortran */
#endif

#ifdef KRANC_C
#define Sign(x)       (signbit(x)?-1:+1)
#else
#define Sign(x)       (sgn(x))
#endif

#define MinMod(x, y)  ((x) * (y) < 0 ? 0 : (fabs((x)) < fabs((y)) ? (x) : (y)))

#define VanLeer(x, y)  ((x) * (y) < 0 ? 0 : (Min3(2*fabs(x),2*fabs(y),0.5*(fabs(x)+fabs(y)))*Sign((x)+(y))))

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
#define E               M_E
#define Pi              M_PI
#else
#define E               2.71828182845904523536029d0
#define Pi              3.14159265358979323846264d0
#endif

#define StepFunction(x) ( (x) > 0 ? 1 : 0 )
