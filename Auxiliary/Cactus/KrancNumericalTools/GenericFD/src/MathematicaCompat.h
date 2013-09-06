 
#define Power(x, y)   (pow(x,y))
#define Sqrt(x)       (sqrt(x))

#define Abs(x)        (fabs(x))
#define Min(x, y)     (fmin(x,y))
#define Min3(x, y, z) (fmin(fmin((x), (y)), (z)))
#define Max(x, y)     (fmax(x,y))
#define IfThen(x,y,z) ((x) ? (y) : (z))

#define Exp(x)        (exp(x))
#define Log(x)        (log(x))

#define Sin(x)        (sin(x))
#define Cos(x)        (cos(x))
#define Tan(x)        (tan(x))
#define Sec(x)        (1.0/cos(x))
#define Csc(x)        (1.0/sin(x))
#define Cot(x)        (1.0/tan(x))

#define ArcSin(x)     (asin(x))
#define ArcCos(x)     (acos(x))
#define ArcTan(x)     (atan(x))
#define ArcSec(x)     (cos(1.0/(x)))
#define ArcCsc(x)     (sin(1.0/(x)))
#define ArcCot(x)     (tan(1.0/(x)))

#define Sinh(x)       (sinh(x))
#define Cosh(x)       (cosh(x))
#define Tanh(x)       (tanh(x))
#define Sech(x)       (1.0/cosh(x))
#define Csch(x)       (1.0/sinh(x))
#define Coth(x)       (1.0/tanh(x))

#define Sign(x)       (sgn(x))

#define ToReal(x)   ((CCTK_REAL)(x))

#define MinMod(x, y)  ((x) * (y) < 0 ? 0 : (fabs((x)) < fabs((y)) ? (x) : (y)))

#define VanLeer(x, y)  ((x) * (y) < 0 ? 0 : (Min3(2*fabs(x),2*fabs(y),0.5*(fabs(x)+fabs(y)))*Sign((x)+(y))))

#define E           M_E
#define Pi          M_PI

#define StepFunction(x) ((x)>0)
