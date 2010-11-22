#ifndef VECTORS_HH
#define VECTORS_HH



// Vectorisation

#include <math.h>
#include <stdlib.h>
#include <cctk.h>



// I: i,j: integer
// R: a,b: real
// V: x,y: vector (of real)
// P: p,q: pointer (i.e. const reference) to something
// L: l,m: L-value (i.e. non-const reference) to something

#define DEFINE_FUNCTION_PR_V(name,expr)         \
inline                                          \
CCTK_REAL_VEC name (CCTK_REAL const& p)         \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_PRV(name,expr)                  \
inline                                                  \
void name (CCTK_REAL& p, CCTK_REAL_VEC const& x)        \
{                                                       \
  expr;                                                 \
}

#define DEFINE_FUNCTION_PVR(name,expr)                  \
inline                                                  \
void name (CCTK_REAL_VEC& p, CCTK_REAL const& a)        \
{                                                       \
  expr;                                                 \
}

#define DEFINE_FUNCTION_V_V(name,expr)          \
inline                                          \
CCTK_REAL_VEC name (CCTK_REAL_VEC const& x)     \
  CCTK_ATTRIBUTE_PURE                           \
{                                               \
  return CCTK_REAL_VEC(expr);                   \
}

#define DEFINE_FUNCTION_V_R(name,expr)          \
inline                                          \
CCTK_REAL name (CCTK_REAL_VEC const& x)         \
  CCTK_ATTRIBUTE_PURE                           \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_R_V(name,expr)          \
inline                                          \
CCTK_REAL_VEC name (CCTK_REAL const& a)         \
  CCTK_ATTRIBUTE_PURE                           \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_VV_V(name,expr)                                 \
inline                                                                  \
CCTK_REAL_VEC name (CCTK_REAL_VEC const& x, CCTK_REAL_VEC const& y)     \
  CCTK_ATTRIBUTE_PURE                                                   \
{                                                                       \
  return expr;                                                          \
}

#define DEFINE_FUNCTION_VR_V(name,expr)                         \
inline                                                          \
CCTK_REAL_VEC name (CCTK_REAL_VEC const& x, CCTK_REAL const& a) \
  CCTK_ATTRIBUTE_PURE                                           \
{                                                               \
  return expr;                                                  \
}

#define DEFINE_FUNCTION_RV_V(name,expr)                         \
inline                                                          \
CCTK_REAL_VEC name (CCTK_REAL const& a, CCTK_REAL_VEC const& x) \
  CCTK_ATTRIBUTE_PURE                                           \
{                                                               \
  return expr;                                                  \
}

#define DEFINE_FUNCTION_RR_V(name,expr)                         \
inline                                                          \
CCTK_REAL_VEC name (CCTK_REAL const& a, CCTK_REAL const& b)     \
  CCTK_ATTRIBUTE_PURE                                           \
{                                                               \
  return expr;                                                  \
}



// Intel, double
#if defined(KRANC_VECTORS) && defined(__SSE2__) && defined(CCTK_REAL_PRECISION_8)

#include <emmintrin.h>

// Vector type corresponding to CCTK_REAL
struct CCTK_REAL_VEC {
  // The underlying scalar and vector types
  typedef double S;
  typedef __m128d V;
  static int const n = sizeof(V)/sizeof(S);
  V v;
  
  // Set a vector from scalars
  CCTK_REAL_VEC(S const& a, S const& b): v(_mm_set_pd(a,b)) { };
  
  // Get a scalar from the vector
  S elt0() const { return _mm_cvtsd_f64(v); /* this is a no-op */ }
  S elt1() const { return _mm_cvtsd_f64(_mm_shuffle_pd(v,v,_MM_SHUFFLE2(1,1))); }
  
  // Set a vector from a scalar, replicating the scalar
  CCTK_REAL_VEC(S const& a): v(_mm_set1_pd(a)) { };
  
  // Convert from and to the underlying vector type
  CCTK_REAL_VEC(V const& v_): v(v_) { };
  operator V const() const { return v; }
  
  CCTK_REAL_VEC() { };
  
  // Copy constructor
  CCTK_REAL_VEC(CCTK_REAL_VEC const& x): v(x) { };
};

// Load a vector from memory (aligned and unaligned); this loads from
// a reference to a scalar
DEFINE_FUNCTION_PR_V(vec_load,_mm_load_pd(&p));
DEFINE_FUNCTION_PR_V(vec_loadu,_mm_loadu_pd(&p));

// Store a vector to memory (aligned and non-temporal); this stores to
// a reference to a scalar
DEFINE_FUNCTION_PRV(vec_store,_mm_store_pd(&p,x))
DEFINE_FUNCTION_PRV(vec_store_nta,_mm_stream_pd(&p,x))

// Double-argument operators, both vectors
DEFINE_FUNCTION_VV_V(operator+,_mm_add_pd(x,y))
DEFINE_FUNCTION_VV_V(operator-,_mm_sub_pd(x,y))
DEFINE_FUNCTION_VV_V(operator*,_mm_mul_pd(x,y))
DEFINE_FUNCTION_VV_V(operator/,_mm_div_pd(x,y))

// Double-argument operators, vector and scalar
DEFINE_FUNCTION_VR_V(operator+,x+CCTK_REAL_VEC(a))
DEFINE_FUNCTION_VR_V(operator-,x-CCTK_REAL_VEC(a))
DEFINE_FUNCTION_VR_V(operator*,x*CCTK_REAL_VEC(a))
DEFINE_FUNCTION_VR_V(operator/,x/CCTK_REAL_VEC(a))

// Double-argument operators, scalar and vector
DEFINE_FUNCTION_RV_V(operator+,CCTK_REAL_VEC(a)+x)
DEFINE_FUNCTION_RV_V(operator-,CCTK_REAL_VEC(a)-x)
DEFINE_FUNCTION_RV_V(operator*,CCTK_REAL_VEC(a)*x)
DEFINE_FUNCTION_RV_V(operator/,CCTK_REAL_VEC(a)/x)

// Single-argument operators
DEFINE_FUNCTION_V_V(operator+,x)
DEFINE_FUNCTION_V_V(operator-,0.0-x)

// Cheap functions
static union {
  unsigned long long const bits[2];
  CCTK_REAL_VEC::V v;
} const fabs_mask = 
  { { 0x7fffffffffffffffULL, 0x7fffffffffffffffULL } };
DEFINE_FUNCTION_V_V(fabs,_mm_and_pd(x,fabs_mask.v))
DEFINE_FUNCTION_VV_V(fmax,_mm_max_pd(x,y))
DEFINE_FUNCTION_VV_V(fmin,_mm_min_pd(x,y))
DEFINE_FUNCTION_V_V(sqrt,_mm_sqrt_pd(x))

// Expensive functions
DEFINE_FUNCTION_V_V(exp,CCTK_REAL_VEC(exp(x.elt0()),exp(x.elt1())))
DEFINE_FUNCTION_V_V(log,CCTK_REAL_VEC(log(x.elt0()),log(x.elt1())))
DEFINE_FUNCTION_VR_V(pow,CCTK_REAL_VEC(pow(x.elt0(),a),pow(x.elt1(),a)))

// Un-implemented functions
DEFINE_FUNCTION_V_R(signbit,0)



#if 0
// Intel, float
#elif defined(KRANC_VECTORS) && defined(__SSE__) && defined(CCTK_REAL_PRECISION_4)

#include <xmmintrin.h>

// A vector type corresponding to CCTK_REAL
typedef __m128 CCTK_REAL_VEC;
#endif



// Fallback: no vectorisation
#else

// There is no vector type corresponding to CCTK_REAL
typedef CCTK_REAL CCTK_REAL_VEC;



DEFINE_FUNCTION_PR_V(vec_load,p)
DEFINE_FUNCTION_PR_V(vec_loadu,p)

DEFINE_FUNCTION_PRV(vec_store,p=x)
DEFINE_FUNCTION_PRV(vec_store_nta,p=x)

DEFINE_FUNCTION_V_R(signbit,x<0)



#endif



#undef DEFINE_FUNCTION_PR_V
#undef DEFINE_FUNCTION_PRV
#undef DEFINE_FUNCTION_V_V
#undef DEFINE_FUNCTION_R_V
#undef DEFINE_FUNCTION_VV_V
#undef DEFINE_FUNCTION_VR_V
#undef DEFINE_FUNCTION_RV_V
#undef DEFINE_FUNCTION_RR_V



// Number of vector elements in a CCTK_REAL_VEC
static
size_t const CCTK_REAL_VEC_SIZE = sizeof(CCTK_REAL_VEC) / sizeof(CCTK_REAL);



#endif  // #ifndef VECTORS_HH
