#ifndef VECTORS_HH
#define VECTORS_HH



// Vectorisation

#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include <cctk.h>



// I: i,j: integer
// R: a,b: real
// V: x,y: vector (of real)
// P: p,q: pointer (i.e. const reference) to something
// L: l,m: L-value (i.e. non-const reference) to something

#define DEFINE_FUNCTION_PR_V(name,expr)         \
static inline                                   \
CCTK_REAL_VEC name (CCTK_REAL const& p)         \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_PRV(name,expr)                  \
static inline                                           \
void name (CCTK_REAL& p, CCTK_REAL_VEC const& x)        \
{                                                       \
  expr;                                                 \
}

#define DEFINE_FUNCTION_PVR(name,expr)                  \
static inline                                           \
void name (CCTK_REAL_VEC& p, CCTK_REAL const& a)        \
{                                                       \
  expr;                                                 \
}

#define DEFINE_FUNCTION_V_V(name,expr)          \
static inline                                   \
CCTK_REAL_VEC name (CCTK_REAL_VEC const& x)     \
  CCTK_ATTRIBUTE_PURE                           \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_V_R(name,expr)          \
static inline                                   \
CCTK_REAL name (CCTK_REAL_VEC const& x)         \
  CCTK_ATTRIBUTE_PURE                           \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_R_V(name,expr)          \
static inline                                   \
CCTK_REAL_VEC name (CCTK_REAL const& a)         \
  CCTK_ATTRIBUTE_PURE                           \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_VV_V(name,expr)                                 \
static inline                                                           \
CCTK_REAL_VEC name (CCTK_REAL_VEC const& x, CCTK_REAL_VEC const& y)     \
  CCTK_ATTRIBUTE_PURE                                                   \
{                                                                       \
  return expr;                                                          \
}

#define DEFINE_FUNCTION_VR_V(name,expr)                         \
static inline                                                   \
CCTK_REAL_VEC name (CCTK_REAL_VEC const& x, CCTK_REAL const& a) \
  CCTK_ATTRIBUTE_PURE                                           \
{                                                               \
  return expr;                                                  \
}

#define DEFINE_FUNCTION_RV_V(name,expr)                         \
static inline                                                   \
CCTK_REAL_VEC name (CCTK_REAL const& a, CCTK_REAL_VEC const& x) \
  CCTK_ATTRIBUTE_PURE                                           \
{                                                               \
  return expr;                                                  \
}

#define DEFINE_FUNCTION_RR_V(name,expr)                         \
static inline                                                   \
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
  V v;
  
  // Set a vector from scalars
  inline CCTK_REAL_VEC(S const& a, S const& b): v(_mm_set_pd(b,a)) { }

  // Set a vector from a scalar, replicating the scalar
  inline CCTK_REAL_VEC(S const& a): v(_mm_set1_pd(a)) { }
  
  // Convert from and to the underlying vector type
  inline CCTK_REAL_VEC(V const& v_): v(v_) { }
  inline operator V const() const { return v; }
  
  inline CCTK_REAL_VEC() { }
  
  // Copy constructor
  inline CCTK_REAL_VEC(CCTK_REAL_VEC const& x): v(x) { }
};

union vec_mask {
  unsigned long long bits[2];
  CCTK_REAL_VEC::V v;
};

DEFINE_FUNCTION_R_V(vec_set1,_mm_set1_pd(a))
DEFINE_FUNCTION_RR_V(vec_set,_mm_set_pd(b,a))

// Get a scalar from the vector
#if defined(__PGI) && defined (__amd64__)
// _mm_cvtsd_f64 does not exist on PGI compilers
static inline
CCTK_REAL vec_elt0 (CCTK_REAL_VEC const& x)
{
  CCTK_REAL a; _mm_store_sd(&a,x); return a;
}
#else
DEFINE_FUNCTION_V_R(vec_elt0,_mm_cvtsd_f64(x)) //this is a no-op
#endif

#if 0
DEFINE_FUNCTION_V_R(vec_elt1,vec_elt0(_mm_shuffle_pd(x,x,_MM_SHUFFLE2(1,1))))
#endif
static inline
CCTK_REAL vec_elt1 (CCTK_REAL_VEC const& x)
{
  CCTK_REAL a; _mm_storeh_pd(&a,x); return a;
}

// Load a vector from memory (aligned and unaligned); this loads from
// a reference to a scalar
DEFINE_FUNCTION_PR_V(vec_load,_mm_load_pd(&p))
DEFINE_FUNCTION_PR_V(vec_loadu,_mm_loadu_pd(&p))

#if 0
// Load a partial vector (duplicating the last loaded element to fill
// the remaining elements)
// TODO: Should this be aligned or unaligned?
static inline
CCTK_REAL_VEC vec_load_partial (CCTK_REAL const& p, int const n)
{
  switch (n) {
  case 1: return _mm_load1_pd(p);
  default: assert(0);
  }
}
#endif

// Load a vector from memory that may or may not be aligned, as
// decided by the offset off and the vector size
// Implementation: default to unaligned load
template<int mod>
DEFINE_FUNCTION_PR_V(vec_loadu_maybe_impl,vec_loadu(p))
template<int mod1,int mod2,int mod3>
DEFINE_FUNCTION_PR_V(vec_loadu_maybe_impl3,vec_loadu(p))
// Implementation: load aligned if the modulus is zero
template<>
inline
CCTK_REAL_VEC vec_loadu_maybe_impl<0> (CCTK_REAL const& p)
{
  return vec_load(p);
}
template<>
inline
CCTK_REAL_VEC vec_loadu_maybe_impl3<0,0,0> (CCTK_REAL const& p)
{
  return vec_load(p);
}
// Call the implementation with the modulus
template<int off>
static inline
CCTK_REAL_VEC vec_loadu_maybe (CCTK_REAL const& p)
{
  return vec_loadu_maybe_impl<off&(2-1)>(p);
}
template<int off1,int off2, int off3>
static inline
CCTK_REAL_VEC vec_loadu_maybe3 (CCTK_REAL const& p)
{
  return vec_loadu_maybe_impl3<off1&(2-1),off2&(2-1),off3&(2-1)>(p);
}

// Store a vector to memory (aligned and non-temporal); this stores to
// a reference to a scalar
DEFINE_FUNCTION_PRV(vec_store,_mm_store_pd(&p,x))
DEFINE_FUNCTION_PRV(vec_store_nta,_mm_stream_pd(&p,x))

// Store a lower or higher partial vector (aligned and non-temporal);
// the non-temporal hint is probably ignored
static inline
void vec_storel_partial (CCTK_REAL& p, CCTK_REAL_VEC const& x, int const n)
{
  switch (n) {
  case 1: _mm_storel_pd(&p,x); break;
  default: assert(0);
  }
}
static inline
void vec_storeh_partial (CCTK_REAL& p, CCTK_REAL_VEC const& x, int const n)
{
  switch (n) {
  case 1: _mm_storeh_pd((&p)+1,x); break;
  default: assert(0);
  }
}

// Double-argument operators, both vectors
DEFINE_FUNCTION_VV_V(operator+,_mm_add_pd(x,y))
DEFINE_FUNCTION_VV_V(operator-,_mm_sub_pd(x,y))
DEFINE_FUNCTION_VV_V(operator*,_mm_mul_pd(x,y))
DEFINE_FUNCTION_VV_V(operator/,_mm_div_pd(x,y))

// Double-argument operators, vector and scalar
DEFINE_FUNCTION_VR_V(operator+,x+vec_set1(a))
DEFINE_FUNCTION_VR_V(operator-,x-vec_set1(a))
DEFINE_FUNCTION_VR_V(operator*,x*vec_set1(a))
DEFINE_FUNCTION_VR_V(operator/,x/vec_set1(a))

// Double-argument operators, scalar and vector
DEFINE_FUNCTION_RV_V(operator+,vec_set1(a)+x)
DEFINE_FUNCTION_RV_V(operator-,vec_set1(a)-x)
DEFINE_FUNCTION_RV_V(operator*,vec_set1(a)*x)
DEFINE_FUNCTION_RV_V(operator/,vec_set1(a)/x)

// Single-argument operators
DEFINE_FUNCTION_V_V(operator+,x)
#if 0
DEFINE_FUNCTION_V_V(operator-,vec_set(0.0,0.0)-x)
#endif
static vec_mask const vec_neg_mask = 
{ { 0x8000000000000000ULL, 0x8000000000000000ULL } };
DEFINE_FUNCTION_V_V(operator-,_mm_xor_pd(x,vec_neg_mask.v))

// Cheap functions
static vec_mask const vec_fabs_mask =
{ { 0x7fffffffffffffffULL, 0x7fffffffffffffffULL } };
DEFINE_FUNCTION_V_V(fabs,_mm_and_pd(x,vec_fabs_mask.v))
DEFINE_FUNCTION_VV_V(fmax,_mm_max_pd(x,y))
DEFINE_FUNCTION_VV_V(fmin,_mm_min_pd(x,y))
DEFINE_FUNCTION_V_V(sqrt,_mm_sqrt_pd(x))

// Expensive functions
DEFINE_FUNCTION_V_V(exp,vec_set(exp(vec_elt0(x)),exp(vec_elt1(x))))
DEFINE_FUNCTION_V_V(log,vec_set(log(vec_elt0(x)),log(vec_elt1(x))))
DEFINE_FUNCTION_VR_V(pow,vec_set(pow(vec_elt0(x),a),pow(vec_elt1(x),a)))

// Special case for PGI to avoid internal compiler error
#if defined(__PGI) && defined (__amd64__)
#undef IfThen
CCTK_REAL_VEC IfThen (bool const cond, CCTK_REAL_VEC const& x, CCTK_REAL_VEC co\
nst& y)
{
  return cond*x + (not cond)*y;
}
#endif



#if 0
// Try to use the __m128d type directly.

// This does not really work, because it is not possible to define
// automatic conversion operators from double to __m128d, so that
// explicit conversions are required.  This makes the code look more
// clumsy.

// Vector type corresponding to CCTK_REAL
typedef __m128d CCTK_REAL_VEC;

DEFINE_FUNCTION_R_V(vec_set1,_mm_set1_pd(a))
DEFINE_FUNCTION_RR_V(vec_set,_mm_set_pd(b,a))

// Get a scalar from the vector
static inline
CCTK_REAL vec_elt0 (CCTK_REAL_VEC const& x)
{
#if 0
  // _mm_cvtsd_f64 does not exist on PGI compilers
  return _mm_cvtsd_f64(x);      // this is a no-op
#endif
  CCTK_REAL a; _mm_store_sd(&a,x); return a;
}

DEFINE_FUNCTION_V_R(vec_elt1,vec_elt0(_mm_shuffle_pd(x,x,_MM_SHUFFLE2(1,1))))

// Load a vector from memory (aligned and unaligned); this loads from
// a reference to a scalar
DEFINE_FUNCTION_PR_V(vec_load,_mm_load_pd(&p))
DEFINE_FUNCTION_PR_V(vec_loadu,_mm_loadu_pd(&p))

// Store a vector to memory (aligned and non-temporal); this stores to
// a reference to a scalar
DEFINE_FUNCTION_PRV(vec_store,_mm_store_pd(&p,x))
DEFINE_FUNCTION_PRV(vec_store_nta,_mm_stream_pd(&p,x))

// Cheap functions
static vec_mask const vec_fabs_mask =
{ { 0x7fffffffffffffffULL, 0x7fffffffffffffffULL } };
DEFINE_FUNCTION_V_V(fabs,_mm_and_pd(x,vec_fabs_mask.v))
DEFINE_FUNCTION_VV_V(fmax,_mm_max_pd(x,y))
DEFINE_FUNCTION_VV_V(fmin,_mm_min_pd(x,y))
DEFINE_FUNCTION_V_V(sqrt,_mm_sqrt_pd(x))

// Expensive functions
DEFINE_FUNCTION_V_V(exp,set(exp(vec_elt0(x)),exp(vec_elt1(x))))
DEFINE_FUNCTION_V_V(log,set(log(vec_elt0(x)),log(vec_elt1(x))))
DEFINE_FUNCTION_VR_V(pow,set(pow(vec_elt0(x),a),pow(vec_elt1(x),a)))

#endif



// Intel, float
#elif defined(KRANC_VECTORS) && defined(__SSE__) && defined(CCTK_REAL_PRECISION_4)

#include <xmmintrin.h>

// A vector type corresponding to CCTK_REAL
typedef __m128 CCTK_REAL_VEC;



// Power, double
#elif defined(KRANC_VECTORS) && defined(__ALTIVEC__) && defined(_ARCH_PWR7) && defined(CCTK_REAL_PRECISION_8)

#include <altivec.h>

// Vector type corresponding to CCTK_REAL
struct CCTK_REAL_VEC {
  // The underlying scalar and vector types
  typedef double S;
  typedef vector double V;
  V v;
  
  // vec_insert, vec_extract, vec_splat

  // Set a vector from scalars
  inline CCTK_REAL_VEC(S const& a, S const& b) { v[0]=a; v[1]=b; }
  
  // Set a vector from a scalar, replicating the scalar
  inline CCTK_REAL_VEC(S const& a): v(vec_splats(a)) { }
  
  // Convert from and to the underlying vector type
  inline CCTK_REAL_VEC(V const& v_): v(v_) { }
  inline operator V const() const { return v; }
  
  inline CCTK_REAL_VEC() { }
  
  // Copy constructor
  inline CCTK_REAL_VEC(CCTK_REAL_VEC const& x): v(x) { }
};

DEFINE_FUNCTION_R_V(vec_set1,CCTK_REAL_VEC(a))
DEFINE_FUNCTION_RR_V(vec_set,CCTK_REAL_VEC(a,b))

// Get a scalar from the vector
DEFINE_FUNCTION_V_R(vec_elt0,x.v[0])
DEFINE_FUNCTION_V_R(vec_elt1,x.v[1])

// Load a vector from memory (aligned and unaligned); this loads from
// a reference to a scalar
DEFINE_FUNCTION_PR_V(vec_load,p)
DEFINE_FUNCTION_PR_V(vec_loadu,vec_xld2(0,const_cast<CCTK_REAL*>(&p)))
// vec_xlds

// Load a vector from memory that may or may not be aligned, as
// decided by the offset off and the vector size
// Implementation: default to unaligned load
template<int mod>
DEFINE_FUNCTION_PR_V(vec_loadu_maybe_impl,vec_loadu(p))
// Implementation: load aligned if the modulus is zero
#define static
template<>
DEFINE_FUNCTION_PR_V(vec_loadu_maybe_impl<0>,vec_load(p))
#undef static
// Call the implementation with the modulus
template<int off>
DEFINE_FUNCTION_PR_V(vec_loadu_maybe,vec_loadu_maybe_impl<off&(2-1)>(p))

// Store a vector to memory (aligned and non-temporal); this stores to
// a reference to a scalar
DEFINE_FUNCTION_PRV(vec_store,*(CCTK_REAL_VEC::V*)&p=x)
DEFINE_FUNCTION_PRV(vec_store_nta,*(CCTK_REAL_VEC::V*)&p=x)

// Store a lower or higher partial vector (aligned and non-temporal);
// the non-temporal hint is probably ignored
static inline
void vec_storel_partial (CCTK_REAL& p, CCTK_REAL_VEC const& x, int const n)
{
  switch (n) {
  case 1: p=x.v[0]; break;
  default: assert(0);
  }
}
static inline
void vec_storeh_partial (CCTK_REAL& p, CCTK_REAL_VEC const& x, int const n)
{
  switch (n) {
  case 1: (&p)[1]=x.v[1]; break;
  default: assert(0);
  }
}

// Double-argument operators, both vectors
DEFINE_FUNCTION_VV_V(operator+,vec_add(x,y))
DEFINE_FUNCTION_VV_V(operator-,vec_sub(x,y))
DEFINE_FUNCTION_VV_V(operator*,vec_mul(x,y))
DEFINE_FUNCTION_VV_V(operator/,vec_div(x,y))

// Double-argument operators, vector and scalar
DEFINE_FUNCTION_VR_V(operator+,x+vec_set1(a))
DEFINE_FUNCTION_VR_V(operator-,x-vec_set1(a))
DEFINE_FUNCTION_VR_V(operator*,x*vec_set1(a))
DEFINE_FUNCTION_VR_V(operator/,x/vec_set1(a))

// Double-argument operators, scalar and vector
DEFINE_FUNCTION_RV_V(operator+,vec_set1(a)+x)
DEFINE_FUNCTION_RV_V(operator-,vec_set1(a)-x)
DEFINE_FUNCTION_RV_V(operator*,vec_set1(a)*x)
DEFINE_FUNCTION_RV_V(operator/,vec_set1(a)/x)

// Single-argument operators
DEFINE_FUNCTION_V_V(operator+,x)
DEFINE_FUNCTION_V_V(operator-,vec_neg(x))

// Cheap functions
DEFINE_FUNCTION_V_V(fabs,vec_abs(x))
DEFINE_FUNCTION_VV_V(fmax,vec_max(x,y))
DEFINE_FUNCTION_VV_V(fmin,vec_min(x,y))

// Expensive functions
DEFINE_FUNCTION_V_V(exp,vec_set(exp(vec_elt0(x)),exp(vec_elt1(x))))
DEFINE_FUNCTION_V_V(log,vec_set(log(vec_elt0(x)),log(vec_elt1(x))))
DEFINE_FUNCTION_VR_V(pow,vec_set(pow(vec_elt0(x),a),pow(vec_elt1(x),a)))
DEFINE_FUNCTION_V_V(sqrt,vec_set(sqrt(vec_elt0(x)),sqrt(vec_elt1(x))))



// Fallback: pseudo-vectorisation
#elif 0

// There is no vector type corresponding to CCTK_REAL
struct CCTK_REAL_VEC {
  // The underlying scalar and vector types
  CCTK_REAL v, w;
  
  // Set a vector from scalars
  inline CCTK_REAL_VEC(CCTK_REAL const& a, CCTK_REAL const& b): v(a), w(b) { }

  // Set a vector from a scalar, replicating the scalar
  inline CCTK_REAL_VEC(CCTK_REAL const& a): v(a), w(a) { }
  
  inline CCTK_REAL_VEC() { }
  
  // Copy constructor
  inline CCTK_REAL_VEC(CCTK_REAL_VEC const& x): v(x.v), w(x.w) { }
};



DEFINE_FUNCTION_PR_V(vec_load,*(CCTK_REAL_VEC const* restrict)&p)
DEFINE_FUNCTION_PR_V(vec_loadu,vec_load(p))
// Load a vector from memory that may or may not be aligned, as
// decided by the offset off and the vector size
template<int off>
DEFINE_FUNCTION_PR_V(vec_loadm,vec_load(p))

DEFINE_FUNCTION_PRV(vec_store,*(CCTK_REAL_VEC* restrict)&p=x)
DEFINE_FUNCTION_PRV(vec_store_nta,vec_store(p,x))

// Double-argument operators, both vectors
DEFINE_FUNCTION_VV_V(operator+,CCTK_REAL_VEC(x.v+y.v,x.w+y.w))
DEFINE_FUNCTION_VV_V(operator-,CCTK_REAL_VEC(x.v-y.v,x.w-y.w))
DEFINE_FUNCTION_VV_V(operator*,CCTK_REAL_VEC(x.v*y.v,x.w*y.w))
DEFINE_FUNCTION_VV_V(operator/,CCTK_REAL_VEC(x.v/y.v,x.w/y.w))

// Double-argument operators, vector and scalar
DEFINE_FUNCTION_VR_V(operator+,CCTK_REAL_VEC(x.v+a,x.w+a))
DEFINE_FUNCTION_VR_V(operator-,CCTK_REAL_VEC(x.v-a,x.w-a))
DEFINE_FUNCTION_VR_V(operator*,CCTK_REAL_VEC(x.v*a,x.w*a))
DEFINE_FUNCTION_VR_V(operator/,CCTK_REAL_VEC(x.v/a,x.w/a))

// Double-argument operators, scalar and vector
DEFINE_FUNCTION_RV_V(operator+,CCTK_REAL_VEC(a+x.v,a+x.w))
DEFINE_FUNCTION_RV_V(operator-,CCTK_REAL_VEC(a-x.v,a-x.w))
DEFINE_FUNCTION_RV_V(operator*,CCTK_REAL_VEC(a*x.v,a*x.w))
DEFINE_FUNCTION_RV_V(operator/,CCTK_REAL_VEC(a/x.v,a/x.w))

// Single-argument operators
DEFINE_FUNCTION_V_V(operator+,x)
DEFINE_FUNCTION_V_V(operator-,CCTK_REAL_VEC(-x.v,-x.w))

// Cheap functions
DEFINE_FUNCTION_V_V(fabs,CCTK_REAL_VEC(fabs(x.v),fabs(x.w)))
DEFINE_FUNCTION_VV_V(fmax,CCTK_REAL_VEC(fmax(x.v,y.v),fmax(x.w,y.w)))
DEFINE_FUNCTION_VV_V(fmin,CCTK_REAL_VEC(fmin(x.v,y.v),fmin(x.w,y.w)))
DEFINE_FUNCTION_V_V(sqrt,CCTK_REAL_VEC(sqrt(x.v),sqrt(x.w)))

// Expensive functions
DEFINE_FUNCTION_V_V(exp,CCTK_REAL_VEC(exp(x.v),exp(x.w)))
DEFINE_FUNCTION_V_V(log,CCTK_REAL_VEC(log(x.v),log(x.w)))
DEFINE_FUNCTION_VR_V(pow,CCTK_REAL_VEC(pow(x.v,a),pow(x.w,a)))



// Fallback: no vectorisation
#else

// There is no vector type corresponding to CCTK_REAL
typedef CCTK_REAL CCTK_REAL_VEC;



DEFINE_FUNCTION_PR_V(vec_load,p)
DEFINE_FUNCTION_PR_V(vec_loadu,p)
// Load a vector from memory that may or may not be aligned, as
// decided by the offset off and the vector size
// Implementation: default to unaligned load
template<int off>
DEFINE_FUNCTION_PR_V(vec_loadu_maybe,p)
template<int off1,int off2, int off3>
DEFINE_FUNCTION_PR_V(vec_loadu_maybe3,p)

DEFINE_FUNCTION_PRV(vec_store,p=x)
DEFINE_FUNCTION_PRV(vec_store_nta,p=x)

// Store a lower or higher partial vector (aligned and non-temporal);
// the non-temporal hint is probably ignored
static inline
void vec_storel_partial (CCTK_REAL& p, CCTK_REAL_VEC const& x, int const n)
{
  assert(0);
}
static inline
void vec_storeh_partial (CCTK_REAL& p, CCTK_REAL_VEC const& x, int const n)
{
  assert(0);
}



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
int const CCTK_REAL_VEC_SIZE = sizeof(CCTK_REAL_VEC) / sizeof(CCTK_REAL);



#endif  // #ifndef VECTORS_HH
