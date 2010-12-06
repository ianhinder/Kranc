// Vectorise using Intel's or AMD's SSE2



#include <emmintrin.h>

// Vector type corresponding to CCTK_REAL
struct CCTK_REAL_VEC {
  // Underlying scalar and vector types
  typedef double S;
  typedef __m128d V;
  
  // Payload
  V v;
  
  // Empty constructur
  inline CCTK_REAL_VEC() { }
  
  // Convert from and to the underlying vector type
  inline CCTK_REAL_VEC(V const v_): v(v_) { }
  inline operator V const() const { return v; }
  
  // Convert from the underlying scalar type
  inline CCTK_REAL_VEC(S const& a): v(_mm_set1_pd(a)) { }
  inline CCTK_REAL_VEC(int const& a): v(_mm_set1_pd(S(a))) { }
  
  // Copy constructor
  inline CCTK_REAL_VEC(CCTK_REAL_VEC const& x): v(x) { }
};

// Number of vector elements in a CCTK_REAL_VEC
static
int const CCTK_REAL_VEC_SIZE = sizeof(CCTK_REAL_VEC) / sizeof(CCTK_REAL);



// Create vectors, extract vector elements

DEFINE_FUNCTION_R_V(vec_set1,_mm_set1_pd(a))
DEFINE_FUNCTION_RR_V(vec_set,_mm_set_pd(b,a))

// Get a scalar from the vector
#if defined(__PGI) && defined (__amd64__)
// _mm_cvtsd_f64 does not exist on PGI compilers
// DEFINE_FUNCTION_V_R(vec_elt0,({ CCTK_REAL a; _mm_store_sd(&a,x); a; }))
// DEFINE_FUNCTION_V_R(vec_elt0,(*(CCTK_REAL const*)&x))
// This generates the fastest code with PGI compilers
DEFINE_FUNCTION_V_R(vec_elt0,({ CCTK_REAL a; asm ("" : "=x" (a) : "0" (x)); a; }))
#else
DEFINE_FUNCTION_V_R(vec_elt0,_mm_cvtsd_f64(x)) // this is a no-op
#endif
DEFINE_FUNCTION_V_R(vec_elt1,vec_elt0(_mm_unpackhi_pd(x,x)))



// Load and store vectors

// Load a vector from memory (aligned and unaligned); this loads from
// a reference to a scalar
DEFINE_FUNCTION_PR_V(vec_load,_mm_load_pd(&p))
DEFINE_FUNCTION_PR_V(vec_loadu,_mm_loadu_pd(&p))

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
#define vec_loadu_maybe(off,p)                                  \
  (vec_loadu_maybe_impl<(off)&(CCTK_REAL_VEC_SIZE-1>(p)))
#define vec_loadu_maybe3(off1,off2,off3,p)                      \
  (vec_loadu_maybe_impl3<(off1)&(CCTK_REAL_VEC_SIZE-1),         \
                         (off2)&(CCTK_REAL_VEC_SIZE-1),         \
                         (off3)&(CCTK_REAL_VEC_SIZE-1)>(p))

// Store a vector to memory (aligned and non-temporal); this stores to
// a reference to a scalar
DEFINE_FUNCTION_PRV(vec_store,_mm_store_pd(&p,x))
DEFINE_FUNCTION_PRV(vec_storeu,_mm_storeu_pd(&p,x))
#if defined(KRANC_CACHE)
DEFINE_FUNCTION_PRV(vec_store_nta,_mm_stream_pd(&p,x))
#else
DEFINE_FUNCTION_PRV(vec_store_nta,_mm_store_pd(&p,x))
#endif

// Store a lower or higher partial vector (aligned and non-temporal);
// the non-temporal hint is probably ignored
static inline
void vec_store_nta_partial_lo (CCTK_REAL& p, CCTK_REAL_VEC const x, int const n)
{
  switch (n) {
  case 1: _mm_storel_pd(&p,x); break;
  default: assert(0);
  }
}
static inline
void vec_store_nta_partial_hi (CCTK_REAL& p, CCTK_REAL_VEC const x, int const n)
{
  switch (n) {
  case 1: _mm_storeh_pd((&p)+1,x); break;
  default: assert(0);
  }
}



// Functions and operators

// Single-argument operators
#if 0
DEFINE_FUNCTION_V_V(operator+,x)
static CCTK_REAL_VEC const vec_neg_mask =
  (CCTK_REAL_VEC::V)(__m128i) { 0x8000000000000000ULL, 0x8000000000000000ULL };
DEFINE_FUNCTION_V_V(operator-,_mm_xor_pd(x,vec_neg_mask))
#endif
DEFINE_FUNCTION_V_V(operator+,+x.v)
DEFINE_FUNCTION_V_V(operator-,-x.v)

// Double-argument operators, both vectors
#if 0
DEFINE_FUNCTION_VV_V(operator+,_mm_add_pd(x,y))
DEFINE_FUNCTION_VV_V(operator-,_mm_sub_pd(x,y))
DEFINE_FUNCTION_VV_V(operator*,_mm_mul_pd(x,y))
DEFINE_FUNCTION_VV_V(operator/,_mm_div_pd(x,y))
#endif
DEFINE_FUNCTION_VV_V(operator+,x.v+y.v)
DEFINE_FUNCTION_VV_V(operator-,x.v-y.v)
DEFINE_FUNCTION_VV_V(operator*,x.v*y.v)
DEFINE_FUNCTION_VV_V(operator/,x.v/y.v)

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

// Cheap functions
#if defined(__PGI) && defined (__amd64__)
// The PGI compiler does not understand __m128d literals
static union {
  CCTK_REAL_VEC::S s[CCTK_REAL_VEC_SIZE];
  CCTK_REAL_VEC::V v;
} vec_fabs_mask_impl = { 0x7fffffffffffffffULL, 0x7fffffffffffffffULL };
#  define vec_fabs_mask (vec_fabs_mask_impl.v)
#else
static CCTK_REAL_VEC const vec_fabs_mask =
  (CCTK_REAL_VEC::V)(__m128i) { 0x7fffffffffffffffULL, 0x7fffffffffffffffULL };
#endif
DEFINE_FUNCTION_V_V(fabs,_mm_and_pd(x,vec_fabs_mask))
DEFINE_FUNCTION_VV_V(fmax,_mm_max_pd(x,y))
DEFINE_FUNCTION_VV_V(fmin,_mm_min_pd(x,y))
DEFINE_FUNCTION_V_V(sqrt,_mm_sqrt_pd(x))

// Expensive functions
DEFINE_FUNCTION_V_V(exp,vec_set(exp(vec_elt0(x)),exp(vec_elt1(x))))
DEFINE_FUNCTION_V_V(log,vec_set(log(vec_elt0(x)),log(vec_elt1(x))))
DEFINE_FUNCTION_VR_V(pow,vec_set(pow(vec_elt0(x),a),pow(vec_elt1(x),a)))



#undef Sign
#define Sign(x) (42)

// #undef ToReal
// #define ToReal(x) vec_set1(x)

#if defined(__PGI) && defined (__amd64__)
// Special case for PGI 9.0.4 to avoid an internal compiler error
#undef IfThen
static inline
CCTK_REAL_VEC IfThen (bool const cond, CCTK_REAL_VEC const x, CCTK_REAL_VEC const y)
{
  union {
    __m128i vi;
    CCTK_REAL_VEC::V v;
  } mask;
  mask.vi = _mm_set1_epi64x(-(long long)cond);
  return _mm_or_pd(_mm_and_pd(x.v, mask.v), _mm_andnot_pd(mask.v, y.v));
}
#endif
