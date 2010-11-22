// Vectorise using Intel's or AMD's SSE2

// Use the type __m128d directly, without introducing a wrapper class
// Use macros instead of inline functions



#include <emmintrin.h>

// Vector type corresponding to CCTK_REAL
typedef __m128d CCTK_REAL_VEC;

// Number of vector elements in a CCTK_REAL_VEC
static
int const CCTK_REAL_VEC_SIZE = sizeof(CCTK_REAL_VEC) / sizeof(CCTK_REAL);



// Create vectors, extract vector elements

#define vec_set1(a) (_mm_set1_pd(a))
#define vec_set(a,b) (_mm_set_pd(b,a))

// Get a scalar from the vector
#if defined(__PGI) && defined (__amd64__)
// _mm_cvtsd_f64 does not exist on PGI compilers
// #  define vec_elt0(x) (*(CCTK_REAL const*)&(x))
#  define vec_elt0(x) ({ CCTK_REAL a_elt0; asm ("" : "=x" (a_elt0) : "0" (x)); a_elt0; })
#else
// this is a no-op
#  define vec_elt0(x) (_mm_cvtsd_f64(x))
#endif
#define vec_elt1(x_) ({ CCTK_REAL_VEC const x_elt1=(x_); vec_elt0(_mm_unpackhi_pd(x_elt1,x_elt1)); })



// Load and store vectors

// Load a vector from memory (aligned and unaligned); this loads from
// a reference to a scalar
#define vec_load(p)  (_mm_load_pd(&(p)))
#define vec_loadu(p) (_mm_loadu_pd(&(p)))

// Load a vector from memory that may or may not be aligned, as
// decided by the offset off and the vector size
// Implementation: Always use unaligned load
#define vec_loadu_maybe(off,p) (vec_loadu(p))
#define vec_loadu_maybe3(off1,off2,off3,p) (vec_loadu(p))
#if 0
#define vec_loadu_maybe(off,p)                  \
  (!((off)&(CCTK_REAL_VEC_SIZE-1)) ?            \
   vec_load(p) : vec_loadu(p))
#define vec_loadu_maybe3(off1,off2,off3,p)      \
  (!((off1)&(CCTK_REAL_VEC_SIZE-1)) &&          \
   !((off2)&(CCTK_REAL_VEC_SIZE-1)) &&          \
   !((off3)&(CCTK_REAL_VEC_SIZE-1)) ?           \
   vec_load(p) : vec_loadu(p))
#endif

// Store a vector to memory (aligned and non-temporal); this stores to
// a reference to a scalar
#define vec_store(p,x) (_mm_store_pd(&(p),x))
#define vec_storeu(p,x) (_mm_storeu_pd(&(p),x))
#if defined(KRANC_CACHE)
#  define vec_store_nta(p,x) (_mm_stream_pd(&(p),x))
#else
#  define vec_store_nta(p,x) (_mm_store_pd(&(p),x))
#endif

// Store a lower or higher partial vector (aligned and non-temporal);
// the non-temporal hint is probably ignored
#define vec_store_nta_partial_lo(p,x,n) (_mm_storel_pd(&(p),x))
#define vec_store_nta_partial_hi(p,x,n) (_mm_storeh_pd((&(p))+1,x))



// Functions and operators

// Operators
#undef fneg
#undef fmul
#undef fdiv
#undef fadd
#undef fsub
#if defined(__PGI) && defined (__amd64__)
// The PGI compiler does not understand __m128d literals
static union {
  unsigned long long s[CCTK_REAL_VEC_SIZE];
  CCTK_REAL_VEC v;
} vec_neg_mask_impl = {0x8000000000000000ULL, 0x8000000000000000ULL};
#  define vec_neg_mask (vec_neg_mask_impl.v)
#else
#  define vec_neg_mask ((CCTK_REAL_VEC)(__m128i){0x8000000000000000ULL, 0x8000000000000000ULL})
#endif
#define fneg(x)   (_mm_xor_pd(x,vec_neg_mask))
#define fmul(x,y) (_mm_mul_pd(x,y))
#define fdiv(x,y) (_mm_div_pd(x,y))
#define fadd(x,y) (_mm_add_pd(x,y))
#define fsub(x,y) (_mm_sub_pd(x,y))

// Cheap functions
#undef kfabs
#undef kfmax
#undef kfmin
#undef ksqrt
#if defined(__PGI) && defined (__amd64__)
// The PGI compiler does not understand __m128d literals
static union {
  unsigned long long s[CCTK_REAL_VEC_SIZE];
  CCTK_REAL_VEC v;
} vec_fabs_mask_impl = {0x7fffffffffffffffULL, 0x7fffffffffffffffULL};
#  define vec_fabs_mask (vec_fabs_mask_impl.v)
#else
#  define vec_fabs_mask ((CCTK_REAL_VEC)(__m128i){0x7fffffffffffffffULL, 0x7fffffffffffffffULL})
#endif
#define kfabs(x)   (_mm_and_pd(x,vec_fabs_mask))
#define kfmax(x,y) (_mm_max_pd(x,y))
#define kfmin(x,y) (_mm_min_pd(x,y))
#define ksqrt(x)   (_mm_sqrt_pd(x))

// Expensive functions
#undef kexp
#undef klog
#undef kpow
#define kexp(x_)    ({ CCTK_REAL_VEC const x_exp=(x_); vec_set(exp(vec_elt0(x_exp)),exp(vec_elt1(x_exp))); })
#define klog(x_)    ({ CCTK_REAL_VEC const x_log=(x_); vec_set(log(vec_elt0(x_log)),log(vec_elt1(x_log))); })
#define kpow(x_,a_) ({ CCTK_REAL_VEC const x_pow=(x_); CCTK_REAL const a_pow=(a_); vec_set(pow(vec_elt0(x_pow),a_pow),pow(vec_elt1(x_pow),a_pow)); })



#undef Sign
#define Sign(x) (42)

#undef ToReal
#define ToReal(x) (vec_set1(x))
