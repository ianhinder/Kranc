// Vectorise using IBM's Altivec

// Use the type vector double directly, without introducing a wrapper class
// Use macros instead of inline functions



#include <altivec.h>

// Vector type corresponding to CCTK_REAL
typedef vector double CCTK_REAL_VEC;

// Number of vector elements in a CCTK_REAL_VEC
static
int const CCTK_REAL_VEC_SIZE = sizeof(CCTK_REAL_VEC) / sizeof(CCTK_REAL);



// Create vectors, extract vector elements

#define vec_set1(a) (vec_splats(a))
#if defined(__GNUC__)
// GNU doesn't support array indices on vectors
union vec_mask {
  double elts[2];
  vector double v;
};
#  define vec_set(a,b) ({ vec_mask x_set; x_set.elts[0]=(a); x_set.elts[1]=(b); x_set.v; })
#else
#  define vec_set(a,b) ({ CCTK_REAL_VEC x_set; x_set[0]=(a); x_set[1]=(b); x_set; })
#endif

// Get a scalar from the vector
#if defined(__GNUC__)
// GNU doesn't support array indices on vectors
#  define vec_elt0(x) ({ vec_mask x_elt0; x_elt0.v=(x); x_elt0.elts[0]; })
#  define vec_elt1(x) ({ vec_mask x_elt1; x_elt1.v=(x); x_elt1.elts[1]; })
#else
#  define vec_elt0(x) ((x)[0])
#  define vec_elt1(x) ((x)[1])
#endif



// Load and store vectors

// Load a vector from memory (aligned and unaligned); this loads from
// a reference to a scalar
#define vec_load(p)  (*(CCTK_REAL_VEC const*)&(p))
#define vec_loadu(p) (vec_load(p))

// Load a vector from memory that may or may not be aligned, as
// decided by the offset off and the vector size
#define vec_loadu_maybe(off,p) (vec_load(p))
#define vec_loadu_maybe3(off1,off2,off3,p) (vec_load(p))

// Store a vector to memory (aligned and non-temporal); this stores to
// a reference to a scalar
#define vec_store(p,x) (*(CCTK_REAL_VEC*)&(p)=(x))
#define vec_storeu(p,x) (*(CCTK_REAL_VEC*)&(p)=(x))
// TODO: Use stvxl instruction?
#define vec_store_nta(p,x) vec_store(p,x)

// Store a lower or higher partial vector (aligned and non-temporal);
// the non-temporal hint is probably ignored
#define vec_store_nta_partial_lo(p,x,n) ((p)=vec_elt0(x))
#define vec_store_nta_partial_hi(p,x,n) ((&(p))[1]=vec_elt1(x))



// Functions and operators

// Other Altivec functions are:
//    nabs: -abs a
//    madd msub nmadd nmsub: [+-]a*b[+-]c

// Triple-argument operators, all vectors
#undef fmadd
#undef fmsub
#undef fnmadd
#undef fnmsub
#define fmadd(x,y,z)  (vec_madd(x,y,z))
#define fmsub(x,y,z)  (vec_msub(x,y,z))
#define fnmadd(x,y,z) (vec_nmadd(x,y,z))
#define fnmsub(x,y,z) (vec_nmsub(x,y,z))

// Cheap functions
#undef kfabs
#undef kfmax
#undef kfmin
#define kfabs(x)   (vec_abs(x))
#define kfmax(x,y) (vec_max(x,y))
#define kfmin(x,y) (vec_min(x,y))

// Expensive functions
#undef kexp
#undef klog
#undef kpow
#undef ksqrt
#define kexp(x_)    ({ CCTK_REAL_VEC const x_exp=(x_); vec_set(exp(vec_elt0(x_exp)),exp(vec_elt1(x_exp))); })
#define klog(x_)    ({ CCTK_REAL_VEC const x_log=(x_); vec_set(log(vec_elt0(x_log)),log(vec_elt1(x_log))); })
#define kpow(x_,a_) ({ CCTK_REAL_VEC const x_pow=(x_); CCTK_REAL const a_pow=(a_); vec_set(pow(vec_elt0(x_pow),a_pow),pow(vec_elt1(x_pow),a_pow)); })
#define ksqrt(x_)   ({ CCTK_REAL_VEC const x_sqrt=(x_); vec_set(sqrt(vec_elt0(x_sqrt)),sqrt(vec_elt1(x_sqrt))); })



#undef Sign
#define Sign(x) (42)

#undef ToReal
#define ToReal(x) (vec_set1((CCTK_REAL)(x)))
