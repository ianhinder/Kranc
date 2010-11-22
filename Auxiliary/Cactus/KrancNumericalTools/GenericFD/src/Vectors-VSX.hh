// Vectorise using IBM's Altivec



#include <altivec.h>

// Vector type corresponding to CCTK_REAL
struct CCTK_REAL_VEC {
  // The underlying scalar and vector types
  typedef double S;
  typedef vector double V;
  V v;
  union vec_mask {
    S elts[2];
    V v;
  };
  
  // Set a vector from scalars
#if 0
  // IBM
  inline CCTK_REAL_VEC(S const a, S const b) { v[0]=a; v[1]=b; }
#endif
#if 0
  inline CCTK_REAL_VEC(S const a, S const b):
    v(vec_mergel(vec_splats(a), vec_splats(b))) { }
#endif
  inline CCTK_REAL_VEC(S const a, S const b)
  {
    vec_mask x;
    x.elts[0] = a;
    x.elts[1] = b;
    v = x.v;
  }
  
  // Set a vector from a scalar, replicating the scalar
  // Note: Could also use vec_xlds instead
  inline CCTK_REAL_VEC(S const a): v(vec_splats(a)) { }
  
  // Convert from and to the underlying vector type
  inline CCTK_REAL_VEC(V const v_): v(v_) { }
  inline operator V const() const { return v; }
  
  inline CCTK_REAL_VEC() { }
  
  // Copy constructor
  inline CCTK_REAL_VEC(CCTK_REAL_VEC const& x): v(x) { }
};

// Number of vector elements in a CCTK_REAL_VEC
static
int const CCTK_REAL_VEC_SIZE = sizeof(CCTK_REAL_VEC) / sizeof(CCTK_REAL);



// Create vectors, extract vector elements
DEFINE_FUNCTION_R_V(vec_set1,CCTK_REAL_VEC(a))
DEFINE_FUNCTION_RR_V(vec_set,CCTK_REAL_VEC(a,b))

// Get a scalar from the vector
#if 0
// IBM
DEFINE_FUNCTION_V_R(vec_elt0,x.v[0])
DEFINE_FUNCTION_V_R(vec_elt1,x.v[1])
#endif
static inline CCTK_REAL vec_elt0(CCTK_REAL_VEC const x)
{
  CCTK_REAL_VEC::vec_mask x1;
  x1.v = x;
  return x1.elts[0];
}
static inline CCTK_REAL vec_elt1(CCTK_REAL_VEC const x)
{
  CCTK_REAL_VEC::vec_mask x1;
  x1.v = x;
  return x1.elts[1];
}



// Load and store vectors

// Load a vector from memory (aligned and unaligned); this loads from
// a reference to a scalar
DEFINE_FUNCTION_PR_V(vec_load,p)
#if 0
// IBM
DEFINE_FUNCTION_PR_V(vec_loadu,vec_xld2(0,const_cast<CCTK_REAL*>(&p)))
#endif
DEFINE_FUNCTION_PR_V(vec_loadu,p)

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
DEFINE_FUNCTION_PRV(vec_store,*(CCTK_REAL_VEC::V*)&p=x)
DEFINE_FUNCTION_PRV(vec_storeu,*(CCTK_REAL_VEC::V*)&p=x)
// TODO: Use stvxl instruction?
DEFINE_FUNCTION_PRV(vec_store_nta,*(CCTK_REAL_VEC::V*)&p=x)

// Store a lower or higher partial vector (aligned and non-temporal);
// the non-temporal hint is probably ignored
static inline
void vec_store_nta_partial_lo (CCTK_REAL& p, CCTK_REAL_VEC const x, int const n)
{
  switch (n) {
  case 1: p=vec_elt0(x); break;
  default: assert(0);
  }
}
static inline
void vec_store_nta_partial_hi (CCTK_REAL& p, CCTK_REAL_VEC const x, int const n)
{
  switch (n) {
  case 1: (&p)[1]=vec_elt1(x); break;
  default: assert(0);
  }
}



// Functions and operators

// Other Altivec functions are:
//    nabs: -abs a
//    madd msub nmadd nmsub: [+-]a*b[+-]c

// Single-argument operators
#if 0
DEFINE_FUNCTION_V_V(operator+,x)
DEFINE_FUNCTION_V_V(operator-,vec_neg(x))
#endif
DEFINE_FUNCTION_V_V(operator+,+x.v)
DEFINE_FUNCTION_V_V(operator-,-x.v)

// Double-argument operators, both vectors
#if 0
DEFINE_FUNCTION_VV_V(operator+,vec_add(x,y))
DEFINE_FUNCTION_VV_V(operator-,vec_sub(x,y))
DEFINE_FUNCTION_VV_V(operator*,vec_mul(x,y))
DEFINE_FUNCTION_VV_V(operator/,vec_div(x,y))
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

// Triple-argument operators, all vectors
#undef fmadd
#undef fmsub
#undef fnmadd
#undef fnmsub
DEFINE_FUNCTION_VVV_V(fmadd,vec_madd(x.v,y.v,z.v))
DEFINE_FUNCTION_VVV_V(fmsub,vec_msub(x.v,y.v,z.v))
DEFINE_FUNCTION_VVV_V(fnmadd,vec_nmadd(x.v,y.v,z.v))
DEFINE_FUNCTION_VVV_V(fnmsub,vec_nmsub(x.v,y.v,z.v))

// Cheap functions
DEFINE_FUNCTION_V_V(fabs,vec_abs(x.v))
DEFINE_FUNCTION_VV_V(fmax,vec_max(x.v,y.v))
DEFINE_FUNCTION_VV_V(fmin,vec_min(x.v,y.v))

// Expensive functions
DEFINE_FUNCTION_V_V(exp,vec_set(exp(vec_elt0(x)),exp(vec_elt1(x))))
DEFINE_FUNCTION_V_V(log,vec_set(log(vec_elt0(x)),log(vec_elt1(x))))
DEFINE_FUNCTION_VR_V(pow,vec_set(pow(vec_elt0(x),a),pow(vec_elt1(x),a)))
DEFINE_FUNCTION_V_V(sqrt,vec_set(sqrt(vec_elt0(x)),sqrt(vec_elt1(x))))



#undef Sign
#define Sign(x) (42)

#undef ToReal
#define ToReal(x) (vec_set1(x))
