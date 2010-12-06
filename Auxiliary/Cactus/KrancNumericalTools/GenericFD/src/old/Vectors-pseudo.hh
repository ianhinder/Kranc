// Pseudo vectorisation using scalar operations



// Number of vector elements in a CCTK_REAL_VEC
static int const CCTK_REAL_VEC_SIZE = 2;

// There is no vector type corresponding to CCTK_REAL
struct CCTK_REAL_VEC {
  // The underlying scalar and vector types
  CCTK_REAL v[CCTK_REAL_VEC_SIZE];
  
  // Set a vector from scalars
  inline CCTK_REAL_VEC(CCTK_REAL const& a, CCTK_REAL const& b): v(a), w(b) { }

  // Set a vector from a scalar, replicating the scalar
  inline CCTK_REAL_VEC(CCTK_REAL const& a): v(a), w(a) { }
  
  inline CCTK_REAL_VEC() { }
  
  // Copy constructor
  inline CCTK_REAL_VEC(CCTK_REAL_VEC const& x) { v[0]=x.v[0]; v[1]=x.v[1]; }
};



// Load and store vectors

DEFINE_FUNCTION_PR_V(vec_load,*(CCTK_REAL_VEC const* restrict)&p)
DEFINE_FUNCTION_PR_V(vec_loadu,vec_load(p))
// Load a vector from memory that may or may not be aligned, as
// decided by the offset off and the vector size
#define vec_loadu_maybe(off,p) (vec_load(p))
#define vec_loadu_maybe3(off1,off2,off3,p) (vec_load(p))

DEFINE_FUNCTION_PRV(vec_store,*(CCTK_REAL_VEC* restrict)&p=x)
DEFINE_FUNCTION_PRV(vec_store_nta,vec_store(p,x))



// Functions and operators

// Double-argument operators, both vectors
DEFINE_FUNCTION_VV_V(operator+,CCTK_REAL_VEC(x.v[0]+y.v[0],x.v[1]+y.v[1]))
DEFINE_FUNCTION_VV_V(operator-,CCTK_REAL_VEC(x.v[0]-y.v[0],x.v[1]-y.v[1]))
DEFINE_FUNCTION_VV_V(operator*,CCTK_REAL_VEC(x.v[0]*y.v[0],x.v[1]*y.v[1]))
DEFINE_FUNCTION_VV_V(operator/,CCTK_REAL_VEC(x.v[0]/y.v[0],x.v[1]/y.v[1]))

// Double-argument operators, vector and scalar
DEFINE_FUNCTION_VR_V(operator+,CCTK_REAL_VEC(x.v[0]+a,x.v[1]+a))
DEFINE_FUNCTION_VR_V(operator-,CCTK_REAL_VEC(x.v[0]-a,x.v[1]-a))
DEFINE_FUNCTION_VR_V(operator*,CCTK_REAL_VEC(x.v[0]*a,x.v[1]*a))
DEFINE_FUNCTION_VR_V(operator/,CCTK_REAL_VEC(x.v[0]/a,x.v[1]/a))

// Double-argument operators, scalar and vector
DEFINE_FUNCTION_RV_V(operator+,CCTK_REAL_VEC(a+x.v[0],a+x.v[1]))
DEFINE_FUNCTION_RV_V(operator-,CCTK_REAL_VEC(a-x.v[0],a-x.v[1]))
DEFINE_FUNCTION_RV_V(operator*,CCTK_REAL_VEC(a*x.v[0],a*x.v[1]))
DEFINE_FUNCTION_RV_V(operator/,CCTK_REAL_VEC(a/x.v[0],a/x.v[1]))

// Single-argument operators
DEFINE_FUNCTION_V_V(operator+,x)
DEFINE_FUNCTION_V_V(operator-,CCTK_REAL_VEC(-x.v[0],-x.v[1]))

// Functions
DEFINE_FUNCTION_V_V(exp,CCTK_REAL_VEC(exp(x.v[0]),exp(x.v[1])))
DEFINE_FUNCTION_V_V(fabs,CCTK_REAL_VEC(fabs(x.v[0]),fabs(x.v[1])))
DEFINE_FUNCTION_VV_V(fmax,CCTK_REAL_VEC(fmax(x.v[0],y.v[0]),fmax(x.v[1],y.v[1])))
DEFINE_FUNCTION_VV_V(fmin,CCTK_REAL_VEC(fmin(x.v[0],y.v[0]),fmin(x.v[1],y.v[1])))
DEFINE_FUNCTION_V_V(log,CCTK_REAL_VEC(log(x.v[0]),log(x.v[1])))
DEFINE_FUNCTION_VR_V(pow,CCTK_REAL_VEC(pow(x.v[0],a),pow(x.v[1],a)))
DEFINE_FUNCTION_V_V(sqrt,CCTK_REAL_VEC(sqrt(x.v[0]),sqrt(x.v[1])))
