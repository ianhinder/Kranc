// Define some macros that simplify defining short function that are
// supposed to be inlined



// Letters defining the prototype (argument and return value types):
//    I: i,j: integer
//    R: a,b: real
//    V: x,y: vector (of real)
//    P: p,q: pointer (i.e. const reference) to something
//    L: l,m: L-value (i.e. non-const reference) to something



// Load and store

#define DEFINE_FUNCTION_PR_V(name,expr)         \
static inline                                   \
CCTK_REAL_VEC name (CCTK_REAL const& p)         \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_PRV(name,expr)          \
static inline                                   \
void name (CCTK_REAL& p, CCTK_REAL_VEC const x) \
{                                               \
  expr;                                         \
}

#define DEFINE_FUNCTION_PVR(name,expr)          \
static inline                                   \
void name (CCTK_REAL_VEC& p, CCTK_REAL const a) \
{                                               \
  expr;                                         \
}



// Functions and operators

#define DEFINE_FUNCTION_V_V(name,expr)          \
static inline                                   \
CCTK_REAL_VEC name (CCTK_REAL_VEC const x)      \
  CCTK_ATTRIBUTE_PURE                           \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_V_R(name,expr)          \
static inline                                   \
CCTK_REAL name (CCTK_REAL_VEC const x)          \
  CCTK_ATTRIBUTE_PURE                           \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_R_V(name,expr)          \
static inline                                   \
CCTK_REAL_VEC name (CCTK_REAL const a)          \
  CCTK_ATTRIBUTE_PURE                           \
{                                               \
  return expr;                                  \
}

#define DEFINE_FUNCTION_VV_V(name,expr)                                 \
static inline                                                           \
CCTK_REAL_VEC name (CCTK_REAL_VEC const x, CCTK_REAL_VEC const y)       \
  CCTK_ATTRIBUTE_PURE                                                   \
{                                                                       \
  return expr;                                                          \
}

#define DEFINE_FUNCTION_VR_V(name,expr)                         \
static inline                                                   \
CCTK_REAL_VEC name (CCTK_REAL_VEC const x, CCTK_REAL const a)   \
  CCTK_ATTRIBUTE_PURE                                           \
{                                                               \
  return expr;                                                  \
}

#define DEFINE_FUNCTION_RV_V(name,expr)                         \
static inline                                                   \
CCTK_REAL_VEC name (CCTK_REAL const a, CCTK_REAL_VEC const x)   \
  CCTK_ATTRIBUTE_PURE                                           \
{                                                               \
  return expr;                                                  \
}

#define DEFINE_FUNCTION_RR_V(name,expr)                         \
static inline                                                   \
CCTK_REAL_VEC name (CCTK_REAL const a, CCTK_REAL const b)       \
  CCTK_ATTRIBUTE_PURE                                           \
{                                                               \
  return expr;                                                  \
}

#define DEFINE_FUNCTION_VVV_V(name,expr)                                \
static inline                                                           \
CCTK_REAL_VEC name (CCTK_REAL_VEC const x, CCTK_REAL_VEC const y, CCTK_REAL_VEC const z) \
  CCTK_ATTRIBUTE_PURE                                                   \
{                                                                       \
  return expr;                                                          \
}
