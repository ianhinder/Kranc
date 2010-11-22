// Fallback vectorisation implementation: Do not vectorise



// Use CCTK_REAL
typedef CCTK_REAL CCTK_REAL_VEC;

// Number of vector elements in a CCTK_REAL_VEC
static int const CCTK_REAL_VEC_SIZE = 1;



// We use macros here, so that we are not surprised by compilers which
// don't like to inline functions (e.g. PGI).  This should also make
// debug builds (which may not inline) more efficient.

#define vec_load(p) (p)
#define vec_loadu(p) (p)

// Load a vector from memory that may or may not be aligned, as
// decided by the offset off and the vector size
#define vec_loadu_maybe(off,p) (p)
#define vec_loadu_maybe3(off1,off2,off3,p) (p)

#define vec_store(p,x) ((p)=(x))
#define vec_store_nta(p,x) ((p)=(x))

// Store a lower or higher partial vector (aligned and non-temporal);
// the non-temporal hint is probably ignored
#define vec_store_nta_partial_lo(p,x,n) (assert(0))
#define vec_store_nta_partial_hi(p,x,n) (assert(0))
