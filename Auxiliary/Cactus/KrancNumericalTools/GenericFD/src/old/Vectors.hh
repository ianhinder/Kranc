#ifndef VECTORS_HH
#define VECTORS_HH



// Vectorisation

#include <assert.h>
#include <math.h>
#include <stdlib.h>

#include <cctk.h>



#include "Vectors-define.hh"

#if defined(KRANC_VECTORS)
// Vectorise

#  if ! defined(CCTK_REAL_PRECISION_8)
#    error "Vectorisation is currently only supported for double precision"
#  endif

#  if defined(__SSE2__)         // SSE2 (Intel)
#    if defined(KRANC_DIRECT)
#      include "Vectors-SSE2-direct.hh"
#    else
#      include "Vectors-SSE2.hh"
#    endif
#  elif defined(__ALTIVEC__) && defined(_ARCH_PWR7) // Altivec (Power)
#    if defined(KRANC_DIRECT)
#      include "Vectors-VSX-direct.hh"
#    else
#      include "Vectors-VSX.hh"
#    endif
#  else
#    include "Vectors-pseudo.hh"
#  endif

#else
// Don't vectorise

#  include "Vectors-default.hh"

#endif

#include "Vectors-undefine.hh"



#endif  // #ifndef VECTORS_HH
