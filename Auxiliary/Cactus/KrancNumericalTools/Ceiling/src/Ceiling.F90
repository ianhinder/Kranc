! file written by s. husa, 5/6/2004

! $Id$

#include "cctk.h"

subroutine apply_check_abs(var, ni, nj, nk, ceiling_value)

implicit none

CCTK_INT,                          intent(in) :: ni, nj, nk
CCTK_REAL, dimension (ni, nj, nk), intent(in) :: var(ni, nj, nk)
CCTK_REAL,                         intent(in) :: ceiling_value

CCTK_REAL       :: criterion
CCTK_REAL, save :: initial_value

CCTK_INT,  save :: counter

counter   = counter + 1
criterion = maxval(abs(var) + epsilon(1.0d0))

if (counter == 1) then
  initial_value = criterion
  write (*,*) "<<<<<< using ceiling initial value", initial_value
else
  criterion = criterion / initial_value
  if ((ceiling_value > 0).AND.(criterion > ceiling_value)) then
          
               call CCTK_INFO("Ceiling thorn terminates evolution")
               call CCTK_TerminateNext(var)
  endif
endif

end subroutine apply_check_abs


subroutine apply_check_diff(var, ni, nj, nk, ceiling_value)

implicit none

CCTK_INT,                          intent(in) :: ni, nj, nk
CCTK_REAL, dimension (ni, nj, nk), intent(in) :: var(ni, nj, nk)
CCTK_REAL,                         intent(in) :: ceiling_value

CCTK_REAL       :: criterion
CCTK_REAL, save :: initial_value

CCTK_INT,  save :: counter

counter   = counter + 1

criterion = maxval(var) - minval(var)

if (counter == 1) then
  initial_value = criterion
  write (*,*) "<<<<<< using ceiling initial value", initial_value
else
  criterion = criterion / initial_value
  if ((ceiling_value > 0).AND.(criterion > ceiling_value)) then

               call CCTK_INFO("Ceiling thorn terminates evolution.")
               call CCTK_TerminateNext (var)
  endif
endif

end subroutine apply_check_diff

