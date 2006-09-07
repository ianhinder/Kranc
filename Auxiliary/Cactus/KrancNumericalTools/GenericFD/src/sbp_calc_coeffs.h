int const deriv_options = -1;
  
/* (drho/dx)^i = D^i_j rho^j, with D^i_j = q(i,j) */
/* imin and imax contain the first and last non-zero entry of the
   two-dimensional array q */
/* All indexing is according to Fortran conventions */
  
int const ni = cctk_lsh[0];
int const nj = cctk_lsh[1];
int const nk = cctk_lsh[2];
  
CCTK_INT  * restrict const sbp_imin = malloc (ni * sizeof *sbp_imin);
CCTK_INT  * restrict const sbp_imax = malloc (ni * sizeof *sbp_imax);
CCTK_REAL * restrict const qx   = malloc (ni * ni * sizeof *qx);
Diff_coeff (cctkGH, 0, ni, sbp_imin, sbp_imax, qx, deriv_options);
  
CCTK_INT  * restrict const sbp_jmin = malloc (nj * sizeof *sbp_jmin);
CCTK_INT  * restrict const sbp_jmax = malloc (nj * sizeof *sbp_jmax);
CCTK_REAL * restrict const qy   = malloc (nj * nj * sizeof *qy);
Diff_coeff (cctkGH, 1, nj, sbp_jmin, sbp_jmax, qy, deriv_options);
  
CCTK_INT  * restrict const sbp_kmin = malloc (nk * sizeof *sbp_kmin);
CCTK_INT  * restrict const sbp_kmax = malloc (nk * sizeof *sbp_kmax);
CCTK_REAL * restrict const qz   = malloc (nk * nk * sizeof *qz);
Diff_coeff (cctkGH, 2, nk, sbp_kmin, sbp_kmax, qz, deriv_options);
