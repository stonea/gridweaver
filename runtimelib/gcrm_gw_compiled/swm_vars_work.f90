   MODULE swm_vars_work
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Purpose:
!
!     This module contains work arrays
!
!  Define:
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers

   USE grid_params

   USE swm_params_vertical

   IMPLICIT NONE
   SAVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  work arrays
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nsdm) :: &
      tmpry01,tmpry02,tmpry03,tmpry04,tmpry05

   CONTAINS
!=======================================================================
!  BEGIN initialize_vars_work
!=======================================================================
   SUBROUTINE initialize_vars_work ()

   tmpry01 = zero
   tmpry02 = zero
   tmpry03 = zero
   tmpry04 = zero
   tmpry05 = zero

   END SUBROUTINE initialize_vars_work
!=======================================================================
!  END   initialize_vars_work
!=======================================================================

   END MODULE swm_vars_work
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
