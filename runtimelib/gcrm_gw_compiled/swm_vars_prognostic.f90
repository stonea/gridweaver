   MODULE swm_vars_prognostic
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Purpose:
!
!     This module contains the shallow water model prognostic variables 
!     and their time tendencies.
!
!  Define:
!     mss    ->  mass or thickness                                   [m]
!     eta    ->  absolute vorticity                               [s^-1]
!     div    ->  divergence                                       [s^-1]
!
!     mss_f    ->  tendency of mass                       [Pa K^-1 s^-1]
!     eta_f    ->  tendency of absolute vorticity                 [s^-2]
!     div_f    ->  tendency of divergence                         [s^-2]
!
!     im,jm,nsdm   -> see grid_params.F
!     km           -> see grid_params_vertical.F
!     nprog,ntend  -> see swm_time.F
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers

   USE grid_params

   USE swm_params_vertical
   USE swm_params_time

   IMPLICIT NONE
   SAVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PROGNOSTICS DEFINED AT CELL CENTERS (FACES) AND LAYER CENTERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nprog,nsdm) :: &
      mss,   &! mass or pseudo density (= -¶p/¶zeta)           [Pa K^-1]
      eta,   &! absolute vorticity                                [s^-1]
      div     ! divergence                                        [s^-1]
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  TIME TENDENCIES DEFINED AT CELL CENTERS (FACES) AND LAYER CENTERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (kind=dbl_kind),DIMENSION(im,jm,km,ntend,nsdm) :: &
      mss_f, &! tendency of mass                          [Pa K^-1 s^-1]       
      eta_f, &! tendency of absolute vorticity                    [s^-2]
      div_f   ! tendency of divergence                            [s^-2]

   CONTAINS
!=======================================================================
!  BEGIN initialize_vars_prognostic
!=======================================================================
   SUBROUTINE initialize_vars_prognostic ()
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  QUANTITIES DEFINED AT CELL CENTERS (FACES) AND LAYER CENTERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   mss  (:,:,:,:,  :) = zero
   eta  (:,:,:,:,  :) = zero
   div  (:,:,:,:,  :) = zero

   mss_f(:,:,:,:,  :) = zero
   eta_f(:,:,:,:,  :) = zero
   div_f(:,:,:,:,  :) = zero

   END SUBROUTINE initialize_vars_prognostic
!=======================================================================
!  END initialize_vars_prognostic
!=======================================================================

   END MODULE swm_vars_prognostic
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
