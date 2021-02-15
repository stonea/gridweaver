   MODULE swm_vars_diagnostic
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Purpose:
!
!     This module contains the shallow water model diagnostic variables 
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
!  QUANTITIES DEFINED AT A SINGLE LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(       im,jm,   nsdm) :: &
      f,              &!  Coriolis force (=2.0*½*SIN (lat))       [s^-1]
      geopot_surf      !  geopotential of the Earth's surface [m^2 s^-2]
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(       im,jm,km,nsdm) :: &
      relative
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  streamfunction and velocity potential and other stuff
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(       im,jm,km,nsdm) :: &
      psi,chi,ke,geopot
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  wind at edges on the face grid
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(  edgm,im,jm,km,nsdm) :: &
      wnd_nrm,wnd_tng
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  wind at edges on the face grid, normal and tangential component
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(2,edgm,im,jm,km,nsdm) :: &
      wnd_edg     ! horizontal velocity at edges normal/tangential  [m s^-1]
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  fluxes at edges on the face grid
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(  edgm,im,jm,km,nsdm) :: &
      mssflx_edg,flx_edg

   CONTAINS
!=======================================================================
!  BEGIN initialize_vars_diagnostic
!=======================================================================
   SUBROUTINE initialize_vars_diagnostic ()

   f           = zero
   geopot_surf = zero

   relative    = zero
   psi         = zero
   chi         = zero
   ke          = zero
   geopot      = zero

   wnd_nrm     = zero
   wnd_tng     = zero
   wnd_edg     = zero

   mssflx_edg  = zero
   flx_edg     = zero

   END SUBROUTINE initialize_vars_diagnostic
!=======================================================================
!  END   initialize_vars_diagnostic
!=======================================================================

   END MODULE swm_vars_diagnostic
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
