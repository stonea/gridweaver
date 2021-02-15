   MODULE physical_params
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PURPOSE:
!
!     Define various physical parameters and their units (MKS is standard)
!
!  DEFINE:
!
!     a            -> the radius of the earth.                       [m]
!     grav         -> gravitational acceleration of the earth.  [m s^-2]
!     omega        -> angular speed of rotation of the earth [rad. s^-1]
!     gas_const_R  -> gas constant for dry air            [J kg^-1 K^-1]
!     spec_heat_cv -> specific heat at constant volume    [J kg^-1 K^-1]
!     spec_heat_cp -> specific heat at constant pressure  [J kg^-1 K^-1]
!     kappa        -> gas_const_R/spec_heat_cp = 2/7
!     p0_sfc       -> standard pressure used in Poisson's eq.       [Pa]
!     vk           -> Von Karman's constant
!     tice         -> freezing temperature of water                  [K]
!     hltm         -> latent heat of vaporization              [J kg^-1]
!     hltf         -> latent heat of fusion                    [J kg^-1]
!     mwdry        -> molecular weight of dry air
!     delta        -> 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   IMPLICIT NONE
   SAVE

   REAL (KIND=dbl_kind),PARAMETER :: &
      a            = 0.001_dbl_kind*6.371229E+06_dbl_kind, &  
      gas_const_R  =  287.04_dbl_kind, &
      spec_heat_cv =  717.60_dbl_kind, &
      spec_heat_cp = 1004.64_dbl_kind, &
      omega        = 0.0_dbl_kind*7.29211E-5_dbl_kind
!      omega        = 0.0E-5_dbl_kind   ! test 3.1
   REAL (KIND=dbl_kind),PARAMETER :: &
      x_scale = 1.0_dbl_kind   ! factor for scaling to a smaller planet
                               ! 1.0 = earth, >1, smaller planet
   REAL (KIND=dbl_kind),PARAMETER :: &
      grav         = 9.80616_dbl_kind, &  
      kappa        = gas_const_R/spec_heat_cp, &
      tice         = 273.15, & 
      hltm         = 2.52E6, &
      hltf         = 3.336E5, &
      vk           = 0.40, &
      delta        = 0.608, &
      mwdry        = 28.991, &
      p0_sfc       = 100000._dbl_kind

   END MODULE physical_params
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
