   MODULE swm_div

   USE kinds
   USE numbers
   USE physical_params

   USE grid_params

   USE wrap_data
   USE operators

   USE swm_params
   USE swm_params_time
   USE swm_params_vertical

   USE swm_vars_work

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC :: &
      set_div

   CONTAINS
!=======================================================================
!  BEGIN set_div
!=======================================================================
   SUBROUTINE set_div (div0,mss,geopot,geopot_surf,eta,psi,chi,ke,div_f,div1)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,      nsdm),INTENT( IN) :: &
      div0
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,      nsdm),INTENT( IN) :: &
      mss,eta,psi,chi,ke
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,      nsdm) :: &
      geopot
   REAL (KIND=dbl_kind),DIMENSION(im,jm,         nsdm),INTENT( IN) :: &
      geopot_surf
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,ntend,nsdm),INTENT(OUT) :: &
      div_f
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,      nsdm),INTENT(OUT) :: &
      div1
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      k,n
   REAL (KIND=dbl_kind) :: &
      k_diffusion
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nsdm) :: &
      tend_sum
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (l_divergence) THEN
!-----------------------------------------------------------------------
!  set geopotential
!-----------------------------------------------------------------------
      DO k = 1,km
         geopot(:,:,k,:) = grav*mss(:,:,k,:) + geopot_surf(:,:,:)
      ENDDO
!-----------------------------------------------------------------------
!  set tendency of divergence
!-----------------------------------------------------------------------
      CALL set_div_tendency (eta,div0,psi,chi,ke,geopot,div_f(:,:,:,nm0,:))
!-----------------------------------------------------------------------
!  step divergence
!-----------------------------------------------------------------------
      tend_sum = zero
      DO n = 1,ntend
         tend_sum = tend_sum + tend_weights(n)*div_f(:,:,:,tend_index(n),:)
      ENDDO
      div1 = div0 + dt_swm*tend_sum
!-----------------------------------------------------------------------  
!  del4 horizontal diffusion
!-----------------------------------------------------------------------   
     IF (l_diffusion_div) THEN

        k_diffusion = k_diffusion_div/(two**(four*FLOAT (level_max-4)))

        CALL wrap ("swm",face=div1)
        CALL laplacian (i1i,km,div1,tmpry01)

        CALL wrap ("swm",face=tmpry01)
        CALL laplacian (i1i,km,tmpry01,tmpry02)

        div1 = div1 - k_diffusion*tmpry02
     ENDIF
!-----------------------------------------------------------------------
!  wrap divergence
!-----------------------------------------------------------------------
      CALL wrap ("swm",face=div1)
   ELSE
      div1 = div0
   ENDIF

   END SUBROUTINE set_div
!=======================================================================
!  END set_div
!=======================================================================

!=======================================================================
!  BEGIN set_div_tendency
!=======================================================================
   SUBROUTINE set_div_tendency (eta,div,psi,chi,ke,geopot,div_f)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nsdm) :: &
      eta,div,psi,chi
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nsdm) :: &
      ke,geopot
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nsdm) :: &
      div_f
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   div_f = zero

!-----------------------------------------------------------------------
!  horizontal advection
!-----------------------------------------------------------------------
   CALL jacobian        (i1i,km,eta,chi,tmpry01)
   CALL flux_divergence (i1i,km,eta,psi,tmpry02)

   div_f = div_f + (tmpry01+tmpry02)
!-----------------------------------------------------------------------
!  laplacian of kinetic energy
!-----------------------------------------------------------------------
   CALL laplacian (i1i,km,ke,tmpry03)

   div_f = div_f - tmpry03
!-----------------------------------------------------------------------
!  pressure gradient
!-----------------------------------------------------------------------
  CALL laplacian (i1i,km,geopot,tmpry04)

  div_f = div_f - tmpry04

   END SUBROUTINE set_div_tendency
!=======================================================================
!  END set_div_tendency
!=======================================================================

   END MODULE swm_div
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
