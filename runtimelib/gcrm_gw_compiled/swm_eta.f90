   MODULE swm_eta

   USE kinds
   USE numbers
   USE physical_params

   USE grid_params

   USE utilities_advection_horz
   USE wrap_data
   USE operators

   USE swm_params
   USE swm_params_time
   USE swm_params_vertical

   USE swm_vars_work

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC :: &
      set_eta

   CONTAINS
!=======================================================================
!  BEGIN set_eta
!=======================================================================
   SUBROUTINE set_eta (eta0,wnd_nrm,eta_f,eta1)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,      nsdm),INTENT( IN) :: &
      eta0
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km,      nsdm),INTENT( IN) :: &
      wnd_nrm
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,ntend,nsdm),INTENT(OUT) :: &
      eta_f
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,      nsdm),INTENT(OUT) :: &
      eta1
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nsdm) :: &
      tend_sum
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (l_vorticity) THEN
!-----------------------------------------------------------------------
!  set tendency of vorticity
!-----------------------------------------------------------------------
      CALL set_eta_tendency (eta0,wnd_nrm,eta_f(:,:,:,nm0,:))
!-----------------------------------------------------------------------
!  step vorticity
!-----------------------------------------------------------------------
      tend_sum = zero
      DO n = 1,ntend
         tend_sum = tend_sum + tend_weights(n)*eta_f(:,:,:,tend_index(n),:)
      ENDDO
      eta1 = eta0 + dt_swm*tend_sum
!-----------------------------------------------------------------------
!  wrap vorticity
!-----------------------------------------------------------------------
      CALL wrap ("swm",face=eta1)
   ELSE
      eta1 = eta0
   ENDIF

   END SUBROUTINE set_eta
!=======================================================================
!  END set_eta
!=======================================================================

!=======================================================================
!  BEGIN set_eta_tendency
!=======================================================================
   SUBROUTINE set_eta_tendency (eta,wnd_nrm,eta_f)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,nsdm) :: &
      eta
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km,nsdm) :: &
      wnd_nrm
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,nsdm) :: &
      eta_f
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km,nsdm) :: &
      flx
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   eta_f = zero

!-----------------------------------------------------------------------
!  horizontal advection
!-----------------------------------------------------------------------
   CALL del_dot_xv ("swm",i1i,km,eta,wnd_nrm,flx,tmpry01)

   eta_f = eta_f-tmpry01

   END SUBROUTINE set_eta_tendency
!=======================================================================
!  END set_eta_tendency
!=======================================================================

   END MODULE swm_eta
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
