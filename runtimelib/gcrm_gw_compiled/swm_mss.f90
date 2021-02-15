   MODULE swm_mss

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
      set_mss

   CONTAINS
!=======================================================================
!  BEGIN set_mss
!=======================================================================
   SUBROUTINE set_mss (mss0,wnd_nrm,mss_f,mss1)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,      nsdm),INTENT( IN) :: &
      mss0
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km,      nsdm),INTENT( IN) :: &
      wnd_nrm
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,ntend,nsdm),INTENT(OUT) :: &
      mss_f
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,      nsdm),INTENT(OUT) :: &
      mss1
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nsdm) :: &
      tend_sum
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (l_mass) THEN
!-----------------------------------------------------------------------
!  set tendency of mass
!-----------------------------------------------------------------------
      CALL set_mss_tendency (mss0,wnd_nrm,mss_f(:,:,:,nm0,:))
!-----------------------------------------------------------------------
!  step mass
!-----------------------------------------------------------------------
      tend_sum = zero
      DO n = 1,ntend
         tend_sum = tend_sum + tend_weights(n)*mss_f(:,:,:,tend_index(n),:)
      ENDDO
      mss1 = mss0 + dt_swm*tend_sum
!-----------------------------------------------------------------------
!  wrap mass
!-----------------------------------------------------------------------
      CALL wrap ("swm",face=mss1)
   ELSE
      mss1 = mss0
   ENDIF

   END SUBROUTINE set_mss
!=======================================================================
!  END set_mss
!=======================================================================

!=======================================================================
!  BEGIN set_mss_tendency
!=======================================================================
   SUBROUTINE set_mss_tendency (mss,wnd_nrm,mss_f)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,nsdm) :: &
      mss
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km,nsdm) :: &
      wnd_nrm
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,nsdm) :: &
      mss_f
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km,nsdm) :: &
      flx
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   mss_f = zero

!-----------------------------------------------------------------------
!  horizontal advection
!-----------------------------------------------------------------------
   CALL del_dot_xv ("swm",i1i,km,mss,wnd_nrm,flx,tmpry01,dt=dt_swm,flx_select=04)

   mss_f = mss_f-tmpry01

   END SUBROUTINE set_mss_tendency
!=======================================================================
!  END set_mss_tendency
!=======================================================================

   END MODULE swm_mss
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
