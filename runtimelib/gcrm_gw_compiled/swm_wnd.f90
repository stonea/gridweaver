   MODULE swm_wnd

   USE kinds
   USE numbers
   USE physical_params

   USE grid_params
   USE grid_metrics

   USE multigrid_2D
   USE wrap_data
   USE operators

   USE utilities_interp

   USE swm_params
   USE swm_params_vertical

   USE swm_vars_work

   IMPLICIT NONE
   PRIVATE
   
   PUBLIC :: &
      set_wnd,set_wnd_nrm,set_wnd_tng,set_kinetic_energy

   CONTAINS
!=======================================================================
!  BEGIN set_wnd
!=======================================================================
   SUBROUTINE set_wnd (eta,div,f,relative,psi,chi,ke,wnd_nrm,wnd_tng,wnd_edg)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(       im,jm,  km,nsdm),INTENT( IN) :: &
      eta,div
   REAL (KIND=dbl_kind),DIMENSION(       im,jm,     nsdm),INTENT( IN) :: &
      f
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(       im,jm,  km,nsdm),INTENT(IN OUT) :: &
      relative,psi,chi,ke
   REAL (KIND=dbl_kind),DIMENSION(  edgm,im,jm,  km,nsdm),INTENT(IN OUT) :: &
      wnd_nrm,wnd_tng
   REAL (KIND=dbl_kind),DIMENSION(2,edgm,im,jm,  km,nsdm),INTENT(IN OUT) :: &
      wnd_edg
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      k,iter
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  solve for 2D stream function
!-----------------------------------------------------------------------
   DO k = 1,km
      relative(:,:,k,:) = eta(:,:,k,:) - f(:,:,:)
   ENDDO
   CALL timer (event_name="mltgrd2D",action="start")
   DO iter = 1,4
      CALL mltgrd2D ("world",km,relative,psi)
   ENDDO
   CALL timer (event_name="mltgrd2D",action="stop")
   CALL wrap ("swm",face=psi)
!-----------------------------------------------------------------------
!  solve for 2D velocity potential
!-----------------------------------------------------------------------
   CALL timer (event_name="mltgrd2D",action="start")
   DO iter = 1,4
      CALL mltgrd2D ("world",km,div,chi)
   ENDDO
   CALL timer (event_name="mltgrd2D",action="stop")
   CALL wrap ("swm",face=chi)
!-----------------------------------------------------------------------
!  set the kinetic energy
!-----------------------------------------------------------------------
   CALL set_kinetic_energy (psi,chi,ke)
   CALL wrap ("swm",face=ke)
!-----------------------------------------------------------------------
!  set wind defined at edges
!-----------------------------------------------------------------------
   CALL set_wnd_nrm (psi,chi,wnd_nrm)
   CALL set_wnd_tng (psi,chi,wnd_tng)

   wnd_edg(1,:,:,:,:,:) = wnd_nrm
   wnd_edg(2,:,:,:,:,:) = wnd_tng

   END SUBROUTINE set_wnd
!=======================================================================
!  END set_wnd
!=======================================================================

!=======================================================================
!  BEGIN set_wnd_nrm
!=======================================================================
   SUBROUTINE set_wnd_nrm (psi,chi,wnd_nrm)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,nsdm) :: &
      psi,chi
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km,nsdm) :: &
      wnd_nrm
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      i,j,k,nsd
  REAL (KIND=dbl_kind),DIMENSION(crnm,im,jm,km,nsdm) :: &
      psi_crn
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   wnd_nrm=zero

   IF (.FALSE.) THEN

   DO nsd = 1,nsdm
      DO k = 1,km
         DO j = 2,jm-1
            DO i = 2,im-1
               wnd_nrm(1,i,j,k,nsd) = &
              d_point_inv(1,i,j,nsd)*(chi(i+1,j  ,k,nsd)-chi(i  ,j  ,k,nsd))- &
        third*d_edge_inv (1,i,j,nsd)*(psi(i+1,j+1,k,nsd)-psi(i  ,j-1,k,nsd))

               wnd_nrm(2,i,j,k,nsd) = &
              d_point_inv(2,i,j,nsd)*(chi(i+1,j+1,k,nsd)-chi(i  ,j  ,k,nsd))- &
        third*d_edge_inv (2,i,j,nsd)*(psi(i  ,j+1,k,nsd)-psi(i+1,j  ,k,nsd))

               wnd_nrm(3,i,j,k,nsd) = &
              d_point_inv(3,i,j,nsd)*(chi(i  ,j+1,k,nsd)-chi(i  ,j  ,k,nsd))- &
        third*d_edge_inv (3,i,j,nsd)*(psi(i-1,j  ,k,nsd)-psi(i+1,j+1,k,nsd))

            ENDDO
         ENDDO
      ENDDO
   ENDDO

   ELSE

   CALL interp_crn_quad ("swm",i1i,km,psi,psi_crn)

   DO nsd = 1,nsdm
      DO k = 1,km
         DO j = 2,jm-1
            DO i = 2,im-1

               wnd_nrm(1,i,j,k,nsd) = &
              d_point_inv(1,i,j,nsd)*(chi    (  i+1,j  ,k,nsd)-chi    (  i  ,j  ,k,nsd))- &
              d_edge_inv (1,i,j,nsd)*(psi_crn(1,i  ,j  ,k,nsd)-psi_crn(2,i  ,j-1,k,nsd))

               wnd_nrm(2,i,j,k,nsd) = &
              d_point_inv(2,i,j,nsd)*(chi    (  i+1,j+1,k,nsd)-chi    (  i  ,j  ,k,nsd))- &
              d_edge_inv (2,i,j,nsd)*(psi_crn(2,i  ,j  ,k,nsd)-psi_crn(1,i  ,j  ,k,nsd))

               wnd_nrm(3,i,j,k,nsd) = &
              d_point_inv(3,i,j,nsd)*(chi    (  i  ,j+1,k,nsd)-chi    (  i  ,j  ,k,nsd))- &
              d_edge_inv (3,i,j,nsd)*(psi_crn(1,i-1,j  ,k,nsd)-psi_crn(2,i  ,j  ,k,nsd))

            ENDDO
         ENDDO
      ENDDO
   ENDDO

   ENDIF

   CALL wrap ("swm",edge_scalar=wnd_nrm)

   END SUBROUTINE set_wnd_nrm
!=======================================================================
!  END set_wnd_nrm
!=======================================================================

!=======================================================================
!  BEGIN set_wnd_tng
!=======================================================================
   SUBROUTINE set_wnd_tng (psi,chi,wnd_tng)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km,nsdm) :: &
      psi,chi
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km,nsdm) :: &
      wnd_tng
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      i,j,k,nsd
  REAL (KIND=dbl_kind),DIMENSION(crnm,im,jm,km,nsdm) :: &
      chi_crn
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   wnd_tng=zero

   IF (.FALSE.) THEN

   DO nsd = 1,nsdm
      DO k = 1,km
         DO j = 2,jm-1
            DO i = 2,im-1
               wnd_tng(1,i,j,k,nsd) = &
              d_point_inv(1,i,j,nsd)*(psi(i+1,j  ,k,nsd)-psi(i  ,j  ,k,nsd))+ &
        third*d_edge_inv (1,i,j,nsd)*(chi(i+1,j+1,k,nsd)-chi(i  ,j-1,k,nsd))

               wnd_tng(2,i,j,k,nsd) = &
              d_point_inv(2,i,j,nsd)*(psi(i+1,j+1,k,nsd)-psi(i  ,j  ,k,nsd))+ &
        third*d_edge_inv (2,i,j,nsd)*(chi(i  ,j+1,k,nsd)-chi(i+1,j  ,k,nsd))

               wnd_tng(3,i,j,k,nsd) = &
              d_point_inv(3,i,j,nsd)*(psi(i  ,j+1,k,nsd)-psi(i  ,j  ,k,nsd))+ &
        third*d_edge_inv (3,i,j,nsd)*(chi(i-1,j  ,k,nsd)-chi(i+1,j+1,k,nsd))

            ENDDO
         ENDDO
      ENDDO
   ENDDO

   ELSE

   CALL interp_crn_quad ("swm",i1i,km,chi,chi_crn)

   DO nsd = 1,nsdm
      DO k = 1,km
         DO j = 2,jm-1
            DO i = 2,im-1

               wnd_tng(1,i,j,k,nsd) = &
              d_point_inv(1,i,j,nsd)*(psi    (  i+1,j  ,k,nsd)-psi    (  i  ,j  ,k,nsd))+ &
              d_edge_inv (1,i,j,nsd)*(chi_crn(1,i  ,j  ,k,nsd)-chi_crn(2,i  ,j-1,k,nsd))

               wnd_tng(2,i,j,k,nsd) = &
              d_point_inv(2,i,j,nsd)*(psi    (  i+1,j+1,k,nsd)-psi    (  i  ,j  ,k,nsd))+ &
              d_edge_inv (2,i,j,nsd)*(chi_crn(2,i  ,j  ,k,nsd)-chi_crn(1,i  ,j  ,k,nsd))

               wnd_tng(3,i,j,k,nsd) = &
              d_point_inv(3,i,j,nsd)*(psi    (  i  ,j+1,k,nsd)-psi    (  i  ,j  ,k,nsd))+ &
              d_edge_inv (3,i,j,nsd)*(chi_crn(1,i-1,j  ,k,nsd)-chi_crn(2,i  ,j  ,k,nsd))

            ENDDO
         ENDDO
      ENDDO
   ENDDO

   ENDIF

   CALL wrap ("swm",edge_scalar=wnd_tng)

   END SUBROUTINE set_wnd_tng
!=======================================================================
!  END set_wnd_tng
!=======================================================================

!=======================================================================
!  BEGIN  set_kinetic_energy
!=======================================================================
   SUBROUTINE set_kinetic_energy (psi,chi,ke)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nsdm) :: &
      psi,chi
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(im,jm,km,nsdm) :: &
      ke
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   CALL grad_dot_grad (i1i,km,psi,psi,tmpry01)
   CALL grad_dot_grad (i1i,km,chi,chi,tmpry02)
   CALL jacobian      (i1i,km,psi,chi,tmpry03)

   ke = half*(tmpry01+tmpry02)+tmpry03

   END SUBROUTINE set_kinetic_energy
!=======================================================================
!  END  set_kinetic_energy
!=======================================================================

   END MODULE swm_wnd
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
