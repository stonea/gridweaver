   MODULE swm_swm
 
   USE kinds
   USE numbers

   USE parallel_params
   USE parallel_utilities

   USE grid_params
   USE grid_subdomain

   USE swm_params
   USE swm_params_time

   USE swm_vars_prognostic
   USE swm_vars_diagnostic
   USE swm_vars_work

   USE swm_mss
   USE swm_eta
   USE swm_div
   USE swm_wnd

   IMPLICIT NONE

   CONTAINS
!=======================================================================
!  BEGIN swm
!=======================================================================
   SUBROUTINE swm ()
!.......................................................................
!  INTENT IN
!.......................................................................
!.......................................................................
!  LOCAL
!.......................................................................
   TYPE (comm_node),POINTER :: &
      comm
   CHARACTER (LEN=47),PARAMETER :: fmt1 = "(A10,E12.4,A1,4I3,A1,E12.4,A1,4I3,A1)"
   CHARACTER (LEN=47),PARAMETER :: fmt2 = "(A10,E12.4,A1,5I3,A1,E12.4,A1,5I3,A1)"
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   comm => parallel_get_communicator ("world")

   step_count_swm = step_count_swm + 1_int_kind
   time_swm = dt_swm*FLOAT (step_count_swm-1)

   prog_index(:) = CSHIFT (prog_index,SHIFT=-1)
   np0 = prog_index(1); np1 = prog_index(2)

   tend_index(:) = CSHIFT (tend_index,SHIFT=-1)
   nm0 = tend_index(1); nm1 = tend_index(2); nm2 = tend_index(3)

   tend_weights(:) = set_tendency_weights (step_count_swm)

   IF (comm%rnk_comm==0) THEN
      PRINT ("(A39,A39)"),"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", &
                          "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
      PRINT ("(A10,F5.0,A20,I8,A14,F12.3,A9)"), &
                                " dt_swm = ",dt_swm, &
                                " step_count_swm = ",step_count_swm, &
                                " time_swm = ",time_swm/3600.," (hours) "
   ENDIF

   CALL set_mss (mss(:,:,:,np0,:),wnd_nrm,mss_f,mss(:,:,:,np1,:))

!  CALL set_eta (eta(:,:,:,np0,:),wnd_nrm,eta_f,eta(:,:,:,np1,:))

!  CALL set_div (div(:,:,:,np0,:),mss(:,:,:,np0,:),geopot,geopot_surf, &
!                          eta(:,:,:,np0,:),psi,chi,ke,div_f,div(:,:,:,np1,:))

!  CALL set_wnd (eta(:,:,:,np1,:),div(:,:,:,np1,:),f,relative,psi,chi,ke, &
!                                                     wnd_nrm,wnd_tng,wnd_edg)

!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()
   IF ((.TRUE.).AND.(rnk_wrld==0)) THEN

      tmpry01 = mss(:,:,:,np1,:)
      PRINT fmt1,"mss       ",MINVAL (tmpry01),"(",MINLOC (tmpry01),")",MAXVAL (tmpry01),"(",MAXLOC (tmpry01),")"
      tmpry01 = eta(:,:,:,np1,:)
      PRINT fmt1,"eta       ",MINVAL (tmpry01),"(",MINLOC (tmpry01),")",MAXVAL (tmpry01),"(",MAXLOC (tmpry01),")"
      tmpry01 = div(:,:,:,np1,:)
      PRINT fmt1,"div       ",MINVAL (tmpry01),"(",MINLOC (tmpry01),")",MAXVAL (tmpry01),"(",MAXLOC (tmpry01),")"

      tmpry01 = psi
      PRINT fmt1,"psi       ",MINVAL (tmpry01),"(",MINLOC (tmpry01),")",MAXVAL (tmpry01),"(",MAXLOC (tmpry01),")"
      tmpry01 = chi
      PRINT fmt1,"chi       ",MINVAL (tmpry01),"(",MINLOC (tmpry01),")",MAXVAL (tmpry01),"(",MAXLOC (tmpry01),")"

      PRINT fmt2,"wnd_nrm   ",MINVAL (wnd_nrm),"(",MINLOC (wnd_nrm),")",MAXVAL (wnd_nrm),"(",MAXLOC (wnd_nrm),")"
      PRINT fmt2,"wnd_tng   ",MINVAL (wnd_tng),"(",MINLOC (wnd_tng),")",MAXVAL (wnd_tng),"(",MAXLOC (wnd_tng),")"
   ENDIF
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()

   IF (.NOT.((-HUGE(zero)<mss(1,1,1,1,1)).AND.(mss(1,1,1,1,1)<HUGE(zero)))) CALL parallel_stop (" stopping for mss ")
   IF (.NOT.((-HUGE(zero)<eta(1,1,1,1,1)).AND.(eta(1,1,1,1,1)<HUGE(zero)))) CALL parallel_stop (" stopping for eta ")
   IF (.NOT.((-HUGE(zero)<div(1,1,1,1,1)).AND.(div(1,1,1,1,1)<HUGE(zero)))) CALL parallel_stop (" stopping for div ")

   END SUBROUTINE swm
!=======================================================================
!  BEGIN swm
!=======================================================================

   END MODULE swm_swm
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
