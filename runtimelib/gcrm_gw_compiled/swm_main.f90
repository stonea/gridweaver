   PROGRAM main

   USE kinds
   USE numbers

   USE parallel_params
   USE parallel_utilities

   USE utilities_io
   USE utilities_timer

   USE multigrid_2D

   USE swm_params_time
   USE swm_params
   USE swm_swm
   USE swm_initialize
!!!USE swm_diagnostics

   include '../../../gridweaver.h'

   IMPLICIT NONE
   SAVE

   LOGICAL (KIND=log_kind) :: &
      l_open

   INTEGER (KIND=int_kind) :: &
      e,i,j,k,nsd
   INTEGER (KIND=4) :: &
      vls(8)

   REAL (KIND=dbl_kind) :: &
      sum_locl,sum_glbl
   REAL (KIND=dbl_kind) :: &
      time
   REAL (KIND=dbl_kind) :: &
      delta_time
   REAL (KIND=dbl_kind) :: &
      sum_two1,sum_two2,sum_inf1,sum_inf2

   REAL (KIND=dbl_kind),DIMENSION(3,edgm,im,jm,km,nsdm) :: &
      tmpry01_edg3D

   CHARACTER (LEN=47),PARAMETER :: fmt1 = "(A10,E12.4,A1,4I3,A1,E12.4,A1,4I3,A1)"
   CHARACTER (LEN=47),PARAMETER :: fmt2 = "(A10,E12.4,A1,5I3,A1,E12.4,A1,5I3,A1)"
   CHARACTER (LEN=256) :: &
      strng


    !>>>>>>>>>>>>GW ADDITION>>>>>>>>>>>>>>>>>>
        integer :: N, blkW, blkH, numIters,ierr
        integer :: gw_gbid, gw_proc, swm_tag
        DOUBLE PRECISION :: t1, t2

        type(Neighbor)     :: n1, n2, n3, n4, n5, n6
        type(Subgrid)      :: sg1L, sg2L, sg3L, sg4L, sg5L
        type(Subgrid)      :: sg1R, sg2R, sg3R, sg4R, sg5R
        type(Subgrid)      :: sgNP, sgSP
        type(Grid)         :: g
        type(Distribution) :: dist
        type(Schedule)     :: sched
        type(Grid)         :: g2
        type(DataObj)      :: data_x

        type(DataObj) data_area_inv
        type(DataObj) data_d_edge_1, data_d_edge_2, data_d_edge_3, data_d_edge_4, data_d_edge_5, data_d_edge_6

        type(DataObj) data_wght_1, data_wght_2, data_wght_3
        type(DataObj) data_indx_1, data_indx_2, data_indx_3
        type(DataObj) data_wnd_1, data_wnd_2, data_wnd_3

        type(DataObj) data_mss
        type(DataObj) data_flx_1, data_flx_2, data_flx_3


        type(grid_node), pointer :: ptr

    !<<<<<<<<<<<<GW ADDITION<<<<<<<<<<<<<<<<<<

!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   CALL parallel_initialize ()

   CALL timer (event_name="initialize_swm",action="start")
   CALL initialize_swm ()
   CALL timer (event_name="initialize_swm",action="stop")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  SUBTRACT FROM step_count_swm AND SHIFT prog_index AND tend_index 
!  IN PREPARATION FOR FIRST TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   step_count_swm = step_count_swm - 1_int_kind
   prog_index(:) = CSHIFT (prog_index,SHIFT= 1)
   tend_index(:) = CSHIFT (tend_index,SHIFT= 1)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  BEGIN TIME INTEGRATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CALL timer (action="report")
   CALL timer (action="reset")
   CALL timer (event_name="total time",action="start")
!еееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееее
   IF (rnk_wrld==0) THEN
      strng = TRIM (path_output)//"/___conserve_total_enery"
      OPEN (UNIT=7020,FILE=TRIM (strng),FORM='FORMATTED')
      strng = TRIM (path_output)//"/___conserve_potential_enstrophy"
      OPEN (UNIT=7021,FILE=TRIM (strng),FORM='FORMATTED')
   ENDIF


    !>>>>>>>>>>>>GW ADDITION>>>>>>>>>>>>>>>>>>
        N = sqrt(float((cell_max-2)/10))
        BlkW  = sbdmn(level_max)%im
        BlkH  = sbdmn(level_max)%jm

        ! Set up environment
        n1 = neighbor_new("neigh1", 0,  1)
        n2 = neighbor_new("neigh2", 1,  1)
        n3 = neighbor_new("neigh3", 1,  0)
        n4 = neighbor_new("neigh4", 0,  -1)
        n5 = neighbor_new("neigh5", -1, -1)
        n6 = neighbor_new("neigh6", -1, 0)

        sg1L = subgrid_new("sg1L", N, N); sg1R = subgrid_new("sg1R", N, N)
        sg2L = subgrid_new("sg2L", N, N); sg2R = subgrid_new("sg2R", N, N)
        sg3L = subgrid_new("sg3L", N, N); sg3R = subgrid_new("sg3R", N, N)
        sg4L = subgrid_new("sg4L", N, N); sg4R = subgrid_new("sg4R", N, N)
        sg5L = subgrid_new("sg5L", N, N); sg5R = subgrid_new("sg5R", N, N)
        sgNP = subgrid_new("sgNP", 1, 1); sgSP = subgrid_new("sgSP", 1, 1)

        g = grid_new("g")
        call grid_addSubgrid(g, sg1L); call grid_addSubgrid(g, sg1R);
        call grid_addSubgrid(g, sg2L); call grid_addSubgrid(g, sg2R);
        call grid_addSubgrid(g, sg3L); call grid_addSubgrid(g, sg3R);
        call grid_addSubgrid(g, sg4L); call grid_addSubgrid(g, sg4R);
        call grid_addSubgrid(g, sg5L); call grid_addSubgrid(g, sg5R);
        call grid_addSubgrid(g, sgNP); call grid_addSubgrid(g, sgSP)

        ! Connect top and left borders
        call grid_addBorder(g,  1, N+1,  N, N+1,  sg1L,    &
                                1, N,    1, 1,    sg2L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg2L,    &
                                N, N,    1, N,    sg1L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg2L,    &
                                1, N,    1, 1,    sg3L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg3L,    &
                                N, N,    1, N,    sg2L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg3L,    &
                                1, N,    1, 1,    sg4L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg4L,    &
                                N, N,    1, N,    sg3L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg4L,    &
                                1, N,    1, 1,    sg5L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg5L,    &
                                N, N,    1, N,    sg4L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg5L,    &
                                1, N,    1, 1,    sg1L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg1L,    &
                                N, N,    1, N,    sg5L, -1)
        
        ! Connect right and bottom borders
        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg1R,    &
                                  N, 1,    1,   1,  sg2R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg2R,      &
                                  N, N,    N,   1,  sg1R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg2R,    &
                                  N, 1,    1,   1,  sg3R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg3R,      &
                                  N, N,    N,   1,  sg2R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg3R,    &
                                  N, 1,    1,   1,  sg4R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg4R,      &
                                  N, N,    N,   1,  sg3R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg4R,    &
                                  N, 1,    1,   1,  sg5R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg5R,      &
                                  N, N,    N,   1,  sg4R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg5R,    &
                                  N, 1,    1,   1,  sg1R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg1R,      &
                                  N, N,    N,   1,  sg5R, 1)
         
        ! Connect east and west borders
        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg1L,    &
                                  1, 1,    1, N,   sg1R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg1R,    &
                                  N, 1,    N, N,   sg1L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg2L,    &
                                  1, 1,    1, N,   sg2R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg2R,    &
                                  N, 1,    N, N,   sg2L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg3L,    &
                                  1, 1,    1, N,   sg3R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg3R,    &
                                  N, 1,    N, N,   sg3L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg4L,    &
                                  1, 1,    1, N,   sg4R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg4R,    &
                                  N, 1,    N, N,   sg4L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg5L,    &
                                  1, 1,    1, N,   sg5R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg5R,    &
                                  N, 1,    N, N,   sg5L, 0)

        ! Connect north and south borders
        call grid_addBorder(g,   1, N+1,  N, N+1, sg1R,  &
                                 1,   1,  N,   1, sg2L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg2L,  &
                                 1,   N,  N,   N, sg1R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg2R,  &
                                 1,   1,  N,   1, sg3L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg3L,  &
                                 1,   N,  N,   N, sg2R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg3R,  &
                                 1,   1,  N,   1, sg4L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg4L,  &
                                 1,   N,  N,   N, sg3R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg4R,  &
                                 1,   1,  N,   1, sg5L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg5L,  &
                                 1,   N,  N,   N, sg4R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg5R,  &
                                 1,   1,  N,   1, sg1L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg1L,  &
                                 1,   N,  N,   N, sg5R, 0)

        ! Connect to NP
        call grid_addBorder(g,  0, N,  0, N, sg1L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  0, 0,  0, 0, sgNP,    &
                                1, N,  1, N, sg1L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg2L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  1, 0,  1, 0, sgNP,    &
                                1, N,  1, N, sg2L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg3L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  2, 1,  2, 1, sgNP,    &
                                1, N,  1, N, sg3L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg4L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  2, 2,  2, 2, sgNP,    &
                                1, N,  1, N, sg4L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg5L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  1, 2,  1, 2, sgNP,    &
                                1, N,  1, N, sg5L, -1)

        ! Connect to SP
        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg1R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    1, 0,    1, 0, sgSP,    &
                                  N, 1,    N, 1, sg1R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg2R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    0, 0,    0, 0, sgSP,    &
                                  N, 1,    N, 1, sg2R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg3R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    0, 1,    0, 1, sgSP,    &
                                  N, 1,    N, 1, sg3R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg4R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    1, 2,    1, 2, sgSP,    &
                                  N, 1,    N, 1, sg4R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg5R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    2, 2,    2, 2, sgSP,    &
                                  N, 1,    N, 1, sg5R,  1)

        dist = distribution_new("dist")
        call distribution_applyBlankDist(dist, g, blkW, blkH)

        ! Copy distribution from SWM
        do i = 1, nsdm_glbl
            swm_tag = (i - 1) * (blkW*blkH) + 3
            gw_gbid = i
            gw_proc = get_proc(level_max, swm_tag)

            call distribution_setProcForBlock(dist, gw_gbid, gw_proc)
        end do

        ! Set poles
        gw_gbid = distribution_gbidAt(dist, sgNP, 1, 1)
        call distribution_setProcForBlock(dist, gw_gbid, 0)
        gw_gbid = distribution_gbidAt(dist, sgSP, 1, 1)
        call distribution_setProcForBlock(dist, gw_gbid, 0)

        sched = schedule_new("sched")
        !call schedule_calculate(sched, g, dist, 1)
        !call schedule_calculate(sched, g, dist, 2)
        call schedule_calculate(sched, g, dist, 3)

        ! Print the environment
!        call environment_print()

        ! Test stencil
        data_x = data_new(sched)
        call data_initializeSeqVals(data_x)
        call data_forceUpdate(data_x)

    !<<<<<<<<<<<<GW ADDITION<<<<<<<<<<<<<<<<<<
        data_area_inv = data_new(sched)
        data_d_edge_1 = data_new(sched)
        data_d_edge_2 = data_new(sched)
        data_d_edge_3 = data_new(sched)
        data_d_edge_4 = data_new(sched)
        data_d_edge_5 = data_new(sched)
        data_d_edge_6 = data_new(sched)

        data_indx_1 = data_new(sched)
        data_indx_2 = data_new(sched)
        data_indx_3 = data_new(sched)
        data_wght_1 = data_new(sched)
        data_wght_2 = data_new(sched)
        data_wght_3 = data_new(sched)
        data_wnd_1  = data_new(sched)
        data_wnd_2  = data_new(sched)
        data_wnd_3  = data_new(sched)

        data_mss   = data_new(sched)
        data_flx_1 = data_new(sched)
        data_flx_2 = data_new(sched)
        data_flx_3 = data_new(sched)

    do i=1,1000
        call data_forceUpdate(data_mss)
        call data_forceUpdate(data_flx_1)
        call data_forceUpdate(data_flx_2)
        call data_forceUpdate(data_flx_3)

       call data_apply_noncompact(data_mss,                         &
            tag(data_mss),                                          &
            rel(data_wght_1), rel(data_wght_2), rel(data_wght_3),   &
            rel(data_indx_1), rel(data_indx_2), rel(data_indx_3),   &
            rel(data_wnd_1),  rel(data_wnd_2),  rel(data_wnd_3),    &
            updateFlux)

        call data_apply10(                                  &
            data_mss,                                       &
            data_area_inv,                                  &
            data_flx_1,    data_flx_2,    data_flx_3,       &
            data_d_edge_1, data_d_edge_2, data_d_edge_3,    &
            data_d_edge_4, data_d_edge_5, data_d_edge_6, applyFlux)
    end do
    !<<<<<<<<<<<<GW ADDITION<<<<<<<<<<<<<<<<<<

print *, "COMPLETED!"
call MPI_BARRIER(MPI_COMM_WORLD,ierr)
call exit(1)



!еееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееееее
!-----------------------------------------------------------------------
!  step timestep
!-----------------------------------------------------------------------
   l_swm = .TRUE.
   DO WHILE (l_swm)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  CALL swm
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CALL timer (event_name="swm",action="start")
      CALL swm ()
      CALL timer (event_name="swm",action="stop")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  diagnostic conserved quantities
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (MOD (time_swm,1800._dbl_kind) < dt_swm) THEN
! TOTAL ENERGY
         tmpry01(:,:,1,:) = area*(mss(:,:,1,np0,:)*ke(:,:,1,:) + half*grav*(mss(:,:,1,np0,:)**two))
         sum_locl = SUM (tmpry01(:,:,1,:),MASK=l_msk_fac)
         sum_glbl = parallel_reduce ("world","sum",i0i,l_reduce_all=.TRUE.,flt_rk0=sum_locl)
         IF (rnk_wrld==0) THEN
            WRITE (UNIT=7020,FMT="(F12.0,E32.24)") time_swm,sum_glbl/(four*pi*a**two)
         ENDIF
! POTENTIAL ENSTROPHY
         tmpry01(:,:,1,:) = area*(half*(eta(:,:,1,np0,:)**two)/mss(:,:,1,np0,:))
         sum_locl = SUM (tmpry01(:,:,1,:),MASK=l_msk_fac)
         sum_glbl = parallel_reduce ("world","sum",i0i,l_reduce_all=.TRUE.,flt_rk0=sum_locl)
         IF (rnk_wrld==0) THEN
            WRITE (UNIT=7021,FMT="(F12.0,E32.24)") time_swm,sum_glbl/(four*pi*a**two)
         ENDIF
      ENDIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  write output
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (MOD (time_swm,dt_output) < dt_swm) THEN
         tmpry01(:,:,1,:) = mss(:,:,1,np0,:) + geopot_surf(:,:,:)/grav
         CALL wrt_fld (path_output,"swm_mss","parallel",proc=rnk_wrld, &
                       time=time_swm,time_unit=time_unit_strng,face=tmpry01(:,:,1:1,:))

!         CALL wrt_fld (path_output,"swm_eta","parallel",proc=rnk_wrld, &
!                       time=time_swm,time_unit=time_unit_strng,face=eta(:,:,1:1,np0,:))

!         relative(:,:,1,:) = eta(:,:,1,np0,:) - f(:,:,:)
!         CALL wrt_fld (path_output,"swm_rel","parallel",proc=rnk_wrld, &
!                       time=time_swm,time_unit=time_unit_strng,face=relative)

!         tmpry01(:,:,1,:) = eta(:,:,1,np0,:)/(grav*mss(:,:,1,np0,:))
!         CALL wrt_fld (path_output,"swm_pv","parallel",proc=rnk_wrld, &
!                       time=time_swm,time_unit=time_unit_strng,face=tmpry01)

!         CALL wrt_fld (path_output,"swm_div","parallel",proc=rnk_wrld, &
!                       time=time_swm,time_unit=time_unit_strng,face=div(:,:,1:1,np0,:))

         DO nsd = 1,nsdm
            DO k = 1,km
               DO i = 2,im-1
                  DO j = 2,jm-1
                     DO e = 1,3
                        tmpry01_edg3D(:,e,i,j,k,nsd) = nrm_edg(:,e,i,j,nsd)*wnd_nrm(e,i,j,k,nsd) + &
                                                       tng_edg(:,e,i,j,nsd)*wnd_tng(e,i,j,k,nsd)
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDDO
         CALL wrt_fld (path_output,"wnd","parallel",proc=rnk_wrld, &
                       time=time_swm,time_unit=time_unit_strng,edge_3D=tmpry01_edg3D)

         IF (initial_condition_select==01) THEN ! williamson test case 01
            DO nsd = 1,nsdm
               DO j = 1,jm
                  DO i = 1,im
                     tmpry02(i,j,:,nsd) = mss_TRUE_IC01 (advctn_alph_IC01,time_swm,point(:,i,j,nsd))
                  ENDDO
               ENDDO
            ENDDO
            CALL wrt_fld (path_output,"swm_mss_TRUE","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit=time_unit_strng,face=tmpry02(:,:,1:1,:))
   
            tmpry03 = tmpry01-tmpry02
            CALL wrt_fld (path_output,"swm_mss_DIFF","parallel",proc=rnk_wrld, &
                          time=time_swm,time_unit=time_unit_strng,face=tmpry03(:,:,1:1,:))
         ENDIF

      ENDIF ! (MOD (time_swm,dt_output) < dt_swm)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  for the pure advection case write norms of difference between appx and true
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF ((.TRUE.).AND.(initial_condition_select==01)) THEN
         INQUIRE (UNIT=937,OPENED=l_open)
         IF (.NOT.l_open) THEN
            strng = TRIM (path_output)//"/diag_mass_norms"
            OPEN (UNIT=937,FILE=TRIM (strng),FORM='FORMATTED')
         ENDIF

         DO nsd = 1,nsdm
            DO j = 1,jm
               DO i = 1,im
                  tmpry02(i,j,:,nsd) = mss_TRUE_IC01 (advctn_alph_IC01,time_swm,point(:,i,j,nsd))
               ENDDO
            ENDDO
         ENDDO

         tmpry01(:,:,1,:) = mss(:,:,1,np0,:) + geopot_surf(:,:,:)/grav

         tmpry03 = tmpry01-tmpry02

         sum_two1 = zero; sum_two2 = zero;
         sum_inf1 = zero; sum_inf2 = zero;
         DO nsd = 1,nsdm
            DO j = 2,jm-1
               DO i = 2,im-1
                  sum_two1 = sum_two1 + area(i,j,nsd)*tmpry02(i,j,1,nsd)**2
                  sum_two2 = sum_two2 + area(i,j,nsd)*tmpry03(i,j,1,nsd)**2
                  sum_inf1 = MAX (sum_inf1,ABS (area(i,j,nsd)*tmpry02(i,j,1,nsd)))
                  sum_inf2 = MAX (sum_inf2,ABS (area(i,j,nsd)*tmpry03(i,j,1,nsd)))
               ENDDO
            ENDDO
         ENDDO
         WRITE (UNIT=937,FMT="(3E16.8)") time_swm/3600._dbl_kind, &
                       SQRT (sum_two2)/SQRT (sum_two1),sum_inf2/sum_inf1
         IF (.NOT.l_swm) CLOSE (UNIT=937)
      ENDIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  TIME PER TIMESTEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      delta_time = FLOAT (time_per_timestep ())/1000._dbl_kind
      IF ((rnk_wrld==0).AND.(step_count_swm>2)) THEN
         PRINT "(A22,F8.3,A18,F8.2,A1)"," time per timestep  = ",delta_time, &
                                "(sec) (speed_up = ",dt_swm/delta_time,")"
      ENDIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  DECIDE TO STOP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (time_swm > time_end_swm) l_swm = .FALSE.
   ENDDO ! DO WHILE (l_swm)
   CALL timer (event_name="total time",action="stop")
   CALL timer (action="report")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  multigrid diagnostics
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (l_mg2D_time) CALL mltgrd2D_time_report ()

   CALL parallel_barrier ("world")
   CALL parallel_finalize ()

  contains

    !>>>>>>>>>>>>GW ADDITION>>>>>>>>>>>>>>>>>>
    real(8) function updateFlux(        &
        VAL_X,                          &
        wght_1, wght_2, wght_3,         &
        indx_1, indx_2, indx_3,         &
        wnd_1,  wnd_2,  wnd_3,          &
        sg, idxI, idxJ)
    !`
        integer, intent(in) :: sg, idxI, idxJ
        interface
            real(8) function VAL_X(pt);   integer, intent(in) :: pt;  end function
            real(8) function wght_1(x,y); integer, intent(in) :: x,y; end function
            real(8) function wght_2(x,y); integer, intent(in) :: x,y; end function
            real(8) function wght_3(x,y); integer, intent(in) :: x,y; end function
            real(8) function indx_1(x,y); integer, intent(in) :: x,y; end function
            real(8) function indx_2(x,y); integer, intent(in) :: x,y; end function
            real(8) function indx_3(x,y); integer, intent(in) :: x,y; end function
            real(8) function wnd_1(x,y);  integer, intent(in) :: x,y; end function
            real(8) function wnd_2(x,y);  integer, intent(in) :: x,y; end function
            real(8) function wnd_3(x,y);  integer, intent(in) :: x,y; end function
        end interface
        real(8) :: thng, gamm, alph, beta, factor, x_tmpry, wnd_nrm
        real(8) :: x_intrp(4, 3)
        integer :: e, p
        real(8), parameter :: epsil = 0.0000001_dbl_kind

        !*************
        ! calculate interpolation values
        !*************
        DO e = 1,3
            DO p = 1,4
                x_intrp(p,e) = wght_1(idxI,idxJ) * VAL_X(int(indx_1(idxI,idxJ))) + &
                               wght_2(idxI,idxJ) * VAL_X(int(indx_2(idxI,idxJ))) + &
                               wght_3(idxI,idxJ) * VAL_X(int(indx_3(idxI,idxJ)))
           ENDDO
        ENDDO

        !*************
        ! integrate
        !*************
        DO e = 1,3
            select case(e)
                case(1); wnd_nrm = wnd_1(idxI, idxJ)
                case(2); wnd_nrm = wnd_2(idxI, idxJ)
                case(3); wnd_nrm = wnd_3(idxI, idxJ)
            end select
            
           factor = 0.1_dbl_kind

           IF (wnd_nrm > zero) THEN
              thng = (x_intrp(1,e)-two*x_intrp(2,e)+x_intrp(3,e))**two + epsil
              gamm = thng/(thng+factor*(MAX (zero,x_intrp(2,e))*MAX (zero,x_intrp(3,e))))
              alph = third*gamm+one; beta = one-gamm;
              x_tmpry = beta*(-0.125_dbl_kind)*x_intrp(1,e) + &
                        alph* (0.750_dbl_kind)*x_intrp(2,e) + &
                        beta* (0.375_dbl_kind)*x_intrp(3,e)
           ELSE
              thng = (x_intrp(2,e)-two*x_intrp(3,e)+x_intrp(4,e))**two + epsil
              gamm = thng/(thng+factor*(MAX (zero,x_intrp(2,e))*MAX (zero,x_intrp(3,e))))
              alph = third*gamm+one; beta = one-gamm;
              x_tmpry = beta*(-0.125_dbl_kind)*x_intrp(4,e) + &
                        alph*( 0.750_dbl_kind)*x_intrp(3,e) + &
                        beta*( 0.375_dbl_kind)*x_intrp(2,e)
           ENDIF

           x_tmpry = MAX(zero,x_tmpry) * wnd_nrm

!           select case(e)
!               case(1); data_flx_1(idxI,idxJ) = wnd_nrm*x_tmpry
!               case(2); data_flx_2(idxI,idxJ) = wnd_nrm*x_tmpry
!               case(3); data_flx_3(idxI,idxJ) = wnd_nrm*x_tmpry
!           end select
        end do
    end function


    real(8) function applyFlux(                 &
        data_area_inv,                          &
        data_flx_1,  data_flx_2,  data_flx_3,   &
        data_edge_1, data_edge_2, data_edge_3,  &
        data_edge_4, data_edge_5, data_edge_6,  &
        idxI, idxJ)
    !`
        integer, intent(in) :: idxI, idxJ
        interface
            real(8) function data_area_inv(x,y); integer, intent(in) :: x,y; end function
            real(8) function data_flx_1(x,y);    integer, intent(in) :: x,y; end function
            real(8) function data_flx_2(x,y);    integer, intent(in) :: x,y; end function
            real(8) function data_flx_3(x,y);    integer, intent(in) :: x,y; end function
            real(8) function data_edge_1(x,y);   integer, intent(in) :: x,y; end function
            real(8) function data_edge_2(x,y);   integer, intent(in) :: x,y; end function
            real(8) function data_edge_3(x,y);   integer, intent(in) :: x,y; end function
            real(8) function data_edge_4(x,y);   integer, intent(in) :: x,y; end function
            real(8) function data_edge_5(x,y);   integer, intent(in) :: x,y; end function
            real(8) function data_edge_6(x,y);   integer, intent(in) :: x,y; end function
        end interface

        applyFlux = data_area_inv(idxI,idxJ) * &
            ((data_flx_1(idxI  , j  ) * data_edge_1(idxI, idxJ) + &
              data_flx_2(idxI  , j  ) * data_edge_2(idxI, idxJ) + &
              data_flx_3(idxI  , j  ) * data_edge_3(idxI, idxJ))- &
             (data_flx_1(idxI-1, j  ) * data_edge_4(idxI, idxJ) + &
              data_flx_2(idxI-1, j-1) * data_edge_5(idxI, idxJ) + &
              data_flx_3(idxI  , j-1) * data_edge_6(idxI, idxJ)))
    end function

    !<<<<<<<<<<<<GW ADDITION<<<<<<<<<<<<<<<<<<
   END PROGRAM main
!=======================================================================
!  END main
!=======================================================================

!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc

    
