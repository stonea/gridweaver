   MODULE multigrid_2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PURPOSE:  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE physical_params

   USE parallel_params
   USE parallel_utilities

   USE grid_params
   USE grid_subdomain
   USE grid_metrics
   USE grid_utilities

   USE multigrid

   USE wrap_data

   USE utilities_timer
   USE utilities_misc

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: &
      mg2D_rlx_dn,mg2D_rlx_up,mg2D_lvl_min,mg2D_rlx_omega,l_mg2D_time
   PUBLIC :: &
      mltgrd2D,mltgrd2D_time_report

   SAVE

   TYPE mg2D_node
      REAL (KIND=dbl_kind),POINTER :: &
         work(:,:,:,:),alph(:,:,:,:),beta(:,:,:,:)
   END TYPE mg2D_node

   TYPE (mg2D_node) :: &
      mg2D(0:level_max)

   INTEGER (KIND=int_kind),PARAMETER :: &
      mg2D_rlx_dn    = 3, &
      mg2D_rlx_up    = 3, &
      mg2D_lvl_min   = 0

   REAL (KIND=dbl_kind),PARAMETER :: &
      mg2D_rlx_omega = 0.875_dbl_kind

!_______________________________________________________________________
!  1 -> total time for multigrid 2D
!  2 -> total time for relax
!  3 -> time for horizontal relax
!  4 -> injection
!  5 -> prolongation
!  6 -> set-up

   LOGICAL (KIND=log_kind) :: &
      l_mg2D_time      = .FALSE., &!
      l_mg2D_time_init = .TRUE.   !
   REAL (KIND=dbl_kind),DIMENSION(6,0:level_max) :: &
      mg2D_time_start,mg2D_time_stop,mg2D_time_total
!_______________________________________________________________________

   CONTAINS
!=======================================================================
!  BEGIN mltgrd2D
!=======================================================================
   SUBROUTINE mltgrd2D (communicator_name,km0,beta,alpha)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name
   INTEGER (KIND=int_kind) :: &
      km0
   REAL (KIND=dbl_kind) :: &
      beta(:,:,:,:),alpha(:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  timing
!-----------------------------------------------------------------------
   IF (l_mg2D_time_init) THEN ! initialize timing
      mg2D_time_total = zero
      l_mg2D_time_init = .FALSE.
   ENDIF
   IF (l_mg2D_time) THEN
      mg2D_time_start(1,level_max) = MPI_WTIME ()
   ENDIF
!-----------------------------------------------------------------------
!  allocate work space memory
!-----------------------------------------------------------------------
   IF (l_mg2D_time) mg2D_time_start(6,level_max) = MPI_WTIME ()

   DO lvl = 0,level_max-1
      ALLOCATE (mg2D(lvl)%alph(mg(lvl)%im,mg(lvl)%jm,km0,mg(lvl)%nsdm))
      mg2D(lvl)%alph = zero
      ALLOCATE (mg2D(lvl)%beta(mg(lvl)%im,mg(lvl)%jm,km0,mg(lvl)%nsdm))
      mg2D(lvl)%beta = zero
   ENDDO
   DO lvl = 0,level_max
      ALLOCATE (mg2D(lvl)%work(mg(lvl)%im,mg(lvl)%jm,km0,mg(lvl)%nsdm))
      mg2D(lvl)%work = zero
   ENDDO

   IF (l_mg2D_time) THEN
      mg2D_time_stop (6,level_max) = MPI_WTIME ()
      mg2D_time_total(6,level_max) = mg2D_time_total(6,level_max) + &
                    (mg2D_time_stop(6,level_max)-mg2D_time_start(6,level_max))
   ENDIF
!-----------------------------------------------------------------------
!  solve
!-----------------------------------------------------------------------
   lvl = level_max
   CALL mltgrd2D_1 (communicator_name,lvl,km0,mg2D,beta,alpha)

   CALL wrap (TRIM (mg(level_max)%mg_strng),face=alpha)
!-----------------------------------------------------------------------
!  deallocate work space memory
!-----------------------------------------------------------------------
   IF (l_mg2D_time) mg2D_time_start(6,level_max) = MPI_WTIME ()

   DO lvl = 0,level_max-1
      DEALLOCATE (mg2D(lvl)%alph)
      DEALLOCATE (mg2D(lvl)%beta)
   ENDDO
   DO lvl = 0,level_max
      DEALLOCATE (mg2D(lvl)%work)
   ENDDO

   IF (l_mg2D_time) THEN
      mg2D_time_stop (6,level_max) = MPI_WTIME ()
      mg2D_time_total(6,level_max) = mg2D_time_total(6,level_max) + &
                    (mg2D_time_stop(6,level_max)-mg2D_time_start(6,level_max))
   ENDIF
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   IF (l_mg2D_time) THEN
      mg2D_time_stop (1,level_max) = MPI_WTIME ()
      mg2D_time_total(1,level_max) = mg2D_time_total(1,level_max) + &
                    (mg2D_time_stop(1,level_max)-mg2D_time_start(1,level_max))
   ENDIF

   END SUBROUTINE mltgrd2D
!=======================================================================
!  END mltgrd2D
!=======================================================================

!=======================================================================
!  BEGIN mltgrd2D_1
!=======================================================================
   RECURSIVE SUBROUTINE mltgrd2D_1 (communicator_name,lvl,km0,mg2D,beta,alph)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name
   INTEGER (KIND=int_kind) :: &
      lvl,km0
   TYPE (mg2D_node) :: &
      mg2D(0:level_max)
   REAL (KIND=dbl_kind) :: &
      beta(:,:,:,:),alph(:,:,:,:)
 !.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (lvl > mg2D_lvl_min) THEN
      CALL mltgrd2D_rlx (lvl,km0,mg2D_rlx_dn,mg2D(lvl)%work,beta,alph)

      CALL mltgrd2D_inject_residual (communicator_name,lvl,km0,mg2D(lvl)%work, &
                                                   alph,beta,mg2D(lvl-1)%beta)

      mg2D(lvl-1)%alph = zero

      CALL mltgrd2D_1 (communicator_name,lvl-1,km0,mg2D, &
                                         mg2D(lvl-1)%beta,mg2D(lvl-1)%alph)

!     CALL mltgrd2D_1 (communicator_name,lvl-1,km0,mg2D, &
!                                        mg2D(lvl-1)%beta,mg2D(lvl-1)%alph)

      CALL mltgrd2D_prolongation (communicator_name,lvl,km0, &
                                         mg2D(lvl-1)%alph,mg2D(lvl)%work,alph)

      CALL mltgrd2D_rlx (lvl,km0,mg2D_rlx_up,mg2D(lvl)%work,beta,alph)
   ELSE
      CALL mltgrd2D_rlx (lvl,km0,12_int_kind,mg2D(mg2D_lvl_min)%work, &
                              mg2D(mg2D_lvl_min)%beta,mg2D(mg2D_lvl_min)%alph)
   ENDIF

   END SUBROUTINE mltgrd2D_1
!=======================================================================
!  END mltgrd2D_1
!=======================================================================

!=======================================================================
!  BEGIN mltgrd2D_rlx
!=======================================================================
   SUBROUTINE mltgrd2D_rlx (lvl,km0,iteration,work,beta,alph)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl,km0,iteration
   REAL (KIND=dbl_kind) :: & 
      work(:,:,:,:),beta(:,:,:,:),alph(:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      iguana,i,j,k,nsd,imx,jmx
   REAL (KIND=dbl_kind) :: &
         tmpry
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (l_mg2D_time) mg2D_time_start(2,lvl) = MPI_WTIME ()

   DO iguana = 1,iteration
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  part 1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (lvl>level_threshold) THEN
         CALL wrap (TRIM (mg(lvl)%mg_strng),face=alph)
      ELSE
         CALL mltgrd_wrap_threshold (km0,alph)
      ENDIF

      IF (l_mg2D_time) mg2D_time_start(3,lvl) = MPI_WTIME ()

      DO nsd = 1,mg(lvl)%nsdm
!$omp parallel
!$omp do private (tmpry)
         DO k = 1,km0
            DO j = 2,mg(lvl)%jm-1
               DO i = 2,mg(lvl)%im-1
                  tmpry = mg(lvl)%wght(1,i,j,nsd)*alph(i+1,j  ,k,nsd)+ &
                          mg(lvl)%wght(2,i,j,nsd)*alph(i+1,j+1,k,nsd)+ &
                          mg(lvl)%wght(3,i,j,nsd)*alph(i  ,j+1,k,nsd)+ &
                          mg(lvl)%wght(4,i,j,nsd)*alph(i-1,j  ,k,nsd)+ &
                          mg(lvl)%wght(5,i,j,nsd)*alph(i-1,j-1,k,nsd)+ &
                          mg(lvl)%wght(6,i,j,nsd)*alph(i  ,j-1,k,nsd)

                  work(i,j,k,nsd) = &
                            (one-mg2D_rlx_omega)*alph(i,j,k,nsd) + &
                                 mg2D_rlx_omega *mg(lvl)%wght(0,i,j,nsd)* &
                            (tmpry-mg(lvl)%area(i,j,nsd)*beta(i,j,k,nsd))
               ENDDO

            ENDDO

!-----------------------------------------------------------------------
! NORTH POLE
!-----------------------------------------------------------------------
            i=2; j=mg(lvl)%jm; imx=mg(lvl)%im;
            tmpry = mg(lvl)%wght(1,i,j,nsd)*alph(i+1,j  ,k,nsd)+ &
                    mg(lvl)%wght(3,i,j,nsd)*alph(imx,  1,k,nsd)+ &
                    mg(lvl)%wght(4,i,j,nsd)*alph(i-1,j  ,k,nsd)+ &
                    mg(lvl)%wght(5,i,j,nsd)*alph(i-1,j-1,k,nsd)+ &
                    mg(lvl)%wght(6,i,j,nsd)*alph(i  ,j-1,k,nsd)
            work(i,j,k,nsd) = &
                            (one-mg2D_rlx_omega)*alph(i,j,k,nsd) + &
                                 mg2D_rlx_omega *mg(lvl)%wght(0,i,j,nsd)* &
                            (tmpry-mg(lvl)%area(i,j,nsd)*beta(i,j,k,nsd))
!-----------------------------------------------------------------------
! SOUTH POLE
!-----------------------------------------------------------------------
            i=mg(lvl)%im; j=2; jmx=mg(lvl)%jm;
            tmpry = mg(lvl)%wght(1,i,j,nsd)*alph(  1,jmx,k,nsd)+ &
                    mg(lvl)%wght(3,i,j,nsd)*alph(i  ,j+1,k,nsd)+ &
                    mg(lvl)%wght(4,i,j,nsd)*alph(i-1,j  ,k,nsd)+ &
                    mg(lvl)%wght(5,i,j,nsd)*alph(i-1,j-1,k,nsd)+ &
                    mg(lvl)%wght(6,i,j,nsd)*alph(i  ,j-1,k,nsd)

            work(i,j,k,nsd) = &
                            (one-mg2D_rlx_omega)*alph(i,j,k,nsd) + &
                                 mg2D_rlx_omega *mg(lvl)%wght(0,i,j,nsd)* &
                            (tmpry-mg(lvl)%area(i,j,nsd)*beta(i,j,k,nsd))
         ENDDO
!$omp end do
!$omp end parallel
      ENDDO
      IF (l_mg2D_time) THEN
         mg2D_time_stop (3,lvl) = MPI_WTIME ()
         mg2D_time_total(3,lvl) = mg2D_time_total(3,lvl) + &
                                 (mg2D_time_stop(3,lvl)-mg2D_time_start(3,lvl))
      ENDIF

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  part 2
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (lvl>level_threshold) THEN
         CALL wrap (TRIM (mg(lvl)%mg_strng),face=work)
      ELSE
         CALL mltgrd_wrap_threshold (km0,work)
      ENDIF

      IF (l_mg2D_time) mg2D_time_start(3,lvl) = MPI_WTIME ()

      DO nsd = 1,mg(lvl)%nsdm
!$omp parallel
!$omp do private (tmpry)
         DO k = 1,km0
            DO j = 2,mg(lvl)%jm-1
               DO i = 2,mg(lvl)%im-1
                  tmpry = mg(lvl)%wght(1,i,j,nsd)*work(i+1,j  ,k,nsd)+ &
                          mg(lvl)%wght(2,i,j,nsd)*work(i+1,j+1,k,nsd)+ &
                          mg(lvl)%wght(3,i,j,nsd)*work(i  ,j+1,k,nsd)+ &
                          mg(lvl)%wght(4,i,j,nsd)*work(i-1,j  ,k,nsd)+ &
                          mg(lvl)%wght(5,i,j,nsd)*work(i-1,j-1,k,nsd)+ &
                          mg(lvl)%wght(6,i,j,nsd)*work(i  ,j-1,k,nsd)

                  alph(i,j,k,nsd) = &
                            (one-mg2D_rlx_omega)*work(i,j,k,nsd) + &
                                 mg2D_rlx_omega *mg(lvl)%wght(0,i,j,nsd)* &
                            (tmpry-mg(lvl)%area(i,j,nsd)*beta(i,j,k,nsd))
               ENDDO
            ENDDO
!-----------------------------------------------------------------------
! NORTH POLE
!-----------------------------------------------------------------------
            i=2; j=mg(lvl)%jm; imx=mg(lvl)%im;
            tmpry = mg(lvl)%wght(1,i,j,nsd)*work(i+1,j  ,k,nsd)+ &
                    mg(lvl)%wght(3,i,j,nsd)*work(imx,  1,k,nsd)+ &
                    mg(lvl)%wght(4,i,j,nsd)*work(i-1,j  ,k,nsd)+ &
                    mg(lvl)%wght(5,i,j,nsd)*work(i-1,j-1,k,nsd)+ &
                    mg(lvl)%wght(6,i,j,nsd)*work(i  ,j-1,k,nsd)
            alph(i,j,k,nsd) = &
                            (one-mg2D_rlx_omega)*work(i,j,k,nsd) + &
                                 mg2D_rlx_omega *mg(lvl)%wght(0,i,j,nsd)* &
                            (tmpry-mg(lvl)%area(i,j,nsd)*beta(i,j,k,nsd))
!-----------------------------------------------------------------------
! SOUTH POLE
!-----------------------------------------------------------------------
            i=mg(lvl)%im; j=2; jmx=mg(lvl)%jm;
            tmpry = mg(lvl)%wght(1,i,j,nsd)*work(  1,jmx,k,nsd)+ &
                    mg(lvl)%wght(3,i,j,nsd)*work(i  ,j+1,k,nsd)+ &
                    mg(lvl)%wght(4,i,j,nsd)*work(i-1,j  ,k,nsd)+ &
                    mg(lvl)%wght(5,i,j,nsd)*work(i-1,j-1,k,nsd)+ &
                    mg(lvl)%wght(6,i,j,nsd)*work(i  ,j-1,k,nsd)
            alph(i,j,k,nsd) = &
                            (one-mg2D_rlx_omega)*work(i,j,k,nsd) + &
                                 mg2D_rlx_omega *mg(lvl)%wght(0,i,j,nsd)* &
                            (tmpry-mg(lvl)%area(i,j,nsd)*beta(i,j,k,nsd))
         ENDDO
!$omp end do
!$omp end parallel
      ENDDO
      IF (l_mg2D_time) THEN
         mg2D_time_stop (3,lvl) = MPI_WTIME ()
         mg2D_time_total(3,lvl) = mg2D_time_total(3,lvl) + &
                                 (mg2D_time_stop(3,lvl)-mg2D_time_start(3,lvl))
      ENDIF

   ENDDO

   IF (l_mg2D_time) THEN
      mg2D_time_stop (2,lvl) = MPI_WTIME ()
      mg2D_time_total(2,lvl) = mg2D_time_total(2,lvl) + &
                              (mg2D_time_stop(2,lvl)-mg2D_time_start(2,lvl))
   ENDIF

   END SUBROUTINE mltgrd2D_rlx
!=======================================================================
!  END mltgrd2D_rlx
!=======================================================================

!=======================================================================
!  BEGIN mltgrd2D_inject_residual
!=======================================================================
   SUBROUTINE mltgrd2D_inject_residual (communicator_name,lvl,km0, &
                                                      work1,alph1,beta1,beta0)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name
   INTEGER (KIND=int_kind) :: &
      lvl,km0
   REAL (KIND=dbl_kind) :: & 
      work1(:,:,:,:),alph1(:,:,:,:),beta1(:,:,:,:)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: & 
      beta0(:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   TYPE (comm_node),POINTER :: &
      comm
   INTEGER (KIND=int_kind) :: &
      buffer_type,count,i,j,k,nsd,imx,jmx,m,ierr
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      send_req(:),recv_req(:),send_status(:,:),recv_status(:,:)
   REAL (KIND=dbl_kind) :: &
      tmpry
   TYPE (mg_proc_node),POINTER :: &
      proc
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (l_mg2D_time) mg2D_time_start(4,lvl) = MPI_WTIME ()

   ALLOCATE (send_req(mg(lvl)%recv_message_total))
   ALLOCATE (recv_req(mg(lvl)%send_message_total))
   send_req(:) = -999; recv_req(:) = -999

   ALLOCATE (send_status(MPI_STATUS_SIZE,mg(lvl)%recv_message_total))
   ALLOCATE (recv_status(MPI_STATUS_SIZE,mg(lvl)%send_message_total))
   send_status(:,:) = 0; recv_status(:,:) = 0

   comm => parallel_get_communicator (communicator_name)

   SELECT CASE (dbl_kind)
      CASE (SELECTED_REAL_KIND (06))
         buffer_type = MPI_REAL
      CASE (SELECTED_REAL_KIND (12))
         buffer_type = MPI_DOUBLE_PRECISION
      CASE DEFAULT
         PRINT *," mg2D_inject_residual :: MPI_precision of real numbers unknown"
         CALL parallel_finalize ()
         STOP
   END SELECT
!-----------------------------------------------------------------------
!  post receives for coarser resolution
!-----------------------------------------------------------------------
   count = 0
   proc => mg(lvl)%proc
   DO WHILE (ASSOCIATED (proc))
      IF ((proc%send_total > 0).AND.(proc%proc_nmbr/=comm%rnk_comm)) THEN
         ALLOCATE (proc%recv_rk2(proc%send_total,km0))
         proc%recv_rk2(:,:) = zero
         count = count + 1
         CALL MPI_IRECV (proc%recv_rk2,proc%send_total*km0,buffer_type, &
                         proc%proc_nmbr,proc%send_msgtag,comm%comm, &
                         recv_req(count),ierr)
      ENDIF
      proc => proc%next
   ENDDO
!-----------------------------------------------------------------------
!  calculate residuals on the finer grid
!-----------------------------------------------------------------------
   IF (lvl>level_threshold) THEN
      CALL wrap (TRIM (mg(lvl)%mg_strng),face=alph1)
   ELSE
      CALL mltgrd_wrap_threshold (km0,alph1)
   ENDIF
   DO nsd = 1,mg(lvl)%nsdm
!$omp parallel
!$omp do private (tmpry)
      DO k = 1,km0
         DO j = 2,mg(lvl)%jm-2,2
            DO i = 2,mg(lvl)%im-2,2
               tmpry = &
            mg(lvl)%wght(1,i,j,nsd)*(alph1(i+1,j  ,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(2,i,j,nsd)*(alph1(i+1,j+1,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(3,i,j,nsd)*(alph1(i  ,j+1,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(4,i,j,nsd)*(alph1(i-1,j  ,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(5,i,j,nsd)*(alph1(i-1,j-1,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(6,i,j,nsd)*(alph1(i  ,j-1,k,nsd)-alph1(i,j,k,nsd))

               work1(i,j,k,nsd) = beta1(i,j,k,nsd) - &
                                         mg(lvl)%area_inv(i,j,nsd)*tmpry
            ENDDO
         ENDDO
! NORTH POLE
         i=2; j=mg(lvl)%jm; imx=mg(lvl)%im;
         tmpry = &
            mg(lvl)%wght(1,i,j,nsd)*(alph1(i+1,j  ,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(3,i,j,nsd)*(alph1(imx,  1,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(4,i,j,nsd)*(alph1(i-1,j  ,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(5,i,j,nsd)*(alph1(i-1,j-1,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(6,i,j,nsd)*(alph1(i  ,j-1,k,nsd)-alph1(i,j,k,nsd))

         work1(i,j,k,nsd) = beta1(i,j,k,nsd) - mg(lvl)%area_inv(i,j,nsd)*tmpry
! SOUTH POLE
         i=mg(lvl)%im; j=2; jmx=mg(lvl)%jm;
         tmpry = &
            mg(lvl)%wght(1,i,j,nsd)*(alph1(  1,jmx,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(3,i,j,nsd)*(alph1(i  ,j+1,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(4,i,j,nsd)*(alph1(i-1,j  ,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(5,i,j,nsd)*(alph1(i-1,j-1,k,nsd)-alph1(i,j,k,nsd))+ &
            mg(lvl)%wght(6,i,j,nsd)*(alph1(i  ,j-1,k,nsd)-alph1(i,j,k,nsd))

         work1(i,j,k,nsd) = beta1(i,j,k,nsd) - mg(lvl)%area_inv(i,j,nsd)*tmpry
      ENDDO
!$omp end do
!$omp end parallel
   ENDDO
!-----------------------------------------------------------------------
!  fill send buffers and post sends for the finer grid
!-----------------------------------------------------------------------
   count = 0
   proc => mg(lvl)%proc
   DO WHILE (ASSOCIATED (proc))
      IF ((proc%recv_total > 0).AND.(proc%proc_nmbr/=comm%rnk_comm)) THEN
         ALLOCATE (proc%send_rk2(proc%recv_total,km0))
         DO m = 1,proc%recv_total
            proc%send_rk2(m,:)=work1(proc%i1(m),proc%j1(m),:,proc%nsd1(m))
         ENDDO
         count = count + 1
         CALL MPI_ISEND (proc%send_rk2,proc%recv_total*km0,buffer_type, &
                         proc%proc_nmbr,proc%recv_msgtag,comm%comm, &
                         send_req(count),ierr)
      ENDIF
      proc => proc%next
   ENDDO
!-----------------------------------------------------------------------
!  wait
!-----------------------------------------------------------------------
   CALL MPI_WAITALL (mg(lvl)%recv_message_total,send_req,send_status,ierr)
   CALL MPI_WAITALL (mg(lvl)%send_message_total,recv_req,recv_status,ierr)
!-----------------------------------------------------------------------
!  unpack
!-----------------------------------------------------------------------
   proc => mg(lvl)%proc
   DO WHILE (ASSOCIATED (proc))
      IF (proc%proc_nmbr/=comm%rnk_comm) THEN
         DO m = 1,proc%send_total
            beta0(proc%i0(m),proc%j0(m),:,proc%nsd0(m)) = proc%recv_rk2(m,:)
         ENDDO
      ELSE
         DO m = 1,proc%send_total
            beta0(proc%i0(m),proc%j0(m),:,proc%nsd0(m)) = &
                                    work1(proc%i1(m),proc%j1(m),:,proc%nsd1(m))
         ENDDO
      ENDIF
      proc => proc%next
   ENDDO
!-----------------------------------------------------------------------
!  deallocate memory
!-----------------------------------------------------------------------
   DEALLOCATE (send_req,recv_req,send_status,recv_status)

   proc => mg(lvl)%proc
   DO WHILE (ASSOCIATED (proc))
      IF ((proc%send_total > 0).AND.(proc%proc_nmbr/=comm%rnk_comm)) THEN
         DEALLOCATE (proc%recv_rk2)
      ENDIF
      IF ((proc%recv_total > 0).AND.(proc%proc_nmbr/=comm%rnk_comm)) THEN
         DEALLOCATE (proc%send_rk2)
      ENDIF
      proc => proc%next
   ENDDO

   IF (l_mg2D_time) THEN
      mg2D_time_stop (4,lvl) = MPI_WTIME ()
      mg2D_time_total(4,lvl) = mg2D_time_total(4,lvl) + &
                              (mg2D_time_stop(4,lvl)-mg2D_time_start(4,lvl))
   ENDIF

   END SUBROUTINE mltgrd2D_inject_residual
!=======================================================================
!  BEGIN mltgrd2D_inject_residual
!=======================================================================

!=======================================================================
!  BEGIN mltgrd2D_prolongation
!=======================================================================
   SUBROUTINE mltgrd2D_prolongation (communicator_name,lvl,km0, &
                                                            alph0,work1,alph1)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name
   INTEGER (KIND=int_kind) :: &
      lvl,km0
   REAL (KIND=dbl_kind) :: & 
      alph0(:,:,:,:),work1(:,:,:,:)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: & 
      alph1(:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   TYPE (comm_node),POINTER :: &
      comm
   INTEGER (KIND=int_kind) :: &
      buffer_type,count,i,j,k,nsd,m,ierr
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      send_req(:),recv_req(:),send_status(:,:),recv_status(:,:)
   TYPE (mg_proc_node),POINTER :: &
      proc
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (l_mg2D_time) mg2D_time_start(5,lvl) = MPI_WTIME ()

   IF (mg(lvl)%l_active) THEN

   ALLOCATE (send_req(mg(lvl)%send_message_total))
   ALLOCATE (recv_req(mg(lvl)%recv_message_total))
   send_req(:) = -999; recv_req(:) = -999

   ALLOCATE (send_status(MPI_STATUS_SIZE,mg(lvl)%send_message_total))
   ALLOCATE (recv_status(MPI_STATUS_SIZE,mg(lvl)%recv_message_total))
   send_status(:,:) = 0; recv_status(:,:) = 0

   comm => parallel_get_communicator (communicator_name)

   SELECT CASE (dbl_kind)
      CASE (SELECTED_REAL_KIND (06))
         buffer_type = MPI_REAL
      CASE (SELECTED_REAL_KIND (12))
         buffer_type = MPI_DOUBLE_PRECISION
      CASE DEFAULT
         PRINT *," mg2D_prolongation :: MPI_precision of real numbers unknown"
         CALL parallel_finalize ()
         STOP
   END SELECT
!-----------------------------------------------------------------------
!  post receives for finer resolution
!-----------------------------------------------------------------------
   count = 0
   proc => mg(lvl)%proc
   DO WHILE (ASSOCIATED (proc))
      IF ((proc%recv_total > 0).AND.(proc%proc_nmbr/=comm%rnk_comm)) THEN
         ALLOCATE (proc%recv_rk2(proc%recv_total,km0))
         proc%recv_rk2(:,:) = zero
         count = count + 1
         CALL MPI_IRECV (proc%recv_rk2,proc%recv_total*km0,buffer_type, &
                         proc%proc_nmbr,proc%recv_msgtag,comm%comm, &
                         recv_req(count),ierr)
      ENDIF
      proc => proc%next
   ENDDO
!-----------------------------------------------------------------------
!  fill send buffers and post sends for the coarser grid
!-----------------------------------------------------------------------
   count = 0
   proc => mg(lvl)%proc
   DO WHILE (ASSOCIATED (proc))
      IF ((proc%send_total > 0).AND.(proc%proc_nmbr/=comm%rnk_comm)) THEN
         ALLOCATE (proc%send_rk2(proc%send_total,km0))
         DO m = 1,proc%send_total
            proc%send_rk2(m,:)=alph0(proc%i0(m),proc%j0(m),:,proc%nsd0(m))
         ENDDO
         count = count + 1
         CALL MPI_ISEND (proc%send_rk2,proc%send_total*km0,buffer_type, &
                         proc%proc_nmbr,proc%send_msgtag,comm%comm, &
                         send_req(count),ierr)
      ENDIF
      proc => proc%next
   ENDDO
!-----------------------------------------------------------------------
!  wait
!-----------------------------------------------------------------------
   CALL MPI_WAITALL (mg(lvl)%send_message_total,send_req,send_status,ierr)
   CALL MPI_WAITALL (mg(lvl)%recv_message_total,recv_req,recv_status,ierr)
!-----------------------------------------------------------------------
!  unpack
!-----------------------------------------------------------------------
   proc => mg(lvl)%proc
   DO WHILE (ASSOCIATED (proc))
      IF (proc%proc_nmbr/=comm%rnk_comm) THEN
         DO m = 1,proc%recv_total
            work1(proc%i1(m),proc%j1(m),:,proc%nsd1(m)) = proc%recv_rk2(m,:)
         ENDDO
      ELSE
         DO m = 1,proc%recv_total
            work1(proc%i1(m),proc%j1(m),:,proc%nsd1(m)) = &
                                    alph0(proc%i0(m),proc%j0(m),:,proc%nsd0(m))
         ENDDO
      ENDIF
      proc => proc%next
   ENDDO
!-----------------------------------------------------------------------
!  wrap work on the finer grid
!-----------------------------------------------------------------------
   IF (lvl>level_threshold) THEN
      CALL wrap (TRIM (mg(lvl)%mg_strng),face=work1)
   ELSE
      CALL mltgrd_wrap_threshold (km0,work1)
   ENDIF
!-----------------------------------------------------------------------
!  mg2D_prolongation
!-----------------------------------------------------------------------
   DO nsd = 1,mg(lvl)%nsdm
!$omp parallel
!$omp do
      DO k = 1,km0
         DO j = 2,mg(lvl)%jm-2,2
            DO i = 2,mg(lvl)%im-2,2

               alph1(i  ,j  ,k,nsd) = alph1(i  ,j  ,k,nsd) + &
                                      work1(i,j,k,nsd)
               alph1(i+1,j  ,k,nsd) = alph1(i+1,j  ,k,nsd) + &
                                  half*(work1(i,j,k,nsd)+work1(i+2,j  ,k,nsd))
               alph1(i+1,j+1,k,nsd) = alph1(i+1,j+1,k,nsd) + &
                                  half*(work1(i,j,k,nsd)+work1(i+2,j+2,k,nsd))
               alph1(i  ,j+1,k,nsd) = alph1(i  ,j+1,k,nsd) + &
                                  half*(work1(i,j,k,nsd)+work1(i  ,j+2,k,nsd))
            ENDDO
         ENDDO

         i=2; j=mg(lvl)%jm;
         alph1(i,j,k,nsd) = alph1(i,j,k,nsd) + work1(i,j,k,nsd)

         i=mg(lvl)%im; j=2;
         alph1(i,j,k,nsd) = alph1(i,j,k,nsd) + work1(i,j,k,nsd)
      ENDDO
!$omp end do
!$omp end parallel
   ENDDO
!-----------------------------------------------------------------------
!  deallocate memory
!-----------------------------------------------------------------------
   DEALLOCATE (send_req,recv_req,send_status,recv_status)

   proc => mg(lvl)%proc
   DO WHILE (ASSOCIATED (proc))
      IF ((proc%recv_total > 0).AND.(proc%proc_nmbr/=comm%rnk_comm)) THEN
         DEALLOCATE (proc%recv_rk2)
      ENDIF
      IF ((proc%send_total > 0).AND.(proc%proc_nmbr/=comm%rnk_comm)) THEN
         DEALLOCATE (proc%send_rk2)
      ENDIF
      proc => proc%next
   ENDDO

   ENDIF ! IF (mg(lvl)%l_active) THEN

   IF (l_mg2D_time) THEN
      mg2D_time_stop (5,lvl) = MPI_WTIME ()
      mg2D_time_total(5,lvl) = mg2D_time_total(5,lvl) + &
                              (mg2D_time_stop(5,lvl)-mg2D_time_start(5,lvl))
   ENDIF

   END SUBROUTINE mltgrd2D_prolongation
!=======================================================================
!  END mltgrd2D_prolongation
!=======================================================================

!=======================================================================
!  BEGIN mltgrd2D_time_report
!=======================================================================
   SUBROUTINE mltgrd2D_time_report ()
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (l_mg2D_time) THEN
      OPEN (UNIT=17,FILE="./mg2D_time/mg2D_time_"//TRIM (rnk_wrld_strng),FORM='FORMATTED')
      DO lvl = level_max,0,-1
         WRITE (UNIT=17,FMT="(6F12.5)") mg2D_time_total(:,lvl)
      ENDDO
      CLOSE (UNIT=17)
   ENDIF

   END SUBROUTINE mltgrd2D_time_report
!=======================================================================
!  BEGIN mltgrd2D_time_report
!=======================================================================

   END MODULE multigrid_2D
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc

