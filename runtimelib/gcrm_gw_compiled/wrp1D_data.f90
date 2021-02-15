   MODULE wrp1D_data
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PURPOSE:  perform boundary updates of ghost cell along edges 
!            of subdomain blocks
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE grid_params
   USE parallel_utilities
   USE utilities_misc
   USE utilities_timer

   IMPLICIT NONE
   SAVE

   TYPE wrp1D_node
      CHARACTER (LEN=128) :: &
         wrap_name
      LOGICAL (KIND=log_kind) :: &
         l_wrap
      INTEGER (KIND=int_kind) :: &
         npe_comm,rnk_comm,comm,proc_total, &
         send_message_total,recv_message_total,int_type,flt_type
      INTEGER (KIND=int_kind) :: &
         partition_factor
      INTEGER (KIND=int_kind),ALLOCATABLE :: &
         sbdmn_lst(:)
      TYPE (wrp1D_proc_node),POINTER :: &
         proc
      TYPE (wrp1D_node),POINTER :: &
         next
   END TYPE wrp1D_node

   TYPE wrp1D_proc_node
      INTEGER (KIND=int_kind) :: &
         proc_nmbr
      INTEGER (KIND=int_kind) :: &
         send_total,recv_total,send_msgtag,recv_msgtag
      INTEGER (KIND=int_kind),POINTER :: &
         send(:),n0(:),recv(:),n1(:)
      INTEGER (KIND=int_kind),POINTER :: &
         send_int_rk2(:,:),send_int_rk4(:,:,:,:), &
         recv_int_rk2(:,:),recv_int_rk4(:,:,:,:)
      REAL (KIND=dbl_kind),POINTER :: &
         send_flt_rk2(:,:),send_flt_rk4(:,:,:,:), &
         recv_flt_rk2(:,:),recv_flt_rk4(:,:,:,:)
      TYPE (wrp1D_list_node),POINTER :: &
         list,list_end
      TYPE (wrp1D_proc_node),POINTER :: &
         next
   END TYPE wrp1D_proc_node

   TYPE :: wrp1D_list_node
      INTEGER (KIND=int_kind) :: &
         tag_glbl,tag_locl
      TYPE (wrp1D_list_node),POINTER :: &
         next
   END TYPE wrp1D_list_node

   LOGICAL (KIND=log_kind) :: &
      l_allocate_wrp1D_head = .TRUE., &
      l_wrptmr = .FALSE.

   TYPE (wrp1D_node),POINTER :: &
      wrp1D_head

   INTEGER (KIND=int_kind) :: &
      msgtag = 0

   CONTAINS
!=======================================================================
!  BEGIN  initialize_wrp1D
!=======================================================================
   SUBROUTINE initialize_wrp1D (communicator_name,wrap_name,lvl)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name,wrap_name
   INTEGER (KIND=int_kind) :: &
      lvl
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      rnk,n,m,recv_total,proc_total,proc_nmbr_send,ierr
   INTEGER (KIND=int_kind) :: &
      status(MPI_STATUS_SIZE)
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      tmpry_list(:)
   TYPE (wrp1D_node),POINTER :: &
      wrp
   TYPE (comm_node),POINTER :: &
      comm
   TYPE (grid_node),POINTER :: &
      ptr
   TYPE (wrp1D_proc_node),POINTER :: &
      proc,proc_send
   TYPE (wrp1D_list_node),POINTER :: &
      list
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  the first time initialize_wrp1D is called allocate the head node
!-----------------------------------------------------------------------
   IF (l_allocate_wrp1D_head) THEN
      l_allocate_wrp1D_head = .FALSE.
      ALLOCATE (wrp1D_head); wrp => wrp1D_head
      wrp%wrap_name = TRIM (wrap_name)
      wrp%l_wrap = .TRUE.
      wrp%proc_total = 0
      wrp%send_message_total = 0; wrp%recv_message_total = 0;
      NULLIFY (wrp%proc);  NULLIFY (wrp%next)                     
   ELSE
!-----------------------------------------------------------------------
!  look through the list to find duplicate wrap_name
!-----------------------------------------------------------------------
      wrp => wrp1D_head
      DO WHILE (ASSOCIATED (wrp))
         IF (TRIM (wrp%wrap_name)==TRIM (wrap_name)) THEN
            PRINT *,' initialize_wrp1D : wrap_name = "'// &
                                  TRIM (wrap_name)//'" ALREADY EXISTS.'
         ENDIF
         wrp => wrp%next
      ENDDO
!-----------------------------------------------------------------------
!  add a new node to the end of the list
!-----------------------------------------------------------------------
      wrp => wrp1D_head
      DO WHILE (ASSOCIATED (wrp%next))
         wrp => wrp%next
      ENDDO
      ALLOCATE (wrp%next)
      wrp => wrp%next
      wrp%wrap_name = TRIM (wrap_name)
      wrp%l_wrap = .TRUE.
      wrp%proc_total = 0
      wrp%send_message_total = 0; wrp%recv_message_total = 0;
      NULLIFY (wrp%proc); NULLIFY (wrp%next)
   ENDIF
!-----------------------------------------------------------------------
!  copy information from the component node to the wrap node
!-----------------------------------------------------------------------
   comm => parallel_get_communicator (communicator_name)

   wrp%npe_comm  = comm%npe_comm
   wrp%rnk_comm  = comm%rnk_comm
   wrp%comm      = comm%comm
   wrp%int_type  = MPI_INTEGER
   wrp%flt_type  = MPI_DOUBLE_PRECISION
!-----------------------------------------------------------------------
!  set receive lists
!-----------------------------------------------------------------------
   ptr => path_ghst(lvl)%p
   DO WHILE (ASSOCIATED (ptr))
      proc => get_wrp1D_proc (wrp,ptr%proc)
      list => get_wrp1D_list (proc)
      list%tag_glbl = ptr%tag_glbl
      list%tag_locl = ptr%tag_locl
      ptr => ptr%next_ghst%p
   ENDDO
!-----------------------------------------------------------------------
!  set receive list arrays
!-----------------------------------------------------------------------
   proc => wrp%proc
   DO WHILE (ASSOCIATED (proc))
      IF (proc%proc_nmbr/=wrp%rnk_comm) THEN
         wrp%recv_message_total = wrp%recv_message_total + 1
      ENDIF
      ALLOCATE (proc%recv(proc%recv_total))
      ALLOCATE (proc%n1  (proc%recv_total))
      list => proc%list; m = 0
      DO WHILE (ASSOCIATED (list))
         m = m + 1
         proc%recv(m) = list%tag_glbl
         proc%n1  (m) = list%tag_locl
         list => list%next
      ENDDO
      proc => proc%next
   ENDDO
!-----------------------------------------------------------------------
!  communicate receive lists
!-----------------------------------------------------------------------
   DO rnk = 0,wrp%npe_comm-1
      proc_total = wrp%proc_total
      CALL MPI_BCAST (proc_total,1,MPI_INTEGER,rnk,wrp%comm,ierr)
!090721      CALL MPI_BARRIER (wrp%comm,ierr)
      IF (proc_total > 0) THEN
         proc => wrp%proc
         DO n = 1,proc_total
            msgtag = msgtag + 1
            IF (rnk==wrp%rnk_comm) THEN
               proc_nmbr_send   = proc%proc_nmbr
               recv_total       = proc%recv_total
               proc%recv_msgtag = msgtag
            ELSE
               proc_nmbr_send = -1
               recv_total     =  0
            ENDIF
            CALL MPI_BCAST (proc_nmbr_send,1,MPI_INTEGER,rnk,wrp%comm,ierr)
!090721            CALL MPI_BARRIER (wrp%comm,ierr)

            CALL MPI_BCAST (recv_total,1,MPI_INTEGER,rnk,wrp%comm,ierr)
!090721            CALL MPI_BARRIER (wrp%comm,ierr)

            IF (recv_total > 0) THEN
               ALLOCATE (tmpry_list (recv_total))
               IF (rnk==wrp%rnk_comm) THEN
                  tmpry_list(:) = proc%recv(:)
               ELSE
                  tmpry_list(:) = 0
               ENDIF

               IF ((rnk==wrp%rnk_comm).OR.(proc_nmbr_send==wrp%rnk_comm)) THEN     !090721
                  IF ((rnk==wrp%rnk_comm).AND.(proc_nmbr_send==wrp%rnk_comm)) THEN !090721
                  ELSE                                                             !090721
                     IF (rnk==wrp%rnk_comm) THEN                                   !090721
                        CALL MPI_SEND (tmpry_list,recv_total,MPI_INTEGER, &        !090721
                                               proc_nmbr_send,0,wrp%comm,ierr)     !090721
                     ENDIF                                                         !090721
                     IF (proc_nmbr_send==wrp%rnk_comm) THEN                        !090721
                        CALL MPI_RECV (tmpry_list,recv_total,MPI_INTEGER, &        !090721
                                                   rnk,0,wrp%comm,status,ierr)     !090721
                     ENDIF                                                         !090721
                  ENDIF                                                            !090721
               ENDIF                                                               !090721

!090721               CALL MPI_BCAST (tmpry_list(1),recv_total,MPI_INTEGER, &
!090721                                                      rnk,wrp%comm,ierr)
!090721               CALL MPI_BARRIER (wrp%comm,ierr)

               IF (proc_nmbr_send==wrp%rnk_comm) THEN
                  proc_send => get_wrp1D_proc (wrp,rnk)
                  proc_send%proc_nmbr = rnk
                  proc_send%send_msgtag = msgtag
                  proc_send%send_total  = recv_total
                  ALLOCATE (proc_send%send(recv_total))
                  proc_send%send(:) = tmpry_list(:)
               ENDIF
               DEALLOCATE (tmpry_list)
            ENDIF

            IF (rnk==wrp%rnk_comm) THEN
               proc => proc%next
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!-----------------------------------------------------------------------
!  set send lists
!-----------------------------------------------------------------------
   proc => wrp%proc
   DO WHILE (ASSOCIATED (proc))
      IF (proc%send_total > 0) THEN
         IF (proc%proc_nmbr/=wrp%rnk_comm) THEN
            wrp%send_message_total = wrp%send_message_total + 1
         ENDIF
         ALLOCATE (proc%n0  (proc%send_total))
         DO n = 1,proc%send_total
            ptr => set_ptr (swmgrid,lvl,proc%send(n))
            proc%n0(n) = ptr%tag_locl
         ENDDO
      ENDIF
      proc => proc%next
   ENDDO

   END SUBROUTINE initialize_wrp1D
!=======================================================================
!  END  initialize_wrp1D
!=======================================================================

!=======================================================================
! BEGIN get_wrp1D_proc
!=======================================================================
   FUNCTION get_wrp1D_proc (wrp,proc_nmbr) RESULT (node)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrp1D_node),POINTER :: wrp
   INTEGER (KIND=int_kind) :: proc_nmbr
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (wrp1D_proc_node),POINTER :: node
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: l_found
   TYPE (wrp1D_proc_node),POINTER :: proc
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (proc_nmbr==rnk_nonexistent) THEN
      PRINT *," get_wrp1D_proc :: proc_nmbr==rnk_nonexistent "
      STOP
   ENDIF

   IF (.NOT.ASSOCIATED (wrp%proc)) THEN
      ALLOCATE (wrp%proc)
      wrp%proc_total = 1
      proc => wrp%proc
      proc%proc_nmbr = proc_nmbr; proc%send_total = 0; proc%recv_total = 0
      proc%send_msgtag = -1; proc%recv_msgtag = -1;
      NULLIFY (proc%list); NULLIFY (proc%next)
   ELSE
      l_found = .FALSE.
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF (proc%proc_nmbr==proc_nmbr) THEN
            l_found = .TRUE.
            EXIT
         ENDIF
         proc => proc%next
      ENDDO
      IF (.NOT.l_found) THEN
         proc => wrp%proc
         DO WHILE (ASSOCIATED (proc%next))
            proc => proc%next
         ENDDO
         ALLOCATE (proc%next)
         wrp%proc_total = wrp%proc_total + 1
         proc => proc%next
         proc%proc_nmbr = proc_nmbr; proc%send_total = 0; proc%recv_total = 0
         proc%send_msgtag = -1; proc%recv_msgtag = -1;
         NULLIFY (proc%list); NULLIFY (proc%next)
      ENDIF
   ENDIF

   node => proc
   
   END FUNCTION get_wrp1D_proc
!=======================================================================
!  END get_wrp1D_proc
!=======================================================================

!=======================================================================
! BEGIN get_wrp1D_list
!=======================================================================
   FUNCTION get_wrp1D_list (proc) RESULT (node)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrp1D_proc_node),POINTER :: proc
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (wrp1D_list_node),POINTER :: node
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   proc%recv_total = proc%recv_total + 1

   IF (.NOT.ASSOCIATED (proc%list)) THEN
      ALLOCATE (proc%list)
      proc%list_end => proc%list
      NULLIFY (proc%list_end%next)
   ELSE
      ALLOCATE (proc%list_end%next)
      proc%list_end => proc%list_end%next
      NULLIFY (proc%list_end%next)
   ENDIF

   node => proc%list_end
   
   END FUNCTION get_wrp1D_list
!=======================================================================
!  END get_wrp1D_list
!=======================================================================

!=======================================================================
!  BEGIN wrp1D
!=======================================================================
   SUBROUTINE wrp1D (wrap_name,face_int_1lyr, &
                       face,face_1lyr, &
                       vrtx,vrtx_1lyr,vrtx_scalar,vrtx_scalar_1lyr, &
                       edge,edge_1lyr,edge_scalar,edge_scalar_1lyr )
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),INTENT ( IN) :: &
      wrap_name
!.......................................................................
!  INTENT IN AND OUT
!.......................................................................
   INTEGER (KIND=int_kind),OPTIONAL :: &
      face_int_1lyr(:,:,:  )
   REAL (KIND=dbl_kind),OPTIONAL :: &
      face       (    :,:),       face_1lyr(    :  ), &
      vrtx       (:,:,:,:),       vrtx_1lyr(:,:,:  ), &
      vrtx_scalar(  :,:,:),vrtx_scalar_1lyr(  :,:  ), &
      edge       (:,:,:,:),       edge_1lyr(:,:,:  ), &
      edge_scalar(  :,:,:),edge_scalar_1lyr(  :,:  )
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_found
   INTEGER (KIND=int_kind) :: &
      i1(1),i2(2),i3(3)
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      temp_int_rk4(:,:,:,:)
   REAL (KIND=dbl_kind),ALLOCATABLE :: &
      temp_rk2(:,:),temp_rk4(:,:,:,:)
   TYPE (wrp1D_node),POINTER :: &
      wrp
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  find the wrap node associated with wrap_name
!-----------------------------------------------------------------------
   IF (l_allocate_wrp1D_head) THEN
      PRINT *,' wrp1D : no wrap nodes '
   ELSE
      wrp => wrp1D_head
      l_found = .FALSE.
      DO WHILE (ASSOCIATED (wrp))
         IF (TRIM (wrp%wrap_name)==TRIM (wrap_name)) THEN
            l_found = .TRUE.
            EXIT
         ENDIF
         wrp => wrp%next
      ENDDO
      IF (.NOT.l_found) THEN
         PRINT *,' wrp1D : the wrap instructions = "'// &
                         TRIM (wrap_name)//'" has not been initialized.'
      ENDIF
   ENDIF
!-----------------------------------------------------------------------
!  if SCM then return
!-----------------------------------------------------------------------
   IF (.NOT.wrp%l_wrap) RETURN
!-----------------------------------------------------------------------
! face
!-----------------------------------------------------------------------
   IF (PRESENT (face)) CALL wrp1D_1 (wrp,x_flt_rk2=face)

   IF (PRESENT (face_1lyr)) THEN
      i1(:) = SHAPE (face_1lyr)
      ALLOCATE (temp_rk2(i1(1),1))
      temp_rk2(:,1) = face_1lyr(:)
      CALL wrp1D_1 (wrp,x_flt_rk2=temp_rk2)
      face_1lyr(:) = temp_rk2(:,1)
   ENDIF
!-----------------------------------------------------------------------
! vrtx
!-----------------------------------------------------------------------
   IF (PRESENT (vrtx)) CALL wrp1D_1 (wrp,x_flt_rk4=vrtx)

   IF (PRESENT (vrtx_1lyr)) THEN
      i3(:) = SHAPE (vrtx_1lyr)
      ALLOCATE (temp_rk4(i3(1),i3(2),i3(3),1))
      temp_rk4(:,:,:,1) = vrtx_1lyr(:,:,:)
      CALL wrp1D_1 (wrp,x_flt_rk4=temp_rk4)
      vrtx_1lyr(:,:,:) = temp_rk4(:,:,:,1)
   ENDIF

   IF (PRESENT (vrtx_scalar)) THEN
      i3(:) = SHAPE (vrtx_scalar)
      ALLOCATE (temp_rk4(1,i3(1),i3(2),i3(3)))
      temp_rk4(1,:,:,:) = vrtx_scalar(:,:,:)
      CALL wrp1D_1 (wrp,x_flt_rk4=temp_rk4)
      vrtx_scalar(:,:,:) = temp_rk4(1,:,:,:)
   ENDIF

   IF (PRESENT (vrtx_scalar_1lyr)) THEN
      i2(:) = SHAPE (vrtx_scalar_1lyr)
      ALLOCATE (temp_rk4(1,i2(1),i2(2),1))
      temp_rk4(1,:,:,1) = vrtx_scalar_1lyr(:,:)
      CALL wrp1D_1 (wrp,x_flt_rk4=temp_rk4)
      vrtx_scalar_1lyr(:,:) = temp_rk4(1,:,:,1)
   ENDIF
!-----------------------------------------------------------------------
! edge
!-----------------------------------------------------------------------
   IF (PRESENT (edge)) CALL wrp1D_1 (wrp,x_flt_rk4=edge)

   IF (PRESENT (edge_1lyr)) THEN
      i3(:) = SHAPE (edge_1lyr)
      ALLOCATE (temp_rk4(i3(1),i3(2),i3(3),1))
      temp_rk4(:,:,:,1) = edge_1lyr(:,:,:)
      CALL wrp1D_1 (wrp,x_flt_rk4=temp_rk4)
      edge_1lyr(:,:,:) = temp_rk4(:,:,:,1)
   ENDIF

   IF (PRESENT (edge_scalar)) THEN
      i3(:) = SHAPE (edge_scalar)
      ALLOCATE (temp_rk4(1,i3(1),i3(2),i3(3)))
      temp_rk4(1,:,:,:) = edge_scalar(:,:,:)
      CALL wrp1D_1 (wrp,x_flt_rk4=temp_rk4)
      edge_scalar(:,:,:) = temp_rk4(1,:,:,:)
   ENDIF

   IF (PRESENT (edge_scalar_1lyr)) THEN
      i2(:) = SHAPE (edge_scalar_1lyr)
      ALLOCATE (temp_rk4(1,i2(1),i2(2),1))
      temp_rk4(1,:,:,1) = edge_scalar_1lyr(:,:)
      CALL wrp1D_1 (wrp,x_flt_rk4=temp_rk4)
      edge_scalar_1lyr(:,:) = temp_rk4(1,:,:,1)
   ENDIF
!-----------------------------------------------------------------------
! int
!-----------------------------------------------------------------------
   IF (PRESENT (face_int_1lyr)) THEN
      i3(:) = SHAPE (face_int_1lyr)
      ALLOCATE (temp_int_rk4(i3(1),i3(2),i3(3),1))
      temp_int_rk4(:,:,:,1) = face_int_1lyr(:,:,:)
      CALL wrp1D_1 (wrp,x_int_rk4=temp_int_rk4)
      face_int_1lyr(:,:,:) = temp_int_rk4(:,:,:,1)
   ENDIF

   IF (ALLOCATED  (temp_rk2)) DEALLOCATE (temp_rk2)
   IF (ALLOCATED  (temp_rk4)) DEALLOCATE (temp_rk4)
   IF (ALLOCATED  (temp_int_rk4)) DEALLOCATE (temp_int_rk4)

   END SUBROUTINE wrp1D
!=======================================================================
!  END wrp1D
!=======================================================================

!=======================================================================
!  BEGIN  wrp1D_1
!=======================================================================
   SUBROUTINE wrp1D_1 (wrp,x_int_rk2,x_int_rk4,x_flt_rk2,x_flt_rk4)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrp1D_node),POINTER :: &
      wrp
!.......................................................................
!  INTENT IN AND OUT
!.......................................................................
   INTEGER (KIND=int_kind),OPTIONAL :: &
      x_int_rk2(:,:),x_int_rk4(:,:,:,:)
   REAL (KIND=dbl_kind),OPTIONAL :: &
      x_flt_rk2(:,:),x_flt_rk4(:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_int_rk2,l_int_rk4,l_flt_rk2,l_flt_rk4
   INTEGER (KIND=int_kind) :: &
      ndim1,ndim2,nlyr,count,m,k,ierr
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      send_req(:),recv_req(:),send_status(:,:),recv_status(:,:)
   TYPE (wrp1D_proc_node),POINTER :: &
      proc
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.


   l_int_rk2 = .FALSE.; l_int_rk4 = .FALSE.;
   l_flt_rk2 = .FALSE.; l_flt_rk4 = .FALSE.;

   IF (PRESENT (x_int_rk2)) THEN
      nlyr = SIZE (x_int_rk2,DIM=2)
      l_int_rk2 = .TRUE.
   ENDIF
   IF (PRESENT (x_int_rk4)) THEN
      ndim1 = SIZE (x_int_rk4,DIM=1)
      ndim2 = SIZE (x_int_rk4,DIM=2)
      nlyr  = SIZE (x_int_rk4,DIM=4)
      l_int_rk4 = .TRUE.
   ENDIF
   IF (PRESENT (x_flt_rk2)) THEN
      nlyr = SIZE (x_flt_rk2,DIM=2)
      l_flt_rk2 = .TRUE.
   ENDIF
   IF (PRESENT (x_flt_rk4)) THEN
      ndim1 = SIZE (x_flt_rk4,DIM=1)
      ndim2 = SIZE (x_flt_rk4,DIM=2)
      nlyr  = SIZE (x_flt_rk4,DIM=4)
      l_flt_rk4 = .TRUE.
   ENDIF

   ALLOCATE (send_req(wrp%send_message_total))
   ALLOCATE (recv_req(wrp%recv_message_total))
   send_req(:) = -999; recv_req(:) = -999

   ALLOCATE (send_status(MPI_STATUS_SIZE,wrp%send_message_total))
   ALLOCATE (recv_status(MPI_STATUS_SIZE,wrp%recv_message_total))
   send_status(:,:) = 0; recv_status(:,:) = 0
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  rk2 CODE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (l_int_rk2) THEN
   ENDIF ! l_int_rk2

   IF (l_flt_rk2) THEN
!-----------------------------------------------------------------------
!  receives
!-----------------------------------------------------------------------
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         ALLOCATE (proc%recv_flt_rk2(proc%recv_total,nlyr))
         proc%recv_flt_rk2(:,:) = 0.0_dbl_kind
         count = count + 1
         CALL MPI_IRECV (proc%recv_flt_rk2,proc%recv_total*nlyr, &
                     wrp%flt_type,proc%proc_nmbr,proc%recv_msgtag,wrp%comm, &
                     recv_req(count),ierr)
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  sends
!-----------------------------------------------------------------------
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         ALLOCATE (proc%send_flt_rk2(proc%send_total,nlyr))
         DO m = 1,proc%send_total
            proc%send_flt_rk2(m,:) = x_flt_rk2(proc%n0(m),:)
         ENDDO
         count = count + 1
         CALL MPI_ISEND (proc%send_flt_rk2,proc%send_total*nlyr, &
                     wrp%flt_type,proc%proc_nmbr,proc%send_msgtag,wrp%comm, &
                     send_req(count),ierr)
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  wait
!-----------------------------------------------------------------------
      CALL MPI_WAITALL (wrp%recv_message_total,recv_req,recv_status,ierr)
      CALL MPI_WAITALL (wrp%send_message_total,send_req,send_status,ierr)
!-----------------------------------------------------------------------
!  unpack
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         DO k = 1,nlyr
            DO m = 1,proc%recv_total
               x_flt_rk2(proc%n1(m),k) = proc%recv_flt_rk2(m,k)
            ENDDO
         ENDDO
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  deallocate memory
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF ((proc%recv_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%recv_flt_rk2)
         ENDIF
         IF ((proc%send_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%send_flt_rk2)
         ENDIF
         proc => proc%next
      ENDDO
   ENDIF ! l_flt_rk2

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  rk4 CODE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (l_int_rk4) THEN
!-----------------------------------------------------------------------
!  receives
!-----------------------------------------------------------------------
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         ALLOCATE (proc%recv_int_rk4(ndim1,ndim2,proc%recv_total,nlyr))
         proc%recv_int_rk4(:,:,:,:) = 0_int_kind
         count = count + 1
         CALL MPI_IRECV (proc%recv_int_rk4,ndim1*ndim2*proc%recv_total*nlyr, &
                     wrp%int_type,proc%proc_nmbr,proc%recv_msgtag,wrp%comm, &
                     recv_req(count),ierr)
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  sends
!-----------------------------------------------------------------------
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         ALLOCATE (proc%send_int_rk4(ndim1,ndim2,proc%send_total,nlyr))
         DO m = 1,proc%send_total
            proc%send_int_rk4(:,:,m,:) = x_int_rk4(:,:,proc%n0(m),:)
         ENDDO
         count = count + 1
         CALL MPI_ISEND (proc%send_int_rk4,ndim1*ndim2*proc%send_total*nlyr, &
                     wrp%int_type,proc%proc_nmbr,proc%send_msgtag,wrp%comm, &
                     send_req(count),ierr)
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  wait
!-----------------------------------------------------------------------
      CALL MPI_WAITALL (wrp%recv_message_total,recv_req,recv_status,ierr)
      CALL MPI_WAITALL (wrp%send_message_total,send_req,send_status,ierr)
!-----------------------------------------------------------------------
!  unpack
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         DO k = 1,nlyr
            DO m = 1,proc%recv_total
               x_int_rk4(:,:,proc%n1(m),k) = proc%recv_int_rk4(:,:,m,k)
            ENDDO
         ENDDO
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  deallocate memory
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF ((proc%recv_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%recv_int_rk4)
         ENDIF
         IF ((proc%send_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%send_int_rk4)
         ENDIF
         proc => proc%next
      ENDDO
   ENDIF ! l_int_rk4

   IF (l_flt_rk4) THEN
!-----------------------------------------------------------------------
!  receives
!-----------------------------------------------------------------------
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         ALLOCATE (proc%recv_flt_rk4(ndim1,ndim2,proc%recv_total,nlyr))
         proc%recv_flt_rk4(:,:,:,:) = 0.0_dbl_kind
         count = count + 1
         CALL MPI_IRECV (proc%recv_flt_rk4,ndim1*ndim2*proc%recv_total*nlyr, &
                     wrp%flt_type,proc%proc_nmbr,proc%recv_msgtag,wrp%comm, &
                     recv_req(count),ierr)
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  sends
!-----------------------------------------------------------------------
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         ALLOCATE (proc%send_flt_rk4(ndim1,ndim2,proc%send_total,nlyr))
         DO m = 1,proc%send_total
            proc%send_flt_rk4(:,:,m,:) = x_flt_rk4(:,:,proc%n0(m),:)
         ENDDO
         count = count + 1
         CALL MPI_ISEND (proc%send_flt_rk4,ndim1*ndim2*proc%send_total*nlyr, &
                     wrp%flt_type,proc%proc_nmbr,proc%send_msgtag,wrp%comm, &
                     send_req(count),ierr)
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  wait
!-----------------------------------------------------------------------
      CALL MPI_WAITALL (wrp%recv_message_total,recv_req,recv_status,ierr)
      CALL MPI_WAITALL (wrp%send_message_total,send_req,send_status,ierr)
!-----------------------------------------------------------------------
!  unpack
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         DO k = 1,nlyr
            DO m = 1,proc%recv_total
               x_flt_rk4(:,:,proc%n1(m),k) = proc%recv_flt_rk4(:,:,m,k)
            ENDDO
         ENDDO
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  deallocate memory
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF ((proc%recv_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%recv_flt_rk4)
         ENDIF
         IF ((proc%send_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%send_flt_rk4)
         ENDIF
         proc => proc%next
      ENDDO
   ENDIF ! l_flt_rk4

   DEALLOCATE (send_req,recv_req,send_status,recv_status)

   END SUBROUTINE wrp1D_1
!=======================================================================
!  END   wrp1D_1
!=======================================================================

   END MODULE wrp1D_data
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc

