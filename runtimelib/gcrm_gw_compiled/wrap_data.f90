   MODULE wrap_data
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PURPOSE:  perform boundary updates of ghost cell along edges 
!            of subdomain blocks
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers
   USE grid_params
   USE parallel_utilities
   USE utilities_misc
   USE utilities_timer

   IMPLICIT NONE
   SAVE

   TYPE wrap_node
      CHARACTER (LEN=128) :: &
         wrap_name
      LOGICAL (KIND=log_kind) :: &
         l_wrap
      LOGICAL (KIND=log_kind),POINTER :: &
         l_sbdmn_pentagon_north(:),l_sbdmn_pentagon_south(:), &
         l_sbdmn_north_pole(:),l_sbdmn_south_pole(:)
      INTEGER (KIND=int_kind) :: &
         npe_comm,rnk_comm,comm,proc_total, &
         send_message_total,recv_message_total,flt_type
      INTEGER (KIND=int_kind) :: &
         sbdmn_iota
      INTEGER (KIND=int_kind),POINTER :: &
         sbdmn_lst(:),sbdmn_perimeter_type(:,:)
      TYPE (wrap_proc_node),POINTER :: &
         proc
      TYPE (wrap_node),POINTER :: &
         next
   END TYPE wrap_node

   TYPE wrap_proc_node
      INTEGER (KIND=int_kind) :: &
         proc_nmbr
      INTEGER (KIND=int_kind) :: &
         send_total,recv_total,send_msgtag,recv_msgtag
      INTEGER (KIND=int_kind),POINTER :: &
         send_lst(:),i0(:),j0(:),nsd0(:),recv_lst(:),i1(:),j1(:),nsd1(:)
      REAL (KIND=dbl_kind),POINTER :: &
         send_rk2(:,:),send_rk4(:,:,:,:), &
         recv_rk2(:,:),recv_rk4(:,:,:,:)
      TYPE (wrap_list_node),POINTER :: &
         send,recv
      TYPE (wrap_proc_node),POINTER :: &
         next
   END TYPE wrap_proc_node

   TYPE :: wrap_list_node
      INTEGER (KIND=int_kind) :: &
         tag_glbl,i1,j1,nsd1
      TYPE (wrap_list_node),POINTER :: &
         next
   END TYPE wrap_list_node

   LOGICAL (KIND=log_kind) :: &
      l_allocate_wrap_head = .TRUE., &
      l_wrptmr = .FALSE.

   TYPE (wrap_node),POINTER :: &
      wrap_head

   INTEGER (KIND=int_kind) :: &
      msgtag = 19

   CONTAINS
!=======================================================================
!  BEGIN  initialize_wrap
!=======================================================================
   SUBROUTINE initialize_wrap (communicator_name,wrap_name, &
                               level,subdomain_iota,subdomain_list,l_report)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name,wrap_name
   INTEGER (KIND=int_kind) :: &
      level,subdomain_iota,subdomain_list(:)
   LOGICAL (KIND=log_kind),OPTIONAL :: &
      l_report
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      sbdmn_len,rnk,n,m,proc_total,proc_nmbr_send,ix(3),ierr
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      send_total_lst(:),recv_total_lst(:)
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      send_req(:),recv_req(:),send_status(:,:),recv_status(:,:)
   TYPE (wrap_node),POINTER :: &
      wrp
   TYPE (comm_node),POINTER :: &
      comm
   TYPE (wrap_proc_node),POINTER :: &
      proc,proc_send
   TYPE (wrap_list_node),POINTER :: &
      instr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  the first time initialize_wrap is called allocate the head node
!-----------------------------------------------------------------------
   IF (l_allocate_wrap_head) THEN
      l_allocate_wrap_head = .FALSE.
      ALLOCATE (wrap_head); wrp => wrap_head
      wrp%wrap_name = TRIM (wrap_name)
      wrp%l_wrap = .TRUE.
      wrp%proc_total = 0
      wrp%send_message_total = 0; wrp%recv_message_total = 0;
      NULLIFY (wrp%proc);  NULLIFY (wrp%next)                     
   ELSE
!-----------------------------------------------------------------------
!  look through the list to find duplicate wrap_name
!-----------------------------------------------------------------------
      wrp => wrap_head
      DO WHILE (ASSOCIATED (wrp))
         IF (TRIM (wrp%wrap_name)==TRIM (wrap_name)) THEN
            PRINT *," initialize_wrap : wrap_name = ", &
                                  TRIM (wrap_name)," ALREADY EXISTS."
            STOP
         ENDIF
         wrp => wrp%next
      ENDDO
!-----------------------------------------------------------------------
!  add a new node to the end of the list
!-----------------------------------------------------------------------
      wrp => wrap_head
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

   comm => parallel_get_communicator (communicator_name)

!-----------------------------------------------------------------------
!  copy information from the component node to the wrap node
!-----------------------------------------------------------------------
   wrp%npe_comm  = comm%npe_comm
   wrp%rnk_comm  = comm%rnk_comm
   wrp%comm      = comm%comm
   wrp%flt_type  = MPI_DOUBLE_PRECISION

   sbdmn_len = SIZE (subdomain_list,DIM=1)
   ALLOCATE (wrp%sbdmn_lst(sbdmn_len))
   wrp%sbdmn_lst(:) = subdomain_list(:)
   wrp%sbdmn_iota   = subdomain_iota
!-----------------------------------------------------------------------
!  allocate memory for the sbdmn_perimeter_type and set sbdmn_perimeter_type
!-----------------------------------------------------------------------
   ALLOCATE (wrp%sbdmn_perimeter_type(4,sbdmn_len))
   ALLOCATE (wrp%l_sbdmn_pentagon_north(sbdmn_len))
   ALLOCATE (wrp%l_sbdmn_pentagon_south(sbdmn_len))

   ALLOCATE (wrp%l_sbdmn_north_pole(sbdmn_len))
   ALLOCATE (wrp%l_sbdmn_south_pole(sbdmn_len))

   CALL set_sbdmn_perimeter_type (wrp,swmgrid(:),level, &
                                              subdomain_iota,subdomain_list)
!-----------------------------------------------------------------------
!  set receive lists
!-----------------------------------------------------------------------
   CALL set_recv_lst (wrp,swmgrid(:),level,subdomain_iota,subdomain_list)

   proc => wrp%proc
   DO WHILE (ASSOCIATED (proc))
      IF (proc%recv_total > 0) THEN
         IF (proc%proc_nmbr/=wrp%rnk_comm) THEN
            wrp%recv_message_total = wrp%recv_message_total + 1
         ENDIF
         ALLOCATE (proc%recv_lst(proc%recv_total))
         ALLOCATE (proc%i1      (proc%recv_total))
         ALLOCATE (proc%j1      (proc%recv_total))
         ALLOCATE (proc%nsd1    (proc%recv_total))
         instr => proc%recv; m = 0
         DO WHILE (ASSOCIATED (instr))
            m = m + 1
            proc%recv_lst(m) = instr%tag_glbl
            proc%i1      (m) = instr%i1
            proc%j1      (m) = instr%j1
            proc%nsd1    (m) = instr%nsd1
            instr => instr%next
         ENDDO
      ENDIF
      proc => proc%next
   ENDDO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  COMMUNICATE RECEIVE LISTS (FANCY VERSION)  (091112)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   msgtag = msgtag + 1

   proc_total = wrp%proc_total ! the total number of processes the local process communicates with

   IF (proc_total > 0) THEN

      ALLOCATE (send_req(proc_total))
      ALLOCATE (recv_req(proc_total))
      send_req(:) = -999; recv_req(:) = -999;

      ALLOCATE (send_status(MPI_STATUS_SIZE,proc_total))
      ALLOCATE (recv_status(MPI_STATUS_SIZE,proc_total))
      send_status(:,:) = 0; recv_status(:,:) = 0;

      ALLOCATE (recv_total_lst (proc_total))
      ALLOCATE (send_total_lst (proc_total))
!-----------------------------------------------------------------------
!  set the buffers to receive the number of pieces of information that
!  the local process will send to the neighboring process
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO n = 1,proc_total
         send_total_lst(n) = 0
         CALL MPI_IRECV (send_total_lst(n),1,MPI_INTEGER, & 
                         proc%proc_nmbr,111,wrp%comm,recv_req(n),ierr)
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  send the number of pieces of information that the local process
!  expects to receive from the neighboring process
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO n = 1,proc_total
         recv_total_lst(n) = proc%recv_total
         CALL MPI_ISEND (recv_total_lst(n),1,MPI_INTEGER, &  
                         proc%proc_nmbr,111,wrp%comm,send_req(n),ierr) 
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  wait
!-----------------------------------------------------------------------
      CALL MPI_WAITALL (proc_total,recv_req,recv_status,ierr)
      CALL MPI_WAITALL (proc_total,send_req,send_status,ierr)
!-----------------------------------------------------------------------
!  allocate memory for the buffer for the global tags to be sent
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO n = 1,proc_total
         proc%recv_msgtag = msgtag
         proc%send_msgtag = msgtag
         proc%send_total  = send_total_lst(n)
         ALLOCATE (proc%send_lst(send_total_lst(n)))
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  post receives for the global tags to be sent
!-----------------------------------------------------------------------
      send_req(:) = -999; recv_req(:) = -999;
      send_status(:,:) = 0; recv_status(:,:) = 0;

      proc => wrp%proc
      DO n = 1,proc_total
         CALL MPI_IRECV (proc%send_lst,send_total_lst(n),MPI_INTEGER, &
                         proc%proc_nmbr,113,wrp%comm,recv_req(n),ierr)
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  send the list of global tags to be received by the local process
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO n = 1,proc_total
         CALL MPI_ISEND (proc%recv_lst,recv_total_lst(n),MPI_INTEGER, &
                         proc%proc_nmbr,113,wrp%comm,send_req(n),ierr)
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  wait
!-----------------------------------------------------------------------
      CALL MPI_WAITALL (proc_total,recv_req,recv_status,ierr)
      CALL MPI_WAITALL (proc_total,send_req,send_status,ierr)
      DEALLOCATE (send_req,recv_req,send_status,recv_status)
   ENDIF    ! (proc_total > 0)
   CALL MPI_BARRIER (wrp%comm,ierr)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  SET MESSAGE TAGS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (.FALSE.) THEN
   DO rnk = 0,wrp%npe_comm-1
      proc_total = wrp%proc_total
      CALL MPI_BCAST (proc_total,1,MPI_INTEGER,rnk,wrp%comm,ierr)
      IF (proc_total > 0) THEN
         IF (rnk==wrp%rnk_comm) proc => wrp%proc
         DO n = 1,proc_total
            IF (rnk==wrp%rnk_comm) proc_nmbr_send = proc%proc_nmbr
            CALL MPI_BCAST (proc_nmbr_send,1,MPI_INTEGER,rnk,wrp%comm,ierr)
            msgtag = msgtag + 1
            IF (rnk==wrp%rnk_comm) THEN
               proc%recv_msgtag = msgtag
            ENDIF
            IF (proc_nmbr_send==wrp%rnk_comm) THEN
               proc_send => get_wrap_proc (wrp,rnk)
               proc_send%send_msgtag = msgtag
            ENDIF
            IF (rnk==wrp%rnk_comm) proc => proc%next
            CALL MPI_BARRIER (wrp%comm,ierr)
         ENDDO
      ENDIF
   ENDDO 
   ENDIF
!-----------------------------------------------------------------------
!  set send lists
!-----------------------------------------------------------------------
   proc => wrp%proc
   DO WHILE (ASSOCIATED (proc))
      IF (proc%send_total > 0) THEN
         IF (proc%proc_nmbr/=wrp%rnk_comm) THEN
            wrp%send_message_total = wrp%send_message_total + 1
         ENDIF
         ALLOCATE (proc%i0  (proc%send_total))
         ALLOCATE (proc%j0  (proc%send_total))
         ALLOCATE (proc%nsd0(proc%send_total))
         DO n = 1,proc%send_total
            ix(:) = get_index (swmgrid,level,subdomain_iota, &
                                     subdomain_list(:),proc%send_lst(n))
            proc%i0(n) = ix(1); proc%j0(n) = ix(2); proc%nsd0(n) = ix(3)
         ENDDO
      ENDIF
      proc => proc%next
   ENDDO
!-----------------------------------------------------------------------
!  report
!-----------------------------------------------------------------------
   IF (PRESENT (l_report)) THEN
      IF (l_report) THEN
         OPEN (UNIT=17,FILE="./wrp_report/"//TRIM (wrap_name)//"_"// &
                     integer_to_string (2,         level)//"_"// &
                     integer_to_string (2,subdomain_iota)//"_"// &
                     integer_to_string (6,  wrp%rnk_comm),FORM='FORMATTED')

         WRITE (UNIT=17,FMT="(2I6,3I3)") wrp%npe_comm,wrp%rnk_comm, &
                  wrp%proc_total,wrp%recv_message_total,wrp%send_message_total 

         proc_total = wrp%proc_total
         IF (proc_total > 0) THEN
            proc => wrp%proc
            DO n = 1,proc_total
               WRITE (UNIT=17,FMT="(A1)") 
               WRITE (UNIT=17,FMT="(I6)") proc%proc_nmbr
               WRITE (UNIT=17,FMT="(A1)") 
               WRITE (UNIT=17,FMT="(I4,I6)") proc%recv_total,proc%recv_msgtag
               IF (proc%recv_total > 0) THEN
                  DO m = 1,proc%recv_total
                     WRITE (UNIT=17,FMT="(I6,2I4,I3)") &
                           proc%recv_lst(m),proc%i1(m),proc%j1(m),proc%nsd1(m)
                  ENDDO
               ENDIF
               WRITE (UNIT=17,FMT="(A1)") 
               WRITE (UNIT=17,FMT="(I4,I6)") proc%send_total,proc%send_msgtag
               IF (proc%send_total > 0) THEN
                  DO m = 1,proc%send_total
                     WRITE (UNIT=17,FMT="(I6,2I4,I3)") &
                           proc%send_lst(m),proc%i0(m),proc%j0(m),proc%nsd0(m)
                  ENDDO
               ENDIF
               proc => proc%next
            ENDDO
         ENDIF
         CLOSE (UNIT=17)
      ENDIF
   ENDIF

   END SUBROUTINE initialize_wrap
!=======================================================================
!  END  initialize_wrap
!=======================================================================

!=======================================================================
!  BEGIN set_sbdmn_perimeter_type
!=======================================================================
   SUBROUTINE set_sbdmn_perimeter_type (wrp,grid,level,iota,subdomain_list)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrap_node),POINTER :: &
      wrp
   TYPE (grid_node) :: &
      grid(:)
   INTEGER (KIND=int_kind) :: &
      level,iota,subdomain_list(:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      nsd,n,tag_glbl_1,tag_glbl_2
   TYPE (grid_node),POINTER :: &
      ptr1,ptr2,ptr_nghbr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   wrp%sbdmn_perimeter_type(:,:) = 0

   IF (level > 0) THEN

      DO nsd = 1,SIZE (subdomain_list(:))

         tag_glbl_1 = 3+(2**(2*(level_max-iota)))*(subdomain_list(nsd)-1)
         ptr1 => set_ptr (grid(:),level,tag_glbl_1) ! bottom-left cell

! subdomain is adjacent to the north pole
         ptr2 => ptr1
         DO n = 1,2**(level-iota)-1; ptr2 => ptr2%nghbr(3)%p; ENDDO;
         wrp%l_sbdmn_north_pole(nsd) = ptr2%nghbr(3)%p%l_pole_north

! subdomain is adjacent to the south pole
         ptr2 => ptr1
         DO n = 1,2**(level-iota)-1; ptr2 => ptr2%nghbr(1)%p; ENDDO;
         wrp%l_sbdmn_south_pole(nsd) = ptr2%nghbr(1)%p%l_pole_south

         ptr2 => ptr1
         DO n = 1,2**(level-iota)-1; ptr2 => ptr2%nghbr(2)%p; ENDDO;

         tag_glbl_2 = ptr2%tag_glbl ! top-right cell

         wrp%l_sbdmn_pentagon_north(nsd) = ptr1%l_pentagon_north
         wrp%l_sbdmn_pentagon_south(nsd) = ptr1%l_pentagon_south

! edge 1 : bottom edge
         IF (ptr1%l_pentagon_south) THEN
            wrp%sbdmn_perimeter_type(1,nsd) = 1
         ELSE
            ptr_nghbr => ptr1%nghbr(6)%p
            IF (ptr_nghbr%nghbr(3)%p%tag_glbl/=ptr1%tag_glbl) THEN
               wrp%sbdmn_perimeter_type(1,nsd) = 1
            ENDIF
         ENDIF

! edge 2 : right edge
         ptr_nghbr => ptr2%nghbr(1)%p
         IF (ptr_nghbr%nghbr(4)%p%tag_glbl/=ptr2%tag_glbl) THEN
            wrp%sbdmn_perimeter_type(2,nsd) = 2
         ENDIF

! edge 3 : top edge
         ptr_nghbr => ptr2%nghbr(3)%p
         IF (ptr_nghbr%nghbr(6)%p%tag_glbl/=ptr2%tag_glbl) THEN
            wrp%sbdmn_perimeter_type(3,nsd) = 3
         ENDIF

! edge 4 : left edge
         IF (ptr1%l_pentagon_north) THEN
            wrp%sbdmn_perimeter_type(4,nsd) = 4
         ELSE
         ptr_nghbr => ptr1%nghbr(4)%p
            IF (ptr_nghbr%nghbr(1)%p%tag_glbl/=ptr1%tag_glbl) THEN
               wrp%sbdmn_perimeter_type(4,nsd) = 4
            ENDIF
         ENDIF
      ENDDO
   ENDIF

   END SUBROUTINE set_sbdmn_perimeter_type
!=======================================================================
!  BEGIN set_sbdmn_perimeter_type
!=======================================================================

!=======================================================================
!  BEGIN set_recv_lst
!=======================================================================
   SUBROUTINE set_recv_lst (wrp,grid,level,iota,subdomain_list)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrap_node),POINTER :: &
      wrp
   TYPE (grid_node) :: &
      grid(:)
   INTEGER (KIND=int_kind) :: &
      level,iota,subdomain_list(:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      tsklen,sbdmn_north(5),sbdmn_south(5),el,nsd,tag_glbl,m,n,offset, &
      tag_north(5),tag_south(5)
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      r1(:),r2(:),n1(:),i1(:),j1(:),di1(:),dj1(:)
   TYPE (grid_node),POINTER :: &
      ptr0,ptr1
   TYPE (wrap_proc_node),POINTER :: &
      proc
   TYPE (wrap_list_node),POINTER :: &
      instr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   el = 2**(level-iota)

   DO nsd = 1,SIZE (subdomain_list(:))

      tag_glbl = 3+(2**(2*(level_max-iota)))*(subdomain_list(nsd)-1)
      ptr0 => set_ptr (grid(:),level,tag_glbl)

      tsklen = 6

      ALLOCATE (r1(tsklen),r2(tsklen),n1(tsklen))
      ALLOCATE (i1(tsklen),j1(tsklen),di1(tsklen),dj1(tsklen))

      n1 = (/   i1i,   el,   el,  i1i,   el,   el /)
      r1 = (/   i0i,  i1i,  i3i,  i0i,  i4i,  i6i /)
      r2 = (/   i5i,  i6i,  i1i,  i2i,  i3i,  i4i /)

      i1  = (/    i1i,   i2i,el+i2i,el+i2i,el+i1i,   i1i /)
      j1  = (/    i1i,   i1i,   i2i,el+i2i,el+i2i,el+i1i /)
      di1 = (/    i0i,   i1i,   i0i,   i0i,  -i1i,   i0i /)
      dj1 = (/    i0i,   i0i,   i1i,   i0i,   i0i,  -i1i /)

      DO m = 1,tsklen
         DO n = 1,n1(m)
            IF (ASSOCIATED (ptr0%nghbr(r2(m))%p)) THEN
               ptr1  => ptr0%nghbr(r2(m))%p
               proc  => get_wrap_proc (wrp,ptr1%proc)
               instr => get_wrap_list ("recv",proc)
               instr%tag_glbl = ptr1%tag_glbl
               instr%i1 = i1(m)+di1(m)*(n-1); instr%j1 = j1(m)+dj1(m)*(n-1)
               instr%nsd1 = nsd
            ENDIF
            IF (n < n1(m)) ptr0 => ptr0%nghbr(r1(m))%p
         ENDDO
      ENDDO

      DEALLOCATE (r1,r2,n1,i1,j1,di1,dj1)
!-----------------------------------------------------------------------
!  north pole
!-----------------------------------------------------------------------
      sbdmn_north(:)=(/ (       2**(2*iota)*m,m=1,9,2) /)
      IF (ANY (sbdmn_north(:)==subdomain_list(nsd))) THEN
         ptr0 => set_ptr (grid(:),level,1_int_kind)
         tag_north(:) = (/ (ptr0%nghbr(m)%p%tag_glbl,m=1,5) /)

         DO m = 1,5
            IF (subdomain_list(nsd)==sbdmn_north(1)) EXIT
            sbdmn_north(:) = CSHIFT (sbdmn_north(:),SHIFT=1)
            tag_north  (:) = CSHIFT (tag_north  (:),SHIFT=1)
         ENDDO

         ptr1 => set_ptr (grid(:),level,tag_north(3))
         proc  => get_wrap_proc (wrp,ptr1%proc)
         instr => get_wrap_list ("recv",proc)
         instr%tag_glbl = ptr1%tag_glbl
         instr%i1 = el+2; instr%j1 = 1; instr%nsd1 = nsd

         ptr1 => set_ptr (grid(:),level,tag_north(4))
         proc  => get_wrap_proc (wrp,ptr1%proc)
         instr => get_wrap_list ("recv",proc)
         instr%tag_glbl = ptr1%tag_glbl
         instr%i1 = 1; instr%j1 = el+2; instr%nsd1 = nsd
      ENDIF
!-----------------------------------------------------------------------
!  south pole
!-----------------------------------------------------------------------
      offset = 1+(((2**iota+1)*(2**iota-1))/3)
      sbdmn_south(:)=(/ (offset+2**(2*iota)*m,m=1,9,2) /)
      IF (ANY (sbdmn_south(:)==subdomain_list(nsd))) THEN
         ptr0 => set_ptr (grid(:),level,2_int_kind)
         tag_south(:) = (/ (ptr0%nghbr(m)%p%tag_glbl,m=5,1,-1) /)

         DO m = 1,5
            IF (subdomain_list(nsd)==sbdmn_south(1)) EXIT
            sbdmn_south(:) = CSHIFT (sbdmn_south(:),SHIFT=1)
            tag_south  (:) = CSHIFT (tag_south  (:),SHIFT=1)
         ENDDO

         ptr1 => set_ptr (grid(:),level,tag_south(4))
         proc  => get_wrap_proc (wrp,ptr1%proc)
         instr => get_wrap_list ("recv",proc)
         instr%tag_glbl = ptr1%tag_glbl
         instr%i1 = el+2; instr%j1 = 1; instr%nsd1 = nsd

         ptr1 => set_ptr (grid(:),level,tag_south(3))
         proc  => get_wrap_proc (wrp,ptr1%proc)
         instr => get_wrap_list ("recv",proc)
         instr%tag_glbl = ptr1%tag_glbl
         instr%i1 = 1; instr%j1 = el+2; instr%nsd1 = nsd
      ENDIF
   ENDDO

   END SUBROUTINE set_recv_lst
!=======================================================================
!  BEGIN set_recv_lst
!=======================================================================

!=======================================================================
! BEGIN get_wrap_proc
!=======================================================================
   FUNCTION get_wrap_proc (wrp,proc_nmbr) RESULT (node)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrap_node),POINTER :: wrp
   INTEGER (KIND=int_kind) :: proc_nmbr
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (wrap_proc_node),POINTER :: node
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: l_found
   TYPE (wrap_proc_node),POINTER :: proc
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (proc_nmbr==rnk_nonexistent) THEN
      PRINT *," get_wrap_proc :: proc_nmbr==rnk_nonexistent "
      STOP
   ENDIF

   IF (.NOT.ASSOCIATED (wrp%proc)) THEN
      ALLOCATE (wrp%proc)
      wrp%proc_total = 1
      proc => wrp%proc
      proc%proc_nmbr = proc_nmbr; proc%send_total = 0; proc%recv_total = 0
      proc%send_msgtag = -1; proc%recv_msgtag = -1;
      NULLIFY (proc%send); NULLIFY (proc%recv); NULLIFY (proc%next)
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
         NULLIFY (proc%send); NULLIFY (proc%recv); NULLIFY (proc%next)
      ENDIF
   ENDIF

   node => proc
   
   END FUNCTION get_wrap_proc
!=======================================================================
!  END get_wrap_proc
!=======================================================================

!=======================================================================
! BEGIN get_wrap_list
!=======================================================================
   FUNCTION get_wrap_list (option,proc) RESULT (node)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: option
   TYPE (wrap_proc_node),POINTER :: proc
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (wrap_list_node),POINTER :: node
!.......................................................................
!  LOCAL
!.......................................................................
   TYPE (wrap_list_node),POINTER :: instr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (TRIM (option)=="send") THEN
      proc%send_total = proc%send_total + 1
      IF (.NOT.ASSOCIATED (proc%send)) THEN
         ALLOCATE (proc%send); instr => proc%send; NULLIFY (instr%next)
      ELSE
         instr => proc%send
         DO WHILE (ASSOCIATED (instr%next))
            instr => instr%next
         ENDDO
         ALLOCATE (instr%next); instr => instr%next; NULLIFY (instr%next)
      ENDIF
   ENDIF

   IF (TRIM (option)=="recv") THEN
      proc%recv_total = proc%recv_total + 1
      IF (.NOT.ASSOCIATED (proc%recv)) THEN
         ALLOCATE (proc%recv); instr => proc%recv; NULLIFY (instr%next)
      ELSE
         instr => proc%recv
         DO WHILE (ASSOCIATED (instr%next))
            instr => instr%next
         ENDDO
         ALLOCATE (instr%next); instr => instr%next; NULLIFY (instr%next)
      ENDIF
   ENDIF

   node => instr
   
   END FUNCTION get_wrap_list
!=======================================================================
!  END get_wrap_list
!=======================================================================

!=======================================================================
!  BEGIN wrap
!=======================================================================
   SUBROUTINE wrap (wrap_name, &
                      face,face_1lyr, &
                      vrtx,vrtx_1lyr,vrtx_scalar,vrtx_scalar_1lyr, &
                      edge,edge_1lyr,edge_scalar,edge_scalar_1lyr )
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
!.......................................................................
!  INTENT IN AND OUT
!.......................................................................
   REAL (KIND=dbl_kind),OPTIONAL :: &
      face       (    :,:,:,:),        face_1lyr(    :,:,  :), &
      vrtx       (:,:,:,:,:,:),        vrtx_1lyr(:,:,:,:,  :), &
      vrtx_scalar(  :,:,:,:,:), vrtx_scalar_1lyr(  :,:,:,  :), &
      edge       (:,:,:,:,:,:),        edge_1lyr(:,:,:,:,  :), &
      edge_scalar(  :,:,:,:,:), edge_scalar_1lyr(  :,:,:,  :)
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_found
   INTEGER (KIND=int_kind) :: &
      i3(3),i4(4),i5(5)
   REAL (KIND=dbl_kind),ALLOCATABLE :: &
      temp_rk4(:,:,:,:),temp_rk6(:,:,:,:,:,:)
   TYPE (wrap_node),POINTER :: &
      wrp
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  find the wrap node associated with wrap_name
!-----------------------------------------------------------------------
   IF (l_allocate_wrap_head) THEN
      PRINT *,' wrap : no wrap nodes '
   ELSE
      wrp => wrap_head
      l_found = .FALSE.
      DO WHILE (ASSOCIATED (wrp))
         IF (TRIM (wrp%wrap_name)==TRIM (wrap_name)) THEN
            l_found = .TRUE.
            EXIT
         ENDIF
         wrp => wrp%next
      ENDDO
      IF (.NOT.l_found) THEN
         PRINT *," wrap : the wrap instructions = ", &
                         TRIM (wrap_name)," has not been initialized."
         STOP
      ENDIF
   ENDIF
!-----------------------------------------------------------------------
!  if SCM then return
!-----------------------------------------------------------------------
   IF (.NOT.wrp%l_wrap) RETURN
!-----------------------------------------------------------------------
!  wrap
!-----------------------------------------------------------------------
   IF (PRESENT (face)) THEN
      CALL adjust_face (wrp,face)
      CALL wrap_1 (wrp,x1=face)
   ENDIF
   IF (PRESENT (vrtx)) THEN
      CALL wrap_1 (wrp,x2=vrtx)
      CALL adjust_vrtx (wrp,vrtx)
   ENDIF
   IF (PRESENT (edge)) THEN
      CALL wrap_1 (wrp,x2=edge)
      CALL adjust_edge (wrp,edge)
   ENDIF

   IF (PRESENT (face_1lyr)) THEN
      i3(:) = SHAPE (face_1lyr)
      ALLOCATE (temp_rk4(i3(1),i3(2),1,i3(3)))
      temp_rk4(:,:,1,:) = face_1lyr(:,:,:)
      CALL adjust_face (wrp,temp_rk4)
      CALL wrap_1 (wrp,x1=temp_rk4)
      face_1lyr(:,:,:) = temp_rk4(:,:,1,:)
   ENDIF

   IF (PRESENT (vrtx_1lyr)) THEN
      i5(:) = SHAPE (vrtx_1lyr)
      ALLOCATE (temp_rk6(i5(1),i5(2),i5(3),i5(4),1,i5(5)))
      temp_rk6(:,:,:,:,1,:) = vrtx_1lyr(:,:,:,:,:)
      CALL wrap_1 (wrp,x2=temp_rk6)
      CALL adjust_vrtx (wrp,temp_rk6)
      vrtx_1lyr(:,:,:,:,:) = temp_rk6(:,:,:,:,1,:)
   ENDIF

   IF (PRESENT (edge_1lyr)) THEN
      i5(:) = SHAPE (edge_1lyr)
      ALLOCATE (temp_rk6(i5(1),i5(2),i5(3),i5(4),1,i5(5)))
      temp_rk6(:,:,:,:,1,:) = edge_1lyr(:,:,:,:,:)
      CALL wrap_1 (wrp,x2=temp_rk6)
      CALL adjust_edge (wrp,temp_rk6)
      edge_1lyr(:,:,:,:,:) = temp_rk6(:,:,:,:,1,:)
   ENDIF

   IF (PRESENT (vrtx_scalar)) THEN
      i5(:) = SHAPE (vrtx_scalar)
      ALLOCATE (temp_rk6(1,i5(1),i5(2),i5(3),i5(4),i5(5)))
      temp_rk6(1,:,:,:,:,:) = vrtx_scalar(:,:,:,:,:)
      CALL wrap_1 (wrp,x2=temp_rk6)
      CALL adjust_vrtx (wrp,temp_rk6)
      vrtx_scalar(:,:,:,:,:) = temp_rk6(1,:,:,:,:,:)
   ENDIF
   
   IF (PRESENT (edge_scalar)) THEN
      i5(:) = SHAPE (edge_scalar)
      ALLOCATE (temp_rk6(1,i5(1),i5(2),i5(3),i5(4),i5(5)))
      temp_rk6(1,:,:,:,:,:) = edge_scalar(:,:,:,:,:)
      CALL wrap_1 (wrp,x2=temp_rk6)
      CALL adjust_edge (wrp,temp_rk6)
      edge_scalar(:,:,:,:,:) = temp_rk6(1,:,:,:,:,:)
   ENDIF

   IF (PRESENT (vrtx_scalar_1lyr)) THEN
      i4(:) = SHAPE (vrtx_scalar_1lyr)
      ALLOCATE (temp_rk6(1,i4(1),i4(2),i4(3),1,i4(4)))
      temp_rk6(1,:,:,:,1,:) = vrtx_scalar_1lyr(:,:,:,:)
      CALL wrap_1 (wrp,x2=temp_rk6)
      CALL adjust_vrtx (wrp,temp_rk6)
      vrtx_scalar_1lyr(:,:,:,:) = temp_rk6(1,:,:,:,1,:)
   ENDIF
   
   IF (PRESENT (edge_scalar_1lyr)) THEN
      i4(:) = SHAPE (edge_scalar_1lyr)
      ALLOCATE (temp_rk6(1,i4(1),i4(2),i4(3),1,i4(4)))
      temp_rk6(1,:,:,:,1,:) = edge_scalar_1lyr(:,:,:,:)
      CALL wrap_1 (wrp,x2=temp_rk6)
      CALL adjust_edge (wrp,temp_rk6)
      edge_scalar_1lyr(:,:,:,:) = temp_rk6(1,:,:,:,1,:)
   ENDIF

   IF (ALLOCATED  (temp_rk4)) DEALLOCATE (temp_rk4)
   IF (ALLOCATED  (temp_rk6)) DEALLOCATE (temp_rk6)

   END SUBROUTINE wrap
!=======================================================================
!  END wrap
!=======================================================================

!=======================================================================
!  BEGIN  wrap_1
!=======================================================================
   SUBROUTINE wrap_1 (wrp,x1,x2)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrap_node),POINTER :: &
      wrp
!.......................................................................
!  INTENT IN AND OUT
!.......................................................................
   REAL (KIND=dbl_kind),OPTIONAL :: &
      x1(:,:,:,:),x2(:,:,:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_rk4,l_rk6
   INTEGER (KIND=int_kind) :: &
      n1,n2,nlyr,nsdm,count,m,n,iota,k,nsd, &
      sbdmn_pentagon_north(5),sbdmn_pentagon_south(5),ierr
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      send_req(:),recv_req(:),send_status(:,:),recv_status(:,:)
   TYPE (wrap_proc_node),POINTER :: &
      proc
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   l_rk4 = .FALSE.; l_rk6 = .FALSE.;

   IF (PRESENT (x1)) THEN
      nlyr = SIZE (x1,DIM=3)
      nsdm = SIZE (x1,DIM=4)
      l_rk4 = .TRUE.
   ENDIF
   IF (PRESENT (x2)) THEN
      n1   = SIZE (x2,DIM=1)
      n2   = SIZE (x2,DIM=2)
      nlyr = SIZE (x2,DIM=5)
      nsdm = SIZE (x2,DIM=6)
      l_rk6 = .TRUE.
   ENDIF

   ALLOCATE (send_req(wrp%send_message_total))
   ALLOCATE (recv_req(wrp%recv_message_total))
   send_req(:) = -999; recv_req(:) = -999

   ALLOCATE (send_status(MPI_STATUS_SIZE,wrp%send_message_total))
   ALLOCATE (recv_status(MPI_STATUS_SIZE,wrp%recv_message_total))
   send_status(:,:) = 0; recv_status(:,:) = 0
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  rk4 CODE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (l_rk4) THEN
!-----------------------------------------------------------------------
!  receives
!-----------------------------------------------------------------------
      IF (l_wrptmr) CALL timer (event_name="wrp_recv",action="start")
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF ((proc%recv_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            ALLOCATE (proc%recv_rk2(proc%recv_total,nlyr))
            proc%recv_rk2(:,:) = 0.0_dbl_kind
            count = count + 1
            CALL MPI_IRECV (proc%recv_rk2,proc%recv_total*nlyr, &
                     wrp%flt_type,proc%proc_nmbr,proc%recv_msgtag,wrp%comm, &
                     recv_req(count),ierr)
         ENDIF
         proc => proc%next
      ENDDO
      IF (l_wrptmr) CALL timer (event_name="wrp_recv",action="stop")
!-----------------------------------------------------------------------
!  sends
!-----------------------------------------------------------------------
      IF (l_wrptmr) CALL timer (event_name="wrp_send",action="start")
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF ((proc%send_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            ALLOCATE (proc%send_rk2(proc%send_total,nlyr))
            DO m = 1,proc%send_total
               proc%send_rk2(m,:) = x1(proc%i0(m),proc%j0(m),:,proc%nsd0(m))
            ENDDO
            count = count + 1
            CALL MPI_ISEND (proc%send_rk2,proc%send_total*nlyr, &
                     wrp%flt_type,proc%proc_nmbr,proc%send_msgtag,wrp%comm, &
                     send_req(count),ierr)
         ENDIF
         proc => proc%next
      ENDDO
      IF (l_wrptmr) CALL timer (event_name="wrp_send",action="stop")
!-----------------------------------------------------------------------
!  wrap local
!-----------------------------------------------------------------------
      IF (l_wrptmr) CALL timer (event_name="wrp_locl",action="start")
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF (proc%proc_nmbr==wrp%rnk_comm) THEN
            DO k = 1,nlyr
               DO m = 1,proc%recv_total
                  x1(proc%i1(m),proc%j1(m),k,proc%nsd1(m)) = &
                                x1(proc%i0(m),proc%j0(m),k,proc%nsd0(m))
               ENDDO
            ENDDO
         ENDIF
         proc => proc%next
      ENDDO
      IF (l_wrptmr) CALL timer (event_name="wrp_locl",action="stop")
!-----------------------------------------------------------------------
!  wait
!-----------------------------------------------------------------------
      IF (l_wrptmr) CALL timer (event_name="wrp_wait",action="start")
      CALL MPI_WAITALL (wrp%recv_message_total,recv_req,recv_status,ierr)
      CALL MPI_WAITALL (wrp%send_message_total,send_req,send_status,ierr)
      IF (l_wrptmr) CALL timer (event_name="wrp_wait",action="stop")
!-----------------------------------------------------------------------
!  unpack
!-----------------------------------------------------------------------
      IF (l_wrptmr) CALL timer (event_name="wrp_unpk",action="start")
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF (proc%recv_total>0) THEN
            IF (proc%proc_nmbr/=wrp%rnk_comm) THEN
               DO k = 1,nlyr
                  DO m = 1,proc%recv_total
                     x1(proc%i1(m),proc%j1(m),k,proc%nsd1(m)) = &
                                                      proc%recv_rk2(m,k)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
         proc => proc%next
      ENDDO
      IF (l_wrptmr) CALL timer (event_name="wrp_unpk",action="stop")
!-----------------------------------------------------------------------
!  deallocate memory
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF ((proc%recv_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%recv_rk2)
         ENDIF
         IF ((proc%send_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%send_rk2)
         ENDIF
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  special
!-----------------------------------------------------------------------
      iota = wrp%sbdmn_iota
      sbdmn_pentagon_north(:)=(/ (1+2**(2*iota)*n,n=0,8,2) /)
      sbdmn_pentagon_south(:)=(/ (1+2**(2*iota)*n,n=1,9,2) /)
      DO nsd = 1,SIZE (wrp%sbdmn_lst(:))
         IF (ANY (sbdmn_pentagon_north(:)==wrp%sbdmn_lst(nsd))) THEN
            x1(1,2,:,nsd) = x1(1,1,:,nsd)
         ENDIF
         IF (ANY (sbdmn_pentagon_south(:)==wrp%sbdmn_lst(nsd))) THEN
            x1(2,1,:,nsd) = x1(1,1,:,nsd)
         ENDIF
      ENDDO
   ENDIF ! rk4

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! rk6 CODE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (l_rk6) THEN
!-----------------------------------------------------------------------
!  receives
!-----------------------------------------------------------------------
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF ((proc%recv_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            ALLOCATE (proc%recv_rk4(n1,n2,proc%recv_total,nlyr))
            proc%recv_rk4(:,:,:,:) = 0.0_dbl_kind
            count = count + 1
            CALL MPI_IRECV (proc%recv_rk4,n1*n2*proc%recv_total*nlyr, &
                     wrp%flt_type,proc%proc_nmbr,proc%recv_msgtag,wrp%comm, &
                     recv_req(count),ierr)
         ENDIF
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  sends
!-----------------------------------------------------------------------
      count = 0
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF ((proc%send_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            ALLOCATE (proc%send_rk4(n1,n2,proc%send_total,nlyr))
            DO m = 1,proc%send_total
               proc%send_rk4(:,:,m,:) = &
                                  x2(:,:,proc%i0(m),proc%j0(m),:,proc%nsd0(m))
            ENDDO
            count = count + 1
            CALL MPI_ISEND (proc%send_rk4,n1*n2*proc%send_total*nlyr, &
                     wrp%flt_type,proc%proc_nmbr,proc%send_msgtag,wrp%comm, &
                     send_req(count),ierr)
         ENDIF
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  wrap local
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF (proc%proc_nmbr==wrp%rnk_comm) THEN
            DO k = 1,nlyr
               DO m = 1,proc%recv_total
                  x2(:,:,proc%i1(m),proc%j1(m),k,proc%nsd1(m)) = &
                                x2(:,:,proc%i0(m),proc%j0(m),k,proc%nsd0(m))
               ENDDO
            ENDDO
         ENDIF
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
         IF (proc%recv_total>0) THEN
            IF (proc%proc_nmbr/=wrp%rnk_comm) THEN
               DO k = 1,nlyr
                  DO m = 1,proc%recv_total
                     x2(:,:,proc%i1(m),proc%j1(m),k,proc%nsd1(m)) = &
                                                      proc%recv_rk4(:,:,m,k)
                  ENDDO
               ENDDO
            ENDIF
         ENDIF
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  deallocate memory
!-----------------------------------------------------------------------
      proc => wrp%proc
      DO WHILE (ASSOCIATED (proc))
         IF ((proc%recv_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%recv_rk4)
         ENDIF
         IF ((proc%send_total>0).AND.(proc%proc_nmbr/=wrp%rnk_comm)) THEN
            DEALLOCATE (proc%send_rk4)
         ENDIF
         proc => proc%next
      ENDDO
!-----------------------------------------------------------------------
!  special
!-----------------------------------------------------------------------
      iota = wrp%sbdmn_iota
      sbdmn_pentagon_north(:)=(/ (1+2**(2*iota)*n,n=0,8,2) /)
      sbdmn_pentagon_south(:)=(/ (1+2**(2*iota)*n,n=1,9,2) /)
      DO nsd = 1,SIZE (wrp%sbdmn_lst(:))
         IF (ANY (sbdmn_pentagon_north(:)==wrp%sbdmn_lst(nsd))) THEN
            x2(:,:,1,2,:,nsd) = x2(:,:,1,1,:,nsd)
         ENDIF
         IF (ANY (sbdmn_pentagon_south(:)==wrp%sbdmn_lst(nsd))) THEN
            x2(:,:,2,1,:,nsd) = x2(:,:,1,1,:,nsd)
         ENDIF
      ENDDO
   ENDIF ! rk6

   DEALLOCATE (send_req,recv_req,send_status,recv_status)

   END SUBROUTINE wrap_1
!=======================================================================
!  END   wrap_1
!=======================================================================

!=======================================================================
!  BEGIN adjust_face
!=======================================================================
   SUBROUTINE adjust_face (wrp,face)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrap_node),POINTER :: &
      wrp
!.......................................................................
!  INTENT IN AND OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      face(:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      im,jm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   im = SIZE (face,DIM=1); jm = SIZE (face,DIM=2);

   face(1,jm,:,:) = face(2,2,:,:)
   face(im,1,:,:) = face(2,2,:,:)

   END SUBROUTINE adjust_face
!=======================================================================
!  END adjust_face
!=======================================================================

!=======================================================================
!  BEGIN adjust_edge
!=======================================================================
   SUBROUTINE adjust_edge (wrp,edge)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrap_node),POINTER :: &
      wrp
!.......................................................................
!  INTENT IN AND OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      edge(:,:,:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      im,jm,i,j,nsd
!   INTEGER (KIND=int_kind) :: &
!      iota,n,sbdmn_pentagon_north(5),sbdmn_pentagon_south(5)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

!   iota = wrp%sbdmn_iota
!   sbdmn_pentagon_north(:)=(/ (1+2**(2*iota)*n,n=0,8,2) /)
!   sbdmn_pentagon_south(:)=(/ (1+2**(2*iota)*n,n=1,9,2) /)

   im = SIZE (edge,DIM=3); jm = SIZE (edge,DIM=4);

   DO nsd = 1,SIZE (wrp%sbdmn_lst(:))
!-----------------------------------------------------------------------
! BOTTOM EDGE
!-----------------------------------------------------------------------
      IF (wrp%sbdmn_perimeter_type(1,nsd)==1) THEN
         IF (wrp%l_sbdmn_pentagon_south(nsd)) THEN
            DO i = 2,im-1
               edge(:,1,i-1,1,:,nsd) = edge(:,3,i, 1,:,nsd)
               edge(:,1:3,i,1,:,nsd) = &
                                 CSHIFT (edge(:,1:3,i,1,:,nsd),SHIFT=-1,DIM=2)
            ENDDO
            edge(:,1,1,1,:,nsd) = zero
         ELSE
            edge(:,1:3,1,1,:,nsd) = &
                                 CSHIFT (edge(:,1:3,1,1,:,nsd),SHIFT=-1,DIM=2)
            DO i = 2,im-1
               edge(:,1,i-1,1,:,nsd) = edge(:,3,i, 1,:,nsd)
               edge(:,1:3,i,1,:,nsd) = &
                                 CSHIFT (edge(:,1:3,i,1,:,nsd),SHIFT=-1,DIM=2)
            ENDDO
         ENDIF
      ENDIF
!-----------------------------------------------------------------------
! RIGHT EDGE
!-----------------------------------------------------------------------
      IF (wrp%sbdmn_perimeter_type(2,nsd)==2) THEN
         edge(:,1:3,im,2,:,nsd) = &
                                CSHIFT (edge(:,1:3,im,2,:,nsd),SHIFT= 1,DIM=2)
         DO j = 3,jm
            edge(:,3,im,j-1,:,nsd) = edge(:,1,im,j,:,nsd)
            edge(:,1:3,im,j,:,nsd) = &
                                CSHIFT (edge(:,1:3,im,j,:,nsd),SHIFT= 1,DIM=2)
         ENDDO
      ENDIF
!-----------------------------------------------------------------------
! TOP EDGE
!-----------------------------------------------------------------------
      IF (wrp%sbdmn_perimeter_type(3,nsd)==3) THEN
         edge(:,1:3,2,jm,:,nsd) = &
                                CSHIFT (edge(:,1:3,2,jm,:,nsd),SHIFT=-1,DIM=2)
         DO i = 3,im
            edge(:,1,i-1,jm,:,nsd) = edge(:,3,i,jm,:,nsd)
            edge(:,1:3,i,jm,:,nsd) = &
                                CSHIFT (edge(:,1:3,i,jm,:,nsd),SHIFT=-1,DIM=2)
         ENDDO
      ENDIF
!-----------------------------------------------------------------------
! LEFT EDGE
!-----------------------------------------------------------------------
      IF (wrp%sbdmn_perimeter_type(4,nsd)==4) THEN
         IF (wrp%l_sbdmn_pentagon_north(nsd)) THEN
            DO j = 2,jm-1
               edge(:,3,1,j-1,:,nsd) = edge(:,1,1,j,:,nsd)
               edge(:,1:3,1,j,:,nsd) = &
                                 CSHIFT (edge(:,1:3,1,j,:,nsd),SHIFT= 1,DIM=2)
            ENDDO
            edge(:,3,1,1,:,nsd) = zero
         ELSE
            edge(:,1:3,1,1,:,nsd) = &
                                 CSHIFT (edge(:,1:3,1,1,:,nsd),SHIFT= 1,DIM=2)
            DO j = 2,jm-1
               edge(:,3,1,j-1,:,nsd) = edge(:,1,1,j,:,nsd)
               edge(:,1:3,1,j,:,nsd) = &
                                 CSHIFT (edge(:,1:3,1,j,:,nsd),SHIFT= 1,DIM=2)
            ENDDO
         ENDIF
      ENDIF
!-----------------------------------------------------------------------
! NORTH AND SOUTH POLE
!-----------------------------------------------------------------------
      edge(:,1:3,1,jm,:,nsd) = & ! north pole
                                CSHIFT (edge(:,1:3,1,jm,:,nsd),SHIFT=-1,DIM=2)
      edge(:,1:3,im,1,:,nsd) = & ! south pole
                                CSHIFT (edge(:,1:3,im,1,:,nsd),SHIFT= 1,DIM=2)
   ENDDO

   END SUBROUTINE adjust_edge
!=======================================================================
!  END adjust_edge
!=======================================================================

!=======================================================================
!  BEGIN adjust_vrtx
!=======================================================================
   SUBROUTINE adjust_vrtx (wrp,vrtx)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (wrap_node),POINTER :: &
      wrp
!.......................................................................
!  INTENT IN AND OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      vrtx(:,:,:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      im,jm,i,j,nsd
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   im = SIZE (vrtx,DIM=3); jm = SIZE (vrtx,DIM=4);

   DO nsd = 1,SIZE (wrp%sbdmn_lst(:))
!-----------------------------------------------------------------------
!  BOTTOM EDGE
!-----------------------------------------------------------------------
      IF (wrp%sbdmn_perimeter_type(1,nsd)==1) THEN
         IF (wrp%l_sbdmn_pentagon_south(nsd)) THEN
            vrtx(:,2,1,1,:,nsd) = vrtx(:,2,1,1,:,nsd)
            vrtx(:,2,2,1,:,nsd) = vrtx(:,1,1,1,:,nsd)
            DO i = 3,im-1
               vrtx(:,1,i-1,1,:,nsd) = vrtx(:,2,i,1,:,nsd)
               vrtx(:,2,i  ,1,:,nsd) = vrtx(:,1,i,1,:,nsd)
            ENDDO
         ELSE
            DO i = 1,im-2
               vrtx(:,2,i,1,:,nsd) = vrtx(:,1,i  ,1,:,nsd)
               vrtx(:,1,i,1,:,nsd) = vrtx(:,2,i+1,1,:,nsd)
            ENDDO
            vrtx(:,2,im-1,1,:,nsd) = vrtx(:,1,im-1,1,:,nsd)
         ENDIF
      ENDIF
!-----------------------------------------------------------------------
!  RIGHT EDGE
!-----------------------------------------------------------------------
      IF (wrp%sbdmn_perimeter_type(2,nsd)==2) THEN
         DO j = 2,jm-1
            vrtx(:,1,im,j  ,:,nsd) = vrtx(:,2,im,j  ,:,nsd)
            vrtx(:,2,im,j  ,:,nsd) = vrtx(:,1,im,j+1,:,nsd)
         ENDDO
      ENDIF
!-----------------------------------------------------------------------
!  TOP EDGE
!-----------------------------------------------------------------------
      IF (wrp%sbdmn_perimeter_type(3,nsd)==3) THEN
         DO i = 2,im-1
            vrtx(:,2,i  ,jm,:,nsd) = vrtx(:,1,i  ,jm,:,nsd)
            vrtx(:,1,i  ,jm,:,nsd) = vrtx(:,2,i+1,jm,:,nsd)
         ENDDO
      ENDIF
!-----------------------------------------------------------------------
!  LEFT EDGE
!-----------------------------------------------------------------------
      IF (wrp%sbdmn_perimeter_type(4,nsd)==4) THEN
         IF (wrp%l_sbdmn_pentagon_north(nsd)) THEN
            vrtx(:,1,1,1,:,nsd) = vrtx(:,1,1,1,:,nsd)
            vrtx(:,1,1,2,:,nsd) = vrtx(:,2,1,1,:,nsd)
            DO j = 2,jm-1
               vrtx(:,2,1,j-1,:,nsd) = vrtx(:,1,1,j  ,:,nsd)
               vrtx(:,1,1,j  ,:,nsd) = vrtx(:,2,1,j  ,:,nsd)
            ENDDO
         ELSE
            DO j = 1,jm-2
               vrtx(:,1,1,j  ,:,nsd) = vrtx(:,2,1,j  ,:,nsd)
               vrtx(:,2,1,j  ,:,nsd) = vrtx(:,1,1,j+1,:,nsd)
            ENDDO
            vrtx(:,1,1,jm-1,:,nsd) = vrtx(:,2,1,jm-1,:,nsd)
         ENDIF
      ENDIF
!-----------------------------------------------------------------------
!  NORTH POLE. see wrap_convention_corner_north_pole.png
!-----------------------------------------------------------------------
      IF (wrp%l_sbdmn_north_pole(nsd)) THEN
         vrtx(:,2,1,jm-1,:,nsd) = vrtx(:,2, 1,jm,:,nsd)
         vrtx(:,1,1,jm  ,:,nsd) = vrtx(:,2,im, 1,:,nsd)
         vrtx(:,2,1,jm  ,:,nsd) = vrtx(:,1,im, 1,:,nsd)
         vrtx(:,2,2,jm  ,:,nsd) = -999.0_dbl_kind ! undefined
      ENDIF
!-----------------------------------------------------------------------
!  SOUTH POLE. see wrap_convention_corner_south_pole.png
!-----------------------------------------------------------------------
      IF (wrp%l_sbdmn_south_pole(nsd)) THEN
         vrtx(:,1,im-1,1,:,nsd) = vrtx(:,1,im, 1,:,nsd)
         vrtx(:,1,im  ,1,:,nsd) = vrtx(:,2, 1,jm,:,nsd)
         vrtx(:,2,im  ,1,:,nsd) = vrtx(:,1, 1,jm,:,nsd)
         vrtx(:,1,im  ,2,:,nsd) = -999.0_dbl_kind ! undefined
      ENDIF
   ENDDO

   END SUBROUTINE adjust_vrtx
!=======================================================================
!  END adjust_vrtx
!=======================================================================

!=======================================================================
!  BEGIN l_wrapped
!=======================================================================
   FUNCTION l_wrapped (wrap_name,face,face_1lyr) RESULT (l_wrp)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),OPTIONAL :: &
      face(:,:,:,:),face_1lyr(:,:,:)
!.......................................................................
!  INTENT IN
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_wrp
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_found
   INTEGER (KIND=int_kind) :: &
      i3(3),i4(4)
   REAL (KIND=dbl_kind),ALLOCATABLE :: &
      temp_rk4(:,:,:,:)
   TYPE (wrap_node),POINTER :: &
      wrp
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  find the wrap node associated with wrap_name
!-----------------------------------------------------------------------
   IF (l_allocate_wrap_head) THEN
      PRINT *,' wrap : no wrap nodes '
   ELSE
      wrp => wrap_head
      l_found = .FALSE.
      DO WHILE (ASSOCIATED (wrp))
         IF (TRIM (wrp%wrap_name)==TRIM (wrap_name)) THEN
            l_found = .TRUE.
            EXIT
         ENDIF
         wrp => wrp%next
      ENDDO
      IF (.NOT.l_found) THEN
         PRINT *," wrap : the wrap instructions = ", &
                         TRIM (wrap_name)," has not been initialized."
         STOP
      ENDIF
   ENDIF

   l_wrp = .TRUE.

   IF (PRESENT (face)) THEN
      i4(:) = SHAPE (face)
      ALLOCATE (temp_rk4(i4(1),i4(2),i4(3),i4(4)))
      temp_rk4(:,:,:,:) = face(:,:,:,:)
      CALL adjust_face (wrp,temp_rk4)
      CALL wrap_1 (wrp,x1=temp_rk4)
      IF (SUM (ABS (temp_rk4-face)) > 0.0000001_dbl_kind) l_wrp = .FALSE.
   ENDIF

   IF (PRESENT (face_1lyr)) THEN
      i3(:) = SHAPE (face_1lyr)
      ALLOCATE (temp_rk4(i3(1),i3(2),1,i3(3)))
      temp_rk4(:,:,1,:) = face_1lyr(:,:,:)
      CALL adjust_face (wrp,temp_rk4)
      CALL wrap_1 (wrp,x1=temp_rk4)
      IF (SUM (ABS (temp_rk4-face)) > 0.0000001_dbl_kind) l_wrp = .FALSE.
   ENDIF

   IF (ALLOCATED  (temp_rk4)) DEALLOCATE (temp_rk4)

   END FUNCTION l_wrapped
!=======================================================================
!  END l_wrapped
!=======================================================================

   END MODULE wrap_data
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc

