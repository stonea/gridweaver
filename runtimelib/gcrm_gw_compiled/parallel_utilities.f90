   MODULE parallel_utilities
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Purpose: This module contains parallel processing specific code
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers

   USE parallel_include
   USE parallel_params

   IMPLICIT NONE
   SAVE

   INTEGER (KIND=int_kind) :: &
      status

   LOGICAL (KIND=log_kind) :: &
      l_parallel_active

   TYPE comm_node
      CHARACTER (LEN=128) :: &
         comm_name
      LOGICAL (KIND=log_kind) :: &
         l_member,  &! .TRUE. iff the local process is a member of the communicator
         l_io_root   ! .TRUE. iff the local process is root of a io communicator
      INTEGER (KIND=int_kind) :: &
         npe_wrld,rnk_wrld,handle_wrld, &
         npe_comm,rnk_comm,handle_comm,comm
      TYPE (comm_node),POINTER :: &
         next
   END TYPE comm_node

   LOGICAL (KIND=log_kind) :: &
      l_initialize_parallel = .TRUE., &
      l_allocate_comm_head  = .TRUE.

   TYPE (comm_node),POINTER :: &
      comm_head

   TYPE (comm_node),POINTER :: &
      comm_io

   INTEGER (KIND=int_kind) :: &
      npe_alloc,rnk_alloc

   CONTAINS
!======================================================================
!  BEGIN parallel_initialize
!======================================================================
   SUBROUTINE parallel_initialize ()

!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,pe,ierr
   INTEGER (KIND=4) :: &
      vls(8)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  initialize MPI
!-----------------------------------------------------------------------
   IF (l_initialize_parallel) THEN

      CALL MPI_INIT (ierr)
      CALL MPI_COMM_SIZE (MPI_COMM_WORLD,npe_alloc,ierr) ! the number of processes allocated
      CALL MPI_COMM_RANK (MPI_COMM_WORLD,rnk_alloc,ierr) ! local process rank within allocated set

      IF (npe_alloc < npe_wrld) THEN ! the number of processes allocated is less the declared number of processes
         PRINT *," parallel_initialize : npe_alloc < npe_wrld "
         PRINT *,"                       npe_alloc = ",npe_alloc
         PRINT *,"                       npe_wrld  = ",npe_wrld
         CALL parallel_finalize ()
         STOP
      ENDIF

      IF (MOD (npe_wrld,npe_io) /= 0) THEN ! npe_wrld must be evenly divisible by npe_io
         PRINT *," parallel_initialize : npe_wrld must be evenly divisible by npe_io "
         PRINT *,"                       npe_wrld = ",npe_wrld
         PRINT *,"                       npe_io  =  ",npe_io
         CALL parallel_finalize ()
         STOP
      ENDIF

      IF (npe_alloc == npe_wrld) THEN ! the number of processes allocated is equal to the declared number of processes
         l_parallel_active = .TRUE.
         rnk_wrld = rnk_alloc
      ELSE ! some processes are not active
         IF (rnk_alloc==0) PRINT *," parallel_initialize : some processes are not active "
         IF (rnk_alloc==0) PRINT *," parallel_initialize : npe_alloc > npe_wrld "
         IF (rnk_alloc==0) PRINT *,"                       npe_alloc = ",npe_alloc
         IF (rnk_alloc==0) PRINT *,"                       npe_wrld  = ",npe_wrld
         IF (rnk_alloc < npe_wrld) THEN
            l_parallel_active = .TRUE.
            rnk_wrld = rnk_alloc
         ELSE
            l_parallel_active = .FALSE.
            rnk_wrld = rnk_alloc
         ENDIF
      ENDIF
!-----------------------------------------------------------------------
!  print the time when parallel_initialize is called
!-----------------------------------------------------------------------
      IF (rnk_alloc==0) THEN
         CALL DATE_AND_TIME (VALUES=vls)
         PRINT "(A32,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I2)", &
            " parallel_initialize :: called ",vls(2),"/",vls(3),"/",vls(1), &
            " at ",vls(5),":",vls(6),":",vls(7)
      ENDIF
!-----------------------------------------------------------------------
!  say hello
!-----------------------------------------------------------------------
      IF (npe_alloc <= i8i) THEN
         DO pe = 0,npe_alloc-1
            IF (pe==rnk_alloc) PRINT ("(I4,A36,L1)"),rnk_alloc, &
                     ": ~~ hello ~~ : l_parallel_active = ",l_parallel_active
            CALL MPI_BARRIER (MPI_COMM_WORLD,ierr)
         ENDDO
      ENDIF
   ENDIF ! l_initialize_parallel

   l_initialize_parallel = .FALSE.
!-----------------------------------------------------------------------
!  always create a communicator called "world"
!-----------------------------------------------------------------------
   CALL parallel_create_communicator ("world")
!-----------------------------------------------------------------------
!  a string for the local process
!-----------------------------------------------------------------------
   WRITE (UNIT=rnk_wrld_strng,FMT="(I08)") rnk_wrld
   DO n = 1,8; IF (rnk_wrld_strng(n:n)==" ") rnk_wrld_strng(n:n) = '0'; ENDDO

   END SUBROUTINE parallel_initialize
!=======================================================================
!  END parallel_initialize
!=======================================================================

!=======================================================================
!  BEGIN parallel_create_communicator 
!=======================================================================
   SUBROUTINE parallel_create_communicator (comm_name,comm_ranks)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),INTENT(IN) :: &
      comm_name
   INTEGER (KIND=int_kind),INTENT(IN),OPTIONAL :: &
      comm_ranks(:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      i,ierr
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      ranks(:)
   TYPE (comm_node),POINTER :: &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  check that subroutine parallel_initialize has been called
!-----------------------------------------------------------------------
   IF ((l_initialize_parallel).AND.(rnk_wrld==0)) THEN
      PRINT *," parallel_create_communicator : parallel_initialize", &
              " has must be called before parallel_create_communicator "
      CALL parallel_finalize ()
      STOP
   ENDIF
!-----------------------------------------------------------------------
!  the first time parallel_create_communicator is called 
!  allocate the head of the communicator list
!-----------------------------------------------------------------------
   IF (l_allocate_comm_head) THEN
      l_allocate_comm_head = .FALSE.
      ALLOCATE (comm_head); NULLIFY (comm_head%next)
      comm => comm_head
   ELSE
!-----------------------------------------------------------------------
!  look through the list to find duplicate comm_name
!-----------------------------------------------------------------------
      comm => comm_head
      DO WHILE (ASSOCIATED (comm))
         IF (TRIM (comm%comm_name)==TRIM (comm_name)) THEN
            PRINT *," parallel_create_communicator : comm_name = ", &
                                    TRIM (comm_name),"   already exists"
         ENDIF
         comm => comm%next
      ENDDO
!-----------------------------------------------------------------------
!  add a new node to the end of the list
!-----------------------------------------------------------------------
      comm => comm_head
      DO WHILE (ASSOCIATED (comm%next))
         comm => comm%next
      ENDDO
      ALLOCATE (comm%next)
      comm => comm%next
      NULLIFY  (comm%next)
   ENDIF

   comm%comm_name = comm_name
   comm%npe_wrld  = npe_wrld
   comm%rnk_wrld  = rnk_wrld
!-----------------------------------------------------------------------
!  define processor ranks
!-----------------------------------------------------------------------
   IF (PRESENT (comm_ranks)) THEN
      comm%npe_comm = SIZE (comm_ranks)
      ALLOCATE (ranks(comm%npe_comm))
      DO i = 1,comm%npe_comm
         IF ((comm_ranks(i) < i0i).OR.(npe_wrld < comm_ranks(i))) THEN
            PRINT *," parallel_create_communicator : comm_ranks(",i,") = ", &
                                          comm_ranks(i)," out of range "
            CALL parallel_finalize ()
            STOP
         ENDIF
         ranks(i) = comm_ranks(i)
      ENDDO
   ELSE
      comm%npe_comm = comm%npe_wrld
      ALLOCATE (ranks(comm%npe_comm))
      DO i = 1,comm%npe_comm
         ranks(i) = i-1
      ENDDO
   ENDIF
!-----------------------------------------------------------------------
!  check that the local process is a member of the communicator
!-----------------------------------------------------------------------
   comm%l_member = (ANY (rnk_wrld==ranks(:)))

!-----------------------------------------------------------------------
!  create the communicator
!-----------------------------------------------------------------------
   CALL MPI_COMM_GROUP (MPI_COMM_WORLD,comm%handle_wrld,ierr)
   CALL MPI_GROUP_INCL (comm%handle_wrld,comm%npe_comm,ranks, &
                                                  comm%handle_comm,ierr)
   CALL MPI_COMM_CREATE (MPI_COMM_WORLD,comm%handle_comm,comm%comm,ierr)
!-----------------------------------------------------------------------
!  if the local process is a member then set rank
!-----------------------------------------------------------------------
   IF (comm%l_member) THEN
      CALL MPI_COMM_RANK (comm%comm,comm%rnk_comm,ierr)
   ELSE
      comm%rnk_comm    = -999
   ENDIF

   DEALLOCATE (ranks)

   END SUBROUTINE parallel_create_communicator
!=======================================================================
!  END parallel_create_communicator
!=======================================================================

!=======================================================================
!  BEGIN parallel_get_communicator 
!=======================================================================
   FUNCTION parallel_get_communicator (comm_name) RESULT (comm)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),INTENT(IN) :: &
      comm_name
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (comm_node),POINTER :: &
      comm
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_found
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (l_initialize_parallel) THEN
      PRINT *," parallel_get_communicator :", &
      " parallel_initialize must be called before parallel_get_communicator "
      CALL parallel_finalize ()
      STOP
   ELSE
      comm => comm_head
      l_found = .FALSE.
      DO WHILE (ASSOCIATED (comm))
         IF (TRIM (comm%comm_name)==TRIM (comm_name)) THEN
            l_found = .TRUE.
            EXIT
         ENDIF
         comm => comm%next
      ENDDO
      IF (.NOT.l_found) THEN
         PRINT *," parallel_get_communicator :", &
                          " the communicator named ",TRIM (comm_name), &
                                                " has not been created "
         CALL parallel_finalize ()
         STOP
      ENDIF
   ENDIF

   END FUNCTION parallel_get_communicator
!=======================================================================
!  END parallel_get_communicator
!=======================================================================

!=======================================================================
!  BEGIN parallel_show_communicator_list 
!=======================================================================
   SUBROUTINE parallel_show_communicator_list ()
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (comm_node),POINTER :: &
      comm
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      pe
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   DO pe = 0,npe_wrld-1

      IF (pe==rnk_wrld) THEN
         PRINT *,pe,":~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

         comm => comm_head
         DO WHILE (ASSOCIATED (comm))
            PRINT *,pe,": ",TRIM (comm%comm_name),comm%l_member,comm%rnk_comm
            comm => comm%next
         ENDDO
      ENDIF

      CALL parallel_barrier ("world")
   ENDDO

   END SUBROUTINE parallel_show_communicator_list
!=======================================================================
!  END parallel_show_communicator_list
!=======================================================================

!=======================================================================
!  BEGIN parallel_barrier
!=======================================================================
   SUBROUTINE parallel_barrier (comm_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),INTENT(IN),OPTIONAL :: &
      comm_name
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      ierr
   TYPE (comm_node),POINTER :: &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (PRESENT (comm_name)) THEN
      comm => parallel_get_communicator (comm_name)
      IF (comm%l_member) CALL MPI_BARRIER (comm%comm,ierr)
   ELSE
       CALL MPI_BARRIER (MPI_COMM_WORLD,ierr)
   ENDIF

   END SUBROUTINE parallel_barrier
!=======================================================================
!  END parallel_barrier
!=======================================================================

!=======================================================================
!  BEGIN parallel_nonblocking_send
!=======================================================================
   SUBROUTINE parallel_nonblocking_send (comm_name,pe_recv_rank,msg_tag, &
                    communication_handle,buffer_1,buffer_int,buffer_dbl)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),INTENT(IN) :: &
      comm_name            ! name of communicator
   INTEGER (KIND=int_kind) :: &
      pe_recv_rank,       &! rank within the communicator of the receiving pe
      msg_tag,            &! message tag
      communication_handle ! communication request handle
   INTEGER (KIND=int_kind),OPTIONAL :: &
      buffer_1             ! buffer for integer type
   INTEGER (KIND=int_kind),OPTIONAL :: &
      buffer_int(:)        ! buffer for integer type
   REAL (KIND=dbl_kind),OPTIONAL :: &
      buffer_dbl(:,:,:,:)  ! buffer for real type
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      buffer_length, &!
      buffer_type,   &!
      ierr
   INTEGER (KIND=int_kind) :: &
      errorcode=0,ierror=0
   TYPE (comm_node),POINTER :: &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   comm => parallel_get_communicator (comm_name)

!-----------------------------------------------------------------------
! send a message of integer type
!-----------------------------------------------------------------------
   IF (PRESENT (buffer_1)) THEN
      buffer_type   = MPI_INTEGER
      CALL MPI_ISEND (buffer_1,1,buffer_type,pe_recv_rank, &
                            msg_tag,comm%comm,communication_handle,ierr)
   ENDIF
   IF (PRESENT (buffer_int)) THEN
      buffer_length = SIZE (buffer_int,DIM=1)
      buffer_type   = MPI_INTEGER
      CALL MPI_ISEND (buffer_int(1),buffer_length,buffer_type,pe_recv_rank, &
                            msg_tag,comm%comm,communication_handle,ierr)
   ENDIF
!-----------------------------------------------------------------------
! send a message of real type
!-----------------------------------------------------------------------
   IF (PRESENT (buffer_dbl)) THEN
      SELECT CASE (dbl_kind)
         CASE (SELECTED_REAL_KIND (06))
            buffer_type = MPI_REAL
         CASE (SELECTED_REAL_KIND (12))
            buffer_type = MPI_DOUBLE_PRECISION
         CASE DEFAULT
            PRINT *," parallel_nonblocking_send :: ", &
                                   "MPI_precision of real numbers unknown"
            CALL MPI_ABORT (comm%comm,errorcode,ierror)
      END SELECT
      buffer_length = SIZE (buffer_dbl)
!060824 CALL MPI_ISEND (buffer_dbl(1,1,1,1),buffer_length,buffer_type, &
!060824        pe_recv_rank,msg_tag,comm%comm,communication_handle,ierr)
   ENDIF

   END SUBROUTINE parallel_nonblocking_send
!=======================================================================
!  END parallel_nonblocking_send
!=======================================================================

!=======================================================================
!  BEGIN parallel_nonblocking_recv
!=======================================================================
   SUBROUTINE parallel_nonblocking_recv (comm_name,pe_send_rank,msg_tag, &
                    communication_handle,buffer_1,buffer_int,buffer_dbl)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),INTENT(IN) ::                                           &
      comm_name            ! name of communicator
   INTEGER (KIND=int_kind) ::                                                &
      pe_send_rank,       &! rank within the communicator of the sending pe
      msg_tag,            &! message tag
      communication_handle ! communication request handle
   INTEGER (KIND=int_kind),OPTIONAL ::                                       &
      buffer_1             ! buffer for integer type
   INTEGER (KIND=int_kind),OPTIONAL ::                                       &
      buffer_int(:)        ! buffer for integer type
   REAL (KIND=dbl_kind),OPTIONAL ::                                          &
      buffer_dbl(:,:,:,:)  ! buffer for real type
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) ::                                                &
      buffer_length, &!
      buffer_type,   &!
      ierr
   INTEGER (KIND=int_kind) ::                                                &
      errorcode=0,ierror=0
   TYPE (comm_node),POINTER ::                                               &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   comm => parallel_get_communicator (comm_name)

!-----------------------------------------------------------------------
! receive a message of integer type
!-----------------------------------------------------------------------
   IF (PRESENT (buffer_1)) THEN
      buffer_type   = MPI_INTEGER
!060824 CALL MPI_IRECV (buffer_1,1,buffer_type,pe_send_rank,                   &
!060824                    msg_tag,comm%comm,communication_handle,ierr)
   ENDIF
   IF (PRESENT (buffer_int)) THEN
      buffer_length = SIZE (buffer_int,DIM=1)
      buffer_type   = MPI_INTEGER
!060824 CALL MPI_IRECV (buffer_int,buffer_length,buffer_type,pe_send_rank,     &
!060824                     msg_tag,comm%comm,communication_handle,ierr)
   ENDIF
!-----------------------------------------------------------------------
! receive a message of real type
!-----------------------------------------------------------------------
   IF (PRESENT (buffer_dbl)) THEN
      SELECT CASE (dbl_kind)
         CASE (SELECTED_REAL_KIND (06))
            buffer_type = MPI_REAL
         CASE (SELECTED_REAL_KIND (12))
            buffer_type = MPI_DOUBLE_PRECISION
         CASE DEFAULT
            PRINT *," parallel_nonblocking_recv :: ", &
                                   "MPI_precision of real numbers unknown"
            CALL MPI_ABORT (comm%comm,errorcode,ierror)
      END SELECT
      buffer_length = SIZE (buffer_dbl)
      CALL MPI_IRECV (buffer_dbl,buffer_length,buffer_type,pe_send_rank, &
                            msg_tag,comm%comm,communication_handle,ierr)
   ENDIF

   END SUBROUTINE parallel_nonblocking_recv
!=======================================================================
!  END parallel_nonblocking_recv
!=======================================================================

!=======================================================================
!  BEGIN parallel_nonblocking_waitall
!=======================================================================
   SUBROUTINE parallel_nonblocking_waitall (send_count,recv_count, &
                                                      send_req,recv_req)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      send_count,          &! length of send list
      recv_count,          &! length of receive list
      send_req(send_count),&! active handles associated with send requests
      recv_req(recv_count)  ! active handles associated with receive requests
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      ierr
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      send_status(:,:),recv_status(:,:)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
! receive a message of integer type
!-----------------------------------------------------------------------
   ALLOCATE (send_status(MPI_STATUS_SIZE,send_count))
   ALLOCATE (recv_status(MPI_STATUS_SIZE,recv_count))
   send_status(:,:) = i0i; recv_status(:,:) = i0i

   CALL MPI_WAITALL (send_count,send_req,send_status,ierr)
   CALL MPI_WAITALL (recv_count,recv_req,recv_status,ierr)

   DEALLOCATE (send_status,recv_status)

   END SUBROUTINE parallel_nonblocking_waitall
!=======================================================================
!  END parallel_nonblocking_waitall
!=======================================================================

!=======================================================================
!  BEGIN parallel_broadcast
!=======================================================================
   SUBROUTINE parallel_broadcast (comm_name,broadcast_rnk, &
                int_rk0,int_rk1,flt_rk0,flt_rk1)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      comm_name        ! name of communicator
   INTEGER (KIND=int_kind),OPTIONAL :: &
      broadcast_rnk        ! broadcasting rank within the communicator
   INTEGER (KIND=int_kind),OPTIONAL :: &
      int_rk0,int_rk1(:) ! buffer for integer types
   REAL (KIND=dbl_kind),OPTIONAL :: &
      flt_rk0,flt_rk1(:) ! buffer for float types
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      broadcast_rnk_tmpry,buffer_length,buffer_type,ierr
   INTEGER (KIND=int_kind) :: &
      errorcode=0,ierror=0
   TYPE (comm_node),POINTER :: &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   comm => parallel_get_communicator (comm_name)

   IF (PRESENT (broadcast_rnk)) THEN
     broadcast_rnk_tmpry = broadcast_rnk
   ELSE
     broadcast_rnk_tmpry = 0
   ENDIF
!-----------------------------------------------------------------------
!  message is of integer type
!-----------------------------------------------------------------------
   IF (PRESENT (int_rk0)) THEN
      buffer_length = 1
      buffer_type   = MPI_INTEGER
      CALL MPI_BCAST (int_rk0,buffer_length,buffer_type, &
                                         broadcast_rnk_tmpry,comm%comm,ierr)
   ENDIF
   IF (PRESENT (int_rk1)) THEN
      buffer_length = SIZE (int_rk1,DIM=1)
      buffer_type   = MPI_INTEGER
      CALL MPI_BCAST (int_rk1,buffer_length,buffer_type, &
                                         broadcast_rnk_tmpry,comm%comm,ierr)
   ENDIF
!-----------------------------------------------------------------------
!  message is of float type
!-----------------------------------------------------------------------
   IF ((PRESENT (flt_rk0)).OR.(PRESENT (flt_rk1))) THEN
      SELECT CASE (dbl_kind)
         CASE (SELECTED_REAL_KIND (06))
            buffer_type = MPI_REAL
         CASE (SELECTED_REAL_KIND (12))
            buffer_type = MPI_DOUBLE_PRECISION
         CASE DEFAULT
            PRINT *," parallel_broadcast :: ", &
                            "MPI_precision of floating point numbers unknown"
            CALL MPI_ABORT (comm%comm,errorcode,ierror)
      END SELECT
      IF (PRESENT (flt_rk0)) THEN
         buffer_length = 1
         CALL MPI_BCAST (flt_rk0,buffer_length,buffer_type, &
                                         broadcast_rnk_tmpry,comm%comm,ierr)
      ENDIF
      IF (PRESENT (flt_rk1)) THEN
         buffer_length = SIZE (flt_rk1,DIM=1)
         CALL MPI_BCAST (flt_rk1,buffer_length,buffer_type, &
                                         broadcast_rnk_tmpry,comm%comm,ierr)
      ENDIF
   ENDIF

   END SUBROUTINE parallel_broadcast
!=======================================================================
!  END parallel_broadcast
!=======================================================================

!=======================================================================
!  BEGIN FUNCTION parallel_reduce
!=======================================================================
   FUNCTION parallel_reduce (comm_name,reduce_op,reduce_rnk,l_reduce_all, &
                          flt_rk0,flt_rk1) RESULT (reduce_result)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      comm_name,  &! name of communicator
      reduce_op    ! string tolken for reduction operation
   INTEGER (KIND=int_kind),OPTIONAL :: &
      reduce_rnk
   LOGICAL (KIND=log_kind),OPTIONAL :: &
      l_reduce_all
   REAL (KIND=dbl_kind),OPTIONAL :: &
      flt_rk0,flt_rk1(:) ! buffers for float type
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: & 
      reduce_result
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_reduce_all_tmpry
   INTEGER (KIND=int_kind) :: &
      count_send,count_recv,reduce_rnk_tmpry,op_handle,buffer_type,ierr
   REAL (KIND=dbl_kind) :: &
      buffer_send,buffer_recv
   INTEGER (KIND=int_kind) :: &
      errorcode=0,ierror=0
   TYPE (comm_node),POINTER :: &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   comm => parallel_get_communicator (comm_name)

   IF (PRESENT (reduce_rnk)) THEN
     reduce_rnk_tmpry = reduce_rnk
   ELSE
     reduce_rnk_tmpry = 0
   ENDIF

   IF (PRESENT (l_reduce_all)) THEN
     l_reduce_all_tmpry = l_reduce_all
   ELSE
     l_reduce_all_tmpry = .FALSE.
   ENDIF
!-----------------------------------------------------------------------
!  determine reduction operation and set the send buffer
!-----------------------------------------------------------------------
   SELECT CASE (TRIM (reduce_op))
      CASE ("min")       
         op_handle = MPI_MIN
         IF (PRESENT (flt_rk0)) buffer_send = flt_rk0
         IF (PRESENT (flt_rk1)) buffer_send = MINVAL (flt_rk1)
      CASE ("max")       
         op_handle = MPI_MAX
         IF (PRESENT (flt_rk0)) buffer_send = flt_rk0
         IF (PRESENT (flt_rk1)) buffer_send = MAXVAL (flt_rk1)
      CASE ("sum") 
         op_handle = MPI_SUM
         IF (PRESENT (flt_rk0)) buffer_send = flt_rk0
         IF (PRESENT (flt_rk1)) buffer_send = SUM (flt_rk1)
      CASE ("summation") 
         op_handle = MPI_SUM
         IF (PRESENT (flt_rk0)) buffer_send = flt_rk0
         IF (PRESENT (flt_rk1)) buffer_send = SUM (flt_rk1)
      CASE ("product")   
         op_handle = MPI_PROD
         IF (PRESENT (flt_rk0)) buffer_send = flt_rk0
         IF (PRESENT (flt_rk1)) buffer_send = PRODUCT (flt_rk1)
      CASE ("average") 
         op_handle = MPI_SUM
         IF (PRESENT (flt_rk0)) buffer_send = flt_rk0
         IF (PRESENT (flt_rk1)) buffer_send = SUM (flt_rk1)

      CASE DEFAULT
         PRINT *," parallel_reduce :: cannot determine reduction operation"
         CALL MPI_ABORT (comm%comm,errorcode,ierror)
   END SELECT
!-----------------------------------------------------------------------
!  set the buffer type
!-----------------------------------------------------------------------
   SELECT CASE (dbl_kind)
      CASE (SELECTED_REAL_KIND (06))
         buffer_type = MPI_REAL
      CASE (SELECTED_REAL_KIND (12))
         buffer_type = MPI_DOUBLE_PRECISION
      CASE DEFAULT
         PRINT *," parallel_reduce :: ", &
                                   "MPI_precision of real numbers unknown"
         CALL MPI_ABORT (comm%comm,errorcode,ierror)
   END SELECT

   buffer_recv = -999.0_dbl_kind

   IF (l_reduce_all_tmpry) THEN
      CALL MPI_ALLREDUCE (buffer_send,buffer_recv,1,buffer_type, &
                                                     op_handle,comm%comm,ierr)
   ELSE
      CALL MPI_REDUCE (buffer_send,buffer_recv,1,buffer_type, &
                                    op_handle,reduce_rnk_tmpry,comm%comm,ierr)
   ENDIF

   IF (TRIM (reduce_op)=="average") THEN
      IF (PRESENT (flt_rk0)) count_send = 1
      IF (PRESENT (flt_rk1)) count_send = SIZE (flt_rk1)

      IF (l_reduce_all_tmpry) THEN
         CALL MPI_ALLREDUCE (count_send,count_recv,1,MPI_INTEGER, &
                                                      MPI_SUM,comm%comm,ierr)
         buffer_recv = buffer_recv/DBLE (count_recv)
      ELSE
         CALL MPI_REDUCE (count_send,count_recv,1,MPI_INTEGER, &
                                     MPI_SUM,reduce_rnk_tmpry,comm%comm,ierr)
         IF (comm%rnk_comm==reduce_rnk_tmpry) THEN
            buffer_recv = buffer_recv/DBLE (count_recv)
         ENDIF
      ENDIF
   ENDIF

   reduce_result = buffer_recv

   END FUNCTION parallel_reduce
!=======================================================================
!  BEGIN FUNCTION parallel_reduce
!=======================================================================

!=======================================================================
!  BEGIN parallel_write_processor_name
!=======================================================================
   SUBROUTINE parallel_write_processor_name (comm_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),OPTIONAL :: &
      comm_name    ! name of communicator
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      pe,strng_len,ierr
   CHARACTER (LEN=MPI_MAX_PROCESSOR_NAME) :: &
      pe_name_strng
   CHARACTER (LEN=128) :: &
      comm_strng

   TYPE (comm_node),POINTER :: &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (PRESENT (comm_name)) THEN
      comm => parallel_get_communicator (comm_name)
      comm_strng = comm_name
   ELSE
      comm => parallel_get_communicator ("world")
      comm_strng = "world"
   ENDIF

   IF (comm%l_member) THEN
      CALL MPI_GET_PROCESSOR_NAME (pe_name_strng,strng_len,ierr)

      DO pe = 0,comm%npe_comm-1
         IF (pe==comm%rnk_comm) THEN
            PRINT ("(A12,I6,A32)"),TRIM (comm_strng),comm%rnk_comm,TRIM (pe_name_strng)
         ENDIF
         CALL MPI_BARRIER (MPI_COMM_WORLD,ierr)
      ENDDO
   ENDIF

   END SUBROUTINE parallel_write_processor_name
!=======================================================================
!  END parallel_write_processor_name
!=======================================================================

!=======================================================================
!  BEGIN parallel_finalize
!=======================================================================
   SUBROUTINE parallel_finalize ()
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      pe,ierr
   INTEGER (KIND=4) :: &
      vls(8)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   CALL MPI_BARRIER (MPI_COMM_WORLD,ierr)

!-----------------------------------------------------------------------
!  print the time when parallel_finalize is called
!-----------------------------------------------------------------------
   IF (rnk_wrld==0) THEN
      CALL DATE_AND_TIME (VALUES=vls)
      PRINT "(A29,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I2)", &
         " parallel_finalize :: called ",vls(2),"/",vls(3),"/",vls(1), &
         " at ",vls(5),":",vls(6),":",vls(7)
   ENDIF
!-----------------------------------------------------------------------
!  say goodbye
!-----------------------------------------------------------------------
   IF (npe_wrld <= i8i) THEN
      DO pe = 0,npe_wrld-1
         IF (pe==rnk_wrld) PRINT ("(I4,A16)"),rnk_wrld,": ~~ goodbye ~~ "
         CALL MPI_BARRIER (MPI_COMM_WORLD,ierr)
      ENDDO
   ENDIF

   CALL MPI_FINALIZE (ierr)

   END SUBROUTINE parallel_finalize
!=======================================================================
!  END parallel_finalize
!=======================================================================

!=======================================================================
!  BEGIN parallel_stop
!=======================================================================
   SUBROUTINE parallel_stop (message)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),OPTIONAL :: &
      message
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (PRESENT (message)) THEN
      PRINT *," parallel_stop :: message = ",TRIM (message)
   ENDIF

   CALL parallel_finalize ()
   STOP

   END SUBROUTINE parallel_stop
!=======================================================================
!  END parallel_stop
!=======================================================================

   END MODULE parallel_utilities
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
