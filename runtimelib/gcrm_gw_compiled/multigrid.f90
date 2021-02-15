   MODULE multigrid
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

   USE wrap_data
   USE utilities_timer
   USE utilities_misc

   IMPLICIT NONE
   PRIVATE

   PUBLIC :: &
      mg_node,mg_proc_node,mg,mg_list_node
   PUBLIC :: &
      mltgrd_init,mltgrd_wrap_threshold

   SAVE

   TYPE mg_node
      CHARACTER (LEN=13) :: &
         mg_strng
      LOGICAL (KIND=log_kind) :: &
         l_active
      INTEGER (KIND=int_kind) :: &
         proc_total,send_message_total,recv_message_total
      INTEGER (KIND=int_kind) :: &
         im,jm,nsdm,sbdmn_iota
      INTEGER (KIND=int_kind),ALLOCATABLE :: &
         sbdmn_lst(:)
      REAL (KIND=dbl_kind),POINTER :: &
         wght(:,:,:,:),area(:,:,:),area_inv(:,:,:)
      TYPE (mg_proc_node),POINTER :: &
         proc
   END TYPE mg_node

   TYPE mg_proc_node
      INTEGER (KIND=int_kind) :: &
         proc_nmbr
      INTEGER (KIND=int_kind) :: &
         send_total,recv_total,send_msgtag,recv_msgtag
      INTEGER (KIND=int_kind),POINTER :: &
         send_lst(:),i0(:),j0(:),nsd0(:),recv_lst(:),i1(:),j1(:),nsd1(:)
      REAL (KIND=dbl_kind),POINTER :: &
         send_rk2(:,:),recv_rk2(:,:)
      TYPE (mg_list_node),POINTER :: &
         recv,recv_tail
      TYPE (mg_proc_node),POINTER :: &
         next
   END TYPE mg_proc_node

   TYPE :: mg_list_node
      INTEGER (KIND=int_kind) :: &
         tag_glbl,i1,j1,nsd1
      TYPE (mg_list_node),POINTER :: &
         next
   END TYPE mg_list_node

   TYPE (mg_node) :: &
      mg(0:level_max)

   INTEGER (KIND=int_kind),PARAMETER :: &
      lvl_min = 0

   LOGICAL (KIND=log_kind) :: &
      l_mlgtmr = .FALSE., &!
      l_squnc  = .FALSE.

   CONTAINS
!=======================================================================
!  BEGIN  mltgrd_init
!=======================================================================
   SUBROUTINE mltgrd_init (communicator_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl,iota
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  set grid related values for the mg_node
!-----------------------------------------------------------------------
   DO lvl = 0,level_max

      mg(lvl)%sbdmn_iota = sbdmn(lvl)%sbdmn_iota

      iota   = mg(lvl)%sbdmn_iota
      mg(lvl)%im = 2**(lvl-iota)+2; mg(lvl)%jm = 2**(lvl-iota)+2

      mg(lvl)%nsdm = SIZE (sbdmn(lvl)%lst(:))
      ALLOCATE (mg(lvl)%sbdmn_lst(mg(lvl)%nsdm))
      IF (mg(lvl)%nsdm > 0) THEN
         mg(lvl)%sbdmn_lst(:) = sbdmn(lvl)%lst(:)
      ENDIF

      IF (mg(lvl)%nsdm > 0) THEN
          mg(lvl)%l_active = .TRUE.
      ELSE
          mg(lvl)%l_active = .FALSE.
      ENDIF
   ENDDO

   CALL mltgrd_init_parallel (communicator_name)

   CALL mltgrd_init_weights ()

   END SUBROUTINE mltgrd_init
!=======================================================================
!  END  mltgrd_init
!=======================================================================

!=======================================================================
!  BEGIN  mltgrd_init_parallel
!=======================================================================
   SUBROUTINE mltgrd_init_parallel (communicator_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl,iota,sbdmn_north,sbdmn_south,nsd,tag_glbl,i,j, &
      m,n,msgtag,rnk,proc_total,proc_nmbr_send,recv_total,ix(3),ierr
   INTEGER (KIND=int_kind) :: &
      status(MPI_STATUS_SIZE)
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      tmpry_lst(:)
   TYPE (comm_node),POINTER :: &
      comm
   TYPE (grid_node),POINTER :: &
      ptr0,ptr1
   TYPE (mg_proc_node),POINTER :: &
      proc,proc_send
   TYPE (mg_list_node),POINTER :: &
      instr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   comm => parallel_get_communicator (communicator_name)

!-----------------------------------------------------------------------
!  initialize wrapping for each resolution level
!-----------------------------------------------------------------------
   DO lvl = 0,level_max
      mg(lvl)%mg_strng( 1:10) = "multigrid_"
      mg(lvl)%mg_strng(11:13) = integer_to_string (3,lvl)
      CALL initialize_wrap (communicator_name,mg(lvl)%mg_strng,lvl, &
                              mg(lvl)%sbdmn_iota,mg(lvl)%sbdmn_lst, &
                              l_report=.FALSE.)
   ENDDO
!-----------------------------------------------------------------------
!  initialize parallel communication information for each resolution level
!-----------------------------------------------------------------------
   DO lvl = 0,level_max
      mg(lvl)%proc_total         = 0
      mg(lvl)%send_message_total = 0
      mg(lvl)%recv_message_total = 0
      NULLIFY (mg(lvl)%proc)
   ENDDO
!-----------------------------------------------------------------------
!  set receive linked lists
!-----------------------------------------------------------------------
   DO lvl = 1,level_max
      iota   = mg(lvl)%sbdmn_iota
      sbdmn_north =        2**(2*iota  )    ! subdomain block of north pole
      sbdmn_south = 2*(1+7*2**(2*iota+1))/3 ! subdomain block of south pole

      DO nsd = 1,mg(lvl)%nsdm

         tag_glbl = 3+(2**(2*(level_max-iota)))*(mg(lvl)%sbdmn_lst(nsd)-1)
         ptr0 => set_ptr (swmgrid(:),lvl,tag_glbl)

         DO j = 2,mg(lvl)%jm-2,2
            ptr1  => ptr0%nghbr(0)%p
            DO i = 2,mg(lvl)%im-2,2
               proc  => mltgrd_get_proc (mg(lvl),ptr1%up%p%proc)
               instr => mltgrd_get_list (proc)
               instr%tag_glbl = ptr1%tag_glbl
               instr%i1 = i; instr%j1 = j; instr%nsd1 = nsd

               ptr1 => ptr1%nghbr(1)%p%nghbr(1)%p
            ENDDO
            ptr0 => ptr0%nghbr(3)%p%nghbr(3)%p
         ENDDO
! north pole
         IF (sbdmn_north==mg(lvl)%sbdmn_lst(nsd)) THEN
            ptr1 => set_ptr (swmgrid(:),lvl,1_int_kind)

            proc  => mltgrd_get_proc (mg(lvl),ptr1%up%p%proc)
            instr => mltgrd_get_list (proc)
            instr%tag_glbl = ptr1%tag_glbl
            instr%i1 = 2; instr%j1 = mg(lvl)%jm; instr%nsd1 = nsd
         ENDIF
! south pole
         IF (sbdmn_south==mg(lvl)%sbdmn_lst(nsd)) THEN
            ptr1 => set_ptr (swmgrid(:),lvl,2_int_kind)

            proc  => mltgrd_get_proc (mg(lvl),ptr1%up%p%proc)
            instr => mltgrd_get_list (proc)
            instr%tag_glbl = ptr1%tag_glbl
            instr%i1 = mg(lvl)%im; instr%j1 = 2; instr%nsd1 = nsd
         ENDIF
      ENDDO
   ENDDO
!-----------------------------------------------------------------------
!  allocate memory for receive array lists and set array lists
!-----------------------------------------------------------------------
   DO lvl = 1,level_max
      proc => mg(lvl)%proc
      DO WHILE (ASSOCIATED (proc))
         IF (proc%recv_total > 0) THEN
            IF (proc%proc_nmbr/=comm%rnk_comm) THEN
               mg(lvl)%recv_message_total = mg(lvl)%recv_message_total + 1
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
   ENDDO
!-----------------------------------------------------------------------
!  communicate receive lists
!-----------------------------------------------------------------------
   msgtag = 0
   DO lvl = 1,level_max

      DO rnk = 0,comm%npe_comm-1
         proc_total = mg(lvl)%proc_total
         CALL MPI_BCAST (proc_total,1,MPI_INTEGER,rnk,comm%comm,ierr)
!090721         CALL MPI_BARRIER (comm%comm,ierr)

         IF (proc_total > 0) THEN

            IF (rnk==comm%rnk_comm) proc => mg(lvl)%proc !090721

!090721            proc => mg(lvl)%proc
            DO n = 1,proc_total

               msgtag = msgtag + 1

               IF (rnk==comm%rnk_comm) THEN
                  proc_nmbr_send   = proc%proc_nmbr
                  recv_total       = proc%recv_total
                  proc%recv_msgtag = msgtag
               ELSE
                  proc_nmbr_send = -1
                  recv_total     =  0
               ENDIF
               CALL MPI_BCAST (proc_nmbr_send,1,MPI_INTEGER,rnk,comm%comm,ierr)
!090721               CALL MPI_BARRIER (comm%comm,ierr)

               CALL MPI_BCAST (recv_total,1,MPI_INTEGER,rnk,comm%comm,ierr)
!090721               CALL MPI_BARRIER (comm%comm,ierr)

               IF (recv_total==0) THEN
                  PRINT *," mltgrd_init_parallel :: ERROR 001 "
                  STOP
               ENDIF

               IF (recv_total > 0) THEN
                  ALLOCATE (tmpry_lst (recv_total))
                  IF (rnk==comm%rnk_comm) THEN
                     tmpry_lst(:) = proc%recv_lst(:)
                  ELSE
                     tmpry_lst(:) = 0
                  ENDIF

                  IF ((rnk==comm%rnk_comm).OR.(proc_nmbr_send==comm%rnk_comm)) THEN
                     IF ((rnk==comm%rnk_comm).AND.(proc_nmbr_send==comm%rnk_comm)) THEN

                     ELSE
                        IF (rnk==comm%rnk_comm) THEN
                           CALL MPI_SEND (tmpry_lst,recv_total,MPI_INTEGER, &
                                              proc_nmbr_send,0,comm%comm,ierr)
                        ENDIF
                        IF (proc_nmbr_send==comm%rnk_comm) THEN
                           CALL MPI_RECV (tmpry_lst,recv_total,MPI_INTEGER, &
                                                  rnk,0,comm%comm,status,ierr)
                        ENDIF
                     ENDIF
                  ENDIF

!090721                  CALL MPI_BCAST (tmpry_lst(1),recv_total,MPI_INTEGER, &
!090721                                                      rnk,comm%comm,ierr)
!090721                  CALL MPI_BARRIER (comm%comm,ierr)

                  IF (proc_nmbr_send==comm%rnk_comm) THEN
                     proc_send => mltgrd_get_proc (mg(lvl),rnk)
                     proc_send%proc_nmbr = rnk
                     proc_send%send_msgtag = msgtag
                     proc_send%send_total  = recv_total
                     ALLOCATE (proc_send%send_lst(recv_total))
                     proc_send%send_lst(:) = tmpry_lst(:)
                  ENDIF
                  DEALLOCATE (tmpry_lst)
               ENDIF

               IF (rnk==comm%rnk_comm) proc => proc%next
            ENDDO
         ENDIF
      ENDDO
   ENDDO
!-----------------------------------------------------------------------
!  allocate memory for send lists and set send lists
!-----------------------------------------------------------------------
   DO lvl = 1,level_max
      proc => mg(lvl)%proc
      DO WHILE (ASSOCIATED (proc))
         IF (proc%send_total > 0) THEN
            IF (proc%proc_nmbr/=comm%rnk_comm) THEN
               mg(lvl)%send_message_total = mg(lvl)%send_message_total + 1
            ENDIF
            ALLOCATE (proc%i0  (proc%send_total))
            ALLOCATE (proc%j0  (proc%send_total))
            ALLOCATE (proc%nsd0(proc%send_total))
            DO n = 1,proc%send_total
               ix(:) = get_index (swmgrid(:),lvl-1, &
                                   mg(lvl-1)%sbdmn_iota, &
                                   mg(lvl-1)%sbdmn_lst(:),proc%send_lst(n))
               proc%i0(n) = ix(1); proc%j0(n) = ix(2); proc%nsd0(n) = ix(3)

            ENDDO
         ENDIF
         proc => proc%next
      ENDDO
   ENDDO

   END SUBROUTINE mltgrd_init_parallel
!=======================================================================
!  END  mltgrd_init_parallel
!=======================================================================

!=======================================================================
!  BEGIN  mltgrd_init_weights
!=======================================================================
   SUBROUTINE mltgrd_init_weights ()
!.......................................................................
!  INTENT IN
!.......................................................................
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl,iota,offset,sbdmn_north(5),sbdmn_south(5),nsd,tag_glbl,i,j,m,n
   REAL (KIND=dbl_kind) :: &
      crn(3,6)
   TYPE (grid_node),POINTER :: &
      ptr0,ptr1
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   DO lvl = 0,level_max
!      ALLOCATE (mg(lvl)%corner(3,6,mg(lvl)%im,mg(lvl)%jm,mg(lvl)%nsdm))
!      mg(lvl)%corner(:,:,:,:,:) = zero
      ALLOCATE (mg(lvl)%area(mg(lvl)%im,mg(lvl)%jm,mg(lvl)%nsdm))
      mg(lvl)%area(  :,:,:) = zero
      ALLOCATE (mg(lvl)%area_inv(mg(lvl)%im,mg(lvl)%jm,mg(lvl)%nsdm))
      mg(lvl)%area_inv(  :,:,:) = zero
      ALLOCATE (mg(lvl)%wght(0:6,mg(lvl)%im,mg(lvl)%jm,mg(lvl)%nsdm))
      mg(lvl)%wght(:,:,:,:) = zero
   ENDDO

   DO lvl = 0,level_max
      iota   = mg(lvl)%sbdmn_iota
      offset = 1+(((2**iota+1)*(2**iota-1))/3)
      sbdmn_north(:) = (/ (       2**(2*iota)*m,m=1,9,2) /)
      sbdmn_south(:) = (/ (offset+2**(2*iota)*m,m=1,9,2) /)

      DO nsd = 1,mg(lvl)%nsdm

         tag_glbl = 3+(2**(2*(level_max-iota)))*(mg(lvl)%sbdmn_lst(nsd)-1)
         ptr0 => set_ptr (swmgrid(:),lvl,tag_glbl)

         DO j = 2,mg(lvl)%jm-1
            ptr1 => ptr0%nghbr(0)%p
            DO i = 2,mg(lvl)%im-1

               crn(:,:)                           = ptr1%corner(:,:) 
!              mg(lvl)%corner  (:,:,i,j,nsd) = ptr1%corner(:,:) 
               mg(lvl)%area    (    i,j,nsd) = ptr1%area
               mg(lvl)%area_inv(    i,j,nsd) = ptr1%area_inv

!              crn(:,:) = mg(lvl)%corner(:,:,i,j,nsd)
               DO n = 1,6
                  IF (ASSOCIATED (ptr1%nghbr(n)%p)) THEN
                     mg(lvl)%wght(n,i,j,nsd) = &
                        arch_distance (crn(:,MOD (n+4,6)+1),crn(:,n))/ &
                        arch_distance (ptr1%point(:),ptr1%nghbr(n)%p%point(:))
                  ENDIF
               ENDDO
               mg(lvl)%wght(0,i,j,nsd) = &
                                       one/SUM (mg(lvl)%wght(1:6,i,j,nsd))

               ptr1 => ptr1%nghbr(1)%p
            ENDDO
            ptr0 => ptr0%nghbr(3)%p
         ENDDO
!-----------------------------------------------------------------------
!  north pole
!-----------------------------------------------------------------------
         IF (ANY (sbdmn_north(:)==mg(lvl)%sbdmn_lst(nsd))) THEN
            ptr0 => set_ptr (swmgrid(:),lvl,i1i)

            crn(:,:)                                       = ptr0%corner(:,:)
!           mg(lvl)%corner  (:,:,2,mg(lvl)%jm,nsd) = ptr0%corner(:,:)
            mg(lvl)%area    (    2,mg(lvl)%jm,nsd) = ptr0%area
            mg(lvl)%area_inv(    2,mg(lvl)%jm,nsd) = ptr0%area_inv

!           crn(:,:) = mg(lvl)%corner(:,:,2,mg(lvl)%jm,nsd)

            mg(lvl)%wght(1:6,2,mg(lvl)%jm,nsd) = &
                        arch_distance (crn(:,1),crn(:,2))/ &
                        arch_distance (ptr0%point(:),ptr0%nghbr(1)%p%point(:))
            mg(lvl)%wght( 0 ,2,mg(lvl)%jm,nsd) = &
                          one/SUM (mg(lvl)%wght(1:5,2,mg(lvl)%jm,nsd))
         ENDIF
!-----------------------------------------------------------------------
!  south pole
!-----------------------------------------------------------------------
         IF (ANY (sbdmn_south(:)==mg(lvl)%sbdmn_lst(nsd))) THEN
            ptr0 => set_ptr (swmgrid(:),lvl,i2i)

            crn(:,:)                                       = ptr0%corner(:,:)
!           mg(lvl)%corner  (:,:,mg(lvl)%im,2,nsd) = ptr0%corner(:,:)
            mg(lvl)%area    (    mg(lvl)%im,2,nsd) = ptr0%area
            mg(lvl)%area_inv(    mg(lvl)%im,2,nsd) = ptr0%area_inv

!           crn(:,:) = mg(lvl)%corner(:,:,mg(lvl)%im,2,nsd)

            mg(lvl)%wght(1:6,mg(lvl)%im,2,nsd) = &
                        arch_distance (crn(:,1),crn(:,2))/ &
                        arch_distance (ptr0%point(:),ptr0%nghbr(1)%p%point(:))
            mg(lvl)%wght( 0 ,mg(lvl)%im,2,nsd) = &
                          one/SUM (mg(lvl)%wght(1:5,mg(lvl)%im,2,nsd))
         ENDIF
      ENDDO

      CALL wrap (TRIM (mg(lvl)%mg_strng),face_1lyr=mg(lvl)%area)
      mg(lvl)%area_inv(:,:,:) = one/mg(lvl)%area(:,:,:)

   ENDDO

   END SUBROUTINE mltgrd_init_weights
!=======================================================================
!  END  mltgrd_init_weights
!=======================================================================

!=======================================================================
! BEGIN mltgrd_get_proc
!=======================================================================
   FUNCTION mltgrd_get_proc (mg,proc_nmbr) RESULT (node)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (mg_node) :: &
      mg
   INTEGER (KIND=int_kind) :: proc_nmbr
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (mg_proc_node),POINTER :: node
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: l_found
   TYPE (mg_proc_node),POINTER :: proc
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (.NOT.ASSOCIATED (mg%proc)) THEN
      ALLOCATE (mg%proc)
      mg%proc_total = 1
      proc => mg%proc
      proc%proc_nmbr = proc_nmbr; proc%send_total = 0; proc%recv_total = 0
      proc%send_msgtag = -1; proc%recv_msgtag = -1;
      NULLIFY (proc%recv); NULLIFY (proc%recv_tail); NULLIFY (proc%next)
   ELSE
      l_found = .FALSE.
      proc => mg%proc
      DO WHILE (ASSOCIATED (proc))
         IF (proc%proc_nmbr==proc_nmbr) THEN
            l_found = .TRUE.
            EXIT
         ENDIF
         proc => proc%next
      ENDDO
      IF (.NOT.l_found) THEN
         proc => mg%proc
         DO WHILE (ASSOCIATED (proc%next))
            proc => proc%next
         ENDDO
         ALLOCATE (proc%next)
         mg%proc_total = mg%proc_total + 1
         proc => proc%next
         proc%proc_nmbr = proc_nmbr; proc%send_total = 0; proc%recv_total = 0
         proc%send_msgtag = -1; proc%recv_msgtag = -1;
         NULLIFY (proc%recv); NULLIFY (proc%recv_tail); NULLIFY (proc%next)
      ENDIF
   ENDIF

   node => proc
   
   END FUNCTION mltgrd_get_proc
!=======================================================================
!  END mltgrd_get_proc
!=======================================================================

!=======================================================================
! BEGIN mltgrd_get_list
!=======================================================================
   FUNCTION mltgrd_get_list (proc) RESULT (node)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (mg_proc_node),POINTER :: &
      proc
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (mg_list_node),POINTER :: &
      node
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (mg_list_node),POINTER :: &
      tmpry
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   proc%recv_total = proc%recv_total + 1

   tmpry => proc%recv

   IF (.NOT.ASSOCIATED (tmpry)) THEN
      ALLOCATE (tmpry)
      proc%recv      => tmpry 
      proc%recv_tail => tmpry
      NULLIFY (proc%recv_tail%next)
   ELSE
      ALLOCATE (proc%recv_tail%next); proc%recv_tail => proc%recv_tail%next; 
      NULLIFY (proc%recv_tail%next)
   ENDIF
   node => proc%recv_tail
   
   END FUNCTION mltgrd_get_list
!=======================================================================
!  END mltgrd_get_list
!=======================================================================

!=======================================================================
!  BEGIN mltgrd_wrap_threshold
!=======================================================================
   SUBROUTINE mltgrd_wrap_threshold (km0,x) 
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      km0
   REAL (KIND=dbl_kind) :: &
      x(:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      im,jm,nsdm,p,q,nsd
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   im   = SIZE (x,DIM=1)
   jm   = SIZE (x,DIM=2)
   nsdm = SIZE (x,DIM=4)

   p = im-1; q = jm-1

   IF (nsdm==10) THEN
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NORTH POLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO nsd = 3,9,2
         x(2,jm,:,nsd) = x(2,jm,:, 1)
      ENDDO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! SOUTH POLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO nsd = 2,8,2
         x(im,2,:,nsd) = x(im,2,:,10)
      ENDDO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NORTHERN HEMISPHERE SUBDOMAINS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO nsd = 1,9,2
         x(    1,2:q  ,:,nsd) = x(p:2:-1,  q   ,:,MOD (nsd+7,10)+1)
         x(2:p  ,    1,:,nsd) = x(2:p   ,  q   ,:,MOD (nsd+8,10)+1)
         x(  p+1,2:q  ,:,nsd) = x(  2   ,2:q   ,:,MOD (nsd+0,10)+1)
         x(3:p+1,  q+1,:,nsd) = x(  2   ,q:2:-1,:,MOD (nsd+1,10)+1)
      ENDDO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! SOUTHERN HEMISPHERE SUBDOMAINS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO nsd = 2,10,2
         x(    1,2:q  ,:,nsd) = x(  p   ,2:q   ,:,MOD (nsd+8,10)+1)
         x(2:p  ,    1,:,nsd) = x(  p   ,q:2:-1,:,MOD (nsd+7,10)+1)
         x(  p+1,3:q+1,:,nsd) = x(p:2:-1,  2   ,:,MOD (nsd+1,10)+1)
         x(2:p  ,  q+1,:,nsd) = x(2:p   ,  2   ,:,MOD (nsd+0,10)+1)
      ENDDO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! FIX-UP THE NORTHERN HEMISPHERE PENTAGONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      x(1,1,:, 1: 9: 2) = x(1,2,:, 1: 9: 2)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! FIX-UP THE SOUTHERN HEMISPHERE PENTAGONS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      x(1,1,:, 2:10: 2) = x(2,1,:, 2:10: 2)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! FIX-UP THE NORTH AND SOUTH POLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO nsd = 1,9,2
         x(im, 1,:,nsd) = x(2,q,:,MOD (nsd+3,10)+1)
         x( 1,jm,:,nsd) = x(2,q,:,MOD (nsd+5,10)+1)
      ENDDO
      DO nsd = 2,10,2
         x(im, 1,:,nsd) = x(p,2,:,MOD (nsd+5,10)+1)
         x( 1,jm,:,nsd) = x(p,2,:,MOD (nsd+3,10)+1)
      ENDDO
   ENDIF

   END SUBROUTINE mltgrd_wrap_threshold
!=======================================================================
!  END mltgrd_wrap_threshold
!=======================================================================

   END MODULE multigrid
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc

