   MODULE grid_connectivity
 
   USE kinds
   USE numbers

   USE parallel_params
   USE parallel_utilities

   USE grid_params
   USE grid_subdomain

   IMPLICIT NONE
   SAVE

   CONTAINS
!=======================================================================
!  BEGIN initialize_grid_connectivity
!=======================================================================
   SUBROUTINE initialize_grid_connectivity (communicator_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,m,lvl,count_next,count_real,count_ghst,tag_locl
   TYPE (grid_node),TARGET :: &
      path_head
   TYPE (grid_node),POINTER :: &
      ptr,ptr_next,ptr_real,ptr_ghst
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  set level 0
!-----------------------------------------------------------------------
   swmgrid(:)%level  = 0
   swmgrid(:)%l_real = .FALSE.
   swmgrid(:)%l_ghst = .FALSE.

   swmgrid(:)%l_north = .FALSE.; swmgrid(1:11:2)%l_north = .TRUE.;
   swmgrid(:)%l_south = .FALSE.; swmgrid(2:12:2)%l_south = .TRUE.; 

   swmgrid(:)%l_pentagon = .TRUE.;

   swmgrid(:)%l_pole_north = .FALSE.; swmgrid(1)%l_pole_north = .TRUE.;
   swmgrid(:)%l_pole_south = .FALSE.; swmgrid(2)%l_pole_south = .TRUE.;

   swmgrid(:)%l_pentagon_north = .FALSE.; swmgrid(3:11:2)%l_pentagon_north = .TRUE.;
   swmgrid(:)%l_pentagon_south = .FALSE.; swmgrid(4:12:2)%l_pentagon_south = .TRUE.;

   swmgrid( 1  )%i = 1; swmgrid( 1  )%j = 2
   swmgrid( 2  )%i = 2; swmgrid( 2  )%j = 1
   swmgrid(3:12)%i = 1; swmgrid(3:12)%j = 1

   swmgrid(:)%proc = rnk_nonexistent
!-----------------------------------------------------------------------
!  nullify resolution links at level 0
!-----------------------------------------------------------------------
   DO n = 1,SIZE (swmgrid)
      DO m = 0,3
         NULLIFY (swmgrid(n)%dn(m)%p)
      ENDDO
      NULLIFY (swmgrid(n)%up%p)
   ENDDO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  SET CONNECTIONS AT LEVEL 0
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!  link the north pole to mid-latitude pentagons
!-----------------------------------------------------------------------
   swmgrid(1)%nghbr(0)%p => swmgrid( 1)

   swmgrid(1)%nghbr(1)%p => swmgrid( 3)
   swmgrid(1)%nghbr(2)%p => swmgrid( 5)
   swmgrid(1)%nghbr(3)%p => swmgrid( 7)
   swmgrid(1)%nghbr(4)%p => swmgrid( 9)
   swmgrid(1)%nghbr(5)%p => swmgrid(11)

   NULLIFY (swmgrid(1)%nghbr(6)%p)
!-----------------------------------------------------------------------
!  link the south pole to mid-latitude pentagons
!-----------------------------------------------------------------------
   swmgrid(2)%nghbr(0)%p => swmgrid( 2)

   swmgrid(2)%nghbr(1)%p => swmgrid(12)
   swmgrid(2)%nghbr(2)%p => swmgrid(10)
   swmgrid(2)%nghbr(3)%p => swmgrid( 8)
   swmgrid(2)%nghbr(4)%p => swmgrid( 6)
   swmgrid(2)%nghbr(5)%p => swmgrid( 4)

   NULLIFY (swmgrid(2)%nghbr(6)%p)
!-----------------------------------------------------------------------
!  link the northern hemosphere pentagons
!-----------------------------------------------------------------------
   DO n = 0,8,2
      swmgrid(n+3)%nghbr(0)%p => swmgrid(n+3)
      swmgrid(n+3)%nghbr(1)%p => swmgrid(3 + MOD (n+ 1,10))
      swmgrid(n+3)%nghbr(2)%p => swmgrid(3 + MOD (n+ 2,10))
      swmgrid(n+3)%nghbr(3)%p => swmgrid(1)
      NULLIFY (swmgrid(n+3)%nghbr(4)%p)
      swmgrid(n+3)%nghbr(5)%p => swmgrid(3 + MOD (n+ 8,10))
      swmgrid(n+3)%nghbr(6)%p => swmgrid(3 + MOD (n+ 9,10))
   ENDDO
!-----------------------------------------------------------------------
!  link the southern hemosphere pentagons
!-----------------------------------------------------------------------
   DO n = 0,8,2
      swmgrid(n+4)%nghbr(0)%p => swmgrid(n+4)
      swmgrid(n+4)%nghbr(1)%p => swmgrid(2)
      swmgrid(n+4)%nghbr(2)%p => swmgrid(3 + MOD (n+ 3,10))
      swmgrid(n+4)%nghbr(3)%p => swmgrid(3 + MOD (n+ 2,10))
      swmgrid(n+4)%nghbr(4)%p => swmgrid(3 + MOD (n+10,10))
      swmgrid(n+4)%nghbr(5)%p => swmgrid(3 + MOD (n+ 9,10))
      NULLIFY (swmgrid(n+4)%nghbr(6)%p)
   ENDDO
!-----------------------------------------------------------------------
!  set global tags at level 0
!-----------------------------------------------------------------------
   swmgrid(1)%tag_glbl = 1; swmgrid(2)%tag_glbl = 2
   DO n = 3,SIZE (swmgrid)
      swmgrid(n)%tag_glbl = 3+(2**(2*level_max))*(n-3)
   ENDDO
!-----------------------------------------------------------------------
!  set spiral seach tags at level 0
!-----------------------------------------------------------------------
   DO n = 1,SIZE (swmgrid)
      swmgrid(n)%tag_sprl = tag_nonexistent
   ENDDO
!-----------------------------------------------------------------------
! 
!-----------------------------------------------------------------------
   DO n = 1,SIZE (swmgrid)
      CALL connectivity_1 (swmgrid(n)%nghbr(0)%p)
   ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   DO n = 3,SIZE (swmgrid)
      CALL connectivity_2 (swmgrid(n)%nghbr(0)%p)
   ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   DO n = 1,SIZE (swmgrid)
      CALL set_real (swmgrid(n)%nghbr(0)%p)
   ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   DO n = 1,SIZE (swmgrid)
      CALL set_ghst (swmgrid(n)%nghbr(0)%p)
   ENDDO
!-----------------------------------------------------------------------
!  make a path through all and real and ghost cells associated
!  with the local process and record the size of local arrays
!-----------------------------------------------------------------------
   DO lvl = 0,level_max
      NULLIFY (path_head%next_next%p)
      NULLIFY (path_head%next_real%p)
      NULLIFY (path_head%next_ghst%p)
      ptr_next => path_head; ptr_real => path_head; ptr_ghst => path_head
      count_next = i0i; count_real = i0i; count_ghst = i0i
      DO n = 1,12
         CALL set_path (lvl,count_next,count_real,count_ghst, &
                            ptr_next,ptr_real,ptr_ghst,swmgrid(n)%nghbr(0)%p)
      ENDDO
      path_next(lvl)%p => path_head%next_next%p
      path_real(lvl)%p => path_head%next_real%p
      path_ghst(lvl)%p => path_head%next_ghst%p

      nm_lvl     (lvl) = count_next
      nm_real_lvl(lvl) = count_real
      nm_ghst_lvl(lvl) = count_ghst
   ENDDO
   nm      = nm_lvl     (level_max)
   nm_real = nm_real_lvl(level_max)
   nm_ghst = nm_ghst_lvl(level_max)
!-----------------------------------------------------------------------
!  set the local tags
!-----------------------------------------------------------------------
   DO lvl = 0,level_max
      tag_locl = i0i
      ptr => path_next(lvl)%p
      DO WHILE (ASSOCIATED (ptr))
         tag_locl = tag_locl + i1i; ptr%tag_locl = tag_locl;
         ptr => ptr%next_next%p
      ENDDO
   ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   DO n = 1,SIZE (swmgrid)
      CALL set_ix_2D (swmgrid(n)%nghbr(0)%p)
   ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   DO n = 1,SIZE (swmgrid)
      CALL set_nghbr_lst (swmgrid(n)%nghbr(0)%p)
   ENDDO
!-----------------------------------------------------------------------
!  
!-----------------------------------------------------------------------
!   CALL communicate_processor (communicator_name,grid(:))
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   DO n = 1,SIZE (swmgrid)
      CALL set_proc (swmgrid(n)%nghbr(0)%p)
   ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   IF (.FALSE.) THEN
      CALL report_grid_node_total (communicator_name)
   ENDIF

   END SUBROUTINE initialize_grid_connectivity
!=======================================================================
!  END initialize_grid_connectivity
!=======================================================================

!=======================================================================
!  BEGIN connectivity_1 
!=======================================================================
   RECURSIVE SUBROUTINE connectivity_1 (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,dn_max,m,d_tag_glbl,status,ierr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (ptr%level < level_max) THEN

      IF ((ptr%l_pole_north).OR.(ptr%l_pole_south)) THEN
         dn_max = 0
      ELSE
         dn_max = 3
      ENDIF

      d_tag_glbl = 2**(2*(level_max-(ptr%level+1)))

      DO n = 0,dn_max
         IF (build (ptr,n,d_tag_glbl)) THEN
            ALLOCATE (ptr%dn(n)%p,STAT=status)

            IF (status /= 0) THEN
               PRINT *," connectivity_1 :: status /= 0 "
               CALL MPI_FINALIZE (ierr)
               STOP
            ENDIF

            grid_node_total = grid_node_total + 1_8
            IF (grid_node_size*grid_node_total > grid_node_memory_max) THEN
               PRINT *," connectivity_1 :: TOO MUCH GRID NODE MEMORY ALLOCATED "
               PRINT *," grid_node_total        = ",grid_node_total
               PRINT *," grid_node_size         = ",grid_node_size
               PRINT *," grid_node_memory_total = ", &
                                                grid_node_size*grid_node_total
               PRINT *," grid_node_memory_max   = ",grid_node_memory_max
               CALL MPI_FINALIZE (ierr)
               STOP
            ENDIF

            ptr%dn(n)%p%up%p => ptr

            ptr%dn(n)%p%nghbr(0)%p => ptr%dn(n)%p
            DO m = 1,6; NULLIFY (ptr%dn(n)%p%nghbr(m)%p); ENDDO
            DO m = 0,3; NULLIFY (ptr%dn(n)%p%dn   (m)%p); ENDDO

            ptr%dn(n)%p%level = ptr%level+1

            ptr%dn(n)%p%tag_glbl = ptr%tag_glbl + d_tag_glbl*n
            ptr%dn(n)%p%tag_locl = tag_nonexistent
            ptr%dn(n)%p%tag_sprl = tag_nonexistent

            ptr%dn(n)%p%l_pole_north = ptr%l_pole_north
            ptr%dn(n)%p%l_pole_south = ptr%l_pole_south

            ptr%dn(n)%p%l_north = ptr%l_north
            ptr%dn(n)%p%l_south = ptr%l_south

            IF (n==0) THEN
               ptr%dn(n)%p%l_pentagon       = ptr%l_pentagon
               ptr%dn(n)%p%l_pentagon_north = ptr%l_pentagon_north
               ptr%dn(n)%p%l_pentagon_south = ptr%l_pentagon_south
            ELSE
               ptr%dn(n)%p%l_pentagon       = .FALSE.
               ptr%dn(n)%p%l_pentagon_north = .FALSE.
               ptr%dn(n)%p%l_pentagon_south = .FALSE.
            ENDIF

            ptr%dn(n)%p%l_real = .FALSE.
            ptr%dn(n)%p%l_ghst = .FALSE.

            ptr%dn(n)%p%i = 2*(ptr%i-1)+1
            ptr%dn(n)%p%j = 2*(ptr%j-1)+1
            IF (n==1) THEN
               ptr%dn(n)%p%i = ptr%dn(n)%p%i+1
            ENDIF
            IF (n==2) THEN
               ptr%dn(n)%p%i = ptr%dn(n)%p%i+1
               ptr%dn(n)%p%j = ptr%dn(n)%p%j+1
            ENDIF
            IF (n==3) THEN
               ptr%dn(n)%p%j = ptr%dn(n)%p%j+1
            ENDIF

            ptr%dn(n)%p%proc        = rnk_nonexistent
            ptr%dn(n)%p%l_point_set = .FALSE.
            ptr%dn(n)%p%point(:)    = (/ -one,-one,-one /)

            NULLIFY (ptr%next_next%p)
            NULLIFY (ptr%next_real%p)
            NULLIFY (ptr%next_ghst%p)
            NULLIFY (ptr%next_sprl%p)

            CALL connectivity_1 (ptr%dn(n)%p)
         ENDIF
      ENDDO
   ENDIF

   END SUBROUTINE connectivity_1
!=======================================================================
!  BEGIN connectivity_1
!=======================================================================

!=======================================================================
!  BEGIN build
!=======================================================================
   FUNCTION build (ptr,n,d_tag_glbl) RESULT (l_build)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
   INTEGER (KIND=int_kind) :: &
      n,lvl,d_tag_glbl
!.......................................................................
!  INTENT OUT
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_build
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      tag_range_min,tag_range_max,iota,tag_glbl
   TYPE (extended_list_node),POINTER :: &
      tmpry
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   l_build = .FALSE.

   IF ((ptr%tag_glbl==1)) THEN
      l_build = .TRUE.
      RETURN
   ENDIF

   IF ((ptr%tag_glbl==2)) THEN
      l_build = .TRUE.
      RETURN
   ENDIF

   IF ((ptr%level < level_glbl).OR.(ptr%level==level_max-1)) THEN
      l_build = .TRUE.
      RETURN
   ENDIF
!-----------------------------------------------------------------------
!  build from outside
!-----------------------------------------------------------------------
   tag_range_min = ptr%tag_glbl+(2**(2*(level_max-ptr%level))/4)*(n  )
   tag_range_max = ptr%tag_glbl+(2**(2*(level_max-ptr%level))/4)*(n+1)
   DO lvl = ptr%level+1,level_max
      tmpry => sbdmn(lvl)%extended_list_head
      DO WHILE (ASSOCIATED (tmpry))
         iota = sbdmn(lvl)%sbdmn_iota
         tag_glbl = 3+(2**(2*(level_max-iota)))*(tmpry%nsd_glbl-1)
         IF ((tag_range_min<=tag_glbl).AND.(tag_glbl<tag_range_max)) THEN
            l_build = .TRUE.
            RETURN
         ENDIF
         tmpry => tmpry%next
      ENDDO
   ENDDO
!-----------------------------------------------------------------------
!  build from inside
!-----------------------------------------------------------------------
   tag_glbl = ptr%tag_glbl + d_tag_glbl*n
   DO lvl = ptr%level+1,level_max
      tmpry => sbdmn(lvl)%extended_list_head
      DO WHILE (ASSOCIATED (tmpry))
         iota = sbdmn(lvl)%sbdmn_iota
         tag_range_min = 3+(2**(2*(level_max-iota)))*(tmpry%nsd_glbl-1)
         tag_range_max = 3+(2**(2*(level_max-iota)))*(tmpry%nsd_glbl  )
         IF ((tag_range_min<=tag_glbl).AND.(tag_glbl<tag_range_max)) THEN
            l_build = .TRUE.
            RETURN
         ENDIF
         tmpry => tmpry%next
      ENDDO
   ENDDO

   END FUNCTION build
!=======================================================================
!  END build
!=======================================================================

!=======================================================================
!  BEGIN connectivity_2 
!=======================================================================
   RECURSIVE SUBROUTINE connectivity_2 (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

!-----------------------------------------------------------------------
!  type A links.  link 1, link 2 and link 3
!-----------------------------------------------------------------------
   IF (ASSOCIATED (ptr%dn(0)%p)) THEN
      IF (ASSOCIATED (ptr%dn(1)%p)) CALL link (1,4,ptr%dn(0)%p,ptr%dn(1)%p)
      IF (ASSOCIATED (ptr%dn(2)%p)) CALL link (2,5,ptr%dn(0)%p,ptr%dn(2)%p)
      IF (ASSOCIATED (ptr%dn(3)%p)) CALL link (3,6,ptr%dn(0)%p,ptr%dn(3)%p)
   ENDIF
!-----------------------------------------------------------------------
!  type B links.
!-----------------------------------------------------------------------
   IF (ASSOCIATED (ptr%dn(1)%p)) THEN
! link 4
      IF (ASSOCIATED (ptr%nghbr(1)%p)) THEN
         IF (ASSOCIATED (ptr%nghbr(1)%p%dn(0)%p)) THEN
            n = get_direction (ptr%nghbr(1)%p,ptr%nghbr(0)%p)
            CALL link (1,n,ptr%dn(1)%p,ptr%nghbr(1)%p%dn(0)%p)
         ENDIF
      ENDIF
! link 5
      IF ((ptr%l_south).AND.(ptr%i==(2**ptr%level))) THEN
         IF (ASSOCIATED (ptr%nghbr(2)%p)) THEN
            IF (ASSOCIATED (ptr%nghbr(2)%p%dn(1)%p)) THEN
               CALL link (2,6,ptr%dn(1)%p,ptr%nghbr(2)%p%dn(1)%p)
            ENDIF
         ENDIF
      ELSE
         IF (ASSOCIATED (ptr%nghbr(1)%p)) THEN
            IF (ASSOCIATED (ptr%nghbr(1)%p%dn(3)%p)) THEN
               CALL link (2,5,ptr%dn(1)%p,ptr%nghbr(1)%p%dn(3)%p)
            ENDIF
         ENDIF
      ENDIF
! link 6
      IF (ASSOCIATED (ptr%dn(2)%p)) THEN
         CALL link (3,6,ptr%dn(1)%p,ptr%dn(2)%p)
      ENDIF
   ENDIF
!-----------------------------------------------------------------------
!  type C links.
!-----------------------------------------------------------------------
   IF (ASSOCIATED (ptr%dn(2)%p)) THEN
! link 7
      IF ((ptr%l_south).AND.(ptr%i==(2**ptr%level))) THEN
         IF (ASSOCIATED (ptr%nghbr(2)%p)) THEN
            IF (ASSOCIATED (ptr%nghbr(2)%p%dn(1)%p)) THEN
               CALL link (1,5,ptr%dn(2)%p,ptr%nghbr(2)%p%dn(1)%p)
            ENDIF
         ENDIF
      ELSE
         IF (ASSOCIATED (ptr%nghbr(1)%p)) THEN
            IF (ASSOCIATED (ptr%nghbr(1)%p%dn(3)%p)) THEN
               CALL link (1,4,ptr%dn(2)%p,ptr%nghbr(1)%p%dn(3)%p)
            ENDIF
         ENDIF
      ENDIF
! link 8
      IF (ASSOCIATED (ptr%nghbr(2)%p)) THEN
         IF (ASSOCIATED (ptr%nghbr(2)%p%dn(0)%p)) THEN
            n = get_direction (ptr%nghbr(2)%p,ptr%nghbr(0)%p)
            CALL link (2,n,ptr%dn(2)%p,ptr%nghbr(2)%p%dn(0)%p)
         ENDIF
      ENDIF
! link 9
      IF ((ptr%l_north).AND.(ptr%j==(2**ptr%level))) THEN
         IF (ASSOCIATED (ptr%nghbr(2)%p)) THEN
            IF (ASSOCIATED (ptr%nghbr(2)%p%dn(3)%p)) THEN
               CALL link (3,5,ptr%dn(2)%p,ptr%nghbr(2)%p%dn(3)%p)
            ENDIF
         ENDIF
      ELSE
         IF (ASSOCIATED (ptr%nghbr(3)%p)) THEN
            IF (ASSOCIATED (ptr%nghbr(3)%p%dn(1)%p)) THEN
               CALL link (3,6,ptr%dn(2)%p,ptr%nghbr(3)%p%dn(1)%p)
            ENDIF
         ENDIF
      ENDIF
   ENDIF
!-----------------------------------------------------------------------
!  type D links.
!-----------------------------------------------------------------------
   IF (ASSOCIATED (ptr%dn(3)%p)) THEN
! link 10
      IF (ASSOCIATED (ptr%dn(2)%p)) THEN
         CALL link (1,4,ptr%dn(3)%p,ptr%dn(2)%p)
      ENDIF
! link 11
      IF ((ptr%l_north).AND.(ptr%j==(2**ptr%level))) THEN
         IF (ASSOCIATED (ptr%nghbr(2)%p)) THEN
            IF (ASSOCIATED (ptr%nghbr(2)%p%dn(3)%p)) THEN
               CALL link (2,4,ptr%dn(3)%p,ptr%nghbr(2)%p%dn(3)%p)
            ENDIF
         ENDIF
      ELSE
         IF (ASSOCIATED (ptr%nghbr(3)%p)) THEN
            IF (ASSOCIATED (ptr%nghbr(3)%p%dn(1)%p)) THEN
               CALL link (2,5,ptr%dn(3)%p,ptr%nghbr(3)%p%dn(1)%p)
            ENDIF
         ENDIF
      ENDIF
! link 12
      IF (ASSOCIATED (ptr%nghbr(3)%p)) THEN
         IF (ASSOCIATED (ptr%nghbr(3)%p%dn(0)%p)) THEN
            n = get_direction (ptr%nghbr(3)%p,ptr%nghbr(0)%p)
            CALL link (3,n,ptr%dn(3)%p,ptr%nghbr(3)%p%dn(0)%p)
         ENDIF
      ENDIF
   ENDIF

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) CALL connectivity_2 (ptr%dn(n)%p)
   ENDDO

   END SUBROUTINE connectivity_2
!=======================================================================
!  BEGIN connectivity_2
!=======================================================================

!=======================================================================
!  BEGIN link 
!=======================================================================
   SUBROUTINE link (n1,n2,ptr1,ptr2)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n1,n2
   TYPE (grid_node),POINTER :: &
      ptr1,ptr2
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ptr1%nghbr(n1)%p => ptr2
   ptr2%nghbr(n2)%p => ptr1

   END SUBROUTINE link
!=======================================================================
!  BEGIN link
!=======================================================================

!=======================================================================
!  BEGIN get_direction
!=======================================================================
   FUNCTION get_direction (ptr1,ptr2) RESULT (dir)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr1,ptr2
!.......................................................................
!  INTENT OUT
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      dir
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   dir = -999

   DO n = 1,6
      IF (ASSOCIATED (ptr1%nghbr(n)%p)) THEN
         IF (ptr1%nghbr(n)%p%tag_glbl == ptr2%tag_glbl) THEN
            dir = n
            EXIT
         ENDIF
      ENDIF
   ENDDO

   IF (dir==-999) THEN
      PRINT *," ERROR get_direction : ptr1%tag_glbl = ",ptr1%tag_glbl
      PRINT *,"                       ptr2%tag_glbl = ",ptr2%tag_glbl

      DO n = 1,6
         IF (ASSOCIATED (ptr1%nghbr(n)%p)) THEN
            PRINT *,"      ptr1%nghbr(",n,") = ",ptr1%nghbr(n)%p%tag_glbl
         ELSE
            PRINT *,"      ptr1%nghbr(",n,") = NULL "
         ENDIF
      ENDDO
      STOP
   ENDIF

   END FUNCTION get_direction
!=======================================================================
!  END get_direction
!=======================================================================

!=======================================================================
!  BEGIN set_real
!=======================================================================
   RECURSIVE SUBROUTINE set_real (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      iota,nsd_glbl,n
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   SELECT CASE (ptr%tag_glbl)

   CASE (01)
      IF (sbdmn(ptr%level)%l_agent_north) ptr%l_real = .TRUE.
   CASE (02) 
      IF (sbdmn(ptr%level)%l_agent_south) ptr%l_real = .TRUE.
   CASE DEFAULT
      iota = sbdmn(ptr%level)%sbdmn_iota
      nsd_glbl = ((ptr%tag_glbl-3)/(2**(2*(level_max-iota))))+1
      IF (ANY (sbdmn(ptr%level)%lst(:)==nsd_glbl)) THEN
         ptr%l_real = .TRUE.
      ENDIF
   END SELECT

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) CALL set_real (ptr%dn(n)%p)
   ENDDO

   END SUBROUTINE set_real
!=======================================================================
!  BEGIN set_real
!=======================================================================

!=======================================================================
!  BEGIN set_ghst
!=======================================================================
   RECURSIVE SUBROUTINE set_ghst (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      sprl_dpth,n
   TYPE (grid_node),POINTER :: &
      ptr1,ptr_sprl
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (ptr%l_real) THEN

      IF (ptr%level==level_max) THEN
         sprl_dpth = i4i
      ELSE
         sprl_dpth = i1i
      ENDIF

      ptr_sprl => spiral_path (sprl_dpth,ptr)
      ptr1     => ptr_sprl
      DO WHILE (ASSOCIATED (ptr1))
         IF (.NOT.ptr1%l_real) THEN
            ptr1%l_ghst = .TRUE.
         ENDIF
         ptr1 => ptr1%next_sprl%p
      ENDDO
   ENDIF

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) CALL set_ghst (ptr%dn(n)%p)
   ENDDO

   END SUBROUTINE set_ghst
!=======================================================================
!  END set_ghst
!=======================================================================

!=======================================================================
!  BEGIN communicate_processor
!=======================================================================
   SUBROUTINE communicate_processor (communicator_name,grid)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name
   TYPE (grid_node) :: &
      grid(:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl,rnk,total,n,m,ierr

   TYPE real_cell_node
      INTEGER (KIND=int_kind) :: &
         total
      INTEGER (KIND=int_kind),ALLOCATABLE :: &
         lst(:)
   END TYPE real_cell_node

   TYPE (real_cell_node) :: &
      real_cell(0:level_max)

   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      tmpry_lst(:)

   TYPE (comm_node),POINTER :: &
      comm
   TYPE (grid_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   comm => parallel_get_communicator (communicator_name)

!-----------------------------------------------------------------------
!  count real cells and set real cell lists
!-----------------------------------------------------------------------
   DO lvl = 0,level_max
      real_cell(lvl)%total = 0
      DO n = 1,SIZE (grid)
         CALL real_count (grid(n)%nghbr(0)%p,lvl,real_cell(lvl)%total)
      ENDDO
      IF (real_cell(lvl)%total > 0) THEN
         ALLOCATE (real_cell(lvl)%lst(real_cell(lvl)%total))
         m = 0
         DO n = 1,SIZE (grid)
            CALL real_list (grid(n)%nghbr(0)%p,lvl,m,real_cell(lvl)%lst(:))
         ENDDO
      ENDIF
   ENDDO
!-----------------------------------------------------------------------
!  communicate processor information
!-----------------------------------------------------------------------
   DO lvl = 0,level_max
      DO rnk = 0,comm%npe_comm-1
         IF (rnk==comm%rnk_comm) THEN
            total = real_cell(lvl)%total
         ELSE
            total = -1
         ENDIF
         CALL MPI_BCAST (total,1,MPI_INTEGER,rnk,comm%comm,ierr)
         CALL MPI_BARRIER (comm%comm,ierr)

         IF (total > 0) THEN
            ALLOCATE (tmpry_lst (total))
            IF (rnk==comm%rnk_comm) THEN
               tmpry_lst(:) = real_cell(lvl)%lst(:)
            ELSE
               tmpry_lst(:) = -1
            ENDIF
            CALL MPI_BCAST (tmpry_lst(1),total,MPI_INTEGER,rnk,comm%comm,ierr)
            CALL MPI_BARRIER (comm%comm,ierr)
            DO m = 1,total
               ptr => set_ptr (grid(:),lvl,tmpry_lst(m))
               IF (ASSOCIATED (ptr)) ptr%proc = rnk
            ENDDO
            DEALLOCATE (tmpry_lst)
         ENDIF
      ENDDO
   ENDDO

   END SUBROUTINE communicate_processor
!=======================================================================
!  END communicate_processor
!=======================================================================

!=======================================================================
!  BEGIN set_proc
!=======================================================================
   RECURSIVE SUBROUTINE set_proc (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      level,tag_glbl,tag_tmpry,nsd,n
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   level = ptr%level; tag_glbl = ptr%tag_glbl;

   IF (tag_glbl >= i3i) THEN
      tag_tmpry = ((tag_glbl-i3i)/(i2i**(i2i*(level_max-level)))) + i3i
      nsd = ((tag_tmpry-i3i)/((sbdmn(level)%im-i2i)*(sbdmn(level)%jm-i2i)))+i1i
   ELSE
      IF (tag_glbl == i1i) nsd = sbdmn(level)%nsd_north_glbl
      IF (tag_glbl == i2i) nsd = sbdmn(level)%nsd_south_glbl
   ENDIF

   ptr%proc = sbdmn(level)%proc(nsd)

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) THEN
         CALL set_proc (ptr%dn(n)%p)
      ENDIF
   ENDDO

   END SUBROUTINE set_proc
!=======================================================================
!  BEGIN set_proc
!=======================================================================

!=======================================================================
!  BEGIN report_grid_node_total
!=======================================================================
   SUBROUTINE report_grid_node_total (communicator_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      communicator_name
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      rnk,total,ierr
   TYPE (comm_node),POINTER :: &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   comm => parallel_get_communicator (communicator_name)

   IF (comm%rnk_comm==0) THEN
      OPEN (UNIT=17,FILE="grid_node_total_"// &
                         integer_to_string (2,level_max)//"_"// &
                         integer_to_string (2,sbdmn_iota)//"_"// &
                         integer_to_string (5,comm%npe_comm),FORM='FORMATTED')
   ENDIF

   DO rnk = 0,comm%npe_comm-1
      IF (rnk==comm%rnk_comm) THEN
         total = grid_node_total
      ELSE
         total = -1
      ENDIF

      CALL MPI_BCAST (total,1,MPI_INTEGER,rnk,comm%comm,ierr)

      IF (comm%rnk_comm==0) THEN
         WRITE (UNIT=17,FMT="(I8,I16)") rnk,total
      ENDIF

   ENDDO

   IF (comm%rnk_comm==0) THEN
      CLOSE (UNIT=17)
   ENDIF

   END SUBROUTINE report_grid_node_total
!=======================================================================
!  END report_grid_node_total
!=======================================================================

!=======================================================================
!  BEGIN real_count
!=======================================================================
   RECURSIVE SUBROUTINE real_count (ptr,level,total)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
   INTEGER (KIND=int_kind) :: &
      level
!.......................................................................
!  INTENT OUT
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      total
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF ((ptr%l_real).AND.(ptr%level==level)) THEN
      total = total + 1_int_kind
   ENDIF

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) CALL real_count (ptr%dn(n)%p,level,total)
   ENDDO

   END SUBROUTINE real_count
!=======================================================================
!  BEGIN real_count
!=======================================================================

!=======================================================================
!  BEGIN real_list
!=======================================================================
   RECURSIVE SUBROUTINE real_list (ptr,level,m,lst)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
   INTEGER (KIND=int_kind) :: &
      level,m
!.......................................................................
!  INTENT OUT
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lst(:)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF ((ptr%l_real).AND.(ptr%level==level)) THEN
      m = m + 1_int_kind
      lst(m) = ptr%tag_glbl
   ENDIF

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) CALL real_list (ptr%dn(n)%p,level,m,lst)
   ENDDO

   END SUBROUTINE real_list
!=======================================================================
!  BEGIN real_list
!=======================================================================

!=======================================================================
!  BEGIN set_path
!=======================================================================
   RECURSIVE SUBROUTINE set_path (lvl,count_next,count_real,count_ghst, &
                                      ptr_next,ptr_real,ptr_ghst,ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl,count_next,count_real,count_ghst
   TYPE (grid_node),POINTER :: &
      ptr_next,ptr_real,ptr_ghst,ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF ((ptr%level==lvl).AND.((ptr%l_real).OR.(ptr%l_ghst))) THEN
      count_next = count_next + i1i
      ptr_next%next_next%p => ptr
      ptr_next             => ptr
   ENDIF

   IF ((ptr%level==lvl).AND.(ptr%l_real)) THEN
      count_real = count_real + i1i
      ptr_real%next_real%p => ptr
      ptr_real             => ptr
   ENDIF

   IF ((ptr%level==lvl).AND.(ptr% l_ghst)) THEN
      count_ghst = count_ghst + i1i
      ptr_ghst%next_ghst%p => ptr
      ptr_ghst             => ptr
   ENDIF

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) THEN
         CALL set_path (lvl,count_next,count_real,count_ghst, &
                            ptr_next,ptr_real,ptr_ghst,ptr%dn(n)%p)
      ENDIF
   ENDDO

   END SUBROUTINE set_path
!=======================================================================
!  BEGIN set_path
!=======================================================================

!=======================================================================
!  BEGIN set_ix_2D
!=======================================================================
   RECURSIVE SUBROUTINE set_ix_2D (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,lvl
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   lvl = ptr%level

   ptr%ix_2D(:) = get_index (swmgrid(:),lvl,sbdmn(lvl)%sbdmn_iota, &
                                 sbdmn(lvl)%lst(:),ptr%tag_glbl)

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) THEN
         CALL set_ix_2D (ptr%dn(n)%p)
      ENDIF
   ENDDO

   END SUBROUTINE set_ix_2D
!=======================================================================
!  BEGIN set_ix_2D
!=======================================================================

!=======================================================================
!  BEGIN set_nghbr_lst
!=======================================================================
   RECURSIVE SUBROUTINE set_nghbr_lst (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,ix5(5),ix6(6)
   TYPE (grid_node),POINTER :: &
      ptr1,ptr2
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (ptr%l_real) THEN
      IF (ptr%l_pentagon) THEN

         ALLOCATE (ptr%nghbr_lst(5,2))

         IF (ptr%l_pole_north    ) ix5(:) = (/ 1,2,3,4,5 /)
         IF (ptr%l_pole_south    ) ix5(:) = (/ 1,2,3,4,5 /)
         IF (ptr%l_pentagon_north) ix5(:) = (/ 1,2,3,5,6 /)
         IF (ptr%l_pentagon_south) ix5(:) = (/ 1,2,3,4,5 /)

         DO n = 1,5
            ptr1 => ptr%nghbr(ix5(n))%p
            ptr%nghbr_lst(n,1) = ptr1%tag_locl
            ptr2 => twist (ptr1,ptr,i2i)
            ptr%nghbr_lst(n,2) = ptr2%tag_locl
         ENDDO
      ELSE
         ALLOCATE (ptr%nghbr_lst(6,2))

         ix6(:) = (/ 1,2,3,4,5,6 /)

         DO n = 1,6
            ptr1 => ptr%nghbr(ix6(n))%p
            ptr%nghbr_lst(n,1) = ptr1%tag_locl
            ptr2 => twist (ptr1,ptr,i2i)
            ptr%nghbr_lst(n,2) = ptr2%tag_locl
         ENDDO
      ENDIF
   ENDIF

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) THEN
         CALL set_nghbr_lst (ptr%dn(n)%p)
      ENDIF
   ENDDO

   END SUBROUTINE set_nghbr_lst
!=======================================================================
!  BEGIN set_nghbr_lst
!=======================================================================

!=======================================================================
!  BEGIN twist
!=======================================================================
   FUNCTION twist (ptr0,ptr1,ntwist) RESULT (ptr2)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr0,ptr1
   INTEGER (KIND=int_kind) :: &
      ntwist
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr2
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_found
   INTEGER (KIND=int_kind) :: &
      n,n1,ix5(5),ix6(6)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (ptr0%l_pentagon) THEN
      IF (ptr0%l_pole_north    ) ix5(:) = (/ 1,2,3,4,5 /)
      IF (ptr0%l_pole_south    ) ix5(:) = (/ 1,2,3,4,5 /)
      IF (ptr0%l_pentagon_north) ix5(:) = (/ 1,2,3,5,6 /)
      IF (ptr0%l_pentagon_south) ix5(:) = (/ 1,2,3,4,5 /)

      l_found = .FALSE.
      DO n = 1,5
         IF (ptr0%nghbr(ix5(n))%p%tag_glbl == ptr1%tag_glbl) THEN
            l_found = .TRUE.; n1 = n; EXIT;
         ENDIF
      ENDDO
      ix5(:) = CSHIFT (ix5(:),ntwist); ptr2 => ptr0%nghbr(ix5(n1))%p;
   ELSE
      ix6(:) = (/ 1,2,3,4,5,6 /)

      l_found = .FALSE.
      DO n = 1,6
         IF (ptr0%nghbr(ix6(n))%p%tag_glbl == ptr1%tag_glbl) THEN
            l_found = .TRUE.; n1 = n; EXIT;
         ENDIF
      ENDDO
      ix6(:) = CSHIFT (ix6(:),ntwist); ptr2   => ptr0%nghbr(ix6(n1))%p;
   ENDIF

   END FUNCTION twist
!=======================================================================
!  BEGIN twist
!=======================================================================

!=======================================================================
!  BEGIN spiral_path
!=======================================================================
   FUNCTION spiral_path (sprl_dpth,ptr0,tag_optional) RESULT (ptr1)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      sprl_dpth
   TYPE (grid_node),POINTER :: &
      ptr0
   INTEGER (KIND=int_kind),OPTIONAL :: &
      tag_optional
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr1
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_found
   INTEGER (KIND=int_kind) :: &
      tag_sprl,nsprl,n,position,sprl_len(20),sprl_lngth
   TYPE (grid_node),POINTER :: &
      ptr0_sprl,ptr1_sprl,ptr2_sprl,ptr_temp
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
   
   ptr0_sprl => ptr0

   IF (ASSOCIATED (ptr0%nghbr(1)%p)) THEN
      ptr0_sprl%next_sprl%p => ptr0%nghbr(1)%p
      ptr1_sprl             => ptr0_sprl%next_sprl%p
   ELSE
      PRINT *," spiral_path :: neighbor 1 not ASSOCIATED "
      PRINT *," spiral_path :: ptr0%tag_glbl = ",ptr0%tag_glbl
      PRINT *," spiral_path :: ptr0%level    = ",ptr0%level
      STOP
   ENDIF

   IF (PRESENT (tag_optional)) THEN 
      tag_sprl = tag_optional
   ELSE
      tag_sprl = ptr0%tag_glbl
   ENDIF

   ptr0_sprl%tag_sprl = tag_sprl
   ptr1_sprl%tag_sprl = tag_sprl

   sprl_len(:) = (/    6,  18,  36,  60,  90, 126, 168, 216, 270, 330, &
                     396, 468, 546, 630, 720, 816, 918,1026,1140,1260 /)

   sprl_lngth = sprl_len(MIN (20,sprl_dpth))-1
   IF (ptr0%level==0) sprl_lngth = MIN (sprl_len(1),sprl_lngth)
   IF (ptr0%level==1) sprl_lngth = MIN (sprl_len(2),sprl_lngth)
   IF (ptr0%level==2) sprl_lngth = MIN (sprl_len(3),sprl_lngth)
   IF (ptr0%level==3) sprl_lngth = MIN (sprl_len(4),sprl_lngth)

   DO nsprl = 1,sprl_lngth
   
      position = 0
      DO n = 1,6
         position = position + 1
         IF (ASSOCIATED (ptr1_sprl%nghbr(n)%p)) THEN
            ptr_temp => ptr1_sprl%nghbr(n)%p
            IF (ptr_temp%tag_glbl==ptr0_sprl%tag_glbl) EXIT
         ENDIF
      ENDDO

      l_found = .FALSE.
      DO n = 1,6
         position = MOD (position+4,6)+1
         IF (ASSOCIATED (ptr1_sprl%nghbr(position)%p)) THEN
            ptr2_sprl => ptr1_sprl%nghbr(position)%p
            IF (ptr2_sprl%tag_sprl /= tag_sprl) THEN
               l_found = .TRUE.
               EXIT
            ENDIF
         ENDIF
      ENDDO

      IF (.NOT.l_found) THEN
         PRINT *," spiral_path :: (.NOT.l_found) "
         PRINT *," spiral_path :: ptr0%tag_glbl = ",ptr0%tag_glbl
         PRINT *," spiral_path :: ptr0%level    = ",ptr0%level
         PRINT *," spiral_path :: sprl_lngth    = ",sprl_lngth
         STOP
      ELSE
         ptr2_sprl%tag_sprl = tag_sprl
         ptr1_sprl%next_sprl%p => ptr2_sprl
         ptr0_sprl => ptr1_sprl
         ptr1_sprl => ptr2_sprl
      ENDIF
   ENDDO

   NULLIFY (ptr2_sprl%next_sprl%p)

   ptr1 => ptr0

   END FUNCTION spiral_path
!=======================================================================
!  BEGIN spiral_path
!=======================================================================

   END MODULE grid_connectivity
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
