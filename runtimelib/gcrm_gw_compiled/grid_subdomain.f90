   MODULE grid_subdomain
 
   USE kinds
   USE numbers
   USE parallel_params
   USE parallel_utilities
   USE grid_params

   USE utilities_misc

   IMPLICIT NONE
   SAVE

   INTEGER (KIND=int_kind),PARAMETER :: &
      level_threshold      =    2, &! resolution at which one processor owns 
                                    ! the whole grid
      cell_min             =   16, &! minimum number of cells per subdomain 
                                    ! must be one of 4,16,64,256,1024,...
      distribution_pattern =    2   ! pattern to assign subdomains to procs
                                    ! at the finest resolution

   LOGICAL (KIND=log_kind) :: &     ! .TRUE. iff subdomain contains 
      l_sbdmn_pntgn_north(nsdm), &  !            mid-latitiude pentagon
      l_sbdmn_pntgn_south(nsdm)

   LOGICAL (KIND=log_kind) :: &     ! .TRUE. iff subdomain contains 
      l_sbdmn_north_pole(nsdm), &   !            pole pentagon
      l_sbdmn_south_pole(nsdm)

   CONTAINS
!=======================================================================
!  BEGIN initialize_subdomain
!=======================================================================
   SUBROUTINE initialize_subdomain ()
!.......................................................................
!  LOCAL
!.......................................................................

   LOGICAL (KIND=log_kind) :: &
      l_include
   INTEGER (KIND=int_kind) :: &
      lvl,m,n,nsd,iota,i,j,pnl,lvl_map,p,q,di(8),dj(8), &
      ix(3),sbdmn_north(5),sbdmn_south(5),sbdmn_offset,rnk
   INTEGER (KIND=int_kind),ALLOCATABLE :: &
      sbdmn_map(:,:,:),map_ix(:,:)
   TYPE (extended_list_node),POINTER :: &
      tmpry
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  check that the gobal grid is built to sufficiently high resoultion 
!  to allow subdomain blocks to be properly attached at resoultions
!  greater than level_glbl
!-----------------------------------------------------------------------
   IF (level_glbl < sbdmn_iota) THEN
      PRINT *," initialize_subdomain : level_glbl < sbdmn_iota "
      PRINT *," sbdmn_iota                  = ",sbdmn_iota
      PRINT *," level_glbl                  = ",level_glbl
      STOP
   ENDIF
!-----------------------------------------------------------------------
!  check that gobal number of subdomains is evenly divisible by npe_wrld
!-----------------------------------------------------------------------
   IF (MOD (nsdm_glbl,npe_wrld) /= 0) THEN
      PRINT *," initialize_subdomain : the global number of subdomains "
      PRINT *," it not divisble by npe_wrld                            "
      PRINT *," nsdm_glbl                  = ",nsdm_glbl
      PRINT *," npe_wrld                   = ",npe_wrld
      STOP
   ENDIF
!-----------------------------------------------------------------------
!  the finest grid resolution
!-----------------------------------------------------------------------
   sbdmn(level_max)%cell_max   = cell_max
   sbdmn(level_max)%sbdmn_iota = sbdmn_iota
   sbdmn(level_max)%nsdm       = nsdm
   sbdmn(level_max)%nsdm_glbl  = nsdm_glbl
   ALLOCATE (sbdmn(level_max)%lst(sbdmn(level_max)%nsdm))

   IF ((cell_max-2)/nsdm_glbl < cell_min) THEN
      PRINT *," initialize_subdomain : subdomain blocks contains less "
      PRINT *," than the minimum number of cells                      "
      PRINT *," level_max                  = ",level_max
      PRINT *," sbdmn(level_max)%cell_max  = ",sbdmn(level_max)%cell_max
      PRINT *," cell_min                   = ",cell_min
      PRINT *," (cell_max-2)/nsdm_glbl     = ",(cell_max-2)/nsdm_glbl
      STOP
   ENDIF

   SELECT CASE (distribution_pattern)
   CASE (01) ! strided 
      sbdmn(level_max)%lst(:) = (/ (rnk_wrld+n,n=1,nsdm_glbl,npe_wrld) /)
   CASE (02) ! contiguous
      sbdmn(level_max)%lst(:) = (/ (nsdm*rnk_wrld+n,n=1,nsdm) /)
   END SELECT
!-----------------------------------------------------------------------
!  finest to level_threshold (see resolution_level_partition.nb or sbdmn.nb)
!-----------------------------------------------------------------------
   DO lvl = level_max-1,level_threshold+1,-1
      sbdmn(lvl)%cell_max = 2+10*2**(2*lvl)

      IF (((sbdmn(lvl)%cell_max-2)/sbdmn(lvl+1)%nsdm_glbl) >= cell_min) THEN
         sbdmn(lvl)%sbdmn_iota = sbdmn(lvl+1)%sbdmn_iota
      ELSE
         sbdmn(lvl)%sbdmn_iota = sbdmn(lvl+1)%sbdmn_iota-1
      ENDIF

      IF (sbdmn(lvl)%sbdmn_iota < 0) THEN
         PRINT *," initialize_subdomain : subdomain blocks contains less "
         PRINT *," than the minimum number of cells                      "
         PRINT *," lvl                  = ",lvl
         PRINT *," sbdmn(lvl)%cell_max  = ",sbdmn(lvl)%cell_max
         PRINT *," cell_min             = ",cell_min
         STOP
      ENDIF

      sbdmn(lvl)%nsdm_glbl = 10*2**(2*(sbdmn(lvl)%sbdmn_iota))

      IF (sbdmn(lvl)%sbdmn_iota==sbdmn(lvl+1)%sbdmn_iota) THEN
         sbdmn(lvl)%nsdm = sbdmn(lvl+1)%nsdm
         ALLOCATE (sbdmn(lvl)%lst(sbdmn(lvl)%nsdm))
         sbdmn(lvl)%lst(:) = sbdmn(lvl+1)%lst(:)
      ELSE
         sbdmn(lvl)%nsdm = COUNT (MOD (sbdmn(lvl+1)%lst(:)-1,4)==0)
         ALLOCATE (sbdmn(lvl)%lst(sbdmn(lvl)%nsdm))
         n = 0
         DO nsd = 1,sbdmn(lvl+1)%nsdm
            IF (MOD (sbdmn(lvl+1)%lst(nsd)-1,4)==0) THEN
               n = n + 1
               sbdmn(lvl)%lst(n) = ((sbdmn(lvl+1)%lst(nsd)-1)/4)+1
            ENDIF
         ENDDO
      ENDIF
   ENDDO
!-----------------------------------------------------------------------
!  IF (level_max == level_threshold) THEN STOP
!-----------------------------------------------------------------------
   IF (level_max == level_threshold) THEN 
      PRINT *," initialize_subdomain : level_max == level_threshold "
      PRINT *,"             all processes must have nsdm unique     "
      PRINT *,"             subdomains on the finest resolution.    "
      CALL parallel_finalize (); STOP;
   ENDIF
!-----------------------------------------------------------------------
!  level_threshold to coarsest
!-----------------------------------------------------------------------
   DO lvl = level_threshold,0,-1

      sbdmn(lvl)%cell_max = 2+10*2**(2*lvl)

      sbdmn(lvl)%sbdmn_iota = 0

      IF (rnk_wrld==0) THEN
         sbdmn(lvl)%nsdm_glbl = 10
         sbdmn(lvl)%nsdm      = 10
         ALLOCATE (sbdmn(lvl)%lst(sbdmn(lvl)%nsdm))
         sbdmn(lvl)%lst(:) = (/ (n,n=1,10) /)
      ELSE
         sbdmn(lvl)%nsdm_glbl = 10
         sbdmn(lvl)%nsdm      =  0
         ALLOCATE (sbdmn(lvl)%lst(sbdmn(lvl)%nsdm))
      ENDIF
   ENDDO
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   IF (.FALSE.) THEN
   IF (rnk_wrld==0) THEN
      DO lvl = level_max,0,-1
         PRINT "(A14,I2,A14,I8,A14,I1,A14,I3,A14,I3)", &
                 " lvl        = ",lvl, &
                 " cell_max   = ",sbdmn(lvl)%cell_max, &
                 " sbdmn_iota = ",sbdmn(lvl)%sbdmn_iota, &
                 " nsdm_glbl  = ",sbdmn(lvl)%nsdm_glbl, &
                 " nsdm       = ",sbdmn(lvl)%nsdm
         PRINT "(A21,64I4)"," sbdmn(lvl)%lst(:) = ",sbdmn(lvl)%lst(:)
         PRINT "(A1)"," "
      ENDDO
   ENDIF
   OPEN (UNIT=100+rnk_wrld,FORM='FORMATTED')
   DO lvl = level_max,0,-1
      WRITE (UNIT=100+rnk_wrld,FMT="(A14,I2,A14,I8,A14,I1,A14,I3,A14,I3)") &
                 " lvl        = ",lvl, &
                 " cell_max   = ",sbdmn(lvl)%cell_max, &
                 " sbdmn_iota = ",sbdmn(lvl)%sbdmn_iota, &
                 " nsdm_glbl  = ",sbdmn(lvl)%nsdm_glbl, &
                 " nsdm       = ",sbdmn(lvl)%nsdm
      WRITE (UNIT=100+rnk_wrld,FMT="(A21,64I4)") &
                 " sbdmn(lvl)%lst(:) = ",sbdmn(lvl)%lst(:)
   ENDDO
   CLOSE (UNIT=100+rnk_wrld)
   ENDIF
!-----------------------------------------------------------------------
! set other stuff
!-----------------------------------------------------------------------
   DO lvl = level_max,0,-1

      iota = sbdmn(lvl)%sbdmn_iota
      sbdmn(lvl)%nsd_north_glbl=       2**(2*iota  )    ! subdomain block 
                                                        ! number of north pole
      sbdmn(lvl)%nsd_south_glbl=2*(1+7*2**(2*iota+1))/3 ! subdomain block 
                                                        ! number of south pole

      sbdmn(lvl)%im   = 2+2**(lvl-iota)
      sbdmn(lvl)%jm   = 2+2**(lvl-iota)

      IF ((lvl>0).AND.(lvl-iota<=0)) THEN
         PRINT *," initialize_subdomain :: a subdomain block must contain", &
                                                   " at least four cells. "
         PRINT *,"                         lvl = ",lvl
         PRINT *," sbdmn(lvl)%sbdmn_iota = ",sbdmn(lvl)%sbdmn_iota
         STOP 
      ENDIF

! north pole
      IF (ANY (sbdmn(lvl)%lst(:)==sbdmn(lvl)%nsd_north_glbl)) THEN
         sbdmn(lvl)%l_agent_north = .TRUE.
         sbdmn(lvl)%nsd_north = MAXLOC ((/ (n,n=1,sbdmn(lvl)%nsdm) /),DIM=1, &
                         MASK=(sbdmn(lvl)%nsd_north_glbl==sbdmn(lvl)%lst(:)))
      ELSE
         sbdmn(lvl)%l_agent_north = .FALSE.
         sbdmn(lvl)%nsd_north = -1
      ENDIF
! south pole
      IF (ANY (sbdmn(lvl)%lst(:)==sbdmn(lvl)%nsd_south_glbl)) THEN
         sbdmn(lvl)%l_agent_south = .TRUE.
         sbdmn(lvl)%nsd_south = MAXLOC ((/ (n,n=1,sbdmn(lvl)%nsdm) /),DIM=1, &
                         MASK=(sbdmn(lvl)%nsd_south_glbl==sbdmn(lvl)%lst(:)))
      ELSE
         sbdmn(lvl)%l_agent_south = .FALSE.
         sbdmn(lvl)%nsd_south = -1
      ENDIF
   ENDDO

   l_agent_north = sbdmn(level_max)%l_agent_north
   l_agent_south = sbdmn(level_max)%l_agent_south

   nsd_north = sbdmn(level_max)%nsd_north
   nsd_south = sbdmn(level_max)%nsd_south
!-----------------------------------------------------------------------
!  set l_sbdmn_pntgn_north and l_sbdmn_pntgn_south
!-----------------------------------------------------------------------
   sbdmn_north(:) = (/ ( (2**(2*sbdmn_iota))*(m-1)+1, m= 1, 9, 2) /)

   l_sbdmn_pntgn_north(:) = .FALSE.
   DO nsd = 1,nsdm
      IF (ANY (sbdmn(level_max)%lst(nsd)==sbdmn_north(:))) THEN
         l_sbdmn_pntgn_north(nsd) = .TRUE.
      ENDIF
   ENDDO

   sbdmn_south(:) = (/ ( (2**(2*sbdmn_iota))*(m-1)+1, m= 2,10, 2) /)

   l_sbdmn_pntgn_south(:) = .FALSE.
   DO nsd = 1,nsdm
      IF (ANY (sbdmn(level_max)%lst(nsd)==sbdmn_south(:))) THEN
         l_sbdmn_pntgn_south(nsd) = .TRUE.
      ENDIF
   ENDDO
!-----------------------------------------------------------------------
!  set l_sbdmn_north_pole and l_sbdmn_south_pole
!-----------------------------------------------------------------------
   sbdmn_north(:) = &
              (/ (       2**(2*sbdmn_iota)*m,m=1,9,2) /) ! subdomains adjacent
                                                         ! to north pole
   l_sbdmn_north_pole(:) = .FALSE.
   DO nsd = 1,nsdm
      IF (ANY (sbdmn(level_max)%lst(nsd)==sbdmn_north(:))) THEN
         l_sbdmn_north_pole(nsd) = .TRUE.
      ENDIF
   ENDDO

   sbdmn_offset = 1+(((2**sbdmn_iota+1)*(2**sbdmn_iota-1))/3)
   sbdmn_south(:) = &
        (/ (sbdmn_offset+2**(2*sbdmn_iota)*m,m=1,9,2) /) ! subdomains adjacent
                                                         ! to south pole
   l_sbdmn_south_pole(:) = .FALSE.
   DO nsd = 1,nsdm
      IF (ANY (sbdmn(level_max)%lst(nsd)==sbdmn_south(:))) THEN
         l_sbdmn_south_pole(nsd) = .TRUE.
      ENDIF
   ENDDO
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  SET EXTENDED SUBDOMAIN LISTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   DO lvl = 0,level_max
!-----------------------------------------------------------------------
!  set subdomain map
!-----------------------------------------------------------------------
      iota = sbdmn(lvl)%sbdmn_iota
      ALLOCATE (sbdmn_map(2**iota+2,2**iota+2,10))
      sbdmn_map(:,:,:) = -1

      lvl_map=1; i=2; j=2; nsd=0;
      DO pnl = 1,10
         CALL set_sbdmn_map (sbdmn_map(:,:,:),sbdmn(lvl)%sbdmn_iota, &
                                                    lvl_map,i,j,pnl,nsd)
      ENDDO
!-----------------------------------------------------------------------
!  wrap subdomain map
!-----------------------------------------------------------------------
      p = 2**iota+1; q = 2**iota+1;
!  northern hemisphere panels
      DO pnl = 1,9,2
         sbdmn_map(    1,2:q  ,pnl) = sbdmn_map(p:2:-1,  q   ,MOD (pnl+7,10)+1)
         sbdmn_map(2:p  ,    1,pnl) = sbdmn_map(2:p   ,  q   ,MOD (pnl+8,10)+1)
         sbdmn_map(  p+1,2:q  ,pnl) = sbdmn_map(  2   ,2:q   ,MOD (pnl+0,10)+1)
         sbdmn_map(2:p  ,  q+1,pnl) = sbdmn_map(  2   ,q:2:-1,MOD (pnl+1,10)+1)
      ENDDO
!  southern hemisphere panels
      DO pnl = 2,10,2
         sbdmn_map(    1,2:q  ,pnl) = sbdmn_map(  p   ,2:q   ,MOD (pnl+8,10)+1)
         sbdmn_map(2:p  ,    1,pnl) = sbdmn_map(  p   ,q:2:-1,MOD (pnl+7,10)+1)
         sbdmn_map(  p+1,2:q  ,pnl) = sbdmn_map(p:2:-1,  2   ,MOD (pnl+1,10)+1)
         sbdmn_map(2:p  ,  q+1,pnl) = sbdmn_map(2:p   ,  2   ,MOD (pnl+0,10)+1)
      ENDDO
!-----------------------------------------------------------------------
!  set map index
!-----------------------------------------------------------------------
      ALLOCATE (map_ix(3,sbdmn(lvl)%nsdm_glbl))
      map_ix(:,:) = -1
      DO pnl = 1,10
         DO j = 2,q
            DO i = 2,p
               map_ix(:,sbdmn_map(i,j,pnl)) = (/ i,j,pnl /)
            ENDDO
         ENDDO
      ENDDO
!-----------------------------------------------------------------------
!  set extended subdomain lists
!-----------------------------------------------------------------------
      NULLIFY (sbdmn(lvl)%extended_list_head)

      IF (sbdmn(lvl)%nsdm > 0) THEN
!  push the local subdomains to the extended list
         DO n = 1,sbdmn(lvl)%nsdm
            CALL push_extended_list (sbdmn(lvl)%extended_list_head, &
                                     sbdmn(lvl)%lst(n))
         ENDDO
!  push to extended list the local neighbor subdomains
         di(:) = (/  1, 1, 0,-1,-1,-1, 0, 1 /)
         dj(:) = (/  0, 1, 1, 1, 0,-1,-1,-1 /)

         DO n = 1,sbdmn(lvl)%nsdm
            ix(:) = map_ix(:,sbdmn(lvl)%lst(n))
            DO m = 1,8
               IF (sbdmn_map(ix(1)+di(m),ix(2)+dj(m),ix(3))/=-1) THEN
                  CALL push_extended_list (sbdmn(lvl)%extended_list_head, &
                                   sbdmn_map(ix(1)+di(m),ix(2)+dj(m),ix(3)))
               ENDIF
            ENDDO
         ENDDO
!  push to extended list the north pole subdomains
         sbdmn_north(:) = (/ (             2**(2*iota)*m,m=1,9,2) /)
         l_include = .FALSE.
         DO m = 1,5
            IF (ANY (sbdmn(lvl)%lst(:)==sbdmn_north(m))) THEN
               l_include = .TRUE.
            ENDIF
         ENDDO
         IF (l_include) THEN
            DO m = 1,5
               CALL push_extended_list (sbdmn(lvl)%extended_list_head, &
                                        sbdmn_north(m))
            ENDDO
         ENDIF
!  push to extended list the south pole subdomains
         sbdmn_offset = (((2**iota+1)*(2**iota-1))/3)+1
         sbdmn_south(:) = (/ (sbdmn_offset+2**(2*iota)*m,m=1,9,2) /)
         l_include = .FALSE.
         DO m = 1,5
            IF (ANY (sbdmn(lvl)%lst(:)==sbdmn_south(m))) THEN
               l_include = .TRUE.
            ENDIF
         ENDDO
         IF (l_include) THEN
            DO m = 1,5
               CALL push_extended_list (sbdmn(lvl)%extended_list_head, &
                                        sbdmn_south(m))
            ENDDO
         ENDIF
      ENDIF
      DEALLOCATE (sbdmn_map,map_ix)
   ENDDO

!_______________________________________________________________________
   IF (.FALSE.) THEN
   OPEN (UNIT=17,FILE="sbdmn_"//integer_to_string (4,rnk_wrld), &
                                                       FORM='FORMATTED')
   DO lvl = level_max,0,-1
      WRITE (UNIT=17,FMT="(I3,I10,I3,I4,I4)") &
                  lvl,sbdmn(lvl)%cell_max,sbdmn(lvl)%sbdmn_iota, &
                  sbdmn(lvl)%nsdm_glbl,sbdmn(lvl)%nsdm

      DO n = 1,SIZE (sbdmn(lvl)%lst(:))
         WRITE (UNIT=17,FMT="(I4)",advance="no") sbdmn(lvl)%lst(n)
      ENDDO
      WRITE (UNIT=17,FMT="(A1)") " "

      tmpry => sbdmn(lvl)%extended_list_head
      DO WHILE (ASSOCIATED (tmpry))
         WRITE (UNIT=17,FMT="(I4)",advance="no") tmpry%nsd_glbl
         tmpry => tmpry%next
      ENDDO
      WRITE (UNIT=17,FMT="(A1)") " "
      WRITE (UNIT=17,FMT="(A1)") " "
   ENDDO
   CLOSE (UNIT=17)
   ENDIF
!_______________________________________________________________________

!-----------------------------------------------------------------------
!  SET THE PROCESS NUMBER
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!  the finest grid resolution
!-----------------------------------------------------------------------
   ALLOCATE (sbdmn(level_max)%proc(sbdmn(level_max)%nsdm_glbl))
   sbdmn(level_max)%proc(:) = rnk_nonexistent
   SELECT CASE (distribution_pattern)
      CASE (01) ! strided 
         sbdmn(level_max)%proc(:)=(/ (MOD (nsd-1,npe_wrld),nsd=1,nsdm_glbl) /)
      CASE (02) ! contiguous
         DO rnk = 0,npe_wrld-1
            sbdmn(level_max)%proc(nsdm*(rnk+0)+1:nsdm*(rnk+1)  ) = rnk
         ENDDO
   END SELECT
!-----------------------------------------------------------------------
!  finest to level_threshold (see sbdmn.nb)
!-----------------------------------------------------------------------
   DO lvl = level_max-1,level_threshold+1,-1
      ALLOCATE (sbdmn(lvl)%proc(sbdmn(lvl)%nsdm_glbl))
      sbdmn(lvl)%proc(:) = rnk_nonexistent
      IF (sbdmn(lvl)%sbdmn_iota==sbdmn(lvl+1)%sbdmn_iota) THEN
         sbdmn(lvl)%proc(:) = sbdmn(lvl+1)%proc(:)
      ELSE
         DO nsd = 1,sbdmn(lvl)%nsdm_glbl
            sbdmn(lvl)%proc(nsd) = sbdmn(lvl+1)%proc(i4i*(nsd-i1i)+i1i)
         ENDDO
      ENDIF
   ENDDO
!-----------------------------------------------------------------------
!  level_threshold to coarsest
!-----------------------------------------------------------------------
   DO lvl = level_threshold,0,-1
      ALLOCATE (sbdmn(lvl)%proc(sbdmn(lvl)%nsdm_glbl))
      sbdmn(lvl)%proc(:) = i0i
   ENDDO

!#######################################################################
   IF (.FALSE.) THEN
      OPEN (UNIT=19, &
               FILE=TRIM ("sbdmn_proc_pe"//integer_to_string (6,rnk_wrld)), &
                                      FORM='FORMATTED',STATUS='UNKNOWN')
      DO lvl = 0,level_max
         WRITE (UNIT=19,FMT="(A1)") " "
         WRITE (UNIT=19,FMT="(A7,I4)") " lvl = ",lvl
         WRITE (UNIT=19,FMT="(2560I4)") sbdmn(lvl)%proc(:)
      ENDDO
      CLOSE (UNIT=19)
   ENDIF
!#######################################################################

   END SUBROUTINE initialize_subdomain
!=======================================================================
!  END initialize_subdomain
!=======================================================================

!=======================================================================
!  BEGIN set_sbdmn_map
!=======================================================================
   RECURSIVE SUBROUTINE set_sbdmn_map (sbdmn_map,iota,lvl_map,i,j,pnl,nsd)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      sbdmn_map(:,:,:),iota,lvl_map,i,j,pnl,nsd
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      del
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   del = 2**(iota-lvl_map)

   IF (lvl_map <= iota) THEN
      CALL set_sbdmn_map (sbdmn_map(:,:,:),iota,lvl_map+1,i    ,j    ,pnl,nsd)
      CALL set_sbdmn_map (sbdmn_map(:,:,:),iota,lvl_map+1,i+del,j    ,pnl,nsd)
      CALL set_sbdmn_map (sbdmn_map(:,:,:),iota,lvl_map+1,i+del,j+del,pnl,nsd)
      CALL set_sbdmn_map (sbdmn_map(:,:,:),iota,lvl_map+1,i    ,j+del,pnl,nsd)
   ELSE
      nsd = nsd + 1
      sbdmn_map(i,j,pnl) = nsd
   ENDIF

   END SUBROUTINE set_sbdmn_map
!=======================================================================
!  BEGIN set_sbdmn_map
!=======================================================================

!=======================================================================
! BEGIN push_extended_list
!=======================================================================
   SUBROUTINE push_extended_list (extended_list_head,nsd_glbl)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (extended_list_node),POINTER :: &
      extended_list_head
   INTEGER (KIND=int_kind) :: &
      nsd_glbl
!.......................................................................
!  LOCAL
!.......................................................................
   TYPE (extended_list_node),POINTER :: &
      tmpry
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (.NOT.ASSOCIATED (extended_list_head)) THEN
      ALLOCATE (extended_list_head)
      tmpry => extended_list_head
      tmpry%nsd_glbl = nsd_glbl
      NULLIFY (tmpry%next)
   ELSE
! search for existing nsd_glbl
      tmpry => extended_list_head
      DO WHILE (ASSOCIATED (tmpry     ))
         IF (tmpry%nsd_glbl==nsd_glbl) RETURN
         tmpry => tmpry%next
      ENDDO
! add to the end of the list
      tmpry => extended_list_head
      DO WHILE (ASSOCIATED (tmpry%next))
         tmpry => tmpry%next
      ENDDO
      ALLOCATE (tmpry%next)
      tmpry => tmpry%next
      tmpry%nsd_glbl = nsd_glbl
      NULLIFY (tmpry%next)
   ENDIF

   END SUBROUTINE push_extended_list
!=======================================================================
!  END push_extended_list
!=======================================================================

!=======================================================================
!  BEGIN FUNCTION get_proc
!=======================================================================
   FUNCTION get_proc (level,tag_glbl) RESULT (proc)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given a grid resolution level and global tag tag_glbl 
!           return the process rnk that owns that grid point
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      level,tag_glbl
!.......................................................................
!  INTENT OUT
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      proc
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      nsd
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (tag_glbl >= i3i) THEN
      nsd = ((tag_glbl-i3i)/(sbdmn(level)%im*sbdmn(level)%jm)) + i1i
   ELSE
      IF (tag_glbl == i1i) nsd = sbdmn(level)%nsd_north_glbl
      IF (tag_glbl == i2i) nsd = sbdmn(level)%nsd_south_glbl
   ENDIF

   proc = sbdmn(level)%proc(nsd)

   END FUNCTION get_proc
!=======================================================================
!  END FUNCTION get_proc
!=======================================================================

!=======================================================================
! BEGIN get_big_block_index
!=======================================================================
   FUNCTION get_big_block_index (lvl,iota,nsd_glbl) RESULT (ix)
!.......................................................................
! PURPOSE : find the index of nsd_glbl within the big block data structure
! 
!   INPUT : lvl      -> the grid resolution level (r value). typically
!                       this is level_max.
!           iota     -> determines the number of subdomain blocks of the
!                       horizontal domain decomposition. typically this
!                       is sbdmn_iota.
!           nsd_glbl -> global number of subdomain.  typically this is
!                       nsd_glbl
!
!  OUTPUT : ix(3)    -> big block index or panel index.  this number is 
!                       in the range [1,10]
!           ix(1:2)  -> the i and j starting index within the big block.
!                       each of these numbers is in the range [1,2**lvl]
!
! EXAMPLE : suppose the big block global array is called x1 
!           and the local subdomain is called x0. then to copy x0 into the x1
!
!    ix(:) = CALL get_big_block_index (level_max,sbdmn_iota,nsd_glbl)
!    x1(ix(1):ix(1)+im-1,ix(2):ix(2)+jm-1,ix(3)) = x0(2:im-1,2:jm-1)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl,iota,nsd_glbl
!.......................................................................
!  INTENT OUT
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      ix(3)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,nblkm,i
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   n     = nsd_glbl-i1i
   nblkm = i2i**(i2i*iota) ! the number of subdomains in one big block

   ix(3) = (n/nblkm) + i1i ! the global big block index [1,10]

   ix(1:2) = (/ i0i,i0i /)
   DO i = 1,iota
      n     = MOD (n,nblkm)
      nblkm = nblkm/i4i

      SELECT CASE (n/nblkm)
         CASE (0)
            ix(1:2) = ix(1:2) + (i2i**(iota-i))*(/ i0i,i0i /)
         CASE (1)
            ix(1:2) = ix(1:2) + (i2i**(iota-i))*(/ i1i,i0i /)
         CASE (2)
            ix(1:2) = ix(1:2) + (i2i**(iota-i))*(/ i1i,i1i /)
         CASE (3)
            ix(1:2) = ix(1:2) + (i2i**(iota-i))*(/ i0i,i1i /)
      END SELECT
   ENDDO

   ix(1:2) = (/ i1i,i1i /) + (i2i**(lvl-iota))*ix(1:2)

   END FUNCTION get_big_block_index
!=======================================================================
!  END get_big_block_index
!=======================================================================

   END MODULE grid_subdomain
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc


    
