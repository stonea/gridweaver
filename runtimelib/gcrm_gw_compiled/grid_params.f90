   MODULE grid_params
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Purpose:
!
!     This module specifies parameters related to the horizontal structure
!     of the model and horizontal grid of the atmosphere.
!
!  Define:
!
!    level_max -> resolution of the grid.  this parameter determines the
!                 global horizontal grid resoultion.  see TABLE 1.
!                 the user will change level_max to change the grid resolution.
!
!                           ____________________________________
!                           |              number   resolution |  
!                           | max_level   of cells     (km)    |  
!                           | ---------------------------------|
!                           |     5           10242   250.2    | 
!                           |     6           40962   125.1    |  
!                           |     7          163842    62.55   | 
!                           |     8          655362    31.27   | 
!                           |     9         2621442    15.64   |
!                           |    10        10485762     7.82   |
!                           |    11        41943042     3.91   |
!                           |    12       167772162     1.95   |
!                           |    13       671088642     0.977  |
!                           |    14      2684354562     0.487  |
!                           |    15     10737418242     0.244  |
!                 TABLE 1.  |    16     42949672962     0.122  |
!                           ------------------------------------
!
!     sbdmn_iota -> determines the global number of subdomain blocks
!                   that constitute the horizontal domain decomposition
!                   see TABLE 2.  the user will change this number 
!                   to set the global number of subdomain blocks.
!
!                           _____________________________ 
!                           |             global number |
!                           |             of subdomains |
!                           | sbdmn_iota   (=nsdm_glbl) |
!                           | --------------------------|
!                           |      0             10     |
!                           |      1             40     |
!                           |      2            160     |
!                           |      3            640     |
!                           |      4           2560     |
!                           |      5          10240     |
!                           |      6          40960     |
!                 TABLE 2.  |      7         163840     |
!                           -----------------------------
!
!     level_glbl        -> maximum level to which the tree grid grid data
!                          stucture is globally generated.  at higher 
!                          resoultions the local process generates only its 
!                          portion of the grid.
!
!     cell_max          -> the global number of cells on the finest resolution
!
!     nsdm_glbl         -> the global number of subdomains on the finest
!                          resolution
!
!     nsdm              -> the number of subdomain blocks managed by the local 
!                          process
!
!     im, jm            -> the extent of local arrays in the i-direction
!                          and j-direction, respectively
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers
   USE parallel_params

   IMPLICIT NONE
   SAVE

   INTEGER (KIND=int_kind),PARAMETER :: &
      level_max   =  5,  &!
      sbdmn_iota  =  2,  &!
      level_glbl  =  sbdmn_iota+1

   INTEGER (KIND=int_kind),PARAMETER :: &
      cell_max = 2 + 10*((2**level_max)**2)
   INTEGER (KIND=int_kind),PARAMETER :: &
      nsdm_glbl = 10*2**(2*sbdmn_iota),nsdm = nsdm_glbl/npe_wrld, &
      im = 2+2**(level_max-sbdmn_iota), jm = im

   INTEGER (KIND=int_kind),PARAMETER :: &
      edgm=i3i,crnm=i2i

   INTEGER (KIND=int_kind),PARAMETER :: &
      tag_nonexistent = -999

   INTEGER (KIND=SELECTED_INT_KIND(12)) :: & ! the total number of grid nodes
      grid_node_total =   0_8, &             ! allocated by the local process
      grid_node_size  = 300_8, &             ! size in bytes of one grid node
      grid_node_memory_max = 500E06_8

   LOGICAL (KIND=log_kind) :: &
      l_agent_north,l_agent_south ! local process manages pole grid point
   INTEGER (KIND=int_kind) :: &
      nsd_north,nsd_south

   REAL (KIND=dbl_kind),PARAMETER :: &
      alfalfa = & ! 2 Pi/5
         1.2566370614359172953850573533118011536788677597500423_dbl_kind, &
      pentagon_latitude = & ! latitude of icosahedron vertex
         0.4636476090008061162142562314612144020285370542861202_dbl_kind

!-----------------------------------------------------------------------
!  begin grid_node_pointer type
!-----------------------------------------------------------------------
   TYPE grid_node_pointer
      TYPE (grid_node),POINTER :: p
   END TYPE grid_node_pointer
!-----------------------------------------------------------------------
!  end grid_node_pointer type
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  begin grid_node type
!-----------------------------------------------------------------------
   TYPE grid_node
      LOGICAL (KIND=log_kind) :: &
         l_real,l_ghst,l_north,l_south,l_pentagon,l_pole_north,l_pole_south, &
         l_pentagon_north,l_pentagon_south,l_point_set
      INTEGER (KIND=int_kind) :: &
         level,tag_glbl,tag_locl,tag_sprl,i,j,ix_2D(3),proc
      INTEGER (KIND=int_kind),POINTER :: &
         nghbr_lst(:,:)
      REAL (KIND=dbl_kind) :: &
         point(3),corner(3,6)
      REAL (KIND=dbl_kind) :: &
         area,area_inv,area_crn(2),area_inv_crn(2)
      REAL (KIND=dbl_kind) :: &
         f
      TYPE (grid_node_pointer) :: &
         nghbr(0:6),up,dn(0:3)
      TYPE (grid_node_pointer) :: &
         next_next,next_real,next_ghst,next_sprl
   END TYPE grid_node
!-----------------------------------------------------------------------
!  end grid_node type
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  begin extended_list_node type
!-----------------------------------------------------------------------
   TYPE :: extended_list_node
      INTEGER (KIND=int_kind) :: &
         nsd_glbl
      TYPE (extended_list_node),POINTER :: &
         next
   END TYPE extended_list_node
!-----------------------------------------------------------------------
!  end extended_list_node type
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!  begin sbdmn_node type
!-----------------------------------------------------------------------
   TYPE sbdmn_node
      LOGICAL (KIND=log_kind) :: &
         l_agent_north,l_agent_south ! local process manages pole grid point
      INTEGER (KIND=int_kind) :: &
         cell_max,sbdmn_iota,im,jm,nsdm_glbl,nsdm, &
         nsd_north_glbl,nsd_south_glbl,nsd_north,nsd_south
      INTEGER (KIND=int_kind),POINTER :: &
         lst(:),proc(:)
      TYPE (extended_list_node),POINTER :: &
         extended_list_head
   END TYPE sbdmn_node
!-----------------------------------------------------------------------
!  end sbdmn_node type
!-----------------------------------------------------------------------

   TYPE (grid_node),TARGET :: &
      swmgrid(12)

   TYPE (sbdmn_node) :: &
      sbdmn(0:level_max)

   TYPE (grid_node_pointer) :: &
      path_next(0:level_max),path_real(0:level_max),path_ghst(0:level_max)

   INTEGER (KIND=int_kind) :: &
      nm,nm_lvl(0:level_max), &
      nm_real,nm_real_lvl(0:level_max), &
      nm_ghst,nm_ghst_lvl(0:level_max)

   CONTAINS
!=======================================================================
! BEGIN set_ptr
!=======================================================================
   FUNCTION set_ptr (grid,level,tag_glbl) RESULT (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node) :: &
      grid(:)
   INTEGER (KIND=int_kind) :: &
      level,tag_glbl
!.......................................................................
!  INTENT OUT
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      powpow,twopowtwo(0:level_max),m,path(0:level_max),lvl
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   twopowtwo(0:level_max) = (/ (2**(2*powpow),powpow=level_max,0,-1) /)

   SELECT CASE (tag_glbl)
      CASE (01)
         m = 1; path(:) = 0; path(0) = m
      CASE (02)
         m = 2; path(:) = 0; path(0) = m
      CASE DEFAULT
         m = 3+(tag_glbl-3)/twopowtwo(0)
         path(:) = MOD ((tag_glbl-grid(m)%tag_glbl)/twopowtwo,4)
         path(0) = m
   END SELECT

   ptr => grid(path(0))%nghbr(0)%p
   DO lvl = 1,level
      IF (ASSOCIATED (ptr%dn(path(lvl))%p)) THEN
         ptr => ptr%dn(path(lvl))%p
      ELSE
         NULLIFY (ptr)
         EXIT
      ENDIF
   ENDDO

   END FUNCTION set_ptr
!=======================================================================
!  END set_ptr
!=======================================================================

!=======================================================================
! BEGIN get_index
!=======================================================================
   FUNCTION get_index (grid,level,iota,subdomain_list,tag_glbl) RESULT (ix)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node) :: &
      grid(:)
   INTEGER (KIND=int_kind) :: &
      level,iota,subdomain_list(:),tag_glbl
!.......................................................................
!  INTENT OUT
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      ix(3)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      el,nsd_glbl,nsd
   TYPE (grid_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ix(:) = -1

   ptr => set_ptr (grid(:),level,tag_glbl)

   el = 2**(level-iota)

   SELECT CASE (tag_glbl)
      CASE (01)
         ix(1:2) = (/ i2i,el+i2i /)
         nsd_glbl = 2**(2*iota)
      CASE (02)
         ix(1:2) = (/ el+i2i,i2i /)
         nsd_glbl = 1+(((2**iota+1)*(2**iota-1))/3)+9*2**(2*iota)
      CASE DEFAULT
         ix(1:2) = 2 + MOD ((/ ptr%i,ptr%j /)-1,el)
         nsd_glbl = 1+(tag_glbl-3)/(2**(2*(level_max-iota)))
   END SELECT

   DO nsd = 1,SIZE (subdomain_list)
      IF (subdomain_list(nsd)==nsd_glbl) THEN
         ix(3) = nsd
         EXIT
      ENDIF
   ENDDO

   END FUNCTION get_index
!=======================================================================
!  END get_index
!=======================================================================

   END MODULE grid_params
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
