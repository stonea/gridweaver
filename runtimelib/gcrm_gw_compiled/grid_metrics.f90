   MODULE grid_metrics
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Purpose:
!
!  Define:
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers
   USE physical_params

   USE parallel_params
   USE parallel_utilities

   USE grid_params
   USE grid_connectivity
   USE grid_utilities

   USE wrap_data

!  USE utilities_netCDF
   USE utilities_misc

   IMPLICIT NONE
   SAVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  logical masks
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LOGICAL (KIND=log_kind),DIMENSION(     im,jm,nsdm) :: &
      l_msk_fac ! logical mask for faces (cell centers)
   LOGICAL (KIND=log_kind),DIMENSION(crnm,im,jm,nsdm) :: &
      l_msk_crn ! logical mask for corners
   LOGICAL (KIND=log_kind),DIMENSION(edgm,im,jm,nsdm) :: &
      l_msk_edg ! logical mask for edges
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  metrics defined at cell centers
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   INTEGER (KIND=int_kind),DIMENSION( im,jm,nsdm) :: &
      tag_glbl
   INTEGER (KIND=int_kind),DIMENSION(6,im,jm,nsdm) :: &
      tag_glbl_nghbr
   REAL (KIND=dbl_kind),DIMENSION(3,  im,jm,nsdm) :: &
      point
   REAL (KIND=dbl_kind),DIMENSION(3,6,im,jm,nsdm) :: &
      corner
   REAL (KIND=dbl_kind),DIMENSION(    im,jm,nsdm) :: &
      area,area_inv
   REAL (KIND=dbl_kind),DIMENSION(  6,im,jm,nsdm) :: &
      d_point,d_point_inv,d_edge,d_edge_inv
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  metrics defined at cell corners (abbreviated crn)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(3,crnm,im,jm,nsdm) :: &
      point_crn
   REAL (KIND=dbl_kind),DIMENSION(  crnm,im,jm,nsdm) :: &
      area_crn,area_inv_crn
   REAL (KIND=dbl_kind),DIMENSION(3,crnm,im,jm,nsdm) :: &
      area_kite_crn
   REAL (KIND=dbl_kind),DIMENSION(3,crnm,im,jm,nsdm) :: &
      d_point_crn,d_point_inv_crn,d_edge_crn,d_edge_inv_crn
   REAL (KIND=dbl_kind),DIMENSION(0:3,crnm,im,jm,nsdm) :: &
      rlx_wght_crn
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  metrics defined at cell edges (abbreviated edg)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(3,edgm,im,jm,nsdm) :: &
      point_edg
   REAL (KIND=dbl_kind),DIMENSION(3,edgm,im,jm,nsdm) :: &
      nrm_edg,tng_edg
   REAL (KIND=dbl_kind),DIMENSION(  edgm,im,jm,nsdm) :: &
      area_edg
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  weights
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind),DIMENSION(3,  crnm,im,jm,nsdm) :: &
      wghts_crn ! weights to average cell centers to corners
   REAL (KIND=dbl_kind),DIMENSION(3,2,crnm,im,jm,nsdm) :: &
      vctr_wghts_crn
   REAL (KIND=dbl_kind),DIMENSION(3,2,crnm,im,jm,nsdm) :: &
      vctr_wghts_crn2
!-----------------------------------------------------------------------
!  for wind at the corners in the GCRM
!-----------------------------------------------------------------------
   REAL (KIND=dbl_kind),DIMENSION(3,3,crnm,im,jm,nsdm) :: &
      wghts_wind_crn

   REAL (KIND=dbl_kind),DIMENSION(       6,im,jm,nsdm) :: &
      laplacian_wghts

   REAL (KIND=dbl_kind),DIMENSION( 3, crnm,im,jm,nsdm) :: &
      laplacian_wghts_crn

   INTEGER,PARAMETER :: &
      grid_point_mode = 1, grid_point_file = 19

   CHARACTER (LEN=128),PARAMETER :: &
      grid_point_path_default  = "../../data/grid/grid_points/tweaked"

   INTEGER (KIND=int_kind) :: &
      file_ncid,var_grd_point_xyz_ncid

   CONTAINS
!=======================================================================
!  BEGIN initialize_grid_metrics
!=======================================================================
   SUBROUTINE initialize_grid_metrics (wrap_name, &
                                grid_point_select,grid_point_path_tmpry)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name,grid_point_select
   CHARACTER (LEN=*),OPTIONAL :: &
      grid_point_path_tmpry
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
   CHARACTER (LEN=128) :: &
      grid_point_path
   INTEGER (KIND=int_kind) :: &
      i,j,nsd,n1,n2,ix(3)
   TYPE (grid_node), POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

!-----------------------------------------------------------------------
!  read the positions of the grid points
!-----------------------------------------------------------------------
   IF (PRESENT (grid_point_path_tmpry)) THEN
      grid_point_path  = grid_point_path_tmpry
   ELSE
      grid_point_path  = grid_point_path_default
   ENDIF

   CALL read_grid_point (grid_point_select,grid_point_path)
!-----------------------------------------------------------------------
!  apply a rotation to all grid points
!-----------------------------------------------------------------------
   IF (.FALSE.) THEN
      DO n = 1,SIZE (swmgrid)
         CALL rotate_grid (swmgrid(n)%nghbr(0)%p)
      ENDDO
   ENDIF
!-----------------------------------------------------------------------
!  set grid metrics within the tree structure
!-----------------------------------------------------------------------
   DO n = 1,SIZE (swmgrid)
      CALL initialize_grid_metrics_tree (swmgrid(n)%nghbr(0)%p)
   ENDDO
!-----------------------------------------------------------------------
!  set grid metrics for local subdomains
!-----------------------------------------------------------------------
   CALL initialize_grid_metrics_face (wrap_name)
!-----------------------------------------------------------------------
!  set logical masks
!-----------------------------------------------------------------------
   l_msk_fac( :    , :    ,:) = .FALSE.
   l_msk_fac(2:im-1,2:jm-1,:) = .TRUE.
   IF (l_agent_north) l_msk_fac(2,jm,nsd_north) = .TRUE.
   IF (l_agent_south) l_msk_fac(im,2,nsd_south) = .TRUE.
!-----------------------------------------------------------------------
!  set metrics associated with the corner
!-----------------------------------------------------------------------
   CALL initialize_grid_metrics_corner (wrap_name,point, &
            point_crn,d_point_crn,d_point_inv_crn,d_edge_crn,d_edge_inv_crn, &
                                                           rlx_wght_crn)
!-----------------------------------------------------------------------
!  set logical masks and weights associated with corners
!-----------------------------------------------------------------------
   CALL initialize_wghts_crn ()
!-----------------------------------------------------------------------
!  set the EDGE grid point, normal and tangent directions
!-----------------------------------------------------------------------
   CALL initialize_grid_metrics_edge (wrap_name,point,point_crn, &
                                           point_edg,area_edg,nrm_edg,tng_edg)
!-----------------------------------------------------------------------
!  write the grid to a file
!-----------------------------------------------------------------------
   IF (.FALSE.) THEN
      IF (rnk_wrld==0) PRINT *," grid_metrics :: write_grid "
      CALL write_grid ()
   ENDIF
!-----------------------------------------------------------------------
!  set neighbor list
!-----------------------------------------------------------------------

   ptr => path_real(level_max)%p

!-----------------------------------------------------------------------
!  loop over all real grid points (not ghost) of the local process
!-----------------------------------------------------------------------
   DO n1 = 1,nm_real
!-----------------------------------------------------------------------
!  loop over neighboring directions.  the pentagons will have a direction
!  where the pointer is not associated.
!-----------------------------------------------------------------------
      ix = get_index(swmgrid,level_max,sbdmn_iota,sbdmn(level_max)%lst, &
                     ptr%tag_glbl)
      i = ix(1)
      j = ix(2)
      nsd = ix(3)
      DO n2 = 1,6
         n = 0
         IF (ASSOCIATED (ptr%nghbr(n2)%p)) THEN

            tag_glbl_nghbr(n2,i,j,nsd) &
               = ptr%nghbr(n2)%p%tag_glbl ! global tag of the
                                          ! neighboring cell
         ELSE
           n = n2
         ENDIF
         IF (n.gt.0) then
           IF (n.gt.1) then
             tag_glbl_nghbr(n,i,j,nsd) = tag_glbl_nghbr(n-1,i,j,nsd)
           ELSE
             tag_glbl_nghbr(1,i,j,nsd) = tag_glbl_nghbr(6,i,j,nsd)
           ENDIF
         ENDIF
      ENDDO
      ptr => ptr%next_real%p
   ENDDO

   END SUBROUTINE initialize_grid_metrics
!=======================================================================
!  END   initialize_grid_metrics
!=======================================================================

!=======================================================================
!  BEGIN read_grid_point
!=======================================================================
   SUBROUTINE read_grid_point (grid_point_select,grid_point_path)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      grid_point_select,grid_point_path
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,level
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

!  CALL RANDOM_SEED (PUT=(/ 17,23,97 /))

!-----------------------------------------------------------------------
!  set grid points
!-----------------------------------------------------------------------
   SELECT CASE (TRIM (grid_point_select))
      CASE ("bisect")
         swmgrid(:)%l_point_set = .TRUE.
         swmgrid(01)%point(:) = (/ zero,zero, one /)
         swmgrid(02)%point(:) = (/ zero,zero,-one /)
         DO n = 0,8,2
            swmgrid(n+3)%point(:) = &
             lonlat_to_xyz ((/alfalfa*(FLOAT (n/2)-half), pentagon_latitude/))
            swmgrid(n+4)%point(:) = &
             lonlat_to_xyz ((/alfalfa*(FLOAT (n/2)     ),-pentagon_latitude/))
         ENDDO

         DO level = 0,level_max-1
            DO n = 1,SIZE (swmgrid)
               CALL set_grid_point (grid_point_select,swmgrid(n)%nghbr(0)%p,level)
            ENDDO
         ENDDO
      CASE ("read from file")

         CALL grid_point_open (grid_point_path)

         DO level = 0,level_max
            DO n = 1,SIZE (swmgrid)
               CALL set_grid_point (grid_point_select,swmgrid(n)%nghbr(0)%p,level)
            ENDDO
         ENDDO

         CALL grid_point_close ()

      CASE DEFAULT
         PRINT *," initialize_grid_metrics :: cannot determine grid_point_select"
         STOP
   END SELECT

   END SUBROUTINE read_grid_point
!=======================================================================
!  END   read_grid_point
!=======================================================================

!=======================================================================
!  BEGIN initialize_grid_metrics_tree
!=======================================================================
   RECURSIVE SUBROUTINE initialize_grid_metrics_tree (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_lst(6),l_bad
   INTEGER (KIND=int_kind) :: &
      n,m
   TYPE (grid_node),POINTER :: &
      ptr_a,ptr_b
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   l_bad = .FALSE.

   IF (.NOT.ptr%nghbr(0)%p%l_point_set) l_bad = .TRUE.

   DO m = 1,6
      IF (ASSOCIATED (ptr%nghbr(m)%p)) THEN
         l_lst(m) = .TRUE.
         IF (.NOT.ptr%nghbr(m)%p%l_point_set) l_bad = .TRUE.
      ELSE
         l_lst(m) = .FALSE.
      ENDIF
   ENDDO

   IF ((l_bad).OR.(COUNT (l_lst(:)) < 5)) THEN
      ptr%corner(:,:)     = zero
      ptr%area            = zero
      ptr%area_inv        = zero
      ptr%area_crn(:)     = zero
      ptr%area_inv_crn(:) = zero
   ELSE
!-----------------------------------------------------------------------
!  set cell corners
!-----------------------------------------------------------------------
      DO m = 1,6
         IF (ASSOCIATED (ptr%nghbr(MOD (m+5,6)+1)%p)) THEN
            ptr_a => ptr%nghbr(MOD (m+5,6)+1)%p
         ELSE
            ptr_a => ptr%nghbr(MOD (m+4,6)+1)%p
         ENDIF

         IF (ASSOCIATED (ptr%nghbr(MOD (m  ,6)+1)%p)) THEN
            ptr_b => ptr%nghbr(MOD (m  ,6)+1)%p
         ELSE
            ptr_b => ptr%nghbr(MOD (m+1,6)+1)%p
         ENDIF
         ptr%corner(:,m) = &
                  voronoi_corner (ptr%point(:),ptr_a%point(:),ptr_b%point(:))
      ENDDO
!-----------------------------------------------------------------------
!  set FACE areas and the inverse of the areas
!-----------------------------------------------------------------------
      IF (ptr%l_pentagon) THEN
         ptr%area = five*spherical_triangle_area (ptr%point (:), &
                                              ptr%corner(:,1),ptr%corner(:,2))
      ELSE
         ptr%area     = zero
         DO m = 1,6
            ptr%area = ptr%area + spherical_triangle_area (ptr%point(:), &
                                    ptr%corner(:,m),ptr%corner(:,MOD (m,6)+1))
         ENDDO
      ENDIF
      ptr%area     = a*a*ptr%area
      ptr%area_inv = one/ptr%area
!-----------------------------------------------------------------------
!  set CORNER areas and the inverse of the CORNER areas
!-----------------------------------------------------------------------
      ptr%area_crn(:)     = zero
      ptr%area_inv_crn(:) = zero
      IF ((ptr%l_pole_north).OR.(ptr%l_pole_south)) THEN
         ptr%area_crn(:)     = zero
         ptr%area_inv_crn(:) = zero
      ELSE
         DO m = 1,2
            IF (.NOT.ASSOCIATED (ptr%nghbr(m  )%p)) EXIT
            IF (.NOT.ASSOCIATED (ptr%nghbr(m+1)%p)) EXIT
            ptr_a => ptr%nghbr(m  )%p; ptr_b => ptr%nghbr(m+1)%p;
            ptr%area_crn(m) = spherical_triangle_area (ptr%point(:), &
                                            ptr_a%point(:),ptr_b%point(:))
            ptr%area_crn(m)     = a*a*ptr%area_crn(m)
            ptr%area_inv_crn(m) = one/ptr%area_crn(m)
         ENDDO
      ENDIF
   ENDIF

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) THEN
         CALL initialize_grid_metrics_tree (ptr%dn(n)%p)
      ENDIF
   ENDDO

   END SUBROUTINE initialize_grid_metrics_tree
!=======================================================================
!  BEGIN initialize_grid_metrics_tree
!=======================================================================

!=======================================================================
!  BEGIN initialize_grid_metrics_face
!=======================================================================
   SUBROUTINE initialize_grid_metrics_face (wrap_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      offset,sbdmn_north(5),sbdmn_south(5),nsd,tag_nsd,i,j,m,n
   TYPE (grid_node),POINTER :: &
      ptr0,ptr1
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   offset = 1+(((2**sbdmn_iota+1)*(2**sbdmn_iota-1))/3)

   sbdmn_north(:) = (/ (       2**(2*sbdmn_iota)*m,m=1,9,2) /)
   sbdmn_south(:) = (/ (offset+2**(2*sbdmn_iota)*m,m=1,9,2) /)

   tag_glbl       (  :,:,:) = i0i

   d_point        (:,:,:,:) = zero
   d_point_inv    (:,:,:,:) = zero
   d_edge         (:,:,:,:) = zero
   d_edge_inv     (:,:,:,:) = zero
   laplacian_wghts(:,:,:,:) = zero
   laplacian_wghts_crn(:,:,:,:,:) = zero
   area           (  :,:,:) = zero
   area_inv       (  :,:,:) = zero

   area_crn    (  :,:,:,:) = zero
   area_inv_crn(  :,:,:,:) = zero

   DO nsd = 1,nsdm

      tag_nsd = 3+(2**(2*(level_max-sbdmn_iota)))*(sbdmn(level_max)%lst(nsd)-1)
      ptr0 => set_ptr (swmgrid(:),level_max,tag_nsd)

      DO j = 2,jm-1
         ptr1  => ptr0%nghbr(0)%p
         DO i = 2,im-1

            tag_glbl    (    i,j,nsd) = ptr1%tag_glbl
            point       (:,  i,j,nsd) = ptr1%point(:)
            corner      (:,:,i,j,nsd) = ptr1%corner(:,:) 
            area        (    i,j,nsd) = ptr1%area
            area_inv    (    i,j,nsd) = ptr1%area_inv
            area_crn    (  :,i,j,nsd) = ptr1%area_crn(:)
            area_inv_crn(  :,i,j,nsd) = ptr1%area_inv_crn(:)
!-----------------------------------------------------------------------
!  distances and lengths associated with the hexagonal grid
!-----------------------------------------------------------------------
            DO n = 1,6
               IF (ASSOCIATED (ptr1%nghbr(n)%p)) THEN
                  d_point(n,i,j,nsd) = &
                              arch_distance (ptr1%point(:), &
                                             ptr1%nghbr(n)%p%point(:))
                  d_edge(n,i,j,nsd) = &
                              arch_distance (corner(:,MOD (n+4,6)+1,i,j,nsd),  &
                                             corner(:,     n       ,i,j,nsd))

                  d_point_inv(n,i,j,nsd) = one/d_point(n,i,j,nsd)
                  d_edge_inv (n,i,j,nsd) = one/d_edge (n,i,j,nsd)

                  laplacian_wghts(n,i,j,nsd) = d_edge (n,i,j,nsd)/ &
                                               d_point(n,i,j,nsd)
               ENDIF
            ENDDO
            ptr1  => ptr1%nghbr(1)%p
         ENDDO
         ptr0  => ptr0%nghbr(3)%p
      ENDDO
!-----------------------------------------------------------------------
!  north pole
!-----------------------------------------------------------------------
      IF (ANY (sbdmn_north(:)==sbdmn(level_max)%lst(nsd))) THEN
         ptr0 => set_ptr (swmgrid(:),level_max,i1i)

         tag_glbl(    2,jm,nsd) = ptr0%tag_glbl
         point   (:,  2,jm,nsd) = ptr0%point(:)
         corner  (:,:,2,jm,nsd) = ptr0%corner(:,:)
         area    (    2,jm,nsd) = ptr0%area
         area_inv(    2,jm,nsd) = ptr0%area_inv

         d_point(1:5,2,jm,nsd) = &
                     arch_distance (ptr0%point(:),ptr0%nghbr(1)%p%point(:))
         d_edge (1:5,2,jm,nsd) = &
                     arch_distance (corner(:,1,2,jm,nsd),corner(:,2,2,jm,nsd))

         d_point_inv(1:5,2,jm,nsd) = one/d_point(1:5,2,jm,nsd)
         d_edge_inv (1:5,2,jm,nsd) = one/d_edge (1:5,2,jm,nsd)

         laplacian_wghts(1:5,2,jm,nsd) = d_edge (1:5,2,jm,nsd)/ &
                                         d_point(1:5,2,jm,nsd)
      ENDIF
!-----------------------------------------------------------------------
!  south pole
!-----------------------------------------------------------------------
      IF (ANY (sbdmn_south(:)==sbdmn(level_max)%lst(nsd))) THEN
         ptr0 => set_ptr (swmgrid(:),level_max,i2i)

         tag_glbl(    im,2,nsd) = ptr0%tag_glbl
         point   (:,  im,2,nsd) = ptr0%point(:)
         corner  (:,:,im,2,nsd) = ptr0%corner(:,:)
         area    (    im,2,nsd) = ptr0%area
         area_inv(    im,2,nsd) = ptr0%area_inv

         d_point(1:5,im,2,nsd) = &
                     arch_distance (ptr0%point(:),ptr0%nghbr(1)%p%point(:))
         d_edge (1:5,im,2,nsd) = &
                     arch_distance (corner(:,1,im,2,nsd),corner(:,2,im,2,nsd))

         d_point_inv(1:5,im,2,nsd) = one/d_point(1:5,im,2,nsd)
         d_edge_inv (1:5,im,2,nsd) = one/d_edge (1:5,im,2,nsd)

         laplacian_wghts(1:5,im,2,nsd) = d_edge (1:5,im,2,nsd)/ &
                                         d_point(1:5,im,2,nsd)
      ENDIF
   ENDDO
!-----------------------------------------------------------------------
!  wrap local metrics
!-----------------------------------------------------------------------
   DO m = 1,3
      CALL wrap (wrap_name,face_1lyr=point(m,:,:,:))
   ENDDO
   CALL wrap (wrap_name,vrtx_1lyr=corner(:,1:2,:,:,:))

   CALL wrap (wrap_name,face_1lyr=area    )
   CALL wrap (wrap_name,face_1lyr=area_inv)
   CALL wrap (wrap_name,vrtx_scalar_1lyr=area_crn)
   CALL wrap (wrap_name,vrtx_scalar_1lyr=area_inv_crn)

   CALL wrap (wrap_name,edge_scalar_1lyr=d_edge    (1:3,:,:,:))
   CALL wrap (wrap_name,edge_scalar_1lyr=d_edge_inv(1:3,:,:,:))
!-----------------------------------------------------------------------
!  get laplacian weights for corners
!-----------------------------------------------------------------------
   do nsd = 1,nsdm
      do j = 2,jm-1
         do i = 2,im-1
            laplacian_wghts_crn(1,1,i,j,nsd) = one / laplacian_wghts(1,i,j,nsd)
            laplacian_wghts_crn(2,1,i,j,nsd) =   &
               arch_distance (point(:,i+1,j,nsd), point(:,i+1,j+1,nsd)) / &
               arch_distance (corner(:,1,i,j,nsd),corner(:,2,i+1,j,nsd))
            laplacian_wghts_crn(3,1,i,j,nsd) = one / laplacian_wghts(2,i,j,nsd)
            laplacian_wghts_crn(1,2,i,j,nsd) = laplacian_wghts_crn(3,1,i,j,nsd)
            laplacian_wghts_crn(2,2,i,j,nsd) =   &
               arch_distance (point(:,i+1,j+1,nsd), point(:,i,j+1,nsd)) / &
               arch_distance (corner(:,2,i,j,nsd),corner(:,1,i,j+1,nsd))
            laplacian_wghts_crn(3,2,i,j,nsd) = one / laplacian_wghts(3,i,j,nsd)
         enddo
      enddo
   enddo
!-----------------------------------------------------------------------
!  set lengths on the earth
!-----------------------------------------------------------------------
   d_point    (:,:,:,:) =      a *d_point    (:,:,:,:)
   d_point_inv(:,:,:,:) = (one/a)*d_point_inv(:,:,:,:)
   d_edge     (:,:,:,:) =      a *d_edge     (:,:,:,:)
   d_edge_inv (:,:,:,:) = (one/a)*d_edge_inv (:,:,:,:)

   END SUBROUTINE initialize_grid_metrics_face
!=======================================================================
!  END   initialize_grid_metrics_face
!=======================================================================

!=======================================================================
!  BEGIN initialize_grid_metrics_corner
!=======================================================================
   SUBROUTINE initialize_grid_metrics_corner (wrap_name,point, &
            point_crn,d_point_crn,d_point_inv_crn,d_edge_crn,d_edge_inv_crn, &
                                                           rlx_wght_crn)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
   REAL (KIND=dbl_kind),DIMENSION(3,     im,jm,nsdm) :: &
      point
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(3,crnm,im,jm,nsdm) :: &
      point_crn
   REAL (KIND=dbl_kind),DIMENSION(3,crnm,im,jm,nsdm) :: &
      d_point_crn,d_point_inv_crn,d_edge_crn,d_edge_inv_crn
   REAL (KIND=dbl_kind),DIMENSION(0:3,crnm,im,jm,nsdm) :: &
      rlx_wght_crn
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      i,j,nsd
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   point_crn       = zero

   d_point_crn     = zero
   d_point_inv_crn = zero
   d_edge_crn      = zero
   d_edge_inv_crn  = zero

   rlx_wght_crn    = zero
!-----------------------------------------------------------------------
!  set point_crn
!-----------------------------------------------------------------------
   DO nsd = 1,nsdm
      DO j = 2,jm-1
         DO i = 2,im-1
            point_crn(:,1,i,j,nsd) = voronoi_corner (point(:,i  ,j  ,nsd), &
                                                     point(:,i+1,j  ,nsd), &
                                                     point(:,i+1,j+1,nsd))
            point_crn(:,2,i,j,nsd) = voronoi_corner (point(:,i  ,j  ,nsd), &
                                                     point(:,i+1,j+1,nsd), &
                                                     point(:,i  ,j+1,nsd))
         ENDDO
      ENDDO
   ENDDO
   CALL wrap (wrap_name,vrtx_1lyr=point_crn)
!-----------------------------------------------------------------------
!  set d_point_crn
!-----------------------------------------------------------------------
   DO nsd = 1,nsdm
      DO j = 2,jm-1
         DO i = 2,im-1

            d_point_crn(1,1,i,j,nsd) = arch_distance (point_crn(:,1,i  ,j  ,nsd), &
                                                      point_crn(:,2,i  ,j-1,nsd))
            d_point_crn(2,1,i,j,nsd) = arch_distance (point_crn(:,1,i  ,j  ,nsd), &
                                                      point_crn(:,2,i+1,j  ,nsd))
            d_point_crn(3,1,i,j,nsd) = arch_distance (point_crn(:,1,i  ,j  ,nsd), &
                                                      point_crn(:,2,i  ,j  ,nsd))

            d_point_crn(1,2,i,j,nsd) = arch_distance (point_crn(:,2,i  ,j  ,nsd), &
                                                      point_crn(:,1,i  ,j  ,nsd))
            d_point_crn(2,2,i,j,nsd) = arch_distance (point_crn(:,2,i  ,j  ,nsd), &
                                                      point_crn(:,1,i  ,j+1,nsd))
            d_point_crn(3,2,i,j,nsd) = arch_distance (point_crn(:,2,i  ,j  ,nsd), &
                                                      point_crn(:,1,i-1,j  ,nsd))
         ENDDO
      ENDDO
   ENDDO
!-----------------------------------------------------------------------
!  set d_edge_crn
!-----------------------------------------------------------------------
   DO nsd = 1,nsdm
      DO j = 2,jm-1
         DO i = 2,im-1

            d_edge_crn(1,1,i,j,nsd) = arch_distance (point(:,i  ,j  ,nsd), &
                                                     point(:,i+1,j  ,nsd))
            d_edge_crn(2,1,i,j,nsd) = arch_distance (point(:,i+1,j  ,nsd), &
                                                     point(:,i+1,j+1,nsd))
            d_edge_crn(3,1,i,j,nsd) = arch_distance (point(:,i+1,j+1,nsd), &
                                                     point(:,i  ,j  ,nsd))

            d_edge_crn(1,2,i,j,nsd) = arch_distance (point(:,i  ,j  ,nsd), &
                                                     point(:,i+1,j+1,nsd))
            d_edge_crn(2,2,i,j,nsd) = arch_distance (point(:,i+1,j+1,nsd), &
                                                     point(:,i  ,j+1,nsd))
            d_edge_crn(3,2,i,j,nsd) = arch_distance (point(:,i  ,j+1,nsd), &
                                                     point(:,i  ,j  ,nsd))
         ENDDO
      ENDDO
   ENDDO
!-----------------------------------------------------------------------
!  set rlx_wght_crn
!-----------------------------------------------------------------------
   DO nsd = 1,nsdm
      DO j = 2,jm-1
         DO i = 2,im-1
            rlx_wght_crn(1,1,i,j,nsd) = d_edge_crn(1,1,i,j,nsd)/d_point_crn(1,1,i,j,nsd)
            rlx_wght_crn(2,1,i,j,nsd) = d_edge_crn(2,1,i,j,nsd)/d_point_crn(2,1,i,j,nsd)
            rlx_wght_crn(3,1,i,j,nsd) = d_edge_crn(3,1,i,j,nsd)/d_point_crn(3,1,i,j,nsd)

            rlx_wght_crn(0,1,i,j,nsd)=one/SUM (rlx_wght_crn(:,1,i,j,nsd),DIM=1)

            rlx_wght_crn(1,2,i,j,nsd) = d_edge_crn(1,2,i,j,nsd)/d_point_crn(1,2,i,j,nsd)
            rlx_wght_crn(2,2,i,j,nsd) = d_edge_crn(2,2,i,j,nsd)/d_point_crn(2,2,i,j,nsd)
            rlx_wght_crn(3,2,i,j,nsd) = d_edge_crn(3,2,i,j,nsd)/d_point_crn(3,2,i,j,nsd)

            rlx_wght_crn(0,2,i,j,nsd)=one/SUM (rlx_wght_crn(:,2,i,j,nsd),DIM=1)

         ENDDO
      ENDDO
   ENDDO
!-----------------------------------------------------------------------
!  make as big as the earth
!-----------------------------------------------------------------------
   d_point_crn = a*d_point_crn
   d_edge_crn  = a*d_edge_crn
!-----------------------------------------------------------------------
!  form the inverse
!-----------------------------------------------------------------------
   WHERE (d_point_crn /= zero) d_point_inv_crn = one/d_point_crn
   WHERE (d_edge_crn  /= zero) d_edge_inv_crn  = one/d_edge_crn

   END SUBROUTINE initialize_grid_metrics_corner
!=======================================================================
!  END initialize_grid_metrics_corner
!=======================================================================

!=======================================================================
!  BEGIN initialize_wghts_crn
!=======================================================================
   SUBROUTINE initialize_wghts_crn ()
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_crn(2)
   INTEGER (KIND=int_kind) :: &
      n,i,j,nsd
   REAL (KIND=dbl_kind) :: &
      p(3,0:3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

        l_msk_crn(    :,:,:,:) = .FALSE.
        wghts_crn(:,  :,:,:,:) = zero
   vctr_wghts_crn(:,:,:,:,:,:) = zero
   vctr_wghts_crn2(:,:,:,:,:,:) = zero

   DO nsd = 1,nsdm
      DO j = 1,jm-1
         DO i = 1,im-1

            p(:,0) = point(:,i  ,j  ,nsd)
            p(:,1) = point(:,i+1,j  ,nsd)
            p(:,2) = point(:,i+1,j+1,nsd)
            p(:,3) = point(:,i  ,j+1,nsd)

            l_crn(:) = .TRUE.
            IF ((i==1).AND.(j==1)) THEN
               IF (l_sbdmn_pntgn_south(nsd)) l_crn(1) = .FALSE.
               IF (l_sbdmn_pntgn_north(nsd)) l_crn(2) = .FALSE.
            ENDIF
!-----------------------------------------------------------------------
!  logical mask
!-----------------------------------------------------------------------
            l_msk_crn(:,i,j,nsd) = l_crn(:)
!-----------------------------------------------------------------------
!  weights to area-weight interpolate from cell centers to cell corners
!-----------------------------------------------------------------------
            DO n = 1,2
               IF (l_crn(n)) THEN
                  area_kite_crn(:,n,i,j,nsd) = &
                                  area_corner_kites (p(:,0),p(:,n  ),p(:,n+1))
                  wghts_crn(:,n,i,j,nsd) = area_kite_crn(:,n,i,j,nsd)/ &
                            spherical_triangle_area (p(:,0),p(:,n  ),p(:,n+1))
               ENDIF
            ENDDO
!-----------------------------------------------------------------------
!  weights for gradient defined at corners
!-----------------------------------------------------------------------
            DO n = 1,2
               IF (l_crn(n)) vctr_wghts_crn(:,:,n,i,j,nsd) = &
                                 set_vctr_wghts_crn (p(:,0),p(:,n  ),p(:,n+1))
            ENDDO
         ENDDO
      ENDDO
!-----------------------------------------------------------------------
!  weights for gradient defined at corners with corner inputs
!-----------------------------------------------------------------------
      DO j = 2,jm-1
         DO i = 2,im-1
            vctr_wghts_crn2(:,:,1,i,j,nsd) = &
                     set_vctr_wghts_crn (point_crn(:,2,i,j-1,nsd),    &
                           point_crn(:,2,i+1,j,nsd),point_crn(:,2,i,j,nsd))
            vctr_wghts_crn2(:,:,2,i,j,nsd) = &
                     set_vctr_wghts_crn (point_crn(:,1,i,j,nsd),    &
                           point_crn(:,1,i,j+1,nsd),point_crn(:,1,i-1,j,nsd))
         ENDDO
      ENDDO
!-----------------------------------------------------------------------
!  north pole
!-----------------------------------------------------------------------
      IF (l_sbdmn_north_pole(nsd)) THEN

         p(:,0) = point(:, 1,jm,nsd)
         p(:,1) = point(:, 2,jm,nsd)
         p(:,2) = point(:,im, 1,nsd)

         l_msk_crn(  1,1,jm,nsd) = .TRUE.
         area_kite_crn(:,1,1,jm,nsd) = &
                                      area_corner_kites (p(:,0),p(:,1),p(:,2))
         wghts_crn(:,1,1,jm,nsd) = area_kite_crn(:,1,1,jm,nsd)/ &
                                spherical_triangle_area (p(:,0),p(:,1),p(:,2))
         vctr_wghts_crn(:,:,1,1,jm,nsd) = &
                                     set_vctr_wghts_crn (p(:,0),p(:,1),p(:,2))

         p(:,0) = point(:, 2,jm,nsd)
         p(:,1) = point(:, 3,jm,nsd)
         p(:,2) = point(:,im, 1,nsd)

         l_msk_crn(  1,2,jm,nsd) = .TRUE.
         area_kite_crn(:,1,2,jm,nsd) = &
                                      area_corner_kites (p(:,0),p(:,1),p(:,2))
         wghts_crn(:,1,2,jm,nsd) = area_kite_crn(:,1,2,jm,nsd)/ &
                                spherical_triangle_area (p(:,0),p(:,1),p(:,2))
         vctr_wghts_crn(:,:,1,2,jm,nsd) = &
                                     set_vctr_wghts_crn (p(:,0),p(:,1),p(:,2))
      ENDIF
!-----------------------------------------------------------------------
!  south pole
!-----------------------------------------------------------------------
      IF (l_sbdmn_south_pole(nsd)) THEN

         p(:,0) = point(:,im, 1,nsd)
         p(:,1) = point(:, 1,jm,nsd)
         p(:,2) = point(:,im, 2,nsd)

         l_msk_crn(  2,im,1,nsd) = .TRUE.
         area_kite_crn(:,2,im,1,nsd) = &
                                      area_corner_kites (p(:,0),p(:,1),p(:,2))
         wghts_crn(:,2,im,1,nsd) = area_kite_crn(:,2,im,1,nsd)/ &
                                spherical_triangle_area (p(:,0),p(:,1),p(:,2))
         vctr_wghts_crn(:,:,2,im,1,nsd) = &
                                     set_vctr_wghts_crn (p(:,0),p(:,1),p(:,2))

         p(:,0) = point(:,im, 2,nsd)
         p(:,1) = point(:, 1,jm,nsd)
         p(:,2) = point(:,im, 3,nsd)

         l_msk_crn(  2,im,2,nsd) = .TRUE.
         area_kite_crn(:,2,im,2,nsd) = &
                                      area_corner_kites (p(:,0),p(:,1),p(:,2))
         wghts_crn(:,2,im,2,nsd) = area_kite_crn(:,2,im,2,nsd)/ &
                                spherical_triangle_area (p(:,0),p(:,1),p(:,2))
         vctr_wghts_crn(:,:,2,im,2,nsd) = &
                                     set_vctr_wghts_crn (p(:,0),p(:,1),p(:,2))
      ENDIF
   ENDDO
!-----------------------------------------------------------------------
!  make as big as the earth
!-----------------------------------------------------------------------
   vctr_wghts_crn(:,:,:,:,:,:) =   a*vctr_wghts_crn(:,:,:,:,:,:)
   vctr_wghts_crn2(:,:,:,:,:,:)=   a*vctr_wghts_crn2(:,:,:,:,:,:)
   area_kite_crn (  :,:,:,:,:) = a*a*area_kite_crn (  :,:,:,:,:)

   END SUBROUTINE initialize_wghts_crn
!=======================================================================
!  END initialize_wghts_crn
!=======================================================================

!=======================================================================
!  BEGIN initialize_grid_metrics_edge
!=======================================================================
   SUBROUTINE initialize_grid_metrics_edge (wrap_name,point,point_crn, &
                                           point_edg,area_edg,nrm_edg,tng_edg)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
   REAL (KIND=dbl_kind),DIMENSION(3,     im,jm,nsdm) :: &
      point
   REAL (KIND=dbl_kind),DIMENSION(3,crnm,im,jm,nsdm) :: &
      point_crn
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(3,edgm,im,jm,nsdm) :: &
      point_edg,nrm_edg,tng_edg
   REAL (KIND=dbl_kind),DIMENSION(  edgm,im,jm,nsdm) :: &
      area_edg
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      e,i,j,nsd,di(3),dj(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   di(:) = (/  1,1,0 /); dj(:) = (/ 0,1,1 /);

   point_edg = zero
   area_edg  = zero
   nrm_edg   = zero
   tng_edg   = zero

   DO nsd = 1,nsdm
      DO j = 2,jm-1
         DO i = 2,im-1
            DO e = 1,3
               point_edg(:,e,i,j,nsd) = &
                                    mid_point (point(:,i      ,j      ,nsd), &
                                               point(:,i+di(e),j+dj(e),nsd))
               nrm_edg(:,e,i,j,nsd) = &
                      tangent_to_sphere (point_edg(:,e,i      ,j      ,nsd), &
                                         point    (:,  i+di(e),j+dj(e),nsd))
               tng_edg(:,e,i,j,nsd) = &
                         unit_vector (cross_product (point_edg(:,e,i,j,nsd), &
                                                     nrm_edg  (:,e,i,j,nsd)))
            ENDDO
         ENDDO
      ENDDO
   ENDDO

   DO nsd = 1,nsdm
      DO j = 2,jm-1
         DO i = 2,im-1
            area_edg(1,i,j,nsd) = &
                        spherical_triangle_area (point    (:,i  ,j,  nsd), &
                                                 point_crn(:,2,i,j-1,nsd), &
                                                 point_crn(:,1,i  ,j,nsd)) + &
                        spherical_triangle_area (point    (:,i+1,j  ,nsd), &
                                                 point_crn(:,1,i  ,j,nsd), &
                                                 point_crn(:,2,i,j-1,nsd))
            area_edg(2,i,j,nsd) = &
                        spherical_triangle_area (point    (:,i  ,j,  nsd), &
                                                 point_crn(:,1,i,j  ,nsd), &
                                                 point_crn(:,2,i  ,j,nsd)) + &
                        spherical_triangle_area (point    (:,i+1,j+1,nsd), &
                                                 point_crn(:,2,i  ,j,nsd), &
                                                 point_crn(:,1,i,j  ,nsd))
            area_edg(3,i,j,nsd) = &
                        spherical_triangle_area (point    (:,i  ,j,  nsd), &
                                                 point_crn(:,2,i,j  ,nsd), &
                                                 point_crn(:,1,i-1,j,nsd)) + &
                        spherical_triangle_area (point    (:,i  ,j+1,nsd), &
                                                 point_crn(:,1,i-1,j,nsd), &
                                                 point_crn(:,2,i,j  ,nsd))
         ENDDO
      ENDDO
   ENDDO

   CALL wrap (wrap_name,edge_1lyr=point_edg)
   CALL wrap (wrap_name,edge_1lyr=nrm_edg)
   CALL wrap (wrap_name,edge_1lyr=tng_edg)

   CALL wrap (wrap_name,edge_scalar_1lyr=area_edg)

   END SUBROUTINE initialize_grid_metrics_edge
!=======================================================================
!  END initialize_grid_metrics_edge
!=======================================================================

!=======================================================================
!  BEGIN grid_point_open
!=======================================================================
   SUBROUTINE grid_point_open (grid_point_path)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      grid_point_path
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_exist
   INTEGER (KIND=int_kind) :: &
      rec_len
   INTEGER (KIND=int_kind) :: &
      status
   CHARACTER (LEN=128) :: &
      strng
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   SELECT CASE (grid_point_mode)
      CASE (01)
         strng = TRIM (grid_point_path)//"/ascii/point_"// &
                                integer_to_string (12,cell_max)
         INQUIRE (FILE=TRIM (strng),EXIST=l_exist)
         IF (.NOT.l_exist) THEN
            PRINT *," grid_point_open : looking for grid point file = ", &
                                                            TRIM (strng)
            STOP
         ELSE
            rec_len = 109
!           rec_len = 49
            OPEN (UNIT=grid_point_file,FILE=TRIM (strng), &
                          FORM='FORMATTED',ACCESS='DIRECT',RECL=rec_len)
         ENDIF
      CASE (02)
         strng = TRIM (grid_point_path)//"/netCDF/point_"// &
                                         integer_to_string (12,cell_max)//".nc"
         INQUIRE (FILE=TRIM (strng),EXIST=l_exist)
         IF (.NOT.l_exist) THEN
            PRINT *," grid_point_open : looking for grid point file = ", &
                                                            TRIM (strng)
            STOP
         ELSE
!           status = NF_OPEN (strng,NF_NOWRITE,file_ncid)
!           CALL NF_handle_error (status," grid_point_open :: NF_OPEN NF_NOWRITE ")
!           status = NF_INQ_VARID (file_ncid,"grd_point_xyz",var_grd_point_xyz_ncid)
!           CALL NF_handle_error (status," grid_point_open :: NF_INQ_VARID ")
         ENDIF
   END SELECT

   END SUBROUTINE grid_point_open
!=======================================================================
!  END grid_point_open
!=======================================================================

!=======================================================================
!  BEGIN grid_point_read
!=======================================================================
   FUNCTION grid_point_read (n) RESULT (grid_point)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      grid_point(3)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      status
   REAL (KIND=SELECTED_REAL_KIND(12)) :: &
      grid_point_tmpry(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF ((n < 1).OR.(cell_max < n)) THEN
      PRINT *," grid_point_read : n out of range : n = ",n
      STOP
   ELSE
      SELECT CASE (grid_point_mode)
         CASE (01)
            READ (UNIT=grid_point_file,REC=n,FMT="(3F36.32)") grid_point(:)
         CASE (02)
!           status = NF_GET_VARA_DOUBLE (file_ncid,var_grd_point_xyz_ncid,(/  1,n /),(/ 3,1 /),grid_point_tmpry(:))
!           CALL NF_handle_error (status," grid_point_read :: NF_GET_VARA_DOUBLE ")
!           grid_point(:) = grid_point_tmpry(:)
      END SELECT
   ENDIF

   END FUNCTION grid_point_read
!=======================================================================
!  END grid_point_read
!=======================================================================

!=======================================================================
!  BEGIN grid_point_close
!=======================================================================
   SUBROUTINE grid_point_close ()
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      status
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   SELECT CASE (grid_point_mode)
      CASE (01)
         CLOSE (UNIT=grid_point_file)
      CASE (02)
!        status = NF_CLOSE (file_ncid)
!        CALL NF_handle_error (status," grid_point_close :: NF_CLOSE ")
   END SELECT

   END SUBROUTINE grid_point_close
!=======================================================================
!  END grid_point_close
!=======================================================================

!=======================================================================
!  BEGIN set_grid_point
!=======================================================================
   RECURSIVE SUBROUTINE set_grid_point (grid_point_select,ptr,level)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      grid_point_select
   TYPE (grid_node),POINTER :: &
      ptr
   INTEGER (KIND=int_kind) :: &
      level
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,m
   REAL (KIND=int_kind) :: &
      pert_coeff,perturbation(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   pert_coeff = 0.1_dbl_kind*(two*pi)/FLOAT (10*2**(level_max-1))

   SELECT CASE (TRIM (grid_point_select))

      CASE ("bisect") 
         IF (ptr%level==level) THEN
            IF (ptr%l_point_set) THEN
               IF (ASSOCIATED (ptr%dn(0)%p)) THEN
                  ptr%dn(0)%p%l_point_set = .TRUE.
                  ptr%dn(0)%p%point(:) = ptr%point(:)
               ENDIF
               DO m = 1,3
                  IF ((ASSOCIATED (ptr%nghbr(m)%p)).AND. &
                                   (ASSOCIATED (ptr%dn   (m)%p))) THEN
                     IF (ptr%nghbr(m)%p%l_point_set) THEN
                        ptr%dn(m)%p%l_point_set = .TRUE.
                        ptr%dn(m)%p%point(:) =  &
                                      mid_point (ptr%nghbr(0)%p%point(:), &
                                                 ptr%nghbr(m)%p%point(:))
                        IF (.FALSE.) THEN
!                          CALL RANDOM_NUMBER (perturbation)
                           perturbation(:) = &
                                    two*(perturbation(:)-(/ half,half,half /))

                           ptr%dn(m)%p%point(:) = ptr%dn(m)%p%point(:) + &
                                                   pert_coeff*perturbation(:)
                           ptr%dn(m)%p%point(:) = &
                                            unit_vector (ptr%dn(m)%p%point(:))
                        ENDIF
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF
         ENDIF

      CASE ("read from file")
         IF (ptr%level==level) THEN
            ptr%l_point_set = .TRUE.
            ptr%point(:) = grid_point_read (ptr%tag_glbl)
         ENDIF

   END SELECT

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) THEN
         CALL set_grid_point (grid_point_select,ptr%dn(n)%p,level)
      ENDIF
   ENDDO

   END SUBROUTINE set_grid_point
!=======================================================================
!  BEGIN set_grid_point
!=======================================================================

!=======================================================================
!  BEGIN rotate_grid
!=======================================================================
   RECURSIVE SUBROUTINE rotate_grid (ptr)
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
   REAL (KIND=dbl_kind),PARAMETER :: &
      alphalph = 0.553574358897045251508532730089268520035023822700716_dbl_kind ! rotate pentagon 1 and pentagon 3 to the equator
   REAL (KIND=dbl_kind) :: &
      rho,rotation_matrix_x(3,3),rotation_matrix_y(3,3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   rho = zero*pi/four
   rotation_matrix_x(:,:) = RESHAPE ( (/ one ,      zero,      zero, &
                                         zero, COS (rho),-SIN (rho), &
                                         zero, SIN (rho), COS (rho)/),(/3,3/))

   ptr%point(:) = unit_vector (MATMUL (rotation_matrix_x,ptr%point(:)))

   rho = alphalph
   rotation_matrix_y(:,:) = RESHAPE ( (/ COS (rho), zero,-SIN (rho), &
                                              zero,  one,      zero, &
                                         SIN (rho), zero, COS (rho) /),(/3,3/))
   ptr%point(:) = unit_vector (MATMUL (rotation_matrix_y,ptr%point(:)))

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) CALL rotate_grid (ptr%dn(n)%p)
   ENDDO

   END SUBROUTINE rotate_grid
!=======================================================================
!  END rotate_grid
!=======================================================================

!=======================================================================
!  BEGIN write_grid
!=======================================================================
   SUBROUTINE write_grid (path_output)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),OPTIONAL :: &
      path_output
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,lvl
   CHARACTER (LEN=128) :: &
      temp_strng
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   CALL parallel_barrier ("world")

   DO lvl = 0,level_max
      IF (PRESENT (path_output)) THEN
         temp_strng=TRIM (path_output)// &
                                  "/grid_lvl"//integer_to_string (2,lvl)// &
                                  "_pe"//integer_to_string (6,rnk_wrld)
      ELSE
         temp_strng=               "grid_lvl"//integer_to_string (2,lvl)// &
                                  "_pe"//integer_to_string (6,rnk_wrld)
      ENDIF
      OPEN (UNIT=19+lvl,FILE=TRIM (temp_strng), &
                                      FORM='FORMATTED',STATUS='UNKNOWN')
   ENDDO

   DO n = 1,SIZE (swmgrid)
      CALL write_grid_1 (swmgrid(n)%nghbr(0)%p)
   ENDDO

   DO lvl = 0,level_max
      CLOSE (UNIT=19+lvl)
   ENDDO

   CALL parallel_barrier ("world")

   END SUBROUTINE write_grid
!=======================================================================
!  END write_grid
!=======================================================================

!=======================================================================
!  BEGIN write_grid_1
!=======================================================================
   RECURSIVE SUBROUTINE write_grid_1 (ptr)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,i,lnk(6),existance
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   lnk(:) = -1
   DO i = 1,6
      IF (ASSOCIATED (ptr%nghbr(i)%p)) lnk(i) = ptr%nghbr(i)%p%tag_glbl
   ENDDO

   IF ((.NOT.ptr%l_ghst).AND.(.NOT.ptr%l_real)) existance = 0
   IF ((     ptr%l_ghst).AND.(.NOT.ptr%l_real)) existance = 1
   IF ((.NOT.ptr%l_ghst).AND.(     ptr%l_real)) existance = 2
   IF ((     ptr%l_ghst).AND.(     ptr%l_real)) existance = 3

   WRITE (UNIT=19+ptr%level,FMT="(I13,I3,I7,2I5,6I13,3F16.12,18F10.6)") &
       ptr%tag_glbl,existance,ptr%proc,ptr%i,ptr%j,lnk(:), &
       ptr%point(:),ptr%corner(:,:)

   DO n = 0,3
      IF (ASSOCIATED (ptr%dn(n)%p)) CALL write_grid_1 (ptr%dn(n)%p)
   ENDDO

   END SUBROUTINE write_grid_1
!=======================================================================
!  END write_grid_1
!=======================================================================

   END MODULE grid_metrics
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
