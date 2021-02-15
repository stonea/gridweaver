   MODULE utilities_advection_horz
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PURPOSE:  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers

   USE grid_params
   USE grid_metrics
   USE grid_utilities

   USE wrp1D_data

   IMPLICIT NONE
   SAVE
   PRIVATE
   
   PUBLIC :: &
      initialize_advection_horz,del_dot_xv,del_dot_xv_2nd,del_dot_flx2

   TYPE advctn_horz_type
      INTEGER (KIND=int_kind) :: &
         indx0
      INTEGER (KIND=int_kind),DIMENSION(      6) :: &
         indx1
      INTEGER (KIND=int_kind),DIMENSION(3,6,4,3) :: &
         indx_trngl
      INTEGER (KIND=int_kind),DIMENSION(3,  4,3) :: &
         indx_intrp
      REAL    (KIND=dbl_kind),DIMENSION(3,  4,3) :: &
         wght_intrp
      REAL    (KIND=dbl_kind) :: &
         point_intrp(3,4,3)
   END TYPE advctn_horz_type

   TYPE (advctn_horz_type),DIMENSION(:),ALLOCATABLE :: &
      x_advctn_1D

   TYPE (advctn_horz_type),DIMENSION(im,jm,nsdm) :: &
      x_advctn

   REAL (KIND=dbl_kind),DIMENSION(6,im,jm,nsdm) :: &
      intrp_edg_to_fac

   REAL (KIND=dbl_kind),ALLOCATABLE :: &
      point_1D(:,:)

   CONTAINS
!=======================================================================
!  BEGIN  initialize_advection_horz
!=======================================================================
   SUBROUTINE initialize_advection_horz (wrap_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,n_locl,i,j,nsd
   TYPE (grid_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ALLOCATE (x_advctn_1D(  nm))
   ALLOCATE (point_1D   (3,nm))

   CALL set_point_1D (wrap_name)

   CALL set_indx ()

   CALL set_point_intrp ()

   CALL set_indx_trngl ()

   CALL set_intrp () 

   ptr => path_real(level_max)%p
   DO n = 1,nm_real
      IF ((.NOT.ptr%l_pole_north).AND.(.NOT.ptr%l_pole_south)) THEN
         n_locl=ptr%tag_locl; i=ptr%ix_2D(1); j=ptr%ix_2D(2); nsd=ptr%ix_2D(3);
         x_advctn(i,j,nsd) = x_advctn_1D(n_locl)
      ENDIF
      ptr => ptr%next_real%p
   ENDDO

   END SUBROUTINE initialize_advection_horz
!=======================================================================
!  END  initialize_advection_horz
!=======================================================================

!=======================================================================
!  BEGIN  set_point_1D
!=======================================================================
   SUBROUTINE set_point_1D (wrap_name)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
   TYPE (grid_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   point_1D(:,:) = -one

   ptr => path_real(level_max)%p
   DO n = 1,nm_real
      point_1D(:,ptr%tag_locl) = point(:,ptr%ix_2D(1),ptr%ix_2D(2),ptr%ix_2D(3))
      ptr => ptr%next_real%p
   ENDDO
   CALL wrp1D (wrap_name,vrtx_scalar_1lyr=point_1D)

   END SUBROUTINE set_point_1D
!=======================================================================
!  END  set_point_1D
!=======================================================================

!=======================================================================
!  BEGIN  set_indx
!=======================================================================
   SUBROUTINE set_indx ()
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
   TYPE (grid_node),POINTER :: &
      ptr0,ptr1,ptr2,ptr3,ptr4,ptr5,ptr6
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

!-----------------------------------------------------------------------
!  set indx0 and indx1. indx0 -> index of control volume center
!                       indx1 -> neighbor across the control volume edge
!-----------------------------------------------------------------------

   ptr0 => path_next(level_max)%p

   DO n = 1,nm
      ptr1 => ptr0%nghbr(1)%p
      ptr2 => ptr0%nghbr(2)%p
      ptr3 => ptr0%nghbr(3)%p

      ptr4 => twist (ptr0,ptr3, i1i)
      ptr5 => twist (ptr0,ptr3, i2i)
      ptr6 => twist (ptr0,ptr3, i3i)

      x_advctn_1D(n)%indx0 = ptr0%tag_locl
      x_advctn_1D(n)%indx1 = (/ ptr1%tag_locl,ptr2%tag_locl,ptr3%tag_locl, &
                                  ptr4%tag_locl,ptr5%tag_locl,ptr6%tag_locl /)  

      ptr0 => ptr0%next_next%p
   ENDDO

   END SUBROUTINE set_indx
!=======================================================================
!  END  set_indx
!=======================================================================

!=======================================================================
!  BEGIN  set_point_intrp
!=======================================================================
   SUBROUTINE set_point_intrp ()
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,n_locl,e,p,indx1(6)
   REAL (KIND=dbl_kind) :: &
      angle,delta(4)
   REAL (KIND=dbl_kind) :: &
      p0(3),mid(3),nrm(3),c0(3),c1(3),tng(3)
   TYPE (grid_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ptr => path_real(level_max)%p
   DO n = 1,nm_real
      IF ((.NOT.ptr%l_pole_north).AND.(.NOT.ptr%l_pole_south)) THEN
         n_locl = ptr%tag_locl

         p0 = point_1D(:,n_locl)
         indx1 = x_advctn_1D(n_locl)%indx1
         DO e = 1,edgm
            SELECT CASE (01)
               CASE (01)
                  mid = mid_point (p0(:),point_1D(:,indx1(1)))
                  nrm = tangent_to_sphere (mid(:),point_1D(:,indx1(1)))
               CASE (02)
                  c0(:) = voronoi_corner (p0(:),point_1D(:,indx1(6)),point_1D(:,indx1(1)))
                  c1(:) = voronoi_corner (p0(:),point_1D(:,indx1(1)),point_1D(:,indx1(2)))
                  mid = mid_point (c0(:),c1(:))
                  tng = tangent_to_sphere (mid(:),point_1D(:,indx1(6)))
                  nrm = unit_vector (cross_product (mid(:),tng(:)))
            END SELECT

            angle  = ACOS (DOT_PRODUCT (p0,mid))
            delta = (/ -three,-one, one, three /)*angle

            DO p = 1,4
               x_advctn_1D(n_locl)%point_intrp(:,p,e) =  &
                                   COS (delta(p))*mid(:)+SIN (delta(p))*nrm(:)
            ENDDO ! (p)

            indx1 = CSHIFT (indx1,SHIFT=1,DIM=1)
         ENDDO ! (e)
      ENDIF
      ptr => ptr%next_real%p
   ENDDO

   END SUBROUTINE set_point_intrp
!=======================================================================
!  END  set_point_intrp
!=======================================================================

!=======================================================================
!  BEGIN  set_indx_trngl
!=======================================================================
   SUBROUTINE set_indx_trngl ()
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      e,n,n_locl,nghbr,trngl,q,indx_trngl(3,6,4,3)
   REAL (KIND=dbl_kind) :: &
      p_lst(3,3)
   TYPE (grid_node),POINTER :: &
      ptr0,ptr1,ptr2,ptr3,ptr4
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ptr0 => path_real(level_max)%p

   DO n = 1,nm_real

      IF ((.NOT.ptr0%l_pole_north).AND.(.NOT.ptr0%l_pole_south)) THEN
         n_locl = ptr0%tag_locl
         DO e = 1,edgm
            ptr2 => ptr0%nghbr(0)%p
            ptr3 => ptr0%nghbr(e)%p
            ptr1 => twist (ptr2,ptr3,i3i)
            ptr4 => twist (ptr3,ptr2,i3i)

            indx_trngl(:,:,1,e) = set_indx_trngl_cell (ptr1)
            indx_trngl(:,:,2,e) = set_indx_trngl_cell (ptr2)
            indx_trngl(:,:,3,e) = set_indx_trngl_cell (ptr3)
            indx_trngl(:,:,4,e) = set_indx_trngl_cell (ptr4)
         ENDDO
         x_advctn_1D(n_locl)%indx_trngl = indx_trngl
      ENDIF
      ptr0 => ptr0%next_real%p
   ENDDO

   IF (.FALSE.) THEN
      ptr0 => path_real(level_max)%p
      DO n = 1,nm_real
         n_locl = ptr0%tag_locl
         indx_trngl = x_advctn_1D(n_locl)%indx_trngl
         DO e = 1,edgm
            DO nghbr = 1,4
               DO trngl = 1,6
                  DO q = 1,3
                     p_lst(:,q) = point_1D(:,indx_trngl(q,trngl,nghbr,e))
                  ENDDO
                  WRITE (100+rnk_wrld,FMT="(9f12.8)") p_lst
               ENDDO
            ENDDO
         ENDDO
         ptr0 => ptr0%next_real%p
      ENDDO

   ENDIF

   END SUBROUTINE set_indx_trngl
!=======================================================================
!  END  set_indx_trngl
!=======================================================================

!=======================================================================
! BEGIN set_indx_trngl_cell
!=======================================================================
   FUNCTION set_indx_trngl_cell (ptr0) RESULT (ix)
!.......................................................................
!  INTENT IN
!.......................................................................
   TYPE (grid_node),POINTER :: &
      ptr0
!.......................................................................
!  INTENT OUT
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      ix(3,6)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      m,lst6(6),lst5(5)
   TYPE (grid_node),POINTER :: &
      ptr1,ptr2
!:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ix = tag_nonexistent

   IF (ptr0%l_pentagon) THEN
      IF (ptr0%l_pole_north    ) lst5(:) = (/ 1,2,3,4,5 /)
      IF (ptr0%l_pole_south    ) lst5(:) = (/ 1,2,3,4,5 /)
      IF (ptr0%l_pentagon_north) lst5(:) = (/ 1,2,3,5,6 /)
      IF (ptr0%l_pentagon_south) lst5(:) = (/ 1,2,3,4,5 /)

      DO m = 1,6
         ptr1 => ptr0%nghbr(lst5(1))%p; ptr2 => ptr0%nghbr(lst5(2))%p;
         ix(:,m) = (/ ptr0%tag_locl,ptr1%tag_locl,ptr2%tag_locl /)
         lst5 =  CSHIFT (lst5,SHIFT=1)
      ENDDO

   ELSE
      lst6 = (/ 1,2,3,4,5,6 /)

      DO m = 1,6
         ptr1 => ptr0%nghbr(lst6(1))%p; ptr2 => ptr0%nghbr(lst6(2))%p;
         ix(:,m) = (/ ptr0%tag_locl,ptr1%tag_locl,ptr2%tag_locl /)
         lst6 =  CSHIFT (lst6,SHIFT=1)
      ENDDO
   ENDIF

   END FUNCTION set_indx_trngl_cell
!=======================================================================
!  END set_indx_trngl_cell
!=======================================================================

!=======================================================================
!  BEGIN set_intrp
!=======================================================================
   SUBROUTINE set_intrp () 
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_found
   INTEGER (KIND=int_kind) :: &
      e,n,n_locl,p,trngl,q,nghbr,indx_trngl(3,6,4,3),pp,ee
   REAL (KIND=dbl_kind),PARAMETER :: &
      epsilon1 = 1.0E-12_dbl_kind, &
      epsilon2 = 1.0E-12_dbl_kind
   REAL (KIND=dbl_kind) :: &
      point_intrp(3,4,3),p_lst(3,3)
   TYPE (grid_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ptr => path_real(level_max)%p

   DO n = 1,nm_real

      IF ((.NOT.ptr%l_pole_north).AND.(.NOT.ptr%l_pole_south)) THEN

         n_locl = ptr%tag_locl

         indx_trngl  = x_advctn_1D(n_locl)%indx_trngl
         point_intrp = x_advctn_1D(n_locl)%point_intrp

         DO e = 1,edgm ! loop on walls
            DO p = 1,4 ! loop on interp points 

               l_found = .FALSE.
               search_loop : DO nghbr = 1,4

                  DO trngl = 1,6

                     IF (ANY (indx_trngl(:,trngl,nghbr,e)==tag_nonexistent)) CYCLE

                     DO q = 1,3
                        p_lst(:,q) = point_1D(:,indx_trngl(q,trngl,nghbr,e))
                     ENDDO
                     l_found = l_polygon_bounding (point_intrp(:,p,e), &
                                                  p_lst(:,:),epsilon=epsilon1)
                     IF (l_found) THEN
                        x_advctn_1D(n_locl)%indx_intrp(:,p,e) = &
                                                   indx_trngl(:,trngl,nghbr,e)
                        x_advctn_1D(n_locl)%wght_intrp(  :,p,e) = &
                                         linear_weights (point_intrp(:,p,e), &
                                         p_lst(:,1),p_lst(:,2),p_lst(:,3), &
                                         epsilon=epsilon2)
                        EXIT search_loop
                     ENDIF
                  ENDDO ! (trngl)
               ENDDO search_loop ! (nghbr)

               IF (.NOT.l_found) THEN
                  PRINT *," GCRM_x_advection :: set_intrp "
                  PRINT *," ERROR :: cannot find bounding triangle in set_intrp"
                  PRINT *," point_intrp(:,p,m) = ",point_intrp(:,p,e)

                  WRITE (700+rnk_wrld,FMT="(3I10)") n,ptr%tag_locl,ptr%tag_glbl
                  DO ee = 1,edgm ! loop on walls
                     DO pp = 1,4 ! loop on interp points 
                        WRITE (700+rnk_wrld,FMT="(3f12.8)") point_intrp(:,pp,ee)
                     ENDDO
                  ENDDO
                  DO nghbr = 1,4
                     DO trngl = 1,6
                        DO q = 1,3
                           p_lst(:,q) = point_1D(:,indx_trngl(q,trngl,nghbr,e))
                        ENDDO
                        WRITE (700+rnk_wrld,FMT="(9f12.8)") p_lst
                     ENDDO
                  ENDDO
                  STOP
               ENDIF
            ENDDO ! interp loop (p)

         ENDDO ! wall loop (e)
      ENDIF 
      ptr => ptr%next_real%p
   ENDDO ! nm_real loop

   END SUBROUTINE set_intrp
!=======================================================================
!  END  set_intrp
!=======================================================================

!=======================================================================
!  BEGIN  del_dot_xv
!=======================================================================
   SUBROUTINE del_dot_xv (wrap_name,km0,km1,x,wnd,flx,x_f,dt,flx_select,l_flx_limit)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
   INTEGER (KIND=int_kind) :: &
      km0,km1
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km0:km1,nsdm) :: &
      x
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km0:km1,nsdm) :: &
      wnd
   REAL (KIND=dbl_kind),OPTIONAL :: &
      dt
   INTEGER (KIND=int_kind),OPTIONAL :: &
      flx_select
   LOGICAL (KIND=log_kind),OPTIONAL :: &
      l_flx_limit
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km0:km1,nsdm) :: &
      flx
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km0:km1,nsdm) :: &
      x_f
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_flx_limit_1
   INTEGER (KIND=int_kind) :: &
      e,n,p,i,j,k,nsd,indx(3),flx_select_1
   REAL (KIND=dbl_kind) :: &
      wnd_nrm,wght(3),x_tmpry,dt_inv,flx_min,flx_max,factor
   REAL (KIND=dbl_kind) :: &
      thng,gamm,alph,beta
   REAL (KIND=dbl_kind) :: &
      x_intrp(4,edgm)
   REAL (KIND=dbl_kind),ALLOCATABLE :: &
      x_1D(:,:)
   REAL (KIND=dbl_kind),PARAMETER :: &
      wght_3rd(3) = (/ -0.125_dbl_kind, 0.750_dbl_kind, 0.375_dbl_kind /)
   REAL (KIND=dbl_kind),PARAMETER :: &
      epsil = 0.0000001_dbl_kind,x_min = 0.0_dbl_kind
   TYPE (grid_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   IF (PRESENT (dt)) THEN
      dt_inv = one/dt
   ELSE
      dt_inv = one
   ENDIF
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   IF (PRESENT (flx_select)) THEN
      flx_select_1 = flx_select
   ELSE
      flx_select_1 = 03
   ENDIF
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
   IF (PRESENT (l_flx_limit)) THEN
      l_flx_limit_1 = l_flx_limit
   ELSE
      l_flx_limit_1 = .FALSE.
   ENDIF
!-----------------------------------------------------------------------
!  allocate memory
!-----------------------------------------------------------------------
   ALLOCATE (x_1D(nm,km0:km1))
   x_1D = zero
!-----------------------------------------------------------------------
!  set x_1D
!-----------------------------------------------------------------------
   ptr => path_real(level_max)%p
   DO n = 1,nm_real
      x_1D(ptr%tag_locl,:) = x(ptr%ix_2D(1),ptr%ix_2D(2),:,ptr%ix_2D(3))
      ptr => ptr%next_real%p
   ENDDO
   CALL wrp1D (wrap_name,face=x_1D)
!-----------------------------------------------------------------------
!  set del_dot_xv
!-----------------------------------------------------------------------
   DO nsd = 1,nsdm
      DO k = km0,km1
         DO j = 2,jm-1
            DO i = 2,im-1
!-----------------------------------------------------------------------
!  set x at interpolation points
!-----------------------------------------------------------------------
               DO e = 1,edgm
                  DO p = 1,4

                     SELECT CASE (02)
                        CASE (01)

                        CASE (02)
                           indx(:) = x_advctn(i,j,nsd)%indx_intrp(:,p,e)
                           wght(:) = x_advctn(i,j,nsd)%wght_intrp(:,p,e)

                           x_intrp(p,e) = wght(1)*x_1D(indx(1),k) + &
                                          wght(2)*x_1D(indx(2),k) + &
                                          wght(3)*x_1D(indx(3),k) 
                     END SELECT
                  ENDDO ! (p)
               ENDDO ! (e)
!-----------------------------------------------------------------------
!  integrate
!-----------------------------------------------------------------------
               DO e = 1,edgm
                  wnd_nrm = wnd(e,i,j,k,nsd)
                  SELECT CASE (flx_select_1)
                     CASE (00)
                        x_tmpry = zero
                     CASE (01) ! first-order upstream
                        IF (wnd_nrm > zero) THEN
                           x_tmpry = x_intrp(2,e)
                        ELSE
                           x_tmpry = x_intrp(3,e)
                        ENDIF
                     CASE (02) ! 2nd-order centered in space
                        x_tmpry = half*(x_intrp(2,e) + x_intrp(3,e))
                     CASE (03) ! 3rd-order upstream biased
                        IF (wnd_nrm > zero) THEN
                           x_tmpry = wght_3rd(1)*x_intrp(1,e) + &
                                     wght_3rd(2)*x_intrp(2,e) + &
                                     wght_3rd(3)*x_intrp(3,e)
                        ELSE
                           x_tmpry = wght_3rd(1)*x_intrp(4,e) + &
                                     wght_3rd(2)*x_intrp(3,e) + &
                                     wght_3rd(3)*x_intrp(2,e)
                        ENDIF
                     CASE (04) ! 3rd-order positive-definite upstream biased

                        factor = 0.1_dbl_kind

                        IF (wnd_nrm > zero) THEN

                           thng = (x_intrp(1,e)-two*x_intrp(2,e)+x_intrp(3,e))**two + epsil

                           gamm = thng/(thng+factor*(MAX (zero,x_intrp(2,e))*MAX (zero,x_intrp(3,e))))

                           alph = third*gamm+one; beta = one-gamm;

                           x_tmpry = beta*wght_3rd(1)*x_intrp(1,e) + &
                                     alph*wght_3rd(2)*x_intrp(2,e) + &
                                     beta*wght_3rd(3)*x_intrp(3,e)
                        ELSE

                           thng = (x_intrp(2,e)-two*x_intrp(3,e)+x_intrp(4,e))**two + epsil

                           gamm = thng/(thng+factor*(MAX (zero,x_intrp(2,e))*MAX (zero,x_intrp(3,e))))

                           alph = third*gamm+one; beta = one-gamm;

                           x_tmpry = beta*wght_3rd(1)*x_intrp(4,e) + &
                                     alph*wght_3rd(2)*x_intrp(3,e) + &
                                     beta*wght_3rd(3)*x_intrp(2,e)
                        ENDIF

                        x_tmpry = MAX (zero,x_tmpry)

                     CASE (05) ! 4th-order centered in space
                        x_tmpry = twoth*(x_intrp(2,e)+x_intrp(3,e))-sixth*(x_intrp(1,e)+x_intrp(4,e))
                  END SELECT

                  flx(e,i,j,k,nsd) = wnd_nrm*x_tmpry

!-----------------------------------------------------------------------
!  apply the flux limiter
!-----------------------------------------------------------------------
                  IF (l_flx_limit_1) THEN

                     IF (wnd_nrm > zero) THEN
                        flx_max = MAX (zero, dt_inv*area(i,j,nsd)*d_edge_inv(e,i,j,nsd)*x_intrp(2,e))
                        flx(e,i,j,k,nsd) = MIN (flx(e,i,j,k,nsd),flx_max)
                     ELSE
                        flx_min = MIN (zero,-dt_inv*area(i,j,nsd)*d_edge_inv(e,i,j,nsd)*x_intrp(3,e))
                        flx(e,i,j,k,nsd) = MAX (flx(e,i,j,k,nsd),flx_min)
                     ENDIF

                  ENDIF
               ENDDO ! (e)
            ENDDO ! (i)
         ENDDO ! (j)
      ENDDO ! (k)
   ENDDO ! (nsdm)

   CALL wrap (wrap_name,edge_scalar=flx)

!-----------------------------------------------------------------------
!  sum the fluxes
!-----------------------------------------------------------------------
   DO nsd = 1,nsdm
      DO k = km0,km1
         DO j = 2,jm-1
            DO i = 2,im-1
               x_f(i,j,k,nsd) = area_inv(i,j,nsd)* &
                                  ((flx(1,i  ,j  ,k,nsd)*d_edge(1,i,j,nsd) + &
                                    flx(2,i  ,j  ,k,nsd)*d_edge(2,i,j,nsd) + &
                                    flx(3,i  ,j  ,k,nsd)*d_edge(3,i,j,nsd))- &
                                   (flx(1,i-1,j  ,k,nsd)*d_edge(4,i,j,nsd) + &
                                    flx(2,i-1,j-1,k,nsd)*d_edge(5,i,j,nsd) + &
                                    flx(3,i  ,j-1,k,nsd)*d_edge(6,i,j,nsd)))
            ENDDO ! (i)
         ENDDO ! (j)
      ENDDO ! (k)
   ENDDO ! (nsdm)

! north pole
   IF (l_agent_north) THEN
      i = 2; j = jm; nsd = nsd_north;
      DO k = km0,km1
         x_f(i,j,k,nsd) = -area_inv(i,j,nsd)* &
                                      (flx(1,i  ,j  ,k,nsd) + &
                                       flx(2, im,  1,k,nsd) + &
                                       flx(1,i-1,j  ,k,nsd) + &
                                       flx(2,i-1,j-1,k,nsd) + &
                                       flx(3,i  ,j-1,k,nsd))*d_edge(1,i,j,nsd)
      ENDDO ! (k)
   ENDIF

! south pole
   IF (l_agent_south) THEN
      i = im; j = 2; nsd = nsd_south;
      DO k = km0,km1
         x_f(i,j,k,nsd) = -area_inv(i,j,nsd)* &
                                      (flx(2,  1, jm,k,nsd) + &
                                       flx(3,i  ,j  ,k,nsd) + &
                                       flx(1,i-1,j  ,k,nsd) + &
                                       flx(2,i-1,j-1,k,nsd) + &
                                       flx(3,i  ,j-1,k,nsd))*d_edge(1,i,j,nsd)
      ENDDO ! (k)
   ENDIF

   DEALLOCATE (x_1D)

   END SUBROUTINE del_dot_xv
!=======================================================================
!  END  del_dot_xv
!=======================================================================

!=======================================================================
!  BEGIN  del_dot_xv_2nd
!=======================================================================
   SUBROUTINE del_dot_xv_2nd (wrap_name,km0,km1,x,wnd,flx,x_f)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
   INTEGER (KIND=int_kind) :: &
      km0,km1        ! extents of verical index                       []
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km0:km1,nsdm) :: &
      x
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km0:km1,nsdm) :: &
      wnd
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km0:km1,nsdm) :: &
      flx
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km0:km1,nsdm) :: &
      x_f
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      i,j,k,nsd
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   DO nsd = 1,nsdm
      DO k = km0,km1
         DO j = 2,jm-1
            DO i = 2,im-1
               flx(1,i,j,k,nsd) = half*wnd(1,i,j,k,nsd)*(x(i,j,k,nsd)+x(i+1,j  ,k,nsd))
               flx(2,i,j,k,nsd) = half*wnd(2,i,j,k,nsd)*(x(i,j,k,nsd)+x(i+1,j+1,k,nsd))
               flx(3,i,j,k,nsd) = half*wnd(3,i,j,k,nsd)*(x(i,j,k,nsd)+x(i  ,j+1,k,nsd))
            ENDDO
         ENDDO
      ENDDO
   ENDDO

   CALL wrap (wrap_name,edge_scalar=flx)

   DO nsd = 1,nsdm
      DO k = km0,km1
         DO j = 2,jm-1
            DO i = 2,im-1
               x_f(i,j,k,nsd) = area_inv(i,j,nsd)* &
                                  ((flx(1,i  ,j  ,k,nsd)*d_edge(1,i,j,nsd) + &
                                    flx(2,i  ,j  ,k,nsd)*d_edge(2,i,j,nsd) + &
                                    flx(3,i  ,j  ,k,nsd)*d_edge(3,i,j,nsd))- &
                                   (flx(1,i-1,j  ,k,nsd)*d_edge(4,i,j,nsd) + &
                                    flx(2,i-1,j-1,k,nsd)*d_edge(5,i,j,nsd) + &
                                    flx(3,i  ,j-1,k,nsd)*d_edge(6,i,j,nsd)))
            ENDDO
         ENDDO
      ENDDO
   ENDDO

! north pole
   IF (l_agent_north) THEN
      i = 2; j = jm; nsd = nsd_north;
      DO k = km0,km1
         x_f(i,j,k,nsd) = -area_inv(i,j,nsd)* &
                                     (flx(1,i  ,j  ,k,nsd) + &
                                      flx(2, im,  1,k,nsd) + &
                                      flx(1,i-1,j  ,k,nsd) + &
                                      flx(2,i-1,j-1,k,nsd) + &
                                      flx(3,i  ,j-1,k,nsd))*d_edge(1,i,j,nsd)
      ENDDO
   ENDIF

! south pole
   IF (l_agent_south) THEN
      i = im; j = 2; nsd = nsd_south;
      DO k = km0,km1
         x_f(i,j,k,nsd) = -area_inv(i,j,nsd)* &
                                     (flx(2,  1, jm,k,nsd) + &
                                      flx(3,i  ,j  ,k,nsd) + &
                                      flx(1,i-1,j  ,k,nsd) + &
                                      flx(2,i-1,j-1,k,nsd) + &
                                      flx(3,i  ,j-1,k,nsd))*d_edge(1,i,j,nsd)
      ENDDO
   ENDIF

   END SUBROUTINE del_dot_xv_2nd
!=======================================================================
!  END  del_dot_xv_2nd
!=======================================================================

!=======================================================================
!  BEGIN  del_dot_flx2
!=======================================================================
   SUBROUTINE del_dot_flx2 (wrap_name,km0,km1,flx,tmpry)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
   INTEGER (KIND=int_kind) :: &
      km0,km1        ! extents of verical index                       []
   REAL (KIND=dbl_kind),DIMENSION(edgm,im,jm,km0:km1,nsdm) :: &
      flx
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km0:km1,nsdm) :: &
      tmpry
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      i,j,k,nsd
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   DO nsd = 1,nsdm
      DO k = km0,km1
         DO j = 2,jm-1
            DO i = 2,im-1
               tmpry(i,j,k,nsd) = area_inv(i,j,nsd)* &
                                  ((flx(1,i  ,j  ,k,nsd)*d_edge(1,i,j,nsd) + &
                                    flx(2,i  ,j  ,k,nsd)*d_edge(2,i,j,nsd) + &
                                    flx(3,i  ,j  ,k,nsd)*d_edge(3,i,j,nsd))- &
                                   (flx(1,i-1,j  ,k,nsd)*d_edge(4,i,j,nsd) + &
                                    flx(2,i-1,j-1,k,nsd)*d_edge(5,i,j,nsd) + &
                                    flx(3,i  ,j-1,k,nsd)*d_edge(6,i,j,nsd)))
            ENDDO
         ENDDO
      ENDDO
   ENDDO

! north pole
   IF (l_agent_north) THEN
      i = 2; j = jm; nsd = nsd_north;
      DO k = km0,km1
         tmpry(i,j,k,nsd) = -area_inv(i,j,nsd)* &
                                     (flx(1,i  ,j  ,k,nsd) + &
                                      flx(2, im,  1,k,nsd) + &
                                      flx(1,i-1,j  ,k,nsd) + &
                                      flx(2,i-1,j-1,k,nsd) + &
                                      flx(3,i  ,j-1,k,nsd))*d_edge(1,i,j,nsd)
      ENDDO
   ENDIF

! south pole
   IF (l_agent_south) THEN
      i = im; j = 2; nsd = nsd_south;
      DO k = km0,km1
         tmpry(i,j,k,nsd) = -area_inv(i,j,nsd)* &
                                     (flx(2,  1, jm,k,nsd) + &
                                      flx(3,i  ,j  ,k,nsd) + &
                                      flx(1,i-1,j  ,k,nsd) + &
                                      flx(2,i-1,j-1,k,nsd) + &
                                      flx(3,i  ,j-1,k,nsd))*d_edge(1,i,j,nsd)
      ENDDO
   ENDIF

   END SUBROUTINE del_dot_flx2
!=======================================================================
!  END  del_dot_flx2
!=======================================================================

   END MODULE utilities_advection_horz
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
