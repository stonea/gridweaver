   MODULE utilities_interp
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PURPOSE:  
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers

   USE grid_params
   USE grid_metrics
   USE grid_utilities

   USE utilities_linear_algebra

   USE wrp1D_data

   IMPLICIT NONE
   SAVE
   PRIVATE
   
   PUBLIC :: &
      initialize_interp,interp_crn_quad

   TYPE interp_type
      INTEGER (KIND=int_kind) :: &
         indx0
      INTEGER (KIND=int_kind),DIMENSION(  6) :: &
         indx1
      INTEGER (KIND=int_kind),DIMENSION(6,2) :: &
         indx_crn_quad
      REAL    (KIND=dbl_kind),DIMENSION(6,2) :: &
         wght_crn_quad
   END TYPE interp_type

   TYPE (interp_type),DIMENSION(:),ALLOCATABLE :: &
      interp_1D

   REAL (KIND=dbl_kind),ALLOCATABLE :: &
      point_1D(:,:),corner_1D(:,:,:)

   CONTAINS
!=======================================================================
!  BEGIN  initialize_interp
!=======================================================================
   SUBROUTINE initialize_interp (wrap_name)
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

   ALLOCATE (interp_1D(    nm))
   ALLOCATE (point_1D (3,  nm))
   ALLOCATE (corner_1D(3,2,nm))

   CALL set_grid_1D (wrap_name)

   CALL set_indx ()

   CALL set_indx_crn_quad ()

   CALL set_wght_crn_quad () 

   DEALLOCATE (point_1D,corner_1D)

   END SUBROUTINE initialize_interp
!=======================================================================
!  END  initialize_interp
!=======================================================================

!=======================================================================
!  BEGIN  set_grid_1D
!=======================================================================
   SUBROUTINE set_grid_1D (wrap_name)
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

      point_1D (:,  ptr%tag_locl) = point (:,  ptr%ix_2D(1),ptr%ix_2D(2),ptr%ix_2D(3))
      corner_1D(:,1,ptr%tag_locl) = corner(:,1,ptr%ix_2D(1),ptr%ix_2D(2),ptr%ix_2D(3))
      corner_1D(:,2,ptr%tag_locl) = corner(:,2,ptr%ix_2D(1),ptr%ix_2D(2),ptr%ix_2D(3))

      ptr => ptr%next_real%p
   ENDDO

   CALL wrp1D (wrap_name,vrtx_scalar_1lyr=point_1D)

   END SUBROUTINE set_grid_1D
!=======================================================================
!  END  set_grid_1D
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

      interp_1D(n)%indx0 = ptr0%tag_locl
      interp_1D(n)%indx1 = (/ ptr1%tag_locl,ptr2%tag_locl,ptr3%tag_locl, &
                              ptr4%tag_locl,ptr5%tag_locl,ptr6%tag_locl /)  

      ptr0 => ptr0%next_next%p
   ENDDO

   END SUBROUTINE set_indx
!=======================================================================
!  END  set_indx
!=======================================================================

!=======================================================================
!  BEGIN  set_indx_crn_quad
!=======================================================================
   SUBROUTINE set_indx_crn_quad ()
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,n_locl,indx_crn_quad(6,2)
   TYPE (grid_node),POINTER :: &
      ptr0,ptr1,ptr2,ptr3,ptr4,ptr5,ptr6,ptr7
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ptr0 => path_real(level_max)%p

   DO n = 1,nm_real

      IF ((.NOT.ptr0%l_pole_north).AND.(.NOT.ptr0%l_pole_south)) THEN

         n_locl = ptr0%tag_locl

         ptr0 => ptr0%nghbr(0)%p
         ptr1 => ptr0%nghbr(1)%p
         ptr2 => ptr0%nghbr(2)%p
         ptr3 => ptr0%nghbr(3)%p

         ptr4 => twist (ptr0,ptr2,-i2i)
         ptr5 => twist (ptr2,ptr0, i2i)
         ptr6 => twist (ptr2,ptr0,-i2i)
         ptr7 => twist (ptr0,ptr2, i2i)

         indx_crn_quad(:,1) = (/ ptr0%tag_locl,ptr1%tag_locl,ptr2%tag_locl, &
                                 ptr4%tag_locl,ptr5%tag_locl,ptr3%tag_locl /)

         indx_crn_quad(:,2) = (/ ptr0%tag_locl,ptr2%tag_locl,ptr3%tag_locl, &
                                 ptr1%tag_locl,ptr6%tag_locl,ptr7%tag_locl /)

         interp_1D(n_locl)%indx_crn_quad = indx_crn_quad
      ENDIF
      ptr0 => ptr0%next_real%p
   ENDDO

   END SUBROUTINE set_indx_crn_quad
!=======================================================================
!  END  set_indx_crn_quad
!=======================================================================

!=======================================================================
!  BEGIN set_wght_crn_quad
!=======================================================================
   SUBROUTINE set_wght_crn_quad () 
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_found
   INTEGER (KIND=int_kind) :: &
      n,n_locl
   REAL (KIND=dbl_kind) :: &
      point_intrp(3,6)
   TYPE (grid_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ptr => path_real(level_max)%p

   DO n = 1,nm_real

      IF ((.NOT.ptr%l_pole_north).AND.(.NOT.ptr%l_pole_south)) THEN

         n_locl = ptr%tag_locl

         point_intrp = point_1D(:,interp_1D(n_locl)%indx_crn_quad(:,1))

         interp_1D(n_locl)%wght_crn_quad(:,1) = &
                       set_wght_crn_quad_1 (corner_1D(:,1,n_locl),point_intrp)

         point_intrp = point_1D(:,interp_1D(n_locl)%indx_crn_quad(:,2))

         interp_1D(n_locl)%wght_crn_quad(:,2) = &
                       set_wght_crn_quad_1 (corner_1D(:,2,n_locl),point_intrp)

      ENDIF 
      ptr => ptr%next_real%p
   ENDDO ! nm_real loop

   END SUBROUTINE set_wght_crn_quad
!=======================================================================
!  END  set_wght_crn_quad
!=======================================================================

!=======================================================================
! BEGIN set_wght_crn_quad_1
!=======================================================================
   FUNCTION set_wght_crn_quad_1 (p0,p1) RESULT (wght)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p0(3),p1(3,6)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      wght(6)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
   REAL (KIND=dbl_kind) :: &
      mtrx1(3,3),mtrx2(3,3),tmpry3D(3),p2D(2,6),mtrx3(6,6),mtrx4(6,6)

   TYPE (grid_node),POINTER :: &
      ptr1,ptr2
!:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   mtrx1(:,1) = p0
   mtrx1(:,2) = unit_vector (tangent_to_sphere (p0,p1(:,1)))
   mtrx1(:,3) = unit_vector (cross_product (p0,mtrx1(:,2)))

   mtrx2 = matrix_inverse (mtrx1)

   DO n = 1,6
      tmpry3D  = MATMUL (mtrx2,p1(:,n))
      p2D(:,n) = tmpry3D(2:3)
   ENDDO

   DO n = 1,6
      mtrx3(:,n)  = (/ one,p2D(1,n),p2D(2,n),p2D(1,n)**two,p2D(1,n)*p2D(2,n),p2D(2,n)**two /)
   ENDDO

   CALL set_mtrx_inv (i6i,mtrx3,mtrx4)

   wght = mtrx4(:,1)

   END FUNCTION set_wght_crn_quad_1
!=======================================================================
!  END set_wght_crn_quad_1
!=======================================================================

!=======================================================================
!  BEGIN  interp_crn_quad
!=======================================================================
   SUBROUTINE interp_crn_quad (wrap_name,km0,km1,x,x_crn)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      wrap_name
   INTEGER (KIND=int_kind) :: &
      km0,km1
   REAL (KIND=dbl_kind),DIMENSION(     im,jm,km0:km1,nsdm) :: &
      x
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(crnm,im,jm,km0:km1,nsdm) :: &
      x_crn
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n,n_locl,k
   REAL (KIND=dbl_kind) :: &
      wght(6)
   REAL (KIND=dbl_kind),ALLOCATABLE :: &
      x_1D(:,:)
   TYPE (grid_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   ALLOCATE (x_1D(nm,km0:km1))

   ptr => path_real(level_max)%p
   DO n = 1,nm_real
      x_1D(ptr%tag_locl,:) = x(ptr%ix_2D(1),ptr%ix_2D(2),:,ptr%ix_2D(3))
      ptr => ptr%next_real%p
   ENDDO

   CALL wrp1D (wrap_name,face=x_1D)

   ptr => path_real(level_max)%p

   DO n = 1,nm_real

      IF ((.NOT.ptr%l_pole_north).AND.(.NOT.ptr%l_pole_south)) THEN

         n_locl = ptr%tag_locl

         wght = interp_1D(n_locl)%wght_crn_quad(:,1)

         DO k = km0,km1
            x_crn(1,ptr%ix_2D(1),ptr%ix_2D(2),k,ptr%ix_2D(3)) = &
               DOT_PRODUCT (wght,x_1D(interp_1D(n_locl)%indx_crn_quad(:,1),k))
         ENDDO

         wght = interp_1D(n_locl)%wght_crn_quad(:,2)

         DO k = km0,km1
            x_crn(2,ptr%ix_2D(1),ptr%ix_2D(2),k,ptr%ix_2D(3)) = &
               DOT_PRODUCT (wght,x_1D(interp_1D(n_locl)%indx_crn_quad(:,2),k))
         ENDDO

      ENDIF 
      ptr => ptr%next_real%p
   ENDDO ! nm_real loop

   CALL wrap (wrap_name,vrtx_scalar=x_crn)

   DEALLOCATE (x_1D)

   END SUBROUTINE interp_crn_quad
!=======================================================================
!  END  interp_crn_quad
!=======================================================================

   END MODULE utilities_interp
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
