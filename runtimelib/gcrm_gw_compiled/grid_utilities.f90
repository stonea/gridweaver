   MODULE grid_utilities

   USE kinds

   IMPLICIT NONE
   SAVE

   PRIVATE 

   PUBLIC :: &
      mid_point,tangent_to_sphere,voronoi_corner,cross_product, &
      unit_vector,xyz_to_lonlat,lonlat_to_xyz,spherical_triangle_area, &
      area_corner_kites,arch_distance,l_polygon_bounding,linear_weights, &
      linear_distance,l_zero_vector,set_vctr_wghts_crn,local_east,local_north, &
      prjct

   CONTAINS
!=======================================================================
!  BEGIN FUNCTION mid_point
!=======================================================================
   FUNCTION mid_point (p1,p2) RESULT (p_out)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given two points (p1,p2) measured in (x,y,z) and lying on
!           the unit sphere, find the mid-point of the arch between p1 
!           and p2.
!
! NOTE : p1 and p2 are assumed to be of unit length
! NOTE : p_out is normalized to unit length
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p1(3),p2(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: & 
      p_out(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   p_out(:) = unit_vector (0.5_dbl_kind*(p1(:)+p2(:)))

   END FUNCTION mid_point
!=======================================================================
!  END FUNCTION mid_point
!=======================================================================

!=======================================================================
!  BEGIN FUNCTION tangent_to_sphere
!=======================================================================
   FUNCTION tangent_to_sphere (p1,p2) RESULT (p_out)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given two points (p1,p2) measured in (x,y,z) and lying on
!           the unit sphere, find the vector (p_out) that lies 
!           in the plane tangent to the unit sphere at the point p1 
!           and points in the direction of the projection of p2 onto 
!           the tangent plane.
!
! NOTE : p1 and p2 are assumed to be of unit length
! NOTE : p_out is normalized to unit length
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p1(3),p2(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: & 
      p_out(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   p_out(:) = unit_vector (cross_product (cross_product (p1(:),p2(:)),p1(:)))

   END FUNCTION tangent_to_sphere
!=======================================================================
!  BEGIN FUNCTION tangent_to_sphere
!=======================================================================

!=======================================================================
!  BEGIN FUNCTION voronoi_corner
!=======================================================================
   FUNCTION voronoi_corner (p1,p2,p3) RESULT (p_out)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given three points (p1,p2,p3) measured in (x,y,z) and lying
!           on the unit sphere, find the point (p_out) that is equidistant
!           from p1, p2 and p3.
!
! NOTE : p1, p2 and p3 are assumed to be given in a counterclockwise fashion
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p1(3),p2(3),p3(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: & 
      p_out(3)
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind) :: & 
      temp1(3),temp2(3),temp3(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   temp1(1) = p2(1) - p1(1)
   temp1(2) = p2(2) - p1(2)
   temp1(3) = p2(3) - p1(3)
   temp2(1) = p3(1) - p1(1)
   temp2(2) = p3(2) - p1(2)
   temp2(3) = p3(3) - p1(3)

   temp3(:) = cross_product (temp1(:),temp2(:))
   p_out(:) = unit_vector (temp3(:))

   END FUNCTION voronoi_corner
!=======================================================================
!  BEGIN FUNCTION voronoi_corner
!=======================================================================

!=======================================================================
!  BEGIN FUNCTION cross_product
!=======================================================================
   FUNCTION cross_product (p1,p2) RESULT (p_out)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given (p1,p2) measured in (x,y,z) and lying on the unit sphere, 
!           compute their cross product.  result returned in p_out
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p1(3),p2(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: & 
      p_out(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   p_out(1) = p1(2)*p2(3)-p1(3)*p2(2)
   p_out(2) = p1(3)*p2(1)-p1(1)*p2(3)
   p_out(3) = p1(1)*p2(2)-p1(2)*p2(1)

   END FUNCTION cross_product
!=======================================================================
!  END FUNCTION cross_product
!=======================================================================

!=======================================================================
!  BEGIN FUNCTION unit_vector
!=======================================================================
   FUNCTION unit_vector (p) RESULT (p_out)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: & 
      p_out(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   p_out(:) = p(:)/SQRT (p(1)*p(1)+p(2)*p(2)+p(3)*p(3))

   END FUNCTION unit_vector
!=======================================================================
!  END FUNCTION unit_vector
!=======================================================================

!=======================================================================
!  BEGIN FUNCTION xyz_to_lonlat
!=======================================================================
   FUNCTION xyz_to_lonlat (p) RESULT (lonlat)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given a point measured in (x,y,z), find the longitude
!           and latitude.
!
! NOTE : the two coordinate systems are related in the following manner:
!          (1, 0, 0) transforms to (0   , 0    )  (greenwich -- equator)
!          (0, 1, 0) transforms to (pi/2, 0    )  (90¼ east  -- equator)
!          (0, 0, 1) transforms to (0   , pi/2 )  (north pole)
!
! NOTE : p is assumed to be of unit length
! NOTE : lonlat is measured in radians with
!           lonlat(1) ranging from -pi   to pi   (longitude)
!           lonlat(2) ranging from -pi/2 to pi/2 (latitude )
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      lonlat(2)
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind),PARAMETER :: &
      zero = 0.0_dbl_kind
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF ((p(1) /= zero).OR.(p(2) /= zero)) THEN
      lonlat(1) = ATAN2 (p(2),p(1))
   ELSE
      lonlat(1) = zero
   ENDIF

   lonlat(2) = ASIN (p(3))

   END FUNCTION xyz_to_lonlat
!=======================================================================
!  END FUNCTION xyz_to_lonlat
!=======================================================================

!=======================================================================
!  BEGIN FUNCTION lonlat_to_xyz
!=======================================================================
   FUNCTION lonlat_to_xyz (lonlat) RESULT (p)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given a point measured in longitude and latitude, 
!           find the Cartesian coordinates p = (x,y,z).
!
! NOTE : x,y,z are in the range [-1,1].
!
! NOTE : the two coordinate systems are related in the following manner:
!          (0   , 0    )  (greenwich -- equator) transforms to (1, 0, 0)
!          (pi/2, 0    )  (90¼ east  -- equator) transforms to (0, 1, 0) 
!          (0   , pi/2 )  (north pole)           transforms to (0, 0, 1)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      lonlat(2)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   p(:) = (/ COS (lonlat(1))*COS (lonlat(2)), &
             SIN (lonlat(1))*COS (lonlat(2)), &
                             SIN (lonlat(2)) /)

   END FUNCTION lonlat_to_xyz
!=======================================================================
!  END FUNCTION unit_vector
!=======================================================================

!=======================================================================
! BEGIN spherical_triangle_area
!=======================================================================
   FUNCTION spherical_triangle_area (p1,p2,p3) RESULT (area)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given three points measured in (x,y,z) lying on the unit
!           sphere, find the spherical area of the triangle formed by
!           connecting these three points
!
! NOTE : p1, p2, p3 are assumed to be of unit length
! NOTE : p_out has units of radians^2
!
! NOTE : see Mathematical CRC Tables for formula
!        area = angle1 + angle2 + angle3 - pi
!        where angle1, angle2 are the vertex angles
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p1(3),p2(3),p3(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      area
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      t1(3),t2(3),dot,angle1,angle2,angle3
   REAL (KIND=dbl_kind),PARAMETER :: &
      one_p =  1.0_dbl_kind, &
      one_m = -1.0_dbl_kind, &
      pi = 3.14159265358979323846264338327950288419716939937510_dbl_kind
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   t1(:) = tangent_to_sphere (p1,p2)
   t2(:) = tangent_to_sphere (p1,p3)
   dot   = MAX (one_m,MIN (one_p,t1(1)*t2(1)+t1(2)*t2(2)+t1(3)*t2(3)))
   angle1 = ACOS (dot)

   t1(:) = tangent_to_sphere (p2,p3)
   t2(:) = tangent_to_sphere (p2,p1)
   dot   = MAX (one_m,MIN (one_p,t1(1)*t2(1)+t1(2)*t2(2)+t1(3)*t2(3)))
   angle2 = ACOS (dot)

   t1(:) = tangent_to_sphere (p3,p1)
   t2(:) = tangent_to_sphere (p3,p2)
   dot   = MAX (one_m,MIN (one_p,t1(1)*t2(1)+t1(2)*t2(2)+t1(3)*t2(3)))
   angle3 = ACOS (dot)

   area = angle1 + angle2 + angle3 - pi

   END FUNCTION spherical_triangle_area
!=======================================================================
!  END spherical_triangle_area
!=======================================================================

!=======================================================================
!  BEGIN FUNCTION area_corner_kites
!=======================================================================
   FUNCTION area_corner_kites (p1,p2,p3) RESULT (area_c)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given three points p1, p2 and p3 measured in (x,y,z) lying 
!           on the unit sphere, find the spherical area formed by
!           connecting these three points.  the total area is returned
!           in three kites. 
!           see /Users/ross/swm/figures/archive/corner_area.eps
!
! NOTE : p1, p2, p3 are assumed to be of unit length
! NOTE : p1, p2, p3 are assumed to be given in a counterclockwise fashion
! NOTE : p_out has units of radians^2
!
! NOTE : see Mathematical CRC Tables for formula
!        area = angle1 + angle2 + angle3 - pi
!        where angle1, angle2 are the vertex angles
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p1(3),p2(3),p3(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      area_c(3)
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      mid12(3),mid23(3),mid31(3),mid123(3)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   mid12 = mid_point (p1,p2)
   mid23 = mid_point (p2,p3)
   mid31 = mid_point (p3,p1)

   mid123 = voronoi_corner (p1(:),p2(:),p3(:))

   area_c(1) = spherical_triangle_area (    p1(:),mid12(:),mid31(:)) + &
               spherical_triangle_area (mid123(:),mid31(:),mid12(:))
   area_c(2) = spherical_triangle_area (    p2(:),mid23(:),mid12(:)) + &
               spherical_triangle_area (mid123(:),mid12(:),mid23(:))
   area_c(3) = spherical_triangle_area (    p3(:),mid31(:),mid23(:)) + &
               spherical_triangle_area (mid123(:),mid23(:),mid31(:))

   END FUNCTION area_corner_kites
!=======================================================================
!  END FUNCTION area_corner_kites
!=======================================================================

!=======================================================================
! BEGIN linear_distance
!=======================================================================
   FUNCTION linear_distance (p1,p2) RESULT (distance)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given two points p1 and p2 measured in (x,y,z).  
!           return the the linear distance between p1 and p2.
!!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (kind=dbl_kind) :: &
      p1(3),p2(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (kind=dbl_kind) :: &
      distance
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   distance = SQRT (DOT_PRODUCT (p1(:)-p2(:),p1(:)-p2(:)))

   END FUNCTION linear_distance
!======================================================================
!  END linear_distance
!======================================================================

!=======================================================================
! BEGIN arch_distance
!=======================================================================
   FUNCTION arch_distance (p1,p2) RESULT (distance)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given two points measured in (x,y,z) lying on the unit
!           sphere, find the distance measured along the surface of the
!           sphere between the two points.
!!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (kind=dbl_kind) :: &
      p1(3),p2(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (kind=dbl_kind) :: &
      distance
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (kind=dbl_kind) :: &
      dot
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   dot = p1(1)*p2(1)+p1(2)*p2(2)+p1(3)*p2(3)

   IF (dot >= 1.0_dbl_kind) dot =  1.0_dbl_kind
   IF (dot <=-1.0_dbl_kind) dot = -1.0_dbl_kind

   distance = ACOS (dot)

   END FUNCTION arch_distance
!======================================================================
!  END arch_distance
!======================================================================

!=======================================================================
! BEGIN l_polygon_bounding
!=======================================================================
   FUNCTION l_polygon_bounding (p,p_list,epsilon) RESULT (l_bound)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : returns true if the point p (measured in (x,y,z)) is 
!           exclusively to the left (or right) of the plane formed by 
!           {(0,0,0),p_list(:,i),p_list(:,i+1)} for i = 1,2,...,
!           Length[p_list]. epsilon allows for a little wiggle room.
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p(3),p_list(:,:)
   REAL (KIND=dbl_kind),OPTIONAL :: &
      epsilon
!.......................................................................
!  INTENT OUT
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_bound
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      list_length,n
   REAL (KIND=dbl_kind),ALLOCATABLE :: &
      p_dist(:),dot_prod(:)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   list_length = SIZE (p_list,DIM=2)

   ALLOCATE (dot_prod(list_length))
   DO n = 1,list_length
      dot_prod(n) = DOT_PRODUCT (p(:), &
            cross_product (p_list(:,n),p_list(:,MOD (n,list_length)+1)))
   ENDDO

   IF (PRESENT (epsilon)) THEN
      IF ((COUNT (dot_prod(:) >=-epsilon) == list_length).OR. &
          (COUNT (dot_prod(:) <= epsilon) == list_length)) THEN
         l_bound = .TRUE.
      ELSE
         l_bound = .FALSE.
      ENDIF
   ELSE
      IF ((COUNT (dot_prod(:) >= 0.0_dbl_kind) == list_length).OR. &
          (COUNT (dot_prod(:) <= 0.0_dbl_kind) == list_length)) THEN
        l_bound = .TRUE.
      ELSE
         l_bound = .FALSE.
      ENDIF
   ENDIF

! exclude the case where p(:) in on the opposite side of the sphere
   ALLOCATE (p_dist(list_length))
   DO n = 1,list_length
      p_dist(n) = linear_distance (p(:),p_list(:,n))
   ENDDO
   IF (MAXVAL (p_dist(:)) > 1.0_dbl_kind) l_bound = .FALSE.

   DEALLOCATE (p_dist,dot_prod)

   END FUNCTION l_polygon_bounding
!======================================================================
!  END l_polygon_bounding
!======================================================================

!=======================================================================
! BEGIN linear_weights
!=======================================================================
   FUNCTION linear_weights (p0,p1,p2,p3,epsilon) RESULT (weights)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : assume points p0, p1, p2 and p3 (measured in (x,y,z)) are on
!           the unit sphere.  p0 lies within the spherical triangle formed
!           by p1, p2 and p3 traversing the perimeter in a counter-clockwise 
!           fashion.  then weights are the weights to linearly interpolate 
!           a scalar from p1, p2 and p3 to the point p0.  epsilon allows 
!           for a little wiggle room.
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p0(3),p1(3),p2(3),p3(3)
   REAL (KIND=dbl_kind),OPTIONAL :: &
      epsilon
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      weights(3)
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p_tmpry(3),dot,d1,d2,d3,area_total
   REAL (KIND=dbl_kind),PARAMETER :: &
      zero  = 0.0_dbl_kind, &! zero
      one   = 1.0_dbl_kind   ! one
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (PRESENT (epsilon)) THEN
      IF (epsilon < zero) THEN
         d1 = arch_distance (p0,p1)
         d2 = arch_distance (p0,p2)
         d3 = arch_distance (p0,p3)

         IF ((d1 <= d2).AND.(d1 <= d3)) weights(:) = (/ one,zero,zero /)
         IF ((d2 <= d1).AND.(d2 <= d3)) weights(:) = (/ zero,one,zero /)
         IF ((d3 <= d1).AND.(d3 <= d2)) weights(:) = (/ zero,zero,one /)
         RETURN
      ENDIF
!-----------------------------------------------------------------------
!  p0 is close to a vertex
!-----------------------------------------------------------------------
      IF (arch_distance (p0,p1) <= epsilon) THEN
         weights(:) = (/ one,zero,zero /)
         RETURN
      ENDIF
      IF (arch_distance (p0,p2) <= epsilon) THEN
         weights(:) = (/ zero,one,zero /)
         RETURN
      ENDIF
      IF (arch_distance (p0,p3) <= epsilon) THEN
         weights(:) = (/ zero,zero,one /)
         RETURN
      ENDIF
!-----------------------------------------------------------------------
!  p0 is close to an edge
!-----------------------------------------------------------------------
      p_tmpry(:) = unit_vector (cross_product (p1(:),p2(:)))
      dot = p0(1)*p_tmpry(1) + p0(2)*p_tmpry(2) + p0(3)*p_tmpry(3)
      IF (ABS (dot) <= epsilon) THEN
         d1 = arch_distance (p0(:),p1(:)); d2 = arch_distance (p0(:),p2(:));
         weights(:) = (/ d2/(d1+d2),d1/(d1+d2),zero /)
         RETURN
      ENDIF
      p_tmpry(:) = unit_vector (cross_product (p2(:),p3(:)))
      dot = p0(1)*p_tmpry(1) + p0(2)*p_tmpry(2) + p0(3)*p_tmpry(3)
      IF (ABS (dot) <= epsilon) THEN
         d1 = arch_distance (p0(:),p2(:)); d2 = arch_distance (p0(:),p3(:));
         weights(:) = (/ zero,d2/(d1+d2),d1/(d1+d2) /)
         RETURN
      ENDIF
      p_tmpry(:) = unit_vector (cross_product (p3(:),p1(:)))
      dot = p0(1)*p_tmpry(1) + p0(2)*p_tmpry(2) + p0(3)*p_tmpry(3)
      IF (ABS (dot) <= epsilon) THEN
         d1 = arch_distance (p0(:),p3(:)); d2 = arch_distance (p0(:),p1(:));
         weights(:) = (/ d1/(d1+d2),zero,d2/(d1+d2) /)
         RETURN
      ENDIF
   ENDIF

   area_total = spherical_triangle_area (p1(:),p2(:),p3(:))

   weights(:) = &
            (/ spherical_triangle_area (p0(:),p2(:),p3(:))/area_total, &
               spherical_triangle_area (p0(:),p3(:),p1(:))/area_total, &
               spherical_triangle_area (p0(:),p1(:),p2(:))/area_total /)

   END FUNCTION linear_weights
!======================================================================
!  END linear_weights
!======================================================================

!=======================================================================
! BEGIN l_zero_vector
!=======================================================================
   FUNCTION l_zero_vector (p,epsilon) RESULT (l_zero)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : returns true iff the point p (measured in (x,y,z)) is 
!           zero vector.  epsilon allows for a little wiggle room.
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p(3)
   REAL (KIND=dbl_kind),OPTIONAL :: &
      epsilon
!.......................................................................
!  INTENT OUT
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
      l_zero
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (PRESENT (epsilon)) THEN
      l_zero = DOT_PRODUCT (p(:),p(:)) <= epsilon
   ELSE
      l_zero = DOT_PRODUCT (p(:),p(:)) == 0.0_dbl_kind
   ENDIF

   END FUNCTION l_zero_vector
!======================================================================
!  END l_zero_vector
!======================================================================

!=======================================================================
! BEGIN set_vctr_wghts_crn
!=======================================================================
   FUNCTION set_vctr_wghts_crn (p1,p2,p3) RESULT (weights)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : assume points p1, p2 and p3 (measured in (x,y,z)) are on
!           the unit sphere.  compute the weights associated with the 
!           gradient operator defined at cell corners.  
!           see Ringler and Randall, A Potential Enstrophy and Energy 
!           Conserving Numerical Scheme for Solution of the Shallow-Water 
!           Equations on a Geodesic Grid, Mon. Wea. Rev. 130, 1397-1410, 
!           equation (40) and (41)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p1(3),p2(3),p3(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      weights(3,2)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
   REAL (KIND=dbl_kind),DIMENSION(3) :: &
      north_pole,local_vert,crn,north,east,mid,tng,lngth
   REAL (KIND=dbl_kind),DIMENSION(3,3) :: &
      trans_mat,p,nrm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   north_pole(:) = (/ 0.0_dbl_kind,0.0_dbl_kind,1.0_dbl_kind /)
   local_vert(:) = (/ 0.0_dbl_kind,0.0_dbl_kind,1.0_dbl_kind /)

   crn  (:) = voronoi_corner (p1(:),p2(:),p3(:))
   north(:) = tangent_to_sphere (crn(:),north_pole(:))
   east (:) = unit_vector (cross_product (north(:),crn(:)))

   trans_mat(1,:)=east(:); trans_mat(2,:)=north(:); trans_mat(3,:)=crn(:);

   p(:,1) = p1(:); p(:,2) = p2(:); p(:,3) = p3(:);

   DO n = 1,3
      mid(:  ) = mid_point (p(:,1),p(:,2))
      tng(:  ) = -tangent_to_sphere (crn(:),mid(:))
      tng(:  ) = unit_vector (MATMUL (trans_mat(:,:),tng(:)))

      nrm(:,n) = unit_vector (cross_product (tng(:),local_vert(:)))
      lngth(n) = arch_distance (crn(:),mid(:))

      p(:,:) = CSHIFT (p(:,:),SHIFT=1,DIM=2)
   ENDDO

   DO n = 1,3
      weights(n,1:2) = lngth(3)*nrm(1:2,3)+lngth(1)*(-nrm(1:2,1))

      lngth(:) = CSHIFT (lngth(:),SHIFT=1,DIM=1)
      nrm(:,:) = CSHIFT (nrm(:,:),SHIFT=1,DIM=2)
   ENDDO

   END FUNCTION set_vctr_wghts_crn
!======================================================================
!  END set_vctr_wghts_crn
!======================================================================

!=======================================================================
! BEGIN local_east
!=======================================================================
   FUNCTION local_east (p) RESULT (east)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : assume point p (measured in (x,y,z)) is on the unit sphere.  
!           compute the unit vector tangent to the sphere at the point p
!           pointing in the direction of the local east.
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      east(3)
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(3) :: &
      north_pole,nrth
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   north_pole(:) = (/ 0.0_dbl_kind,0.0_dbl_kind,1.0_dbl_kind /)

   nrth = tangent_to_sphere (p,north_pole)
   east = unit_vector (cross_product (nrth,p))

   END FUNCTION local_east
!======================================================================
!  END local_east
!======================================================================

!=======================================================================
! BEGIN local_north
!=======================================================================
   FUNCTION local_north (p) RESULT (nrth)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : assume point p (measured in (x,y,z)) is on the unit sphere.  
!           compute the unit vector tangent to the sphere at the point p
!           pointing in the direction of the local north.
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      p(3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      nrth(3)
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(3) :: &
      north_pole
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   north_pole(:) = (/ 0.0_dbl_kind,0.0_dbl_kind,1.0_dbl_kind /)

   nrth = tangent_to_sphere (p,north_pole)

   END FUNCTION local_north
!======================================================================
!  END local_north
!======================================================================

!=======================================================================
! BEGIN prjct
!=======================================================================
   FUNCTION prjct (x0,x1) RESULT (tmpry)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION(3) :: &
      x0,x1
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      tmpry
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      mgntd
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   mgntd = SQRT (DOT_PRODUCT (x0,x0))

   IF (mgntd >= 1.0E-32_dbl_kind) THEN
      tmpry = mgntd*DOT_PRODUCT (unit_vector (x0),unit_vector (x1))
   ELSE
      tmpry = 0.0_dbl_kind
   ENDIF

   END FUNCTION prjct
!=======================================================================
!  END prjct
!=======================================================================

   END MODULE grid_utilities
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
