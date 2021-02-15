   MODULE utilities_linear_algebra

   USE kinds
   USE numbers

   IMPLICIT NONE
   SAVE

   PRIVATE 

   PUBLIC :: &
      matrix_inverse,set_mtrx_inv

   CONTAINS
!=======================================================================
! BEGIN matrix_inverse
!=======================================================================
   FUNCTION matrix_inverse (aa) RESULT (aa_inv)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! PURPOSE : given a 3X3 matrix find it inverse 
!           see http://mathworld.wolfram.com/MatrixInverse.html
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      aa(3,3)
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      aa_inv(3,3)
!.......................................................................
!  LOCAL
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      det
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   det = aa(1,1)*(aa(2,2)*aa(3,3)-aa(2,3)*aa(3,2))- &
         aa(1,2)*(aa(2,1)*aa(3,3)-aa(2,3)*aa(3,1))+ &
         aa(1,3)*(aa(2,1)*aa(3,2)-aa(2,2)*aa(3,1))

   IF (ABS (det) < 1.0E-33_dbl_kind) THEN
      PRINT *," utilities_linear_algebra::matrix_inverse::MATRIX LOOKS BAD "
      STOP
   ENDIF

   aa_inv(1,1) = (aa(2,2)*aa(3,3)-aa(2,3)*aa(3,2))/det
   aa_inv(1,2) = (aa(1,3)*aa(3,2)-aa(1,2)*aa(3,3))/det
   aa_inv(1,3) = (aa(1,2)*aa(2,3)-aa(1,3)*aa(2,2))/det

   aa_inv(2,1) = (aa(2,3)*aa(3,1)-aa(2,1)*aa(3,3))/det
   aa_inv(2,2) = (aa(1,1)*aa(3,3)-aa(1,3)*aa(3,1))/det
   aa_inv(2,3) = (aa(1,3)*aa(2,1)-aa(1,1)*aa(2,3))/det

   aa_inv(3,1) = (aa(2,1)*aa(3,2)-aa(2,2)*aa(3,1))/det
   aa_inv(3,2) = (aa(1,2)*aa(3,1)-aa(1,1)*aa(3,2))/det
   aa_inv(3,3) = (aa(1,1)*aa(2,2)-aa(1,2)*aa(2,1))/det

   END FUNCTION matrix_inverse
!======================================================================
!  END matrix_inverse
!======================================================================

!=======================================================================
!  BEGIN set_mtrx_inv
!=======================================================================
   SUBROUTINE set_mtrx_inv (n,mtrx,mtrx_inv)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind),            INTENT ( IN) :: &
      n
   REAL (KIND=dbl_kind),DIMENSION (n,n),INTENT( IN) :: &
      mtrx
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind),DIMENSION (n,n),INTENT(OUT) :: &
      mtrx_inv
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      i,j,k
   INTEGER (KIND=int_kind),DIMENSION (  n) :: &
      indx
   REAL (KIND=dbl_kind),DIMENSION    (n,n) :: &
      mtrx_tmpry1,mtrx_tmpry2
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   mtrx_tmpry1 = mtrx

   mtrx_tmpry2 = zero
   DO i = 1,n
      mtrx_tmpry2(i,i) = one
   ENDDO

  CALL set_pivot_index (n,mtrx_tmpry1,indx)

   DO i = 1,n-1
      DO j = i+1,n
         DO k = 1,n
            mtrx_tmpry2(indx(j),k) = mtrx_tmpry2(indx(j),k) - &
                                     mtrx_tmpry1(indx(j),i)*mtrx_tmpry2(indx(i),k)
         ENDDO
      ENDDO
   ENDDO

   DO i = 1,n
      mtrx_inv(n,i) = mtrx_tmpry2(indx(n),i)/mtrx_tmpry1(indx(n),n)
      DO j = n-1,1,-1
         mtrx_inv(j,i) = mtrx_tmpry2(indx(j),i)
         DO k = j+1,n
            mtrx_inv(j,i) = mtrx_inv(j,i)-mtrx_tmpry1(indx(j),k)*mtrx_inv(k,i)
         ENDDO
         mtrx_inv(j,i) =  mtrx_inv(j,i)/mtrx_tmpry1(indx(j),j)
      ENDDO
   ENDDO

   END SUBROUTINE set_mtrx_inv
!=======================================================================
!  END set_mtrx_inv
!=======================================================================

!=======================================================================
!  BEGIN set_pivot_index
!=======================================================================
   SUBROUTINE set_pivot_index (n,mtrx,indx)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind),INTENT (IN   ) :: &
      n
   REAL (KIND=dbl_kind),DIMENSION(n,n) :: &
      mtrx
!.......................................................................
!  INTENT OUT
!.......................................................................
   INTEGER (KIND=int_kind), DIMENSION (n),INTENT (  OUT) :: &
      indx
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      i,j,k,itmpry
   REAL (KIND=dbl_kind) :: &
      cc1,pip,pip1,pj
   REAL (kind=dbl_kind),DIMENSION (n) :: &
      cc
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   DO i = 1,n
      indx(i) = i
   ENDDO

   DO i = 1,n
      cc1 = zero
      DO j = 1,n
         cc1 = MAX (cc1,ABS (mtrx(i,j)))
      ENDDO
      cc(i) = cc1
   ENDDO

   DO j = 1,n-1
      pip1 = zero
      DO i = j,n
         pip = ABS (mtrx(indx(i),j))/cc(indx(i))
         IF (pip > pip1) THEN
            pip1 = pip
            k    = i
         ENDIF
      ENDDO

      itmpry = indx(j)
      indx(j) = indx(k)
      indx(k) = itmpry
      DO i = j+1,n
         pj  = mtrx(indx(i),j)/mtrx(indx(j),j)

         mtrx(indx(i),j) = pj

         DO k = j+1,n
            mtrx(indx(i),k) = mtrx(indx(i),k)-pj*mtrx(indx(j),k)
         ENDDO
      ENDDO
   ENDDO

   END SUBROUTINE set_pivot_index
!=======================================================================
!  END set_pivot_index
!=======================================================================

   END MODULE utilities_linear_algebra
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
