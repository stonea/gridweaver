   MODULE utilities_netCDF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Purpose: 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds

   IMPLICIT NONE

   INCLUDE 'netcdf.inc'

   SAVE

   INTEGER (KIND=int_kind),PARAMETER :: & ! the maximum rank of an array
      rank_max = 7                        ! in fortran

   TYPE netCDF_node
      CHARACTER (LEN=128) :: &
         field_strng,path_strng,file_strng
      LOGICAL (KIND=log_kind) :: &
         l_file_open
      INTEGER (KIND=int_kind) :: &
         rank,index_max(rank_max+1),start_unlimited
      CHARACTER (LEN=128) :: &
         index_strng(rank_max+1),unit_strng(rank_max+1)
      INTEGER (KIND=int_kind) :: &
         file_ncid,index_ncid(rank_max+1),field_ncid
      TYPE (netCDF_node),POINTER :: &
         next
   END TYPE netCDF_node

   TYPE (netCDF_node),POINTER :: &
      netCDF_head

   LOGICAL (KIND=log_kind) :: &
      l_allocate_netCDF_head = .TRUE.

   CONTAINS
!======================================================================
!  BEGIN write_netCDF
!======================================================================
   SUBROUTINE write_netCDF (field,path,proc,nsdm_glbl,sbdmn_lst, &
                               index_max,index_strng,unit_strng, &
                               rk0,rk1,rk2,rk3,rk4,rk5,rk6,rk7)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*),INTENT(IN) :: &
      field,path
   INTEGER (KIND=int_kind),OPTIONAL :: &
      proc,nsdm_glbl,sbdmn_lst(:),index_max(:)
   CHARACTER (LEN=128),INTENT(IN),OPTIONAL :: &
      index_strng(:),unit_strng(:)
   REAL (KIND=dbl_kind),INTENT(IN),OPTIONAL :: &
      rk0,rk1(:),rk2(:,:),rk3(:,:,:),rk4(:,:,:,:),rk5(:,:,:,:,:), &
      rk6(:,:,:,:,:,:),rk7(:,:,:,:,:,:,:)
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL :: &
      l_add_new_node,l_array_passed
   INTEGER (KIND=int_kind) :: &
      n,nsdm,rank_tmpry,index_tmpry(rank_max+1), &
      start(rank_max+1),count(rank_max+1),status
   CHARACTER (LEN=  1) :: &
      none_strng(8)
   CHARACTER (LEN=  6) :: &
      proc_strng
   CHARACTER (LEN=23) :: &
      time_strng
   TYPE (netCDF_node),POINTER :: &
      ptr
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!-----------------------------------------------------------------------
!  determine if an array is passed to subroutine.  if so, determine its
!  rank and shape.
!-----------------------------------------------------------------------
      l_array_passed=.FALSE.; rank_tmpry=-1; index_tmpry(:)=-1;
      IF (PRESENT (rk0)) THEN
         l_array_passed=.TRUE.; rank_tmpry=0;
      ENDIF
      IF (PRESENT (rk1)) THEN
         l_array_passed=.TRUE.; rank_tmpry=1; index_tmpry(1:1)=SHAPE (rk1);
      ENDIF
      IF (PRESENT (rk2)) THEN
         l_array_passed=.TRUE.; rank_tmpry=2; index_tmpry(1:2)=SHAPE (rk2);
      ENDIF
      IF (PRESENT (rk3)) THEN
         l_array_passed=.TRUE.; rank_tmpry=3; index_tmpry(1:3)=SHAPE (rk3);
      ENDIF
      IF (PRESENT (rk4)) THEN
         l_array_passed=.TRUE.; rank_tmpry=4; index_tmpry(1:4)=SHAPE (rk4);
      ENDIF
      IF (PRESENT (rk5)) THEN
         l_array_passed=.TRUE.; rank_tmpry=5; index_tmpry(1:5)=SHAPE (rk5);
      ENDIF
      IF (PRESENT (rk6)) THEN
         l_array_passed=.TRUE.; rank_tmpry=6; index_tmpry(1:6)=SHAPE (rk6);
      ENDIF
      IF (PRESENT (rk7)) THEN
         l_array_passed=.TRUE.; rank_tmpry=7; index_tmpry(1:7)=SHAPE (rk7);
      ENDIF
!-----------------------------------------------------------------------
!  the first time write_netCDF is called allocate the head node
!-----------------------------------------------------------------------
   l_add_new_node = .FALSE.
   IF (l_allocate_netCDF_head) THEN
      l_allocate_netCDF_head = .FALSE.
      l_add_new_node = .TRUE.
      ALLOCATE (netCDF_head); ptr => netCDF_head
      NULLIFY (ptr%next)
   ELSE
!-----------------------------------------------------------------------
!  look through the list to find a pre-existing field
!-----------------------------------------------------------------------
      ptr => netCDF_head
      DO 
         IF (TRIM (ptr%field_strng)==TRIM (field)) EXIT
         IF (.NOT.ASSOCIATED (ptr%next)) THEN
            l_add_new_node = .TRUE.
            EXIT
         ENDIF
         ptr => ptr%next
      ENDDO
   ENDIF
!-----------------------------------------------------------------------
!  if pre-existing field not found add a new node to the end of list
!-----------------------------------------------------------------------
   IF (l_add_new_node) THEN
      PRINT *," write_netCDF :: add new node :: field = ",TRIM (field)
      ALLOCATE (ptr%next)
      ptr => ptr%next
      NULLIFY (ptr%next)
   ENDIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  create file and set attributes of a new node
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (l_add_new_node) THEN
!-----------------------------------------------------------------------
!  set the process number string
!-----------------------------------------------------------------------
      proc_strng = "      "
      IF (PRESENT (proc)) THEN
         WRITE (UNIT=proc_strng,FMT="(I06)") proc
         IF (proc_strng(1:1)==" ") proc_strng(1:1) = "_"
         DO n = 2,6; IF (proc_strng(n:n)==" ") proc_strng(n:n) = "0"; ENDDO
      ENDIF
!-----------------------------------------------------------------------
!  create a netCDF file and add timestamp attribute
!-----------------------------------------------------------------------
      ptr%field_strng = TRIM (field)
      ptr%path_strng  = TRIM (path)
      ptr%file_strng  = TRIM (path)//"/"//TRIM (field)//TRIM (proc_strng)//".nc"
      status = NF_CREATE (TRIM (ptr%file_strng),NF_CLOBBER,ptr%file_ncid)
      CALL NF_handle_error (status," write_netCDF :: NF_CREATE")
      time_strng = time_stamp ()
      status = NF_PUT_ATT_TEXT (ptr%file_ncid,NF_GLOBAL, &
                          "time_strng_::_time_of_file_creation",23,time_strng)
      CALL NF_handle_error (status," write_netCDF :: NF_PUT_ATT time_strng")
!-----------------------------------------------------------------------
!  add attributes to describe parallel domain decomposition and the subset of
!  global subdomain blocks owned by the local process
!-----------------------------------------------------------------------
      IF (PRESENT (nsdm_glbl)) THEN
         status = NF_PUT_ATT_INT (ptr%file_ncid,NF_GLOBAL, &
                "nsdm_glbl_::_global_number_of_subdomains",NF_INT,1,nsdm_glbl)
         CALL NF_handle_error (status," write_netCDF :: NF_PUT_ATT nsdm_glbl")
      ENDIF
      IF (PRESENT (sbdmn_lst)) THEN
         nsdm = SIZE (sbdmn_lst(:),DIM=1)
         status = NF_PUT_ATT_INT (ptr%file_ncid,NF_GLOBAL, &
                           "nsdm_::_local_number_of_subdomains",NF_INT,1,nsdm)
         CALL NF_handle_error (status," write_netCDF :: nsdm")
         status = NF_PUT_ATT_INT (ptr%file_ncid,NF_GLOBAL, &
            "sbdmn_lst_::_global_subdomain_list_subset",NF_INT,nsdm,sbdmn_lst)
         CALL NF_handle_error (status," write_netCDF :: NF_PUT_ATT sbdmn_lst")
      ENDIF
!-----------------------------------------------------------------------
!  if index_max is passed to subroutine it determines the rank and shape
!  of the field.  verify that it is consistent with the passed array (if any).  
!  if index_max is not passed use the passed array to determine the rank and 
!  shape of the field.
!-----------------------------------------------------------------------
      IF (PRESENT (index_max)) THEN
         ptr%rank = SIZE (index_max(:),DIM=1)
         ptr%index_max(1:ptr%rank) = index_max(1:ptr%rank)

         IF (l_array_passed) THEN
            IF ((ptr%rank/=rank_tmpry).OR. &
                (ANY (ptr%index_max(1:ptr%rank)/=index_tmpry(1:ptr%rank)))) THEN
               PRINT "(A34,A128)"," netCDF ERROR :: ptr%file_strng = ", &
                                                         TRIM (ptr%file_strng)
               PRINT "(A45)"," index_max and passed array and inconsistent "
               STOP
            ENDIF
         ENDIF
      ELSE 
         IF (l_array_passed) THEN
            ptr%rank         = rank_tmpry
            ptr%index_max(:) = index_tmpry(:)
         ELSE
            PRINT "(A34,A128)"," netCDF ERROR :: ptr%file_strng = ", &
                                                         TRIM (ptr%file_strng)
            PRINT "(A37)"," cannot determine rank and index_max "
         ENDIF
      ENDIF
!-----------------------------------------------------------------------
! determine index_strng
!-----------------------------------------------------------------------
      none_strng = (/ "1","2","3","4","5","6","7","8" /)
      ptr%index_strng(:) = "undefined"
      IF (PRESENT (index_strng)) THEN
         DO n = 1,ptr%rank
            ptr%index_strng(n) = index_strng(n)
         ENDDO
      ELSE
         DO n = 1,ptr%rank
            ptr%index_strng(n) = "none_"//none_strng(n)
         ENDDO
      ENDIF
      ptr%index_strng(ptr%rank+1) = "unlimited"
!-----------------------------------------------------------------------
! define dimensions. the last dimension is defined UNLIMITED
!-----------------------------------------------------------------------
      DO n = 1,ptr%rank
         status = NF_DEF_DIM (ptr%file_ncid,ptr%index_strng(n), &
                                     ptr%index_max(n),ptr%index_ncid(n))
         CALL NF_handle_error (status," write_netCDF :: NF_DEF_DIM loop ")
      ENDDO
      status = NF_DEF_DIM (ptr%file_ncid,ptr%index_strng(ptr%rank+1), &
                                  NF_UNLIMITED,ptr%index_ncid(ptr%rank+1))
      CALL NF_handle_error (status," write_netCDF :: NF_DEF_DIM UNLIMITED ")
!-----------------------------------------------------------------------
! define field
!-----------------------------------------------------------------------
      status = NF_DEF_VAR (ptr%file_ncid,ptr%field_strng,NF_DOUBLE, &
                       ptr%rank+1,ptr%index_ncid(1:ptr%rank+1),ptr%field_ncid)
      CALL NF_handle_error (status," write_netCDF :: NF_DEF_VAR ")
!-----------------------------------------------------------------------
! end definition
!-----------------------------------------------------------------------
      status = NF_ENDDEF (ptr%file_ncid)
      CALL NF_handle_error (status," write_netCDF :: NF_ENDDEF ")

      ptr%start_unlimited = 0

   ENDIF ! (l_add_new_node)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  write field
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   IF (l_array_passed) THEN
      status = NF_OPEN (TRIM (ptr%file_strng),NF_WRITE,ptr%file_ncid)
      CALL NF_handle_error (status," write_netCDF :: NF_OPEN ")

      ptr%start_unlimited = ptr%start_unlimited + 1
      start(1:ptr%rank  ) = 1
      start(  ptr%rank+1) = ptr%start_unlimited

      count(1:ptr%rank  ) = ptr%index_max(1:ptr%rank)
      count(  ptr%rank+1) = 1

      SELECT CASE (ptr%rank)
      CASE (01)
         status = NF_PUT_VARA_DOUBLE (ptr%file_ncid,ptr%field_ncid, &
                                                    start(1:2),count(1:2),rk1)
      CASE (02)
         status = NF_PUT_VARA_DOUBLE (ptr%file_ncid,ptr%field_ncid, &
                                                    start(1:3),count(1:3),rk2)
      CASE (03)
         status = NF_PUT_VARA_DOUBLE (ptr%file_ncid,ptr%field_ncid, &
                                                    start(1:4),count(1:4),rk3)
      CASE (04)
         status = NF_PUT_VARA_DOUBLE (ptr%file_ncid,ptr%field_ncid, &
                                                    start(1:5),count(1:5),rk4)
      CASE (05)
         status = NF_PUT_VARA_DOUBLE (ptr%file_ncid,ptr%field_ncid, &
                                                    start(1:6),count(1:6),rk5)
      CASE (06)
         status = NF_PUT_VARA_DOUBLE (ptr%file_ncid,ptr%field_ncid, &
                                                    start(1:7),count(1:7),rk6)
      CASE (07)
         status = NF_PUT_VARA_DOUBLE (ptr%file_ncid,ptr%field_ncid, &
                                                    start(1:8),count(1:8),rk7)
      END SELECT
      CALL NF_handle_error (status," write_netCDF :: NF_PUT_VARA_DOUBLE ")

      status = NF_CLOSE (ptr%file_ncid)
      CALL NF_handle_error (status," write_netCDF :: NF_CLOSE ")
   ENDIF

   END SUBROUTINE write_netCDF
!=======================================================================
!  END write_netCDF
!=======================================================================

!=======================================================================
!  BEGIN time_stamp
!=======================================================================
   FUNCTION time_stamp () RESULT (time_strng)
!.......................................................................
!  INTENT OUT
!.......................................................................
   CHARACTER (LEN=23) :: &
      time_strng
!.......................................................................
!  LOCAL
!.......................................................................
    INTEGER (KIND=int_kind) :: &
      date_time(8)
    CHARACTER (LEN=4) :: &
      s1
    CHARACTER (LEN=2) :: &
      s2,s3,s4,s5,s6
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   CALL DATE_AND_TIME (VALUES=date_time)

   WRITE (UNIT=s1,FMT="(I04)") date_time(1)
   WRITE (UNIT=s2,FMT="(I02)") date_time(2)
   WRITE (UNIT=s3,FMT="(I02)") date_time(3)
   WRITE (UNIT=s4,FMT="(I02)") date_time(5)
   WRITE (UNIT=s5,FMT="(I02)") date_time(6)
   WRITE (UNIT=s6,FMT="(I02)") date_time(7)

   time_strng = "("//s1//" "//s2//" "//s3//") ("//s4//":"//s5//":"//s6//")"

   END FUNCTION time_stamp
!=======================================================================
!  END time_stamp
!=======================================================================

!======================================================================
!  BEGIN NF_handle_error
!======================================================================
   SUBROUTINE NF_handle_error (status,message)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      status
   CHARACTER (LEN=*),INTENT(IN),OPTIONAL :: &
      message
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (status==NF_NOERR) RETURN

   IF (PRESENT (message)) THEN
      PRINT "(A27,A128)"," netCDF ERROR :: message = ",TRIM (message)
   ENDIF

   PRINT "(A32,A128)"," netCDF ERROR :: NF_STRERROR = ",NF_STRERROR (STATUS)

   STOP

   END SUBROUTINE NF_handle_error
!=======================================================================
!  END NF_handle_error
!=======================================================================

   END MODULE utilities_netCDF
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
