   MODULE utilities_io

   USE kinds
   USE grid_params
   USE grid_metrics
   USE parallel_utilities
   USE utilities_misc

   IMPLICIT NONE
   SAVE

   TYPE (comm_node),POINTER :: &
      io_comm_locl

   CONTAINS
!=======================================================================
!  BEGIN wrt_fld
!=======================================================================
   SUBROUTINE wrt_fld (path_strng,name_strng,mode_strng, &
                       level,proc,time,time_unit, &
                       int_rk1,int_rk2, &
                       face_1lyr,face,face_1lyr_3D,face_3D, &
                       corn_1lyr,corn,corn_1lyr_3D,corn_3D, &
                       edge_1lyr,edge,edge_1lyr_3D,edge_3D)
!.......................................................................
!  INTENT IN
!.......................................................................
   CHARACTER (LEN=*) :: &
      path_strng,name_strng,mode_strng
   INTEGER (KIND=int_kind),OPTIONAL :: &
      level,proc
   REAL (KIND=big_kind),OPTIONAL :: &
      time
   CHARACTER (LEN=01),OPTIONAL :: &
      time_unit
   INTEGER (KIND=int_kind),OPTIONAL :: &
      int_rk1(:),int_rk2(:,:)
   REAL (KIND=dbl_kind),OPTIONAL :: &
      face_1lyr(  :,:,  :),face(  :,:,:,:),face_1lyr_3D(:,  :,:,  :),face_3D(:,  :,:,:,:), &
      corn_1lyr(:,:,:,  :),corn(:,:,:,:,:),corn_1lyr_3D(:,:,:,:,  :),corn_3D(:,:,:,:,:,:), &
      edge_1lyr(:,:,:,  :),edge(:,:,:,:,:),edge_1lyr_3D(:,:,:,:,  :),edge_3D(:,:,:,:,:,:)
!.......................................................................
!  INTENT OUT
!.......................................................................
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      lvl,i,j,k,nsd,n,time_integer
   INTEGER (KIND=int_kind) :: &
      bytes_per_number,numbers_per_record,bytes_per_record
   INTEGER (KIND=int_kind) :: &
      l3(3),l4(4),l5(5),l6(6),msg_len,pe,rnk_io_root,status(MPI_STATUS_SIZE),ierr
   REAL (KIND=dbl_kind),ALLOCATABLE :: &
      recv_rk3(:,:,:),recv_rk4(:,:,:,:),recv_rk5(:,:,:,:,:),recv_rk6(:,:,:,:,:,:)
   REAL (KIND=SELECTED_REAL_KIND( 6)),ALLOCATABLE :: &
      rk3_32bit(:,:,:),rk4_32bit(:,:,:,:),rk5_32bit(:,:,:,:,:),rk6_32bit(:,:,:,:,:,:)
   REAL (KIND=big_kind) :: &
      time_tmpry
   CHARACTER (LEN=01) :: &
      unit_strng
   CHARACTER (LEN=01) :: &
      strng1,strng2,strng3
   CHARACTER (LEN=04) :: &
      file_type_strng
   CHARACTER (LEN=07) :: &
      grp_strng
   CHARACTER (LEN=06) :: &
      lvl_strng
   CHARACTER (LEN=07) :: &
      pe_strng
   CHARACTER (LEN=08) :: &
      time_strng
   CHARACTER (LEN=128) :: &
      strng
   TYPE (comm_node),POINTER :: &
      comm
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  ASSEMBLE THE FILE NAME STRING
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!  include sub-string containing the grid resolution level
!-----------------------------------------------------------------------
   IF (PRESENT (level)) THEN
      lvl       = level
      lvl_strng = "_lvl"//integer_to_string (2,level)
   ELSE
      lvl       = level_max
      lvl_strng = "      "
   ENDIF
!-----------------------------------------------------------------------
!  include sub-string containing the local process rank
!-----------------------------------------------------------------------
   IF (PRESENT (proc)) THEN
      pe_strng = "_"//integer_to_string (6,proc)
   ELSE
      pe_strng = "       "
   ENDIF
!-----------------------------------------------------------------------
!  include sub-string containing the current time
!-----------------------------------------------------------------------
   IF (PRESENT (time)) THEN
      IF (PRESENT (time_unit)) THEN
         SELECT CASE (time_unit)
            CASE ("s")
               time_tmpry = time
               unit_strng = "s"
            CASE ("h")
               time_tmpry = (time-MOD (time, 3600._big_kind))/ 3600._big_kind
               unit_strng = "h"
            CASE ("d")
               time_tmpry = (time-MOD (time,86400._big_kind))/86400._big_kind
               unit_strng = "d"
            CASE DEFAULT
               time_tmpry = time
               unit_strng = "_"
         END SELECT
         time_integer = INT (time_tmpry)
         time_strng   = "_"//integer_to_string (6,time_integer)//unit_strng
      ELSE
         time_integer = INT (time)
         time_strng   = "_"//integer_to_string (7,time_integer)
      ENDIF
   ELSE
      time_strng = "         "
   ENDIF

!-----------------------------------------------------------------------
!  set the file type string
!-----------------------------------------------------------------------

   strng1 = "_"
   IF (PRESENT (face_1lyr).OR.PRESENT (face).OR.PRESENT (face_1lyr_3D).OR.PRESENT (face_3D)) strng1 = "f"
   IF (PRESENT (edge_1lyr).OR.PRESENT (edge).OR.PRESENT (edge_1lyr_3D).OR.PRESENT (edge_3D)) strng1 = "e"
   IF (PRESENT (corn_1lyr).OR.PRESENT (corn).OR.PRESENT (corn_1lyr_3D).OR.PRESENT (corn_3D)) strng1 = "c"

   strng2 = "_"
   IF (PRESENT (face_1lyr)   .OR.PRESENT (face   ).OR.PRESENT (edge_1lyr   ).OR.PRESENT (edge   ).OR. &
       PRESENT (corn_1lyr   ).OR.PRESENT (corn   )) strng2 = "1"
   IF (PRESENT (face_1lyr_3D).OR.PRESENT (face_3D).OR.PRESENT (edge_1lyr_3D).OR.PRESENT (edge_3D).OR. &
       PRESENT (corn_1lyr_3D).OR.PRESENT (corn_3D)) strng2 = "3"

   SELECT CASE (mode_strng)
   CASE ("ascii")
      strng3 = "a"
   CASE ("binary")
      strng3 = "b"
   CASE ("parallel")
      strng3 = "p"
   CASE DEFAULT
      strng3 = "_"
   END SELECT

   file_type_strng = "."//strng1//strng2//strng3
!-----------------------------------------------------------------------
!  set strng
!-----------------------------------------------------------------------
   strng = TRIM (path_strng)//"/"//TRIM (name_strng)//TRIM (file_type_strng)// &
           TRIM (lvl_strng)//TRIM (pe_strng)//TRIM (time_strng)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  WRITE THE FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   SELECT CASE (mode_strng)
      CASE ("ascii")
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! OPEN ASCII FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         OPEN (UNIT=17,FILE=TRIM (strng),FORM='FORMATTED')
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  INTEGERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         IF (PRESENT (int_rk1)) THEN
            DO n = 1,SIZE (int_rk1,DIM=1)
               WRITE (UNIT=17,FMT="(I8)") int_rk1(n)
            ENDDO
         ENDIF 

         IF (PRESENT (int_rk2)) THEN
            DO n = 1,SIZE (int_rk2,DIM=2)
               WRITE (UNIT=17,FMT="(2I8)") int_rk2(1,n),int_rk2(2,n)
            ENDDO
         ENDIF 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  FACE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!  face_1lyr
!-----------------------------------------------------------------------
         IF (PRESENT (face_1lyr)) THEN
            IF (sbdmn(lvl)%l_agent_north) THEN ! north pole
               i = 2; j = sbdmn(lvl)%jm; nsd = sbdmn(lvl)%nsd_north;
               WRITE (UNIT=17,FMT="(I12,E20.12)") tag_glbl(i,j,nsd), &
                                               trim_number (face_1lyr(i,j,nsd))
            ENDIF
            IF (sbdmn(lvl)%l_agent_south) THEN ! south pole
               i = sbdmn(lvl)%im; j = 2; nsd = sbdmn(lvl)%nsd_south;
               WRITE (UNIT=17,FMT="(I12,E20.12)") tag_glbl(i,j,nsd), &
                                               trim_number (face_1lyr(i,j,nsd))
            ENDIF
            DO nsd = 1,sbdmn(lvl)%nsdm
               DO j = 2,sbdmn(lvl)%jm-1
                  DO i = 2,sbdmn(lvl)%im-1
                     WRITE (UNIT=17,FMT="(I12,E20.12)") tag_glbl(i,j,nsd), &
                                               trim_number (face_1lyr(i,j,nsd))
                  ENDDO
               ENDDO
            ENDDO
         ENDIF 
!-----------------------------------------------------------------------
!  face
!-----------------------------------------------------------------------
         IF (PRESENT (face)) THEN
            DO k = 1,SIZE (face,DIM=3)
               IF (sbdmn(lvl)%l_agent_north) THEN ! north pole
                  i = 2; j = sbdmn(lvl)%jm; nsd = sbdmn(lvl)%nsd_north;
                  WRITE (UNIT=17,FMT="(I12,E20.12)") tag_glbl(i,j,nsd), &
                                               trim_number (face(i,j,k,nsd))
               ENDIF
               IF (sbdmn(lvl)%l_agent_south) THEN ! south pole
                  i = sbdmn(lvl)%im; j = 2; nsd = sbdmn(lvl)%nsd_south;
                  WRITE (UNIT=17,FMT="(I12,E20.12)") tag_glbl(i,j,nsd), &
                                               trim_number (face(i,j,k,nsd))
               ENDIF
               DO nsd = 1,sbdmn(lvl)%nsdm
                  DO j = 2,sbdmn(lvl)%jm-1
                     DO i = 2,sbdmn(lvl)%im-1
                        WRITE (UNIT=17,FMT="(I12,E20.12)") tag_glbl(i,j,nsd), &
                                               trim_number (face(i,j,k,nsd))
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF 
!-----------------------------------------------------------------------
!  face_1lyr_3D
!-----------------------------------------------------------------------
         IF (PRESENT (face_1lyr_3D)) THEN
            IF (sbdmn(lvl)%l_agent_north) THEN ! north pole
               i = 2; j = sbdmn(lvl)%jm; nsd = sbdmn(lvl)%nsd_north;
               WRITE (UNIT=17,FMT="(I12,3E20.12)") tag_glbl(i,j,nsd), &
                                      trim_number (face_1lyr_3D(1,i,j,nsd)), &
                                      trim_number (face_1lyr_3D(2,i,j,nsd)), &
                                      trim_number (face_1lyr_3D(3,i,j,nsd))
            ENDIF
            IF (sbdmn(lvl)%l_agent_south) THEN ! south pole
               i = sbdmn(lvl)%im; j = 2; nsd = sbdmn(lvl)%nsd_south;
               WRITE (UNIT=17,FMT="(I12,3E20.12)") tag_glbl(i,j,nsd), &
                                      trim_number (face_1lyr_3D(1,i,j,nsd)), &
                                      trim_number (face_1lyr_3D(2,i,j,nsd)), &
                                      trim_number (face_1lyr_3D(3,i,j,nsd))
            ENDIF
            DO nsd = 1,sbdmn(lvl)%nsdm
               DO j = 2,sbdmn(lvl)%jm-1
                  DO i = 2,sbdmn(lvl)%im-1
                     WRITE (UNIT=17,FMT="(I12,3E20.12)") tag_glbl(i,j,nsd), &
                                      trim_number (face_1lyr_3D(1,i,j,nsd)), &
                                      trim_number (face_1lyr_3D(2,i,j,nsd)), &
                                      trim_number (face_1lyr_3D(3,i,j,nsd))
                  ENDDO
               ENDDO
            ENDDO
         ENDIF 
!-----------------------------------------------------------------------
!  face_3D
!-----------------------------------------------------------------------
         IF (PRESENT (face_3D)) THEN
            DO k = 1,SIZE (face_3D,DIM=4)
               IF (sbdmn(lvl)%l_agent_north) THEN ! north pole
                  i = 2; j = sbdmn(lvl)%jm; nsd = sbdmn(lvl)%nsd_north;
                  WRITE (UNIT=17,FMT="(I12,3E20.12)") tag_glbl(i,j,nsd), &
                                         trim_number (face_3D(1,i,j,k,nsd)), &
                                         trim_number (face_3D(2,i,j,k,nsd)), &
                                         trim_number (face_3D(3,i,j,k,nsd))
               ENDIF
               IF (sbdmn(lvl)%l_agent_south) THEN ! south pole
                  i = sbdmn(lvl)%im; j = 2; nsd = sbdmn(lvl)%nsd_south;
                  WRITE (UNIT=17,FMT="(I12,3E20.12)") tag_glbl(i,j,nsd), &
                                         trim_number (face_3D(1,i,j,k,nsd)), &
                                         trim_number (face_3D(2,i,j,k,nsd)), &
                                         trim_number (face_3D(3,i,j,k,nsd))
               ENDIF
               DO nsd = 1,sbdmn(lvl)%nsdm
                  DO j = 2,sbdmn(lvl)%jm-1
                     DO i = 2,sbdmn(lvl)%im-1
                        WRITE (UNIT=17,FMT="(I12,3E20.12)") tag_glbl(i,j,nsd), &
                                         trim_number (face_3D(1,i,j,k,nsd)), &
                                         trim_number (face_3D(2,i,j,k,nsd)), &
                                         trim_number (face_3D(3,i,j,k,nsd))
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  CORNER
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!  corn_1lyr
!-----------------------------------------------------------------------
         IF (PRESENT (corn_1lyr)) THEN
            DO nsd = 1,sbdmn(lvl)%nsdm
               DO j = 2,sbdmn(lvl)%jm-1
                  DO i = 2,sbdmn(lvl)%im-1
                     WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                      tag_glbl(i,j,nsd),i1i,trim_number (corn_1lyr(1,i,j,nsd))
                     WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                      tag_glbl(i,j,nsd),i2i,trim_number (corn_1lyr(2,i,j,nsd))
                  ENDDO
               ENDDO
            ENDDO
         ENDIF 
!-----------------------------------------------------------------------
!  corn
!-----------------------------------------------------------------------
         IF (PRESENT (corn)) THEN
            DO k = 1,SIZE (corn,DIM=4)
               DO nsd = 1,sbdmn(lvl)%nsdm
                  DO j = 2,sbdmn(lvl)%jm-1
                     DO i = 2,sbdmn(lvl)%im-1
                        WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                          tag_glbl(i,j,nsd),i1i,trim_number (corn(1,i,j,k,nsd))
                        WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                          tag_glbl(i,j,nsd),i2i,trim_number (corn(2,i,j,k,nsd))
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF 
!-----------------------------------------------------------------------
!  corn_1lyr_3D
!-----------------------------------------------------------------------
         IF (PRESENT (corn_1lyr_3D)) THEN
            DO nsd = 1,sbdmn(lvl)%nsdm
               DO j = 2,sbdmn(lvl)%jm-1
                  DO i = 2,sbdmn(lvl)%im-1
                     WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                       tag_glbl(i,j,nsd),i1i,trim_number (corn_1lyr_3D(1,1,i,j,nsd)), &
                                             trim_number (corn_1lyr_3D(2,1,i,j,nsd)), &
                                             trim_number (corn_1lyr_3D(3,1,i,j,nsd))
                     WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                       tag_glbl(i,j,nsd),i2i,trim_number (corn_1lyr_3D(1,2,i,j,nsd)), &
                                             trim_number (corn_1lyr_3D(2,2,i,j,nsd)), &
                                             trim_number (corn_1lyr_3D(3,2,i,j,nsd))
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
!-----------------------------------------------------------------------
!  corn_3D
!-----------------------------------------------------------------------
         IF (PRESENT (corn_3D)) THEN
            DO k = 1,SIZE (corn_3D,DIM=5)
               DO nsd = 1,sbdmn(lvl)%nsdm
                  DO j = 2,sbdmn(lvl)%jm-1
                     DO i = 2,sbdmn(lvl)%im-1
                        WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                          tag_glbl(i,j,nsd),i1i,trim_number (corn_3D(1,1,i,j,k,nsd)), &
                                                trim_number (corn_3D(2,1,i,j,k,nsd)), &
                                                trim_number (corn_3D(3,1,i,j,k,nsd))
                        WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                          tag_glbl(i,j,nsd),i2i,trim_number (corn_3D(1,2,i,j,k,nsd)), &
                                                trim_number (corn_3D(2,2,i,j,k,nsd)), &
                                                trim_number (corn_3D(3,2,i,j,k,nsd))
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  EDGE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!-----------------------------------------------------------------------
!  edge_1lyr
!-----------------------------------------------------------------------
         IF (PRESENT (edge_1lyr)) THEN
            DO nsd = 1,sbdmn(lvl)%nsdm
               DO j = 2,sbdmn(lvl)%jm-1
                  DO i = 2,sbdmn(lvl)%im-1
                     WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                          tag_glbl(i,j,nsd),i1i,trim_number (edge_1lyr(1,i,j,nsd))
                     WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                          tag_glbl(i,j,nsd),i2i,trim_number (edge_1lyr(2,i,j,nsd))
                     WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                          tag_glbl(i,j,nsd),i3i,trim_number (edge_1lyr(3,i,j,nsd))
                  ENDDO
               ENDDO
            ENDDO
         ENDIF 
!-----------------------------------------------------------------------
!  edge
!-----------------------------------------------------------------------
         IF (PRESENT (edge)) THEN
            DO k = 1,SIZE (edge,DIM=4)
               DO nsd = 1,sbdmn(lvl)%nsdm
                  DO j = 2,sbdmn(lvl)%jm-1
                     DO i = 2,sbdmn(lvl)%im-1
                        WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                          tag_glbl(i,j,nsd),i1i,trim_number (edge(1,i,j,k,nsd))
                        WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                          tag_glbl(i,j,nsd),i2i,trim_number (edge(2,i,j,k,nsd))
                        WRITE (UNIT=17,FMT="(I12,I3,E20.12)") &
                          tag_glbl(i,j,nsd),i3i,trim_number (edge(3,i,j,k,nsd))
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF 
!-----------------------------------------------------------------------
!  edge_1lyr_3D
!-----------------------------------------------------------------------
         IF (PRESENT (edge_1lyr_3D)) THEN
            DO nsd = 1,sbdmn(lvl)%nsdm
               DO j = 2,sbdmn(lvl)%jm-1
                  DO i = 2,sbdmn(lvl)%im-1
                     WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                          tag_glbl(i,j,nsd),i1i,trim_number (edge_1lyr_3D(1,1,i,j,nsd)), &
                                                trim_number (edge_1lyr_3D(2,1,i,j,nsd)), &
                                                trim_number (edge_1lyr_3D(3,1,i,j,nsd))
                     WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                          tag_glbl(i,j,nsd),i2i,trim_number (edge_1lyr_3D(1,2,i,j,nsd)), &
                                                trim_number (edge_1lyr_3D(2,2,i,j,nsd)), &
                                                trim_number (edge_1lyr_3D(3,2,i,j,nsd))
                     WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                          tag_glbl(i,j,nsd),i3i,trim_number (edge_1lyr_3D(1,3,i,j,nsd)), &
                                                trim_number (edge_1lyr_3D(2,3,i,j,nsd)), &
                                                trim_number (edge_1lyr_3D(3,3,i,j,nsd))
                  ENDDO
               ENDDO
            ENDDO
         ENDIF 
!-----------------------------------------------------------------------
!  edge_3D
!-----------------------------------------------------------------------
         IF (PRESENT (edge_3D)) THEN
            DO k = 1,SIZE (edge_3D,DIM=5)
               DO nsd = 1,sbdmn(lvl)%nsdm
                  DO j = 2,sbdmn(lvl)%jm-1
                     DO i = 2,sbdmn(lvl)%im-1
                        WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                          tag_glbl(i,j,nsd),i1i,trim_number (edge_3D(1,1,i,j,k,nsd)), &
                                                trim_number (edge_3D(2,1,i,j,k,nsd)), &
                                                trim_number (edge_3D(3,1,i,j,k,nsd))
                        WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                          tag_glbl(i,j,nsd),i2i,trim_number (edge_3D(1,2,i,j,k,nsd)), &
                                                trim_number (edge_3D(2,2,i,j,k,nsd)), &
                                                trim_number (edge_3D(3,2,i,j,k,nsd))
                        WRITE (UNIT=17,FMT="(I12,I3,3E20.12)") &
                          tag_glbl(i,j,nsd),i3i,trim_number (edge_3D(1,3,i,j,k,nsd)), &
                                                trim_number (edge_3D(2,3,i,j,k,nsd)), &
                                                trim_number (edge_3D(3,3,i,j,k,nsd))
                     ENDDO
                  ENDDO
               ENDDO
            ENDDO
         ENDIF 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! CLOSE ASCII FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         CLOSE (UNIT=17)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  BINARY WRITE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      CASE ("binary")

         IF (PRESENT (face)) THEN
            bytes_per_number   = KIND (face)
            numbers_per_record = SIZE (face,DIM=3)
            bytes_per_record   = bytes_per_number*numbers_per_record
            OPEN (UNIT=17,FILE=TRIM (strng), &
                  FORM="UNFORMATTED",ACCESS="DIRECT",RECL=bytes_per_record)
! north pole
            IF (sbdmn(lvl)%l_agent_north) THEN
               i = 2; j = sbdmn(lvl)%jm; nsd = sbdmn(lvl)%nsd_north;
               WRITE (UNIT=17,REC=1) face(i,j,:,nsd)
            ENDIF
! south pole
            IF (sbdmn(lvl)%l_agent_south) THEN
               i = sbdmn(lvl)%im; j = 2; nsd = sbdmn(lvl)%nsd_south;
               WRITE (UNIT=17,REC=2) face(i,j,:,nsd)
            ENDIF
            DO nsd = 1,nsdm
               DO j = 2,jm-1
                  DO i = 2,im-1
                     WRITE (UNIT=17,REC=tag_glbl(i,j,nsd)) face(i,j,:,nsd)
                  ENDDO
               ENDDO
            ENDDO
            CLOSE (UNIT=17)
         ENDIF

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PARALLEL WRITE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      CASE ("parallel")

         comm => parallel_get_communicator ("world")
         rnk_io_root = npe_io*(rnk_wrld/npe_io)

         IF (PRESENT (face)) THEN
            l4(:) = SHAPE (face); msg_len = l4(1)*l4(2)*l4(3)*l4(4);

            IF (MOD (rnk_wrld,npe_io)==i0i) THEN

               ALLOCATE (recv_rk4 (l4(1),l4(2),l4(3),l4(4)))
               ALLOCATE (rk4_32bit(l4(1),l4(2),l4(3),l4(4)))

               grp_strng = "_"//integer_to_string (6,rnk_io_root)
               strng = TRIM (path_strng)//"/"//TRIM (name_strng)//TRIM (file_type_strng)// &
                       TRIM (grp_strng)//TRIM (time_strng)

               OPEN  (UNIT=17,FILE=TRIM (strng),FORM="UNFORMATTED")
               rk4_32bit = face
               WRITE (UNIT=17) rk4_32bit

               DO pe = 1,npe_io-1
                  CALL MPI_RECV (recv_rk4,msg_len,MPI_DOUBLE_PRECISION, &
                                rnk_wrld+pe,rnk_wrld+pe,comm%comm,status,ierr)
                  rk4_32bit = recv_rk4
                  WRITE (UNIT=17) rk4_32bit
               ENDDO
               DEALLOCATE (recv_rk4,rk4_32bit)

               CLOSE (UNIT=17)
            ELSE
               CALL MPI_SEND (face,msg_len,MPI_DOUBLE_PRECISION, &
                                          rnk_io_root,rnk_wrld,comm%comm,ierr)
            ENDIF
         ENDIF

         IF (PRESENT (face_1lyr)) THEN
            l3(:) = SHAPE (face_1lyr); msg_len = l3(1)*l3(2)*l3(3)

            IF (MOD (rnk_wrld,npe_io)==i0i) THEN

               ALLOCATE (recv_rk3 (l3(1),l3(2),l3(3)))
               ALLOCATE (rk3_32bit(l3(1),l3(2),l3(3)))

               grp_strng = "_"//integer_to_string (6,rnk_io_root)
               strng = TRIM (path_strng)//"/"//TRIM (name_strng)//TRIM (file_type_strng)// &
                       TRIM (grp_strng)//TRIM (time_strng)

               OPEN  (UNIT=17,FILE=TRIM (strng),FORM="UNFORMATTED")
               rk3_32bit = face_1lyr
               WRITE (UNIT=17) rk3_32bit

               DO pe = 1,npe_io-1
                  CALL MPI_RECV (recv_rk3,msg_len,MPI_DOUBLE_PRECISION, &
                                rnk_wrld+pe,rnk_wrld+pe,comm%comm,status,ierr)
                  rk3_32bit = recv_rk3
                  WRITE (UNIT=17) rk3_32bit
               ENDDO
               DEALLOCATE (recv_rk3,rk3_32bit)

               CLOSE (UNIT=17)
            ELSE
               CALL MPI_SEND (face_1lyr,msg_len,MPI_DOUBLE_PRECISION, &
                                          rnk_io_root,rnk_wrld,comm%comm,ierr)
            ENDIF
         ENDIF

         IF (PRESENT (corn)) THEN
            l5(:) = SHAPE (corn); msg_len = l5(1)*l5(2)*l5(3)*l5(4)*l5(5);

            IF (MOD (rnk_wrld,npe_io)==i0i) THEN
               ALLOCATE (recv_rk5 (l5(1),l5(2),l5(3),l5(4),l5(5)))
               ALLOCATE (rk5_32bit(l5(1),l5(2),l5(3),l5(4),l5(5)))

               grp_strng = "_"//integer_to_string (6,rnk_io_root)
               strng = TRIM (path_strng)//"/"//TRIM (name_strng)//TRIM (file_type_strng)// &
                       TRIM (grp_strng)//TRIM (time_strng)
               OPEN (UNIT=17,FILE=TRIM (strng),FORM="UNFORMATTED")
               rk5_32bit = corn
               WRITE (UNIT=17) rk5_32bit

               DO pe = 1,npe_io-1
                  CALL MPI_RECV (recv_rk5,msg_len,MPI_DOUBLE_PRECISION, &
                                rnk_wrld+pe,rnk_wrld+pe,comm%comm,status,ierr)
                  rk5_32bit = recv_rk5
                  WRITE (UNIT=17) rk5_32bit
               ENDDO
               DEALLOCATE (recv_rk5,rk5_32bit)

               CLOSE (UNIT=17)
            ELSE
               CALL MPI_SEND (corn,msg_len,MPI_DOUBLE_PRECISION, &
                                          rnk_io_root,rnk_wrld,comm%comm,ierr)
            ENDIF
         ENDIF

         IF (PRESENT (edge)) THEN
            l5(:) = SHAPE (edge); msg_len = l5(1)*l5(2)*l5(3)*l5(4)*l5(5);

            IF (MOD (rnk_wrld,npe_io)==i0i) THEN
               ALLOCATE (recv_rk5 (l5(1),l5(2),l5(3),l5(4),l5(5)))
               ALLOCATE (rk5_32bit(l5(1),l5(2),l5(3),l5(4),l5(5)))

               grp_strng = "_"//integer_to_string (6,rnk_io_root)
               strng = TRIM (path_strng)//"/"//TRIM (name_strng)//TRIM (file_type_strng)// &
                       TRIM (grp_strng)//TRIM (time_strng)
               OPEN (UNIT=17,FILE=TRIM (strng),FORM="UNFORMATTED")
               rk5_32bit = edge
               WRITE (UNIT=17) rk5_32bit

               DO pe = 1,npe_io-1
                  CALL MPI_RECV (recv_rk5,msg_len,MPI_DOUBLE_PRECISION, &
                                rnk_wrld+pe,rnk_wrld+pe,comm%comm,status,ierr)
                  rk5_32bit = recv_rk5
                  WRITE (UNIT=17) rk5_32bit
               ENDDO
               DEALLOCATE (recv_rk5,rk5_32bit)

               CLOSE (UNIT=17)
            ELSE
               CALL MPI_SEND (edge,msg_len,MPI_DOUBLE_PRECISION, &
                                          rnk_io_root,rnk_wrld,comm%comm,ierr)
            ENDIF
         ENDIF

         IF (PRESENT (corn_3D)) THEN
            l6(:) = SHAPE (corn_3D); msg_len = l6(1)*l6(2)*l6(3)*l6(4)*l6(5)*l6(6);

            IF (MOD (rnk_wrld,npe_io)==i0i) THEN
               ALLOCATE (recv_rk6 (l6(1),l6(2),l6(3),l6(4),l6(5),l6(6)))
               ALLOCATE (rk6_32bit(l6(1),l6(2),l6(3),l6(4),l6(5),l6(6)))

               grp_strng = "_"//integer_to_string (6,rnk_io_root)
               strng = TRIM (path_strng)//"/"//TRIM (name_strng)//TRIM (file_type_strng)// &
                       TRIM (grp_strng)//TRIM (time_strng)
               OPEN (UNIT=17,FILE=TRIM (strng),FORM="UNFORMATTED")
               rk6_32bit = corn_3D
               WRITE (UNIT=17) rk6_32bit

               DO pe = 1,npe_io-1
                  CALL MPI_RECV (recv_rk6,msg_len,MPI_DOUBLE_PRECISION, &
                                rnk_wrld+pe,rnk_wrld+pe,comm%comm,status,ierr)
                  rk6_32bit = recv_rk6
                  WRITE (UNIT=17) rk6_32bit
               ENDDO
               DEALLOCATE (recv_rk6,rk6_32bit)

               CLOSE (UNIT=17)
            ELSE
               CALL MPI_SEND (corn_3D,msg_len,MPI_DOUBLE_PRECISION, &
                                          rnk_io_root,rnk_wrld,comm%comm,ierr)
            ENDIF
         ENDIF

         IF (PRESENT (edge_3D)) THEN
            l6(:) = SHAPE (edge_3D); msg_len = l6(1)*l6(2)*l6(3)*l6(4)*l6(5)*l6(6);

            IF (MOD (rnk_wrld,npe_io)==i0i) THEN
               ALLOCATE (recv_rk6 (l6(1),l6(2),l6(3),l6(4),l6(5),l6(6)))
               ALLOCATE (rk6_32bit(l6(1),l6(2),l6(3),l6(4),l6(5),l6(6)))

               grp_strng = "_"//integer_to_string (6,rnk_io_root)
               strng = TRIM (path_strng)//"/"//TRIM (name_strng)//TRIM (file_type_strng)// &
                       TRIM (grp_strng)//TRIM (time_strng)
               OPEN (UNIT=17,FILE=TRIM (strng),FORM="UNFORMATTED")
               rk6_32bit = edge_3D
               WRITE (UNIT=17) rk6_32bit

               DO pe = 1,npe_io-1
                  CALL MPI_RECV (recv_rk6,msg_len,MPI_DOUBLE_PRECISION, &
                                rnk_wrld+pe,rnk_wrld+pe,comm%comm,status,ierr)
                  rk6_32bit = recv_rk6
                  WRITE (UNIT=17) rk6_32bit
               ENDDO
               DEALLOCATE (recv_rk6,rk6_32bit)

               CLOSE (UNIT=17)
            ELSE
               CALL MPI_SEND (edge_3D,msg_len,MPI_DOUBLE_PRECISION, &
                                          rnk_io_root,rnk_wrld,comm%comm,ierr)
            ENDIF
         ENDIF

   END SELECT

   END SUBROUTINE wrt_fld
!=======================================================================
!  END wrt_fld
!=======================================================================

!=======================================================================
!  BEGIN trim_number
!=======================================================================
   FUNCTION trim_number (x) RESULT (x_trim)
!.......................................................................
!  INTENT IN
!.......................................................................
   REAL (KIND=dbl_kind) ::                                                   &
      x
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) ::                                                   &
      x_trim
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   x_trim = x
   IF (x >  1.0E47_dbl_kind) x_trim =  1.0E47_dbl_kind
   IF (x < -1.0E47_dbl_kind) x_trim = -1.0E47_dbl_kind
   IF (ABS (x) < 1.0E-47_dbl_kind) x_trim = 0.0_dbl_kind

   END FUNCTION trim_number
!=======================================================================
!  END   trim_number
!=======================================================================

   END MODULE utilities_io
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
