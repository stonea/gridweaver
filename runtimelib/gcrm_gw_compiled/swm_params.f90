   MODULE swm_params

   USE kinds
   USE parallel_params
   USE grid_params
   USE grid_subdomain
   USE grid_metrics

   USE utilities_misc

   USE swm_params_vertical
   USE swm_params_time

   IMPLICIT NONE
   SAVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  run the model or not run
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LOGICAL (KIND=log_kind) :: &
      l_swm        = .TRUE.,  &!  .TRUE. -> the model will continue to run
      l_mass       = .TRUE.,  &!  .TRUE. -> timestep the continuity equation
      l_tracer     = .FALSE., &!  .TRUE. -> timestep the tracer     equation
      l_vorticity  = .TRUE.,  &!  .TRUE. -> timestep the vorticity  equation
      l_divergence = .TRUE.    !  .TRUE. -> timestep the divergence equation
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  model string
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CHARACTER (LEN=128) :: &
      swm_strng = "swm"
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  begin simulation reading from restart file (or not)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LOGICAL (KIND=log_kind) :: &
      l_restart    = .FALSE.  !  .TRUE. -> the model will be initialized 
                              !            from the restart file
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  initial_condition_select -> select the initial condition
!     0 -> barotropic vorticity test
!     1 -> williamson test case 1 (JOURNAL OF COMPUTATIONAL PHYSICS 102, 211-224 (1992) )
!     2 -> williamson test case 2 (JOURNAL OF COMPUTATIONAL PHYSICS 102, 211-224 (1992) )
!     5 -> williamson test case 5 (JOURNAL OF COMPUTATIONAL PHYSICS 102, 211-224 (1992) )
!     6 -> Rossby-Haurwitz Wave.  williamson test case 6 (JOURNAL OF COMPUTATIONAL PHYSICS 102, 211-224 (1992) )
!    11 -> GALEWSKY.  TELLUS (2004), 56A, 429-440
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   INTEGER (KIND=int_kind) :: &
      initial_condition_select = 1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  paths and files
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CHARACTER (LEN=128),PARAMETER :: &
      path_output       = "../../output/swm/IC01/grd04/000", &
      path_restart      = "../../output/swm", &
      path_data         = "../../data", &
      file_namelist     = "../../namelist", &
      file_restart      = "../../restart"
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  logical control of various things
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LOGICAL (KIND=log_kind) :: &
      l_diffusion_div = .FALSE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  diffusion coeffiecients
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind) :: &! nominal coeffiecient for 2562 cells
      k_diffusion_div = 1.0E+16_dbl_kind
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  determine the method to generate the grid points
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CHARACTER (LEN=128),PARAMETER :: &
      grid_point_path   = TRIM (path_data)//"/grid/grid_points/tweaked", &
      grid_point_select = "read from file"
!     grid_point_select = "bisect" 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  talk or not talk
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   LOGICAL (KIND=log_kind) :: &
      l_verbose    = .TRUE.   !  .TRUE. -> write to standard ontput
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   REAL (KIND=dbl_kind) :: &! angle of advection for williamson IC01
      advctn_alph_IC01 = zero

   CONTAINS
!=======================================================================
!  BEGIN write_report_swm
!=======================================================================
   SUBROUTINE write_report_swm ()
!.......................................................................
!  LOCAL
!.......................................................................
   LOGICAL (KIND=log_kind) :: &
     l_exist
   INTEGER (KIND=int_kind) :: &
      i,j,k,nsd
   INTEGER (KIND=4) :: &
      vls(8)
   CHARACTER (LEN=07) :: &
      pe_strng
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   IF (rnk_wrld==i0i) THEN

      PRINT "(A14,F6.1)"," dt_swm     = ",dt_swm
      PRINT "(A14,I6  )"," npe_wrld   = ",npe_wrld
      PRINT "(A14,I6  )"," level_max  = ",level_max
      PRINT "(A14,I6  )"," sbdmn_iota = ",sbdmn_iota
      PRINT "(A14,I6  )"," level_glbl = ",level_glbl
      PRINT "(A14,I6  )"," km         = ",km

!-----------------------------------------------------------------------
!  write configuration file
!-----------------------------------------------------------------------
      OPEN (UNIT=17,FILE=TRIM (path_output)//"/report_configuration", &
                                                             FORM='FORMATTED')

      CALL DATE_AND_TIME (VALUES=vls)
      WRITE (UNIT=17,FMT="(A17,I2,A1,I2,A1,I4,A4,I2,A1,I2,A1,I2)") &
         " program started ",vls(2),"/",vls(3),"/",vls(1), &
         " at ",vls(5),":",vls(6),":",vls(7)

      WRITE (UNIT=17,FMT="(A01     )") " "
      WRITE (UNIT=17,FMT="(A24,F6.1)") " dt_swm               = ",dt_swm
      WRITE (UNIT=17,FMT="(A24,I6  )") " npe_wrld             = ",npe_wrld
      WRITE (UNIT=17,FMT="(A24,I6  )") " npe_io               = ",npe_io
      WRITE (UNIT=17,FMT="(A24,I6  )") " level_max            = ",level_max
      WRITE (UNIT=17,FMT="(A24,I6  )") " sbdmn_iota           = ",sbdmn_iota
      WRITE (UNIT=17,FMT="(A24,I6  )") " level_glbl           = ",level_glbl
      WRITE (UNIT=17,FMT="(A24,I6  )") " distribution_pattern = ", &
                                                           distribution_pattern
      WRITE (UNIT=17,FMT="(A24,I6  )") " km                   = ",km
      WRITE (UNIT=17,FMT="(A24,I6  )") " level_threshold      = ",level_threshold
      WRITE (UNIT=17,FMT="(A24,I6  )") " cell_min             = ",cell_min
      WRITE (UNIT=17,FMT="(A01     )") " "
      WRITE (UNIT=17,FMT="(A30,I3  )") " initial_condition_select   = ", &
                                                   initial_condition_select
      WRITE (UNIT=17,FMT="(A30,A32 )") " grid_point_select          = ", &
                                                   grid_point_select
      CLOSE (UNIT=17)
!-----------------------------------------------------------------------
!  write coordinate surface interfaces
!-----------------------------------------------------------------------
      OPEN (UNIT=17,FILE=TRIM (path_output)//"/report_z", &
                                                             FORM='FORMATTED')
      WRITE (UNIT=17,FMT="(F12.4)") 0.0_dbl_kind                               
      WRITE (UNIT=17,FMT="(F12.4)") 1.0_dbl_kind                                    
      CLOSE (UNIT=17)
!-----------------------------------------------------------------------
!  write coordinate surface layers
!-----------------------------------------------------------------------
      OPEN (UNIT=17,FILE=TRIM (path_output)//"/report_z_lyr", &
                                                             FORM='FORMATTED')
      WRITE (UNIT=17,FMT="(F12.4)") 0.5_dbl_kind                                    
      CLOSE (UNIT=17)
   ENDIF
!-----------------------------------------------------------------------
!  write grid face
!-----------------------------------------------------------------------
   IF (.TRUE.) THEN
   pe_strng = "_"//integer_to_string (6,rnk_wrld)
   OPEN (UNIT=17,FILE=TRIM (path_output)//"/report_fac"//pe_strng, &
                                                             FORM='FORMATTED')
   IF (sbdmn(level_max)%l_agent_north) THEN
      i = 2; j = sbdmn(level_max)%jm; nsd = sbdmn(level_max)%nsd_north;
      WRITE (UNIT=17,FMT="(I12,21F12.8)") tag_glbl(i,j,nsd),point(:,i,j,nsd), &
    corner(:,1,i  ,j  ,nsd),corner(:,1,i-1,j  ,nsd),corner(:,2,i-1,j-1,nsd), &
    corner(:,1,i-1,j-1,nsd),corner(:,2,i  ,j-1,nsd),corner(:,2,i  ,j-1,nsd)
   ENDIF
   IF (sbdmn(level_max)%l_agent_south) THEN
      i = sbdmn(level_max)%im; j = 2; nsd = sbdmn(level_max)%nsd_south;
      WRITE (UNIT=17,FMT="(I12,21F12.8)") tag_glbl(i,j,nsd),point(:,i,j,nsd), &
    corner(:,2,i  ,j-1,nsd),corner(:,2,i  ,j  ,nsd),corner(:,1,i-1,j  ,nsd), &
    corner(:,2,i-1,j-1,nsd),corner(:,1,i-1,j-1,nsd),corner(:,1,i-1,j-1,nsd)
   ENDIF
   DO nsd = 1,sbdmn(level_max)%nsdm
      DO j = 2,sbdmn(level_max)%jm-1
         DO i = 2,sbdmn(level_max)%im-1
            WRITE (UNIT=17,FMT="(I12,21F12.8)") tag_glbl(i,j,nsd), &
                                          point(:,i,j,nsd),corner(:,:,i,j,nsd)
         ENDDO
      ENDDO
   ENDDO
   CLOSE (UNIT=17)
!-----------------------------------------------------------------------
!  write grid edge information
!-----------------------------------------------------------------------
      pe_strng = "_"//integer_to_string (6,rnk_wrld)
      OPEN (UNIT=17,FILE=TRIM (path_output)//"/report_edg"//pe_strng, &
                                                             FORM='FORMATTED')
      DO nsd = 1,sbdmn(level_max)%nsdm
         DO j = 2,sbdmn(level_max)%jm-1
            DO i = 2,sbdmn(level_max)%im-1
               WRITE (UNIT=17,FMT="(I12,I3,15F24.20)") tag_glbl(i,j,nsd),1,point_edg(:,1,i,j,nsd), &
    point(:,i,j,nsd),corner(:,2,i,j-1,nsd),point(:,i+1,j  ,nsd),corner(:,1,i  ,j,nsd)
               WRITE (UNIT=17,FMT="(I12,I3,15F24.20)") tag_glbl(i,j,nsd),2,point_edg(:,2,i,j,nsd), &
    point(:,i,j,nsd),corner(:,1,i,j  ,nsd),point(:,i+1,j+1,nsd),corner(:,2,i  ,j,nsd)
               WRITE (UNIT=17,FMT="(I12,I3,15F24.20)") tag_glbl(i,j,nsd),3,point_edg(:,3,i,j,nsd), &
    point(:,i,j,nsd),corner(:,2,i,j  ,nsd),point(:,i  ,j+1,nsd),corner(:,1,i-1,j,nsd)
            ENDDO
         ENDDO
      ENDDO
      CLOSE (UNIT=17)

      IF (.TRUE.) THEN ! used for VVM
         pe_strng = "_"//integer_to_string (6,rnk_wrld)
         OPEN (UNIT=17,FILE=TRIM (path_output)//"/report_edgnrm"//pe_strng, &
                                                                FORM='FORMATTED')
         DO nsd = 1,sbdmn(level_max)%nsdm
            DO j = 2,sbdmn(level_max)%jm-1
               DO i = 2,sbdmn(level_max)%im-1
                  WRITE (UNIT=17,FMT="(I12,I3,3F24.20)") tag_glbl(i,j,nsd),1,nrm_edg(:,1,i,j,nsd)
                  WRITE (UNIT=17,FMT="(I12,I3,3F24.20)") tag_glbl(i,j,nsd),2,nrm_edg(:,2,i,j,nsd)
                  WRITE (UNIT=17,FMT="(I12,I3,3F24.20)") tag_glbl(i,j,nsd),3,nrm_edg(:,3,i,j,nsd)
               ENDDO
            ENDDO
         ENDDO
         CLOSE (UNIT=17)

         pe_strng = "_"//integer_to_string (6,rnk_wrld)
         OPEN (UNIT=17,FILE=TRIM (path_output)//"/report_edgtng"//pe_strng, &
                                                             FORM='FORMATTED')
         DO nsd = 1,sbdmn(level_max)%nsdm
            DO j = 2,sbdmn(level_max)%jm-1
               DO i = 2,sbdmn(level_max)%im-1
                  WRITE (UNIT=17,FMT="(I12,I3,3F24.20)") tag_glbl(i,j,nsd),1,tng_edg(:,1,i,j,nsd)
                  WRITE (UNIT=17,FMT="(I12,I3,3F24.20)") tag_glbl(i,j,nsd),2,tng_edg(:,2,i,j,nsd)
                  WRITE (UNIT=17,FMT="(I12,I3,3F24.20)") tag_glbl(i,j,nsd),3,tng_edg(:,3,i,j,nsd)
               ENDDO
            ENDDO
         ENDDO
         CLOSE (UNIT=17)
      ENDIF

   ENDIF

   END SUBROUTINE write_report_swm
!=======================================================================
!  END write_report_swm
!=======================================================================

   END MODULE swm_params
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
