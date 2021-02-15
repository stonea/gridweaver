   MODULE parallel_params
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PURPOSE :
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE ginput
  
   IMPLICIT NONE
   SAVE

   INTEGER (KIND=int_kind),PARAMETER :: &
      npe_wrld =   8

   INTEGER (KIND=int_kind) :: &
      npe_io   = 01, & ! number of process elements within an io group
      rnk_wrld =  0    ! an identifier for the local process within the
                       ! world communicator

   INTEGER (KIND=int_kind),PARAMETER :: &
      rnk_nonexistent = -1 ! an identifier for a non-existent process

   CHARACTER (LEN=8) :: & ! a string identifier for the local process within
      rnk_wrld_strng      ! the world communicator

   CONTAINS
!=======================================================================
!  BEGIN load_input_options
!  It is assumed that the main program has called gin_load_file
!  Variable values will only be changed if they appeared in the file
!=======================================================================
   SUBROUTINE parallel_load_inputs ()

     call gin_get_value("io_procs",npe_io)

   END SUBROUTINE parallel_load_inputs
!=======================================================================
!  END parallel_load_inputs
!=======================================================================

   END MODULE parallel_params
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
