   MODULE swm_params_time
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  Purpose:
!
!     This module specifies parameters related to the temporal structure
!     of the shallow water model and timestepping.
!
!  Define:
!
!     dt_swm -> timestep of the shallow water model [s]
!
!     nprog  -> number of time levels stored for each prognostic
!     ntend  -> number of time levels stored for each tendency
!
!     np0    -> array index of the current     time level (i.e. (n))
!     np1    -> array index of the next future time level (i.e. (n+1))
!
!     nm0    -> array index of the current         time level (i.e. (n))
!     nm1    -> array index of the first previous  time level (i.e. (n-1))
!     nm2    -> array index of the second previous time level (i.e. (n-2))
!
!     step_count_swm -> counts the number of timesteps performed
!                       by the model.  during the first timestep 
!                       we have step_count_swm = 1
!     time_start_swm -> time at the beginning of the simulation [s].
!                       if model begins from restart file, then this
!                       number comes from the restart file.  otherwise,
!                       this number is set to zero.
!     time_swm       -> current time in the model [s]
!                       time_swm = dt_swm*(step_count_swm-1)
!     time_end_swm   -> time at the end of the simulation [s].
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   USE kinds
   USE numbers

   USE grid_params

   IMPLICIT NONE
   SAVE
 
   REAL (KIND=big_kind) :: &
      dt_swm        ! timestep of the shallow water model             [s]

   INTEGER (KIND=int_kind),PARAMETER :: &
      nprog   = 2, &! number of time levels stored for each prognostic
      ntend   = 3   ! number of time levels stored for each tendency
 
   INTEGER (KIND=int_kind) :: &
      prog_index(nprog),tend_index(ntend), &
      np0,np1,nm0,nm1,nm2

   REAL (KIND=dbl_kind) :: &
      tend_weights(ntend)

   INTEGER (KIND=int_kind) :: &
      step_count_swm       ! counts the number of timesteps performed
                           ! by the shallow water model
                                                    
   REAL (KIND=big_kind) :: &
      time_start_swm,     &! time at the beginning of the simulation  [s]
      time_swm,           &! current time in the model  [s]
                           ! time_swm = dt_swm*FLOAT (step_count_swm-1)
      time_end_swm         ! time at the end of the simulation [s]

   REAL (KIND=big_kind) :: &
      dt_output

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  time unit string
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   CHARACTER (LEN=128) :: &
      time_unit_strng = "h"

   CONTAINS
!=======================================================================
!  BEGIN initialize_params_time
!=======================================================================
   SUBROUTINE initialize_params_time ()
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
   INTEGER (KIND=int_kind) :: &
      wallclock_start(8)
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   SELECT CASE (level_max)
      CASE (04); dt_swm =  240._big_kind; ! 00002562
      CASE (05); dt_swm =  120._big_kind; ! 00010242
      CASE (06); dt_swm =   80._big_kind; ! 00040962
      CASE (07); dt_swm =   40._big_kind; ! 00163842
      CASE (08); dt_swm =   20._big_kind; ! 00655362
      CASE (09); dt_swm =   10._big_kind; ! 02621442
      CASE (10); dt_swm =    8._big_kind; ! 10485762
      CASE (11); dt_swm =    4._big_kind; ! 41943042
      CASE DEFAULT
         PRINT *," initialize_params_time :: CANNOT DETERMINE dt_swm "
         STOP
   END SELECT

   dt_swm = 300._big_kind;

   time_start_swm = zero
   !time_end_swm   = 12._big_kind*24._big_kind*3600._big_kind
   time_end_swm   = 1._big_kind*24._big_kind*3600._big_kind

   dt_output      = 01._big_kind*24._big_kind*3600._big_kind

   step_count_swm = i1i

   prog_index(:) = (/ (n,n=1,nprog) /)
   np0 = prog_index(1); np1 = prog_index(2)

   tend_index(:) = (/ (n,n=1,ntend) /)
   nm0 = tend_index(1); nm1 = tend_index(2); nm2 = tend_index(3)

   tend_weights(:) = set_tendency_weights (step_count_swm) 

   END SUBROUTINE initialize_params_time
!=======================================================================
!  END   initialize_params_time
!=======================================================================

!=======================================================================
!  BEGIN set_tendency_weights
!=======================================================================
   FUNCTION set_tendency_weights (step_count) RESULT (weights)
!.......................................................................
!  INTENT IN
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      step_count
!.......................................................................
!  INTENT OUT
!.......................................................................
   REAL (KIND=dbl_kind) :: &
      weights(ntend)
!.......................................................................
!  LOCAL
!.......................................................................
   INTEGER (KIND=int_kind) :: &
      n
!.:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:. .:.

   SELECT CASE (step_count)
      CASE (01)
         weights(:) = (/ &
           (/  1.0_dbl_kind                               /) / 1.0_dbl_kind, & 
                                                   (0.0_dbl_kind,n=2,ntend) /)
      CASE (02)
         weights(:) = (/ &
           (/  3.0_dbl_kind, -1.0_dbl_kind                /) / 2.0_dbl_kind, & 
                                                   (0.0_dbl_kind,n=3,ntend) /)
      CASE DEFAULT
         weights(:) = (/ &
           (/ 23.0_dbl_kind,-16.0_dbl_kind,  5.0_dbl_kind /) /12.0_dbl_kind, & 
                                                   (0.0_dbl_kind,n=4,ntend) /)
   END SELECT

   END FUNCTION set_tendency_weights
!=======================================================================
!  END set_tendency_weights
!=======================================================================

   END MODULE swm_params_time
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc

