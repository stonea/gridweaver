   MODULE kinds

   IMPLICIT NONE
   SAVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  KIND FOR LOGICAL VARIABLES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   INTEGER,PARAMETER :: &
      log_kind = KIND(.TRUE.)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PRECISIONS FOR INTEGER NUMBERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   INTEGER,PARAMETER :: &
      int_kind = SELECTED_INT_KIND( 8), & ! 4 byte integer
      ibg_kind = SELECTED_INT_KIND(16)    ! 8 byte integer
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!  PRECISIONS FOR FLOATING POINT NUMBERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   INTEGER,PARAMETER :: &
      sgl_kind = SELECTED_REAL_KIND( 6), & !  4 byte floating point number
      dbl_kind = SELECTED_REAL_KIND(12), & !  8 byte floating point number
      big_kind = SELECTED_REAL_KIND(12)    ! 16 byte floating point number
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   INTEGER,PARAMETER :: &
      char_len = 128

   END MODULE kinds
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc
