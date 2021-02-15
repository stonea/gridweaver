   MODULE numbers
   USE kinds

   IMPLICIT NONE
   SAVE 
!-----------------------------------------------------------------------
!  integer numbers
!-----------------------------------------------------------------------
   INTEGER (KIND=int_kind),PARAMETER :: &
      i0i = 0_int_kind, &! zero
      i1i = 1_int_kind, &! one
      i2i = 2_int_kind, &! two
      i3i = 3_int_kind, &! three
      i4i = 4_int_kind, &! four
      i5i = 5_int_kind, &! five
      i6i = 6_int_kind, &! six
      i7i = 7_int_kind, &! seven
      i8i = 8_int_kind, &! eight
      i9i = 9_int_kind   ! nine
!-----------------------------------------------------------------------
!  rational numbers
!-----------------------------------------------------------------------
   REAL (KIND=dbl_kind),PARAMETER :: &
      zero  = 0.0_dbl_kind, &! zero
      sixth = 0.1666666666666666666666666666666666666666666666_dbl_kind, &! 1/6
      fifth = 0.2_dbl_kind, &! 1/5
      third = 0.3333333333333333333333333333333333333333333333_dbl_kind, &! 1/3
      half  = 0.5_dbl_kind, &! 1/2
      twoth = 0.6666666666666666666666666666666666666666666666_dbl_kind, &! 2/3
      one   = 1.0_dbl_kind, &! one
      two   = 2.0_dbl_kind, &! two
      three = 3.0_dbl_kind, &! three
      four  = 4.0_dbl_kind, &! four
      five  = 5.0_dbl_kind, &! five
      six   = 6.0_dbl_kind, &! six
      seven = 7.0_dbl_kind, &! seven
      eight = 8.0_dbl_kind, &! eight
      nine  = 9.0_dbl_kind   ! nine
!-----------------------------------------------------------------------
!  irrational numbers
!-----------------------------------------------------------------------
   REAL (KIND=dbl_kind),PARAMETER :: &
      sqrt2 = 1.414213562373095048801688724209698078569671875376948_dbl_kind, &
      sqrt3 = 1.732050807568877293527446341505872366942805253810381_dbl_kind, &
      sqrt5 = 2.236067977499789696409173668731276235440618359611526_dbl_kind, &
      e_e   = 2.718281828459045235360287471352662497757247093699959_dbl_kind, &
      pi    = 3.141592653589793238462643383279502884197169399375106_dbl_kind

   END MODULE numbers
!cccccccc1ccccccccc2ccccccccc3ccccccccc4ccccccccc5cccgtgccc6ccccccccc7cc


