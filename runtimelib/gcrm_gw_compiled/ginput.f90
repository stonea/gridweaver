module ginput

! Derived from Design patterns and Fortran 90/95 Arjen Markus, WL Delft Hydraulics

   use strings
   use parallel_include

   implicit none

   interface gin_get_valid_value  ! Gets value of various types
                                  ! call gin_get_valid_value("key",myvar,vmin,vmax)

      module procedure get_valid_int
      module procedure get_valid_intd
      module procedure get_valid_real
      module procedure get_valid_reald

   end interface

   interface gin_get_value  ! Gets value of various types
                            ! call gin_get_value("key",myvar)
                            ! string values should be declared (len=value_length)

      module procedure get_int
      module procedure get_intd
      module procedure get_real
      module procedure get_reald
      module procedure get_line
      module procedure get_bool

   end interface


   integer, parameter :: key_length   = 30
   integer, parameter :: value_length = 120

   !
   ! Public functions/subroutines
   !

   public :: gin_load_inputs, gin_echo


   private :: get_int,get_intd, get_real, get_reald, get_line, get_bool
   private :: get_valid_int,get_valid_intd, get_valid_real, get_valid_reald

   integer, parameter, private :: max_params   = 100
   integer, private :: num_params = 0
   logical, private :: init_done = .false.

   type KEY_VALUE

      character(len=key_length)  :: key

      character(len=value_length) :: value

   end type KEY_VALUE 

   type (KEY_VALUE), private :: dict_data(max_params)
   

contains 

subroutine gin_load_inputs(filename )
   implicit none
   character(len=*)            :: filename

   logical :: file_exists
   integer :: fid = 18
   integer :: ios = 0
   integer :: linecnt = 1
   character(len=key_length+value_length) line
   character(len=key_length) key
   
   if (init_done .eqv. .false.) then
      INQUIRE(FILE=filename,EXIST=file_exists)
      if (file_exists) then
         init_done = .true.
         write(6,"(A20,A80)") "Loading inputs from ",filename
         OPEN (fid, FILE =filename,status="OLD") 
         do while (ios .eq. 0) 
            call readline(fid,line,ios)
            if (ios .eq. 0) then
               call compact(line)
               call split(line,"= "//achar(9),key)
               dict_data(linecnt)%key = trim(key)
               dict_data(linecnt)%value = trim(line)

               linecnt = linecnt + 1
           endif 
         end do
         CLOSE(fid)
         num_params = linecnt - 1

         
      else
         write(6, "(A36,A80)") "WARNING: Input file does not exist: ",filename
      endif
      call gin_echo()
   endif

end subroutine 

subroutine gin_broadcast()
   integer :: idx = 1     ! BCAST loop
   integer :: ierr = 0    ! For MPI calls


   integer :: rnk
   call MPI_COMM_RANK(MPI_COMM_WORLD,rnk,ierr)

   idx = 1
   call MPI_BCAST(num_params, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
   do 
      if (idx .gt. num_params) exit
      call MPI_BCAST(dict_data(idx)%key, key_length, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
      call MPI_BCAST(dict_data(idx)%value, value_length, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
      idx = idx + 1
   end do

end subroutine gin_broadcast


! Pull the line from the dictionary
subroutine get_line( key, value )

   implicit none
   character(len=*) :: key
   character(len=*) :: value

   character(len=key_length) :: k
   integer :: idx
   logical :: found

   idx = 1
   do 
       if (idx .gt. num_params) exit
       k = dict_data(idx)%key
       if (key .eq. k) then
          value = trim(dict_data(idx)%value)
          exit
       endif
       idx = idx + 1
   end do

end subroutine get_line

subroutine gin_echo( )
   implicit none
   character(len=key_length) :: k
   character(len=value_length) :: v
   integer :: idx

   if (num_params .gt. 0) then

      write(6,*) "Input options:"
      idx = 1
      do 
          if (idx .gt. num_params) exit
          k = trim(dict_data(idx)%key)
          v = trim(dict_data(idx)%value)
          idx = idx + 1
          write(6,*) trim(k), " = ", trim(v)
      end do
      write (6,*) ""
   endif
end subroutine gin_echo

! Interpret the data associated with key as a boolean string.  Accepts true/false any case
subroutine get_bool( key, v )

   implicit none
   character(len=*)            :: key
   logical :: v
  
   character(len=value_length) :: line = ""

   call get_line(key,line)
   if (line .ne. "") then
      line = lowercase(line)
      if (line .eq. "true") then
         v = .TRUE.
      else if (line .eq. "false") then
         v = .FALSE.
      endif
   endif

end subroutine get_bool


subroutine get_valid_int( key, v , vmin, vmax)
   implicit none
   character(len=*)            :: key
   integer(4) :: v,vmin,vmax
   call get_int(key,v)
   if (v .le. vmin .or. v .ge. vmax) then
      write(6,*) "Invalid value found for '",key,"'. Found ",v," Expected range:",vmin,vmax
   endif
end subroutine get_valid_int

! Interpret the data associated with key as an integer
subroutine get_int( key, v )

   implicit none
   character(len=*)            :: key
   integer(4) :: v

   integer :: tmpv,ios
   character(len=value_length) :: line

   line = ""
   call get_line(key,line)
   if (line .ne. "") then
      call value(line,tmpv,ios)
      if (ios .eq. 0) v = tmpv
   endif

end subroutine get_int

subroutine get_valid_intd( key, v , vmin, vmax)
   implicit none
   character(len=*)            :: key
   integer(8) :: v,vmin,vmax
   call gin_get_value(key,v)
   if (v .le. vmin .or. v .ge. vmax) then
      write(6,*) "Invalid value found for '",key,"'. Found ",v," Expected range:",vmin,vmax
   endif
end subroutine get_valid_intd

subroutine get_intd( key, v )

   implicit none
   character(len=*)            :: key
   integer(8) :: v

   integer(8) :: tmpv
   integer(4) :: ios
   character(len=value_length) :: line

   line = ""
   call get_line(key,line)
   if (line .ne. "") then
      call value(line,tmpv,ios)
      if (ios .eq. 0) v = tmpv
   endif

end subroutine get_intd


subroutine get_valid_real( key, v , vmin, vmax)
   implicit none
   character(len=*)            :: key
   real(4) :: v,vmin,vmax
   call gin_get_value(key,v)
   if (v .le. vmin .or. v .ge. vmax) then
      write(6,*) "Invalid value found for '",key,"'. Found ",v," Expected range:",vmin,vmax
   endif
end subroutine get_valid_real

! Interpret the data associated with key as a float
subroutine get_real( key, v )

   implicit none
   character(len=*)            :: key
   real(4) :: v
   
   real(4) :: tmpv
   integer(4) :: ios
   character(len=value_length) :: line

   line = ""
   call get_line(key,line)
   if (line .ne. "") then
      call value(line,tmpv,ios)
      if (ios .eq. 0) v = tmpv
   endif

end subroutine get_real

subroutine get_valid_reald( key, v , vmin, vmax)
   implicit none
   character(len=*)            :: key
   real(8) :: v,vmin,vmax
   call gin_get_value(key,v)
   if (v .le. vmin .or. v .ge. vmax) then
      write(6,*) "Invalid value found for '",key,"'. Found ",v," Expected range:",vmin,vmax
   endif
end subroutine get_valid_reald

! Interpret the data associated with key as a float
subroutine get_reald( key, v )

   implicit none
   character(len=*)            :: key
   real(8) :: v
   
   real(8) :: tmpv
   integer(4) :: ios
   character(len=value_length) :: line

   line = ""
   call get_line(key,line)
   if (line .ne. "") then
      call value(line,tmpv,ios)
      if (ios .eq. 0) v = tmpv
   endif

end subroutine get_reald

end module ginput 

