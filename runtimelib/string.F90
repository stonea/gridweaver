module mod_String
    implicit none

    type String
        integer             :: len = 0
        character(len=80)   :: val
    end type

    public :: string_new,              &
              string_init,             &
              string_init_from_cstr,   &
              string_size,             &
              string_append,           &
              string_equals,           &
              string_bcast,            &
              tstr,                    &
              cstr

  contains

! *****************************************************************************
    subroutine string_new(self)
        type(String), intent(inout) :: self

        self%val = ""
        self%len = 0
    end subroutine


    subroutine string_init(self, from)
        type(String), intent(inout) :: self
        character(*), intent(in) :: from

        self%val = from
        self%len = len(from)
    end subroutine


    subroutine string_init_from_cstr(self, from)
        type(String), intent(inout) :: self
        character, intent(in) :: from(256)
        integer :: i, j

        do i = 1,256
            if(from(i) == char(0)) then
                exit
            end if
        end do
        do j = 1,i-1
            self%val(j:j) = from(j)
        end do
        self%len = i
    end subroutine

    integer function string_size(self)
        type(String), intent(in) :: self

        string_size = self%len
    end function


    subroutine string_append(self, str)
        type(String), intent(inout) :: self
        character(*), intent(in)    :: str
        integer :: length

        self%val(self%len+1:self%len+len(str)) = str
        self%len = self%len + len(str)
    end subroutine


    logical function string_equals(self, compare)
        type(String), intent(in) :: self, compare

        string_equals = (tstr(self) .eq. tstr(compare))
    end function


    subroutine string_bcast(self)
        include 'mpif.h'
        type(String), intent(inout) :: self
        integer :: ierr
        
        call MPI_BCAST(self%len, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
        call MPI_BCAST(self%val, self%len, MPI_CHARACTER, 0, MPI_COMM_WORLD, ierr)
    end subroutine


    function tstr(self)
        type(String), intent(in) :: self
        character(self%len) :: tstr
        tstr(1:self%len)  = self%val(1:self%len)
    end function

    function cstr(self)
        type(String), intent(in) :: self
        character(self%len + 1)  :: cstr

        cstr(1:self%len) = self%val(1:self%len)
        cstr(self%len+1:self%len+1) = char(0)
    end function

end module
