module mod_integerList
    implicit none

    ! TODO: Allocate dynamically instead of statically
    type IntegerList
        integer values(20)
        integer :: nIntegers = 0
    end type

    public integerList_new,      &
           integerList_add,      &
           integerList_printSimp

 contains
    subroutine integerList_new(self)
        type(IntegerList), intent(inout) :: self

        self%nIntegers = 0
    end subroutine

    subroutine integerList_add(self, val)
        type(IntegerList), intent(inout) :: self
        integer, intent(in) :: val
        
        self%nIntegers = self%nIntegers + 1
        self%values(self%nIntegers) = val
    end subroutine

    subroutine integerList_printSimp(self, out)
        type(IntegerList), intent(inout) :: self
        integer, intent(in) :: out
        !`--
        integer :: i

        write(out, '(A,$)') "["
        do i=1,self%nIntegers
            if(i /= 1) write(out, '(A,$)') ", "
            write(out, '(I0,$)'), self%values(i)
        end do
        write(out, '(A,$)') "]"
    end subroutine
end module
