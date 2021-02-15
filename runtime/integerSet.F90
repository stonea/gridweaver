module mod_integerSet
    implicit none

    ! TODO: Allocate dynamically instead of statically
    type IntegerSet
        integer integers(100)
        integer :: nIntegers = 0
    end type

    public integerSet_new,      &
           integerSet_add,      &
           integerSet_remove,   &
           integerSet_printSimp

 contains
    subroutine integerSet_new(self)
        type(IntegerSet), intent(inout) :: self

        self%nIntegers = 0
    end subroutine

    ! Return index of added element
    integer function integerSet_add(self, val)
        type(IntegerSet), intent(inout) :: self
        integer, intent(in) :: val
        integer :: i

        ! Find if integer if it is already added
        do i=1,self%nIntegers
            if(self%integers(i) == val) then
                integerSet_add = i
                return
            end if
        end do
        
        self%nIntegers = self%nIntegers + 1
        self%integers(self%nIntegers) = val
        integerSet_add = self%nIntegers
    end function


    subroutine integerSet_remove(self, val)
        type(IntegerSet), intent(inout) :: self
        integer, intent(in) :: val
        integer :: i, j

        ! Iterate through integers until you find what should be removed
        do i=1,self%nIntegers
            if(self%integers(i) == val) then
                ! Shift all integers above the one thats been found down
                do j=1+1,self%nIntegers
                    self%integers(j-1) = self%integers(j)
                end do
                self%nIntegers = self%nIntegers -1

                return
            end if
        end do
    end subroutine

    subroutine integerSet_printSimp(self, out)
        type(IntegerSet), intent(inout) :: self
        integer, intent(in) :: out
        !`--
        integer :: i

        write(out, '(A,$)') "["
        do i=1,self%nIntegers
            if(i /= 1) write(out, '(A,$)') ", "
            write(out, '(I0,$)') self%integers(i)
        end do
        write(out, '(A,$)') "]"
    end subroutine
end module
