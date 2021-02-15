module mod_blockSet
    implicit none

    ! TODO: Allocate dynamically instead of statically
    type BlockSet
        integer blocks(20)
        integer :: nBlocks = 0
    end type

    public blockSet_new,    &
           blockSet_add,    &
           blockSet_remove, &
           blockSet_printSimp

 contains
    subroutine blockSet_new(self)
        type(BlockSet), intent(inout) :: self

        self%nBlocks = 0
    end subroutine

    subroutine blockSet_add(self, gbid)
        type(BlockSet), intent(inout) :: self
        integer, intent(in) :: gbid
        
        self%nBlocks = self%nBlocks + 1
        self%blocks(self%nBlocks) = gbid
    end subroutine


    subroutine blockSet_remove(self, gbid)
        type(BlockSet), intent(inout) :: self
        integer, intent(in) :: gbid
        integer :: i, j

        ! Iterate through blocks until you find what should be removed
        do i=1,self%nBlocks
            if(self%blocks(i) == gbid) then
                ! Shift all blocks above the one thats been found down
                do j=i+1,self%nBlocks
                    self%blocks(j-1) = self%blocks(j)
                end do
                self%nBlocks = self%nBlocks - 1

                return
            end if
        end do
    end subroutine


    subroutine blockSet_printSimp(self, out)
        type(BlockSet), intent(inout) :: self
        integer, intent(in) :: out
        !`--
        integer :: i

        write(out, '(A,$)') "["
        do i=1,self%nBlocks
            if(i /= 1) write(out, '(A,$)') ", "
            write(out, '(I2,$)') self%blocks(i)
        end do
        write(out, '(A,$)') "]"
    end subroutine
end module
