module mod_regionSet
    use mod_region
    implicit none

    ! TODO: Allocate dynamically instead of statically
    type RegionSet
        type(Region) regions(20)
        integer :: nRegions = 0
    end type

    public regionSet_new,      &
           regionSet_add,      &
           regionSet_remove,   &
           regionSet_printSimp

 contains
    subroutine regionSet_new(self)
        type(RegionSet), intent(inout) :: self

        self%nRegions = 0
    end subroutine

    subroutine regionSet_add(self, reg)
        type(RegionSet), intent(inout) :: self
        type(Region), intent(in) :: reg
        
        self%nRegions = self%nRegions + 1
        self%regions(self%nRegions) = reg
    end subroutine


    subroutine regionSet_remove(self, reg)
        type(RegionSet), intent(inout) :: self
        type(Region), intent(in) :: reg
        integer :: i, j

        ! Iterate through regions until you find what should be removed
        do i=1,self%nRegions
            if(region_eq(self%regions(i), reg)) then
                ! Shift all regions above the one thats been found down
                do j=i+1,self%nRegions
                    self%regions(j-1) = self%regions(j)
                end do
                self%nRegions = self%nRegions -1

                return
            end if
        end do
    end subroutine

    subroutine regionSet_printSimp(self, out)
        type(RegionSet), intent(inout) :: self
        integer, intent(in) :: out
        !`--
        integer :: i

        write(out, '(A,$)') "["
        do i=1,self%nRegions
            if(i /= 1) write(out, '(A,$)') ", "
            call region_printSimp(self%regions(i), out)
        end do
        write(out, '(A,$)') "]"
    end subroutine
end module
