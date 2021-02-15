module submod_grid_input
    use submod_grid_types
    use mod_io
    use submod_environment_get
    implicit none

    public :: neighbor_input,       &
              neighborvec_input,    &
              neighborptrvec_input

    public :: subgrid_input,       &
              subgridPtrVec_input

    public :: grid_input

  contains
! *****************************************************************************

    subroutine neighbor_input(self, in)
        type(Neighbor), intent(inout) :: self
        type(File), intent(inout) :: in

        call file_readString(in, self%name, MODE_GLOBAL)
        call file_readIntVec(in, self%offsets, MODE_GLOBAL)
    end subroutine


    subroutine neighborVec_input(self, in)
        type(Neighbor), allocatable, intent(inout) :: self(:)
        type(File), intent(inout) :: in
        integer :: i, sz
        
        call file_readInt(in, sz, MODE_GLOBAL)
        allocate(self(sz))
        do i=1,sz
            call neighbor_input(self(i), in)
        end do
    end subroutine


    subroutine neighborPtrVec_input(self, in)
        type(NeighborPtr), allocatable :: self(:)
        type(File), intent(inout) :: in
        integer :: i, sz
        type(String) :: id

        call file_readInt(in, sz, MODE_GLOBAL)
        allocate(self(sz))
        do i=1,sz
            call file_readString(in, id, MODE_GLOBAL)
            self(i)%val => environment_getNeighbor(id)
        end do
    end subroutine


    subroutine subgrid_input(self, in)
        type(SubGrid), intent(inout) :: self
        type(File), intent(inout) :: in

        call file_readString(in, self%name, MODE_GLOBAL)
        call file_readInt(in, self%nDims, MODE_GLOBAL)
        call file_readIntVec(in, self%extents, MODE_GLOBAL)
        call neighborPtrVec_input(self%neighbors, in)
    end subroutine


    subroutine subgridPtrVec_input(self, in)
        type(SubgridPtr), allocatable :: self(:)
        type(File), intent(inout) :: in
        integer :: i, sz
        type(String) :: id

        call file_readInt(in, sz, MODE_GLOBAL)
        allocate(self(sz))
        do i=1,sz
            call file_readString(in, id, MODE_GLOBAL)
            self(i)%val => environment_getSubGrid(id)
        end do
    end subroutine


    subroutine grid_input(self, in)
        type(Grid), intent(inout) :: self
        type(File), intent(inout) :: in

        call file_readString(in, self%name, MODE_GLOBAL)
        call file_readInt(in, self%nDims, MODE_GLOBAL)
        ! TODO: Input subgrids!!!
        !call subgridPtrVec_input(self%subgrids, in)
        call file_readRegionVec(in, self%borderSrcRegions, MODE_GLOBAL)
        call neighborPtrVec_input(self%borderSrcNeighbors, in)
        call subgridPtrVec_input(self%borderSrcSubgrids, in)
        call file_readRegionVec(in, self%borderTgtRegions, MODE_GLOBAL)
        call neighborPtrVec_input(self%borderTgtNeighbors, in)
        call subgridPtrVec_input(self%borderTgtSubgrids, in)
    end subroutine
end module submod_grid_input
