module submod_distribution_input
    use submod_distribution_types
    use mod_string
    use mod_io
    use submod_grid_types
    use submod_grid_main
    use submod_environment_get
    implicit none

    public :: distribution_input

  contains

! *****************************************************************************
    subroutine distribution_input(self, in)
        type(Distribution), intent(inout) :: self
        type(File), intent(inout) :: in
        type(String) :: id

        call file_readString(in, self%name, MODE_GLOBAL)
        call file_readString(in, id, MODE_GLOBAL)
        self%grd => environment_getGrid(id)
        call file_readInt(in, self%nProcs, MODE_GLOBAL)
        call file_readIntVec(in, self%blockSize, MODE_GLOBAL)
        call file_readIntVec(in, self%gbid2lbid, MODE_GLOBAL)
        call file_readIntVec(in, self%gbid2proc, MODE_GLOBAL)
        call file_readIntVec(in, self%lbid2gbid, MODE_DISTRIBUTED)
        call file_readIntVec(in, self%sg2gbid, MODE_GLOBAL)
    end subroutine
end module submod_distribution_input
