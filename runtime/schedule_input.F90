module submod_schedule_input
    use submod_schedule_types
    use submod_environment_get
    use mod_io
    use mod_string

    implicit none

    public schedule_input,       &
           schedule_newFromFile

  contains
    subroutine schedule_input(self, f)
        type(Schedule), intent(inout) :: self
        type(File), intent(in)        :: f
        type(String) :: id

        call file_readString(f, self%name, MODE_GLOBAL)
        call file_readString(f, id, MODE_GLOBAL)
        self%grid => environment_getGrid(id)
        call file_readString(f, id, MODE_GLOBAL)
        self%dist => environment_getDistribution(id)

        call file_readIntSet(f, self%msgRecvFrom, MODE_DISTRIBUTED)
        call file_readIntVecVec(f, self%transferRecvAtLBID, MODE_DISTRIBUTED)
        call file_readRegionVecVec(f, self%transferRegionRecv, MODE_DISTRIBUTED)
        
        call file_readIntSet(f, self%msgSendTo, MODE_DISTRIBUTED)
        call file_readIntVecVec(f, self%transferSendFromLBID, MODE_DISTRIBUTED)
        call file_readRegionVecVec(f, self%transferRegionSend, MODE_DISTRIBUTED)
    end subroutine


    subroutine schedule_newFromFile(self, filename)
        type(Schedule), intent(inout) :: self
        type(String), intent(in)      :: filename
        type(File)  :: f

        call file_open(f, filename)
        call schedule_input(self, f)
        call file_close(f)
    end subroutine
end module
