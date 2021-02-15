module submod_schedule_types
    use mod_string
    use mod_region
    use mod_distributedVec
    use submod_distribution_main
    use mod_integerSet

    implicit none

    type Schedule
        type(String) :: name
        type(Grid), pointer :: grid
        type(Distribution), pointer :: dist
        
        type(IntegerSet)     :: msgRecvFrom
        type(VecVecInt)      :: transferRecvAtLBID
        type(VecVecRegion)   :: transferRegionRecv
        
        type(IntegerSet)     :: msgSendTo
        type(VecVecInt)      :: transferSendFromLBID
        type(VecVecRegion)   :: transferRegionSend
    end type
    type SchedulePtr
        type(Schedule), pointer :: val
    end type

end module
