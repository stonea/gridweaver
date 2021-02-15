module submod_environment_get
    use submod_environment_types
    use submod_grid_main
    use submod_distribution_main
    use submod_schedule_main
    use mod_string

    implicit none

    public environment_getNeighbor,     &
           environment_getSubGrid,      &
           environment_getGrid,         &
           environment_getDistribution, &
           environment_getSchedule

  contains

    function environment_getNeighbor(id)
        type(Neighbor), pointer  :: environment_getNeighbor
        type(String), intent(in) :: id
        type(LinkedList), pointer :: ltrav
        type(NeighborPtr) :: nbr

        ! Iterate through neighbors list until we find the one that
        ! matches the given ID
        ltrav => env%neighbors
        do while(associated(ltrav) .and. associated(ltrav%val))
            nbr = transfer(ltrav%val, nbr)
            if(string_equals(nbr%val%name, id)) then
                environment_getNeighbor => nbr%val
                return
            end if
            ltrav => ltrav%next
        end do

        environment_getNeighbor => NULL()
    end function


    function environment_getSubGrid(id)
        type(SubGrid), pointer :: environment_getSubGrid
        type(String), intent(in)      :: id
        type(LinkedList), pointer :: ltrav
        type(SubGridPtr) :: sg

        ! Iterate through neighbors list until we find the one that
        ! matches the given ID
        ltrav => env%subgrids
        do while(associated(ltrav) .and. associated(ltrav%val))
            sg = transfer(ltrav%val, sg)
            if(string_equals(sg%val%name, id)) then
                environment_getSubGrid => sg%val
                return
            end if
            ltrav => ltrav%next
        end do

        environment_getSubGrid => NULL()
    end function


    function environment_getGrid(id)
        type(Grid), pointer :: environment_getGrid
        type(String), intent(in)  :: id
        type(LinkedList), pointer :: ltrav
        type(GridPtr) :: g

        ! Iterate through neighbors list until we find the one that
        ! matches the given ID
        ltrav => env%grids
        do while(associated(ltrav) .and. associated(ltrav%val))
            g = transfer(ltrav%val, g)
            if(string_equals(g%val%name, id)) then
                environment_getGrid => g%val
                return
            end if
            ltrav => ltrav%next
        end do
        
        environment_getGrid => NULL()
    end function


    function environment_getDistribution(id)
        type(Distribution), pointer :: environment_getDistribution
        type(String), intent(in)    :: id
        type(LinkedList), pointer   :: ltrav
        type(DistributionPtr) :: d
        
        ! Iterate through neighbors list until we find the one that
        ! matches the given ID
        ltrav => env%distributions
        do while(associated(ltrav) .and. associated(ltrav%val))
            d = transfer(ltrav%val, d)
            if(string_equals(d%val%name, id)) then
                environment_getDistribution => d%val
                return
            end if
            ltrav => ltrav%next
        end do
        
        environment_getDistribution => NULL()
    end function


    function environment_getSchedule(id)
        type(Schedule), pointer :: environment_getSchedule
        type(String), intent(in)    :: id
        type(LinkedList), pointer   :: ltrav
        type(SchedulePtr) :: sched
        
        ! Iterate through neighbors list until we find the one that
        ! matches the given ID
        ltrav => env%schedules
        do while(associated(ltrav) .and. associated(ltrav%val))
            sched = transfer(ltrav%val, sched)

            if(string_equals(sched%val%name, id)) then
                environment_getSchedule => sched%val
                return
            end if
            ltrav => ltrav%next
        end do
        
        environment_getSchedule => NULL()
    end function
end module submod_environment_get
