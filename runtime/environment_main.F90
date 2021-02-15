module submod_environment_main
    include 'grid.h'
    include 'distribution.h'
    include 'schedule.h'

    implicit none

    public environment_init,            &
           environment_newNeighbor,     &
           environment_newSubGrid,      &
           environment_newGrid,         &
           environment_newDistribution, &
           environment_newSchedule,     &
           environment_print,           &
           environment_printSimp,       &
           environment_input,           &

           subgrid_new,                 &
           grid_new

  contains
    subroutine environment_init()
        call linkedList_new(env%neighbors)
        call linkedList_new(env%subgrids)
        call linkedList_new(env%grids)
        call linkedList_new(env%distributions)
        call linkedList_new(env%schedules)
    end subroutine


    function environment_newNeighbor()
        type(Neighbor), pointer :: environment_newNeighbor
        type(NeighborPtr) :: n

        allocate(n%val)
        call linkedList_push(env%neighbors, transfer(n, list_mold))
        environment_newNeighbor => n%val
    end function


    function environment_newSubGrid()
        type(SubGrid), pointer :: environment_newSubGrid
        type(SubGridPtr) :: sg

        allocate(sg%val)
        call linkedList_push(env%subgrids, transfer(sg, list_mold))
        environment_newSubGrid => sg%val
    end function


    function environment_newGrid()
        type(Grid), pointer :: environment_newGrid
        type(GridPtr) :: g

        allocate(g%val)
        call linkedList_push(env%grids, transfer(g, list_mold))
        environment_newGrid => g%val
    end function


    function environment_newDistribution()
        type(Distribution), pointer :: environment_newDistribution
        type(DistributionPtr) :: d

        allocate(d%val)
        call linkedList_push(env%distributions, transfer(d, list_mold))
        environment_newDistribution => d%val
    end function


    function environment_newSchedule()
        type(Schedule), pointer :: environment_newSchedule
        type(SchedulePtr) :: sched

        allocate(sched%val)
        call linkedList_push(env%schedules, transfer(sched, list_mold))
        environment_newSchedule => sched%val
    end function


    subroutine environment_print(out)
        integer, intent(in) :: out

        type(LinkedList), pointer :: ltrav
        type(NeighborPtr)     :: neigh
        type(SubGridPtr)      :: sg
        type(GridPtr)         :: g
        type(DistributionPtr) :: d
        type(SchedulePtr)     :: sched
        
        ! Print neighbors
        ltrav => env%neighbors
        do while(associated(ltrav) .and. associated(ltrav%val))
            neigh = transfer(ltrav%val, neigh)
            call neighbor_print(neigh%val, out)
            ltrav => ltrav%next
        end do
         
        ! Print subgrids
        ltrav => env%subgrids
        do while(associated(ltrav) .and. associated(ltrav%val))
            sg = transfer(ltrav%val, sg)
            call subgrid_print(sg%val, out)
            ltrav => ltrav%next
        end do

        ! Print grids
        ltrav => env%grids
        do while(associated(ltrav) .and. associated(ltrav%val))
            g = transfer(ltrav%val, g)
            call grid_print(g%val, out)
            ltrav => ltrav%next
        end do

        ! Print distributions
        ltrav => env%distributions
        do while(associated(ltrav) .and. associated(ltrav%val))
            d = transfer(ltrav%val, d)
            call distribution_print(d%val, out)
            ltrav => ltrav%next
        end do

        ! Print schedules
        ltrav => env%schedules
        do while(associated(ltrav) .and. associated(ltrav%val))
            sched = transfer(ltrav%val, sched)
            call schedule_print(sched%val, out)
            ltrav => ltrav%next
        end do
    end subroutine


    subroutine environment_printSimp(out)
        integer, intent(in) :: out

        write(out, '(A)') "<environment>"
    end subroutine


    subroutine environment_input(in)
        type(File), intent(inout) :: in
        integer :: i, sz
        type(String) :: id
        type(Neighbor), pointer     :: nbr
        type(SubGrid), pointer      :: sg
        type(Grid), pointer         :: g
        type(Distribution), pointer :: d
        type(Schedule), pointer     :: sched

        ! Read neighbors
        call file_readInt(in, sz, MODE_GLOBAL)
        do i=1,sz
            call file_readString(in, id, MODE_GLOBAL)
            nbr => environment_newNeighbor()
            call neighbor_input(nbr, in)
        end do

        ! Read subgrids
        call file_readInt(in, sz, MODE_GLOBAL)
        do i=1,sz
            call file_readString(in, id, MODE_GLOBAL)
            sg => environment_newSubGrid()
            call subGrid_input(sg, in)
        end do

        ! Read grids
        call file_readInt(in, sz, MODE_GLOBAL)
        do i=1,sz
            call file_readString(in, id, MODE_GLOBAL)
            g => environment_newGrid()
            call grid_input(g, in)
        end do

        ! Read distributions
        call file_readInt(in, sz, MODE_GLOBAL)
        do i=1,sz
            call file_readString(in, id, MODE_GLOBAL)
            d => environment_newDistribution()
            call distribution_input(d, in)
        end do

        ! Read schedules
        call file_readInt(in, sz, MODE_GLOBAL)
        do i=1,sz
            call file_readString(in, id, MODE_GLOBAL)
            sched => environment_newSchedule()
            call schedule_input(sched, in)
        end do
    end subroutine


    !** Construct a new n by m subgrid.
    subroutine subgrid_new(self, n, m)
        type(SubGrid), pointer, intent(inout) :: self
        integer, intent(in) :: n, m

        self => environment_newSubGrid()
        self%name = newID("subgrid")
        self%ndims = 2
        allocate(self%extents(2))
        self%extents(1) = n
        self%extents(2) = m
    end subroutine


    subroutine grid_new(self)
        type(Grid), pointer, intent(inout) :: self

        self => environment_newGrid()
        self%name = newID("grid")
        self%nDims = 2
        call linkedList_new(self%subgrids)

        self%nBMaps = 0
        ! TODO: Do not hard-code a limit of border maps
        allocate(self%borderSrcRegions(100))
        allocate(self%borderSrcNeighbors(100))
        allocate(self%borderSrcSubgrids(100))
        allocate(self%borderTgtRegions(100))
        allocate(self%borderTgtNeighbors(100))
        allocate(self%borderTgtSubgrids(100))
    end subroutine
end module submod_environment_main
