module Func
  contains
    integer function sumNeighbors(neighs)
        integer, intent(in) :: neighs(:)
        sumNeighbors = neighs(1) + neighs(2) + neighs(3) + neighs(4)
    end function
end module

program Stencil
    use Func
    implicit none
    include 'mpif.h'
    include 'gridlib.h'

    type(String) :: filename, gridname
    type(File)   :: f
    integer      :: i, mpierr, ierr
    type(String) :: grid_ID, dist_ID, sched_ID
    type(Data)  :: gridData, modifiedData

    type(Grid), pointer :: gr
    type(Distribution), pointer :: ds

    ! Initialize MPI
    call MPI_INIT(mpierr)
    
    ! Read metadata from input-file
    call string_init(filename, "input.dat")
    call file_open(f, filename)
 
    call environment_init()
    call environment_input(f)
 
    call environment_print(6)
 
    call file_close(f)
 
    ! Create a data object and print it out
    call string_init(grid_ID, "g")
    call string_init(dist_ID, "blockedDist")
    call string_init(sched_ID, "sched")
    call data_new(gridData,                             &
                  environment_getGrid(grid_ID),         &
                  environment_getSchedule(sched_ID),    &
                  environment_getDistribution(dist_ID))
    call data_print(gridData, 6)
 
    do i=0,numRanks()-1
        call data_printForProc(gridData, i, 6)
        if(myRank() == 0) &
            print *, ""
    end do
 
    call MPI_BARRIER(MPI_COMM_WORLD, ierr)
 
    ! Update the data's halo and print it out again
    !call data_forceUpdate(gridData)
    !if(myRank() == 0) then
    !    print *, "******************************************"
    !    print *, "After updating the halo:"
    !    print *, "******************************************"
    !endif
    !do i=0,numRanks()-1
    !    call data_printForProc(gridData, i, 6)
    !    if(myRank() == 0) &
    !        print *, ""
    !end do
 
    ! Apply a summing stencil
    modifiedData = data_apply(gridData, sumNeighbors)
 
    if(myRank() == 0) then
        print *, "******************************************"
        print *, "After applying a summing stencil: "
        print *, "******************************************"
    end if
 
    call data_print(modifiedData, 6)
 
    ! Exit
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program
