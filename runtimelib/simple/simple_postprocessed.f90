# 1 "/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib/simple/simplmXoISg.F90"
# 1 "<command-line>"
# 1 "/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib/simple/simplmXoISg.F90"
program Simple
    implicit none
    include 'gridweaver_header.h'
    include 'mpif.h'
    
    integer, parameter :: GRID_WIDTH  = 10
    integer, parameter :: GRID_HEIGHT = 10
    integer      :: mpierr
    type(Neighbor)     :: n1, n2
    type(Subgrid)      :: sgA
    type(Grid)         :: g
    type(Distribution) :: dist
    type(Schedule)     :: sched
    type(DataObj)      :: data_in, data_out


    ! Initialize MPI and GridWeaver
    call MPI_INIT(mpierr)
    call gridweaver_initialize()
            
    ! Set up environment
    n1 = neighbor_new("neigh1", 1, 1)
    n2 = neighbor_new("neigh2", -1, -1)
    
    sgA = subgrid_new("sgA", GRID_WIDTH, GRID_HEIGHT)
    
    g = grid_new("g")
    call grid_addSubgrid(g, sgA)

    ! Wrap left border to right border
    call grid_addBorder(g,          0, 1,          0, GRID_HEIGHT, sgA,  &
                           GRID_WIDTH, 1, GRID_WIDTH, GRID_HEIGHT, sgA, 0)

    ! Wrap right border to left border
    call grid_addBorder(g, GRID_WIDTH+1, 1, GRID_WIDTH+1, GRID_HEIGHT, sgA,  &
                                      1, 1,            1, GRID_HEIGHT, sgA, 0)

    dist = distribution_new("dist")
    call distribution_applyFillBlock(dist, g, 5)

    sched = schedule_new("sched")
    call schedule_calculate(sched, g, dist)

    !! Print the environment
    call environment_print()
    call schedule_printFortranVersion(sched)

    !! Test stencil
    data_in  = data_new(sched)
    data_out = data_new(sched)
    call data_print(data_in, 6)
    call data_forceUpdate(data_in)
    call data_printForProcs(data_in, 6)

    call data_apply1(data_out, data_in, addTen)
    call data_print(data_out, 6)
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)

  contains    
    include 'gridweaver_footer.h'

    real function addTen(A, idxI, idxJ)
        integer, intent(in) :: idxI, idxJ
        interface
            real function A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface
        
        addTen = A(idxI, idxJ) + 10
    end function
end program Simple





