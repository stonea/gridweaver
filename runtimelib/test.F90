program Test
    include 'gridweaver.h'
    implicit none
    include 'mpif.h'
    
    integer      :: mpierr
    
    type(Neighbor)     :: n1, n2
    type(Subgrid)      :: sgA
    type(Grid)         :: g
    type(Distribution) :: dist
    type(Schedule)     :: sched
    type(Grid)         :: g2
    type(DataObj)      :: x

    ! Initialize MPI and GridLib
    call MPI_INIT(mpierr)
    call gridweaver_initialize()
    
    ! Set up environment
    n1 = neighbor_new("neigh1", 1, 1)
    n2 = neighbor_new("neigh2", -1, -1)
    
    sgA = subgrid_new("sgA", 10, 10)
    
    g = grid_new("g")
    call grid_addSubgrid(g, sgA)
    call grid_addBorder(g, 0, 1, 0, 10, sgA, 10, 1, 10, 10, sgA, 0)
    
    dist = distribution_new("dist")
    call distribution_applyBlockCyclic(dist, g, 5, 5)
    
    sched = schedule_new("sched")
    call schedule_calculate(sched, g, dist)

    ! Print the environment
    call environment_print()
    call schedule_printFortranVersion(sched)

    ! Test stencil
    x = data_new(sched)
    call data_print(x, 6)
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
    
  contains
    integer function addTen(A, i, j)
        integer, intent(in) :: i, j
        interface
            integer function A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface
        
        addTen = A(i, j) + 10
    end function
end program Test
