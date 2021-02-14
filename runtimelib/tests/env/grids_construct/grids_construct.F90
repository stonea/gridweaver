program Test
    use mod_gridweaver
    implicit none
    include 'mpif.h'
    
    integer       :: mpierr
    
    type(Subgrid) :: sg
    type(Grid)    :: gr
    
    ! Initialize MPI and GridLib
    call MPI_INIT(mpierr)
    call gridweaver_initialize()
    
    ! Set up environment
    sg = subgrid_new("subgrid", 10, 10)
    gr = grid_new("grid")
    call grid_addSubgrid(gr, sg)
    call grid_addBorder(gr,  0, 0,  0, 10, sg, 10, 1, 10, 10, sg, 0)
    
    ! Print the environment
    call environment_print()

    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Test

