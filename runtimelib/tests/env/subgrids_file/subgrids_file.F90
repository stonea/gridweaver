program Test
    use mod_gridweaver
    implicit none
    include 'mpif.h'
    
    integer      :: mpierr
    
    type(Subgrid)     :: subgrid_small, subgrid_large
    
    ! Initialize MPI and GridLib
    call MPI_INIT(mpierr)
    call gridweaver_initialize()
    
    ! Set up environment
    subgrid_small  = subgrid_new(     "subgrid_small",  10,    10)
    subgrid_large  = subgrid_new(     "subgrid_large",  12345, 12345)
    
    ! Save the environment
    call environment_output("env.dat")

    ! Clear the environment and print it
    call environment_clear()
    call environment_print()

    ! Load the environment and print it
    call environment_input("env.dat")
    call environment_print()

    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Test

