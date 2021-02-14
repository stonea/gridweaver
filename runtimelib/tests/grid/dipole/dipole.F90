program Dipole
    use mod_gridweaver
    implicit none
    include 'mpif.h'
    
    integer       :: mpierr

    type(Neighbor) :: neigh_n,      &
                      neigh_ne,     &
                      neigh_e,      &
                      neigh_se,     &
                      neigh_s,      &
                      neigh_sw,     &
                      neigh_w,      &
                      neigh_nw
    
    type(Subgrid) :: sg_dipole
    type(Grid)    :: gr_dipole
    
    ! Initialize MPI and GridLib
    call MPI_INIT(mpierr)
    call gridweaver_initialize()

    ! Create neighbors
    neigh_n  = neighbor_new("north",      0,   1)
    neigh_ne = neighbor_new("north_east", 1,   1)
    neigh_e  = neighbor_new("east",       1,   0)
    neigh_se = neighbor_new("south_east", 1,  -1)
    neigh_s  = neighbor_new("south",      0,  -1)
    neigh_sw = neighbor_new("south_west", -1, -1)
    neigh_w  = neighbor_new("west",       -1,  0)
    neigh_nw = neighbor_new("north_west", -1,  1)
    
    ! Create subgrids and grids
    sg_dipole = subgrid_new("sg_dipole", 10, 10)
    gr_dipole = grid_new("gr_dipole")

    ! Add subgrids to grids
    call grid_addSubgrid(gr_dipole, sg_dipole)
    call grid_addSubgrid(gr_dipole, sg_dipole)

    ! Tie subgrids together into grids
    call grid_wrapLR(gr_dipole, sg_dipole)
    
    ! Print the environment
    call environment_print()
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Dipole
