program Tripole
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
    
    type(Subgrid) :: sg_tripole
    type(Grid)    :: gr_tripole
    
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
    sg_tripole = subgrid_new("sg_tripole", 10, 10)
    gr_tripole = grid_new("gr_tripole")

    ! Add subgrids to grids
    call grid_addSubgrid(gr_tripole, sg_tripole)
    call grid_addSubgrid(gr_tripole, sg_tripole)

    ! Tie subgrids together into grids
    call grid_wrapLR(gr_tripole, sg_tripole)
    call grid_foldT(gr_tripole, sg_tripole)
    
    ! Print the environment
    call environment_print()
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Tripole
