program Wrapping
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
    
    type(Subgrid) :: sg_wrapTB,  &
                     sg_wrapLR
    type(Grid)    :: gr_wrapTB,  &
                     gr_wrapLR
    
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
    sg_wrapLR = subgrid_new("sg_wrapLR", 10, 10)
    sg_wrapTB = subgrid_new("sg_wrapTB", 10, 10)

    gr_wrapLR = grid_new("gr_wrapLR")
    gr_wrapTB = grid_new("gr_wrapTB")

    ! Add subgrids to grids
    call grid_addSubgrid(gr_wrapLR, sg_wrapLR)
    call grid_addSubgrid(gr_wrapTB, sg_wrapTB)

    ! Tie subgrids together into grids
    call grid_wrapLR(gr_wrapLR, sg_wrapLR)
    call grid_wrapTB(gr_wrapTB, sg_wrapTB)
    
    ! Print the environment
    call environment_print()
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Wrapping
