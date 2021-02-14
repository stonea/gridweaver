program Adjacent
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
    
    type(Subgrid) :: sg_mirrorT,  &
                     sg_mirrorB,  &
                     sg_mirrorL,  &
                     sg_mirrorR
    type(Grid)    :: gr_mirrorT,  &
                     gr_mirrorB,  &
                     gr_mirrorL,  &
                     gr_mirrorR
    
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
    sg_mirrorT = subgrid_new("sg_mirrorT", 10, 10)
    sg_mirrorB = subgrid_new("sg_mirrorB", 10, 10)
    sg_mirrorL = subgrid_new("sg_mirrorL", 10, 10)
    sg_mirrorR = subgrid_new("sg_mirrorR", 10, 10)

    gr_mirrorT = grid_new("gr_mirrorT")
    gr_mirrorB = grid_new("gr_mirrorB")
    gr_mirrorL = grid_new("gr_mirrorL")
    gr_mirrorR = grid_new("gr_mirrorR")

    ! Add subgrids to grids
    call grid_addSubgrid(gr_mirrorT, sg_mirrorT)
    call grid_addSubgrid(gr_mirrorB, sg_mirrorB)
    call grid_addSubgrid(gr_mirrorL, sg_mirrorL)
    call grid_addSubgrid(gr_mirrorR, sg_mirrorR)

    ! Tie subgrids together into grids
    call grid_mirrorT(gr_mirrorT, sg_mirrorT)
    call grid_mirrorB(gr_mirrorB, sg_mirrorB)
    call grid_mirrorL(gr_mirrorL, sg_mirrorL)
    call grid_mirrorR(gr_mirrorR, sg_mirrorR)
    
    ! Print the environment
    call environment_print()
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Adjacent
