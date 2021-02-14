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

    type(Subgrid) :: sg_adjacentLR_L,   &
                     sg_adjacentLR_R,   &
                     sg_adjacentRL_R,   &
                     sg_adjacentRL_L,   &
                     sg_adjacentTB_T,   &
                     sg_adjacentTB_B,   &
                     sg_adjacentBT_B,   &
                     sg_adjacentBT_T
    type(Grid)    :: gr_adjacentLR,  &
                     gr_adjacentRL,  &
                     gr_adjacentTB,  &
                     gr_adjacentBT
    
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
    sg_adjacentLR_L = subgrid_new("sg_adjacentLR_L", 10, 10)
    sg_adjacentLR_R = subgrid_new("sg_adjacentLR_R", 10, 10)
    sg_adjacentRL_R = subgrid_new("sg_adjacentRL_R", 10, 10)
    sg_adjacentRL_L = subgrid_new("sg_adjacentRL_L", 10, 10)
    sg_adjacentTB_T = subgrid_new("sg_adjacentTB_T", 10, 10)
    sg_adjacentTB_B = subgrid_new("sg_adjacentTB_B", 10, 10)
    sg_adjacentBT_B = subgrid_new("sg_adjacentBT_B", 10, 10)
    sg_adjacentBT_T = subgrid_new("sg_adjacentBT_T", 10, 10)
    gr_adjacentLR = grid_new("gr_adjacentLR")
    gr_adjacentRL = grid_new("gr_adjacentRL")
    gr_adjacentTB = grid_new("gr_adjacentTB")
    gr_adjacentBT = grid_new("gr_adjacentBT")

    ! Add subgrids to grids
    call grid_addSubgrid(gr_adjacentLR, sg_adjacentLR_L)
    call grid_addSubgrid(gr_adjacentLR, sg_adjacentLR_R)
    call grid_addSubgrid(gr_adjacentRL, sg_adjacentRL_R)
    call grid_addSubgrid(gr_adjacentRL, sg_adjacentRL_L)
    call grid_addSubgrid(gr_adjacentTB, sg_adjacentTB_T)
    call grid_addSubgrid(gr_adjacentTB, sg_adjacentTB_B)
    call grid_addSubgrid(gr_adjacentBT, sg_adjacentBT_B)
    call grid_addSubgrid(gr_adjacentBT, sg_adjacentBT_T)

    ! Tie subgrids together into grids
    call grid_placeAdjacentLR(gr_adjacentLR, sg_adjacentLR_L,     &
                              sg_adjacentLR_R)
    call grid_placeAdjacentRL(gr_adjacentRL, sg_adjacentRL_R,     &
                              sg_adjacentRL_L)
    call grid_placeAdjacentTB(gr_adjacentTB, sg_adjacentTB_T,     &
                              sg_adjacentTB_B)
    call grid_placeAdjacentBT(gr_adjacentBT, sg_adjacentBT_B,     &
                              sg_adjacentBT_T)
    
    ! Print the environment
    call environment_print()
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Adjacent
