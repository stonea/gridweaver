program ConnectSideBySide
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

    type(Subgrid) :: sg_connectTtoB_sg1,    &
                     sg_connectTtoB_sg2,    &
                     sg_connectRtoL_sg1,    &
                     sg_connectRtoL_sg2,    &
                     sg_connectBtoT_sg1,    &
                     sg_connectBtoT_sg2,    &
                     sg_connectLtoR_sg1,    &
                     sg_connectLtoR_sg2
    type(Grid)    :: gr_connectTtoB,        &
                     gr_connectRtoL,        &
                     gr_connectBtoT,        &
                     gr_connectLtoR

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
    sg_connectTtoB_sg1 = subgrid_new("sg_connectTtoB_sg1", 10, 10)
    sg_connectTtoB_sg2 = subgrid_new("sg_connectTtoB_sg2", 10, 10)
    sg_connectRtoL_sg1 = subgrid_new("sg_connectRtoL_sg1", 10, 10)
    sg_connectRtoL_sg2 = subgrid_new("sg_connectRtoL_sg2", 10, 10)
    sg_connectBtoT_sg1 = subgrid_new("sg_connectBtoT_sg1", 10, 10)
    sg_connectBtoT_sg2 = subgrid_new("sg_connectBtoT_sg2", 10, 10)
    sg_connectLtoR_sg1 = subgrid_new("sg_connectLtoR_sg1", 10, 10)
    sg_connectLtoR_sg2 = subgrid_new("sg_connectLtoR_sg2", 10, 10)
    
    gr_connectTtoB = grid_new("gr_connectTtoB")
    gr_connectRtoL = grid_new("gr_connectRtoL")
    gr_connectBtoT = grid_new("gr_connectBtoT")
    gr_connectLtoR = grid_new("gr_connectLtoR")
    
    ! Add subgrids to grids
    call grid_addSubgrid(gr_connectTtoB, sg_connectTtoB_sg1)
    call grid_addSubgrid(gr_connectTtoB, sg_connectTtoB_sg2)
    call grid_addSubgrid(gr_connectRtoL, sg_connectRtoL_sg1)
    call grid_addSubgrid(gr_connectRtoL, sg_connectRtoL_sg2)
    call grid_addSubgrid(gr_connectBtoT, sg_connectBtoT_sg1)
    call grid_addSubgrid(gr_connectBtoT, sg_connectBtoT_sg2)
    call grid_addSubgrid(gr_connectLtoR, sg_connectLtoR_sg1)
    call grid_addSubgrid(gr_connectLtoR, sg_connectLtoR_sg2)
    
    ! Tie subgrids together into grids
    call grid_connectTtoB(gr_connectTtoB, sg_connectTtoB_sg1,   &
                          sg_connectTtoB_sg2)
    call grid_connectRtoL(gr_connectRtoL, sg_connectRtoL_sg1,   &
                          sg_connectRtoL_sg2)
    call grid_connectBtoT(gr_connectBtoT, sg_connectBtoT_sg1,   &
                          sg_connectBtoT_sg2)
    call grid_connectLtoR(gr_connectLtoR, sg_connectLtoR_sg1,   &
                          sg_connectLtoR_sg2)
        
    ! Print the environment
    call environment_print()
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program ConnectSideBySide
