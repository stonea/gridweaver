program Connect90deg
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

    type(Subgrid) :: sg_connectLtoT_sg,     &
                     sg_connectLtoT_sgBL,   &
                     sg_connectLtoB_sg,     &
                     sg_connectLtoB_sgTL,   &
                     sg_connectRtoT_sg,     &
                     sg_connectRtoT_sgBR,   &
                     sg_connectRtoB_sg,     &
                     sg_connectRtoB_sgTR,   &
                     sg_connectTtoL_sg,     &
                     sg_connectTtoL_sgTR,   &
                     sg_connectTtoR_sg,     &
                     sg_connectTtoR_sgTL,   &
                     sg_connectBtoL_sg,     &
                     sg_connectBtoL_sgBR,   &
                     sg_connectBtoR_sg,     &
                     sg_connectBtoR_sgBL

    type(Grid)    :: gr_connectLtoT,    &
                     gr_connectLtoB,    &
                     gr_connectRtoT,    &
                     gr_connectRtoB,    &
                     gr_connectTtoL,    &
                     gr_connectTtoR,    &
                     gr_connectBtoL,    &
                     gr_connectBtoR

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
    sg_connectLtoT_sg   = subgrid_new("sg_connectLtoT_sg",   10, 10)
    sg_connectLtoT_sgBL = subgrid_new("sg_connectLtoT_sgBL", 10, 10)
    sg_connectLtoB_sg   = subgrid_new("sg_connectLtoB_sg",   10, 10)
    sg_connectLtoB_sgTL = subgrid_new("sg_connectLtoB_sgTL", 10, 10)
    sg_connectRtoT_sg   = subgrid_new("sg_connectRtoT_sg",   10, 10)
    sg_connectRtoT_sgBR = subgrid_new("sg_connectRtoT_sgBR", 10, 10)
    sg_connectRtoB_sg   = subgrid_new("sg_connectRtoB_sg",   10, 10)
    sg_connectRtoB_sgTR = subgrid_new("sg_connectRtoB_sgTR", 10, 10)
    sg_connectTtoL_sg   = subgrid_new("sg_connectTtoL_sg",   10, 10)
    sg_connectTtoL_sgTR = subgrid_new("sg_connectTtoL_sgTR", 10, 10)
    sg_connectTtoR_sg   = subgrid_new("sg_connectTtoR_sg",   10, 10)
    sg_connectTtoR_sgTL = subgrid_new("sg_connectTtoR_sgTL", 10, 10)
    sg_connectBtoL_sg   = subgrid_new("sg_connectBtoL_sg",   10, 10)
    sg_connectBtoL_sgBR = subgrid_new("sg_connectBtoL_sgBR", 10, 10)
    sg_connectBtoR_sg   = subgrid_new("sg_connectBtoR_sg",   10, 10)
    sg_connectBtoR_sgBL = subgrid_new("sg_connectBtoR_sgBL", 10, 10)

    gr_connectLtoT = grid_new("gr_connectLtoT")
    gr_connectLtoB = grid_new("gr_connectLtoB")
    gr_connectRtoT = grid_new("gr_connectRtoT")
    gr_connectRtoB = grid_new("gr_connectRtoB")
    gr_connectTtoL = grid_new("gr_connectTtoL")
    gr_connectTtoR = grid_new("gr_connectTtoR")
    gr_connectBtoL = grid_new("gr_connectBtoL")
    gr_connectBtoR = grid_new("gr_connectBtoR")
    
    ! Add subgrids to grids
    call grid_addSubgrid(gr_connectLtoT, sg_connectLtoT_sg)
    call grid_addSubgrid(gr_connectLtoT, sg_connectLtoT_sgBL)
    call grid_addSubgrid(gr_connectLtoB, sg_connectLtoB_sg)
    call grid_addSubgrid(gr_connectLtoB, sg_connectLtoB_sgTL)
    call grid_addSubgrid(gr_connectRtoT, sg_connectRtoT_sg)
    call grid_addSubgrid(gr_connectRtoT, sg_connectRtoT_sgBR)
    call grid_addSubgrid(gr_connectRtoB, sg_connectRtoB_sg)
    call grid_addSubgrid(gr_connectRtoB, sg_connectRtoB_sgTR)
    call grid_addSubgrid(gr_connectTtoL, sg_connectTtoL_sg)
    call grid_addSubgrid(gr_connectTtoL, sg_connectTtoL_sgTR)
    call grid_addSubgrid(gr_connectTtoR, sg_connectTtoR_sg)
    call grid_addSubgrid(gr_connectTtoR, sg_connectTtoR_sgTL)
    call grid_addSubgrid(gr_connectBtoL, sg_connectBtoL_sg)
    call grid_addSubgrid(gr_connectBtoL, sg_connectBtoL_sgBR)
    call grid_addSubgrid(gr_connectBtoR, sg_connectBtoR_sg)
    call grid_addSubgrid(gr_connectBtoR, sg_connectBtoR_sgBL)
    
    ! Tie subgrids together into grids
    call grid_connectLtoT(gr_connectLtoT, sg_connectLtoT_sg, &
                          sg_connectLtoT_sgBL)
    call grid_connectLtoB(gr_connectLtoB, sg_connectLtoB_sg, &
                          sg_connectLtoB_sgTL)
    call grid_connectRtoT(gr_connectRtoT, sg_connectRtoT_sg, &
                          sg_connectRtoT_sgBR)
    call grid_connectRtoB(gr_connectRtoB, sg_connectRtoB_sg, &
                          sg_connectRtoB_sgTR)
    call grid_connectTtoL(gr_connectTtoL, sg_connectTtoL_sg, &
                          sg_connectTtoL_sgTR)
    call grid_connectTtoR(gr_connectTtoR, sg_connectTtoR_sg, &
                          sg_connectTtoR_sgTL)
    call grid_connectBtoL(gr_connectBtoL, sg_connectBtoL_sg, &
                          sg_connectBtoL_sgBR)
    call grid_connectBtoR(gr_connectBtoR, sg_connectBtoR_sg, &
                          sg_connectBtoR_sgBL)
    
    ! Print the environment
    call environment_print()
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Connect90deg
