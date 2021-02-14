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
    
    ! This tests folding for grids of even width and height (10x10) and
    ! odd width and height (9x9).
    type(Subgrid) :: sg_foldT_even,  &
                     sg_foldB_even,  &
                     sg_foldL_even,  &
                     sg_foldR_even,  &
                     sg_foldT_odd,   &
                     sg_foldB_odd,   &
                     sg_foldL_odd,   &
                     sg_foldR_odd
    type(Grid)    :: gr_foldT_even,  &
                     gr_foldB_even,  &
                     gr_foldL_even,  &
                     gr_foldR_even,  &
                     gr_foldT_odd,   &
                     gr_foldB_odd,   &
                     gr_foldL_odd,   &
                     gr_foldR_odd
    
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
    sg_foldT_even = subgrid_new("sg_foldT_even", 10, 10)
    sg_foldB_even = subgrid_new("sg_foldB_even", 10, 10)
    sg_foldL_even = subgrid_new("sg_foldL_even", 10, 10)
    sg_foldR_even = subgrid_new("sg_foldR_even", 10, 10)

    sg_foldT_odd = subgrid_new("sg_foldT_odd", 9, 9)
    sg_foldB_odd = subgrid_new("sg_foldB_odd", 9, 9)
    sg_foldL_odd = subgrid_new("sg_foldL_odd", 9, 9)
    sg_foldR_odd = subgrid_new("sg_foldR_odd", 9, 9)

    gr_foldT_even = grid_new("gr_foldT_even")
    gr_foldB_even = grid_new("gr_foldB_even")
    gr_foldL_even = grid_new("gr_foldL_even")
    gr_foldR_even = grid_new("gr_foldR_even")

    gr_foldT_odd = grid_new("gr_foldT_odd")
    gr_foldB_odd = grid_new("gr_foldB_odd")
    gr_foldL_odd = grid_new("gr_foldL_odd")
    gr_foldR_odd = grid_new("gr_foldR_odd")

    ! Add subgrids to grids
    call grid_addSubgrid(gr_foldT_even, sg_foldT_even)
    call grid_addSubgrid(gr_foldB_even, sg_foldB_even)
    call grid_addSubgrid(gr_foldL_even, sg_foldL_even)
    call grid_addSubgrid(gr_foldR_even, sg_foldR_even)

    call grid_addSubgrid(gr_foldT_odd, sg_foldT_odd)
    call grid_addSubgrid(gr_foldB_odd, sg_foldB_odd)
    call grid_addSubgrid(gr_foldL_odd, sg_foldL_odd)
    call grid_addSubgrid(gr_foldR_odd, sg_foldR_odd)

    ! Tie subgrids together into grids
    call grid_foldT(gr_foldT_even, sg_foldT_even)
    call grid_foldB(gr_foldB_even, sg_foldB_even)
    call grid_foldL(gr_foldL_even, sg_foldL_even)
    call grid_foldR(gr_foldR_even, sg_foldR_even)

    call grid_foldT(gr_foldT_odd, sg_foldT_odd)
    call grid_foldB(gr_foldB_odd, sg_foldB_odd)
    call grid_foldL(gr_foldL_odd, sg_foldL_odd)
    call grid_foldR(gr_foldR_odd, sg_foldR_odd)
    
    ! Print the environment
    call environment_print()
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Adjacent
