program Cubed_sphere
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

    type(Subgrid) :: sg_back,  &
                     sg_left,  &
                     sg_top,   &
                     sg_right, &
                     sg_btm,   &
                     sg_front
                     
    type(Grid)    :: gr

    integer :: N = 3
    
    ! Initialize MPI and GridLib
    call MPI_INIT(mpierr)
    call gridweaver_initialize()
    
    ! Create neighbors
    neigh_n  = neighbor_new("north",      0,   1)
    neigh_e  = neighbor_new("east",       1,   0)
    neigh_s  = neighbor_new("south",      0,  -1)
    neigh_w  = neighbor_new("west",       -1,  0)
    
    ! Create subgrids and grids
    sg_back  = subgrid_new("sg_back",  N, N)
    sg_left  = subgrid_new("sg_left",  N, N)
    sg_top   = subgrid_new("sg_top",   N, N)
    sg_right = subgrid_new("sg_right", N, N)
    sg_btm   = subgrid_new("sg_btm",   N, N)
    sg_front = subgrid_new("sg_front", N, N)
    gr = grid_new("gr")
    
    ! Add subgrids to grids
    call grid_addSubgrid(gr, sg_back)
    call grid_addSubgrid(gr, sg_left)
    call grid_addSubgrid(gr, sg_top)
    call grid_addSubgrid(gr, sg_right)
    call grid_addSubgrid(gr, sg_btm)
    call grid_addSubgrid(gr, sg_front)
    
    ! connect left and top
    call grid_addBorder(gr, N+1, 1, N+1, N, sg_left, &
                              1, 1,   1, N, sg_top, 0)
    call grid_addBorder(gr,   0, 1,   0, N, sg_top,  &
                              N, 1,   N, N, sg_left, 0)

    ! connect top and right
    call grid_addBorder(gr, N+1, 1, N+1, N, sg_top,   &
                              1, 1,   1, N, sg_right, 0)
    call grid_addBorder(gr,   0, 1,   0, N, sg_right, &
                              N, 1,   N, N, sg_top, 0)

    ! connect right and bottom
    call grid_addBorder(gr, N+1, 1, N+1, N, sg_top,   &
                              1, 1,   1, N, sg_right, 0)
    call grid_addBorder(gr,   0, 1,   0, N, sg_right, &
                              N, 1,   N, N, sg_top,   0)
    
    ! connect bottom and left
    call grid_addBorder(gr, N+1, 1, N+1, N, sg_left, &
                              1, 1,   1, N, sg_btm,  0)
    call grid_addBorder(gr,   0, 1,   0, N, sg_btm,  &
                              N, 1,   N, N, sg_left, 0)

    !---------------------------------

    ! connect back and top
    call grid_addBorder(gr, 1,   0, N,   0, sg_back, &
                            1,   N, N,   N, sg_top,  0)
    call grid_addBorder(gr, 1, N+1, N, N+1, sg_top,  &
                            1,   1, N,   1, sg_back, 0)

    ! connect top and front
    call grid_addBorder(gr, 1,   0, N,   0, sg_top,   &
                            1,   N, N,   N, sg_front, 0)
    call grid_addBorder(gr, 1, N+1, N, N+1, sg_front, &
                            1,   1, N,   1, sg_top,   0)

    !---------------------------------

    ! connect left and back
    call grid_addBorder(gr, 1, N+1,  N, N+1, sg_left, &
                            1,   1,  1,   N, sg_back, 1)
    call grid_addBorder(gr, 1, N+1,  N, N+1, sg_back, &
                            1,   1,  N,   1, sg_left, 1)
    
    ! connect back and right
    call grid_addBorder(gr, N+1,   1, N+1,   N, sg_back,  &
                              1,   N,   N,   N, sg_right, 1)
    call grid_addBorder(gr,   1, N+1,   N, N+1, sg_right, &
                              N,   1,   N,   N, sg_back,  1)

    ! connect right and front
    call grid_addBorder(gr,  1,  0, N,   0, sg_right, &
                             N,  1, N,   N, sg_front, 1)
    call grid_addBorder(gr, N+1, 1, N+1, N, sg_front, &
                             1,  1, N,   1, sg_right, 1)
    
    ! connect front and left
    call grid_addBorder(gr,  0,  1, 0, N, sg_front, &
                             1,  1, N, 1, sg_left,  1)
    call grid_addBorder(gr,  1,  0, N, 0, sg_left,  &
                             1,  1, 1, N, sg_front, 1)
 
    !---------------------------------

    ! connect back and btm
    call grid_addBorder(gr,  1, N+1, N, N+1, sg_back, &
                             1,   N, N, N,   sg_btm,  2)
    call grid_addBorder(gr,  1, N+1, N, N+1, sg_btm,  &
                             1,   N, N, N,   sg_back, 2)
    
    ! connect btm and front
    call grid_addBorder(gr,  1,  0, N,  0, sg_btm,   &
                             1,  1, N,  1, sg_front, 2)
    call grid_addBorder(gr,  1,  0, N,  0, sg_front, &
                             1,  1, N,  1, sg_btm,   2)
    
    !---------------------------------

    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
end program Cubed_sphere
