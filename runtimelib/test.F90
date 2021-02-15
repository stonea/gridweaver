program Test
    include 'gridweaver.h'
    implicit none
    include 'mpif.h'
    
    integer      :: mpierr

    ! Initialize MPI and GridWeaver
    call MPI_INIT(mpierr)
    call gridweaver_initialize()
    
    ! Run test
    call simpleNoncompactTest()
    !call dipoleTest()
    !call flippedBorderTest()
    !call tripoleTest()
    !call cubedSphereTest()
    !call miniIcosTest()
    !call swm_stencil()
    
    ! Deinitialize libraries and stop execution
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)
    
  contains
    !integer function sumNeighbors(A, i , j)
    !    integer, intent(in) :: i, j
    !    interface
    !        integer function A(x,y)
    !            integer, intent(in) :: x,y
    !        end function
    !    end interface
    ! 
    !    sumNeighbors = &
    !        (A(i, j) + A(i-1, j) + A(i+1, j) + A(i, j-1) + A(i, j+1))
    !end function

    real(8) function addTen(A, i, j)
        integer, intent(in) :: i, j
        interface
            real(8) function A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface
        
        addTen = A(i, j) + 10
    end function

    real(8) function grabLeft(A, i, j)
        integer, intent(in) :: i, j
        interface
            real(8) function A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface
        
        grabLeft = A(i-1, j)
    end function

    real(8) function grabRight(A, i, j)
        integer, intent(in) :: i, j
        interface
            real(8) function A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface
        
        grabRight = A(i+1, j)
    end function

    real(8) function grabUp(A, i, j)
        integer, intent(in) :: i, j
        interface
            real(8) function A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface
        
        grabUp = A(i, j+1)
    end function

    real(8) function grabDown(A, i, j)
        integer, intent(in) :: i, j
        interface
            real(8) function A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface
        
        grabDown = A(i, j-1)
    end function

    real(8) function fivePtCompact(A, i, j)
        integer, intent(in) :: i, j
        interface
            real(8) function A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface
        
        fivePtCompact = 0.2 * &
            (A(i, j) + A(i-1, j) + A(i+1, j) + A(i, j-1) + A(i, j+1))
    end function



    ! Example non-compact stencil
    real(8) function grab2Up(A_rel, A_abs, sg, i, j)
        integer, intent(in) :: sg, i, j
        interface
            real(8) function A_rel(x,y)
                integer, intent(in) :: x,y
            end function

            real(8) function A_abs(sg,x,y)
                integer, intent(in) :: sg,x,y
            end function
        end interface
        integer :: newJ

        newJ = mod((j-1) + 2, 10)+1
        grab2Up = A_rel(i,j) + 0.01 * A_abs(sg,i,newJ)
    end function


    real(8) function test_tag(atag,  sg,i,j)
        integer, intent(in) :: sg,i,j
        interface
            real(8) function atag(t); integer, intent(in) :: t; end function
        end interface

        test_tag = atag(-1)
    end function



    ! **********************************
    ! Construct and test a regular grid
    ! **********************************
    subroutine simpleNoncompactTest()
        integer, parameter :: GRID_WIDTH  = 9
        integer, parameter :: GRID_HEIGHT = 9
        integer, parameter :: blkW = 3
        integer, parameter :: blkH = 3

        type(Neighbor)     :: n1, n2, n3, n4, n5, n6
        type(Subgrid)      :: sgA
        type(Grid)         :: g
        type(Distribution) :: dist
        type(Schedule)     :: sched
        type(Grid)         :: g2
        type(DataObj)      :: data_x
        
       ! Set up environment
        n1 = neighbor_new("neigh1", 0,  1)
        n2 = neighbor_new("neigh2", 1,  1)
        n3 = neighbor_new("neigh3", 1,  0)
        n4 = neighbor_new("neigh4", 0,  -1)
        n5 = neighbor_new("neigh5", -1, -1)
        n6 = neighbor_new("neigh6", -1, 0)
 
        sgA = subgrid_new("sgA", GRID_WIDTH, GRID_HEIGHT)
        
        g = grid_new("g")
        call grid_addSubgrid(g, sgA)

        dist = distribution_new("dist")
        call distribution_applyBlockCyclic(dist, g, blkW, blkH)
        !call distribution_applyBlockChunked(dist, g, blkW, blkH)
        !call distribution_applyFillBlock(dist, g, 5)
        !call distribution_applyBlockCyclic(dist, g, 5,5)

        sched = schedule_new("sched")
        call schedule_calculate(sched, g, dist, 2)

        ! Print the environment
        call environment_print()
        !call schedule_printFortranVersion(sched)
        !call schedule_printGhostsFortranVersion(sched)

        ! Test stencil
        data_x  = data_new(sched)
        call data_initializeSeqVals(data_x)

        call data_forceUpdate(data_x)
        call data_printForProcs(data_x, 6)

        call data_apply_noncompact(data_x, tag(data_x), test_tag)
        call data_print(data_x, 6)
    end subroutine



    ! **********************************
    ! Construct and test the Dipole grid
    ! **********************************
    subroutine dipoleTest()
        integer, parameter :: GRID_WIDTH  = 10
        integer, parameter :: GRID_HEIGHT = 10

        type(Neighbor)     :: n1, n2, n3, n4, n5, n6
        type(Subgrid)      :: sgA
        type(Grid)         :: g
        type(Distribution) :: dist
        type(Schedule)     :: sched
        type(Grid)         :: g2
        type(DataObj)      :: data_in, data_out
        
        
        ! Set up environment
        n1 = neighbor_new("neigh1", 1, 1)
        n2 = neighbor_new("neigh2", -1, -1)
        n3 = neighbor_new("neigh3", 1,  0)
        n4 = neighbor_new("neigh4", 0,  -1)
        n5 = neighbor_new("neigh5", -1, -1)
        n6 = neighbor_new("neigh6", -1, 0)
 
        sgA = subgrid_new("sgA", GRID_WIDTH, GRID_HEIGHT)
        
        g = grid_new("g")
        call grid_addSubgrid(g, sgA)

        ! Wrap left border to right border
        call grid_addBorder(g,          0, 1,          0, GRID_HEIGHT, sgA,  &
                               GRID_WIDTH, 1, GRID_WIDTH, GRID_HEIGHT, sgA, 0)

        ! Wrap right border to left border
        call grid_addBorder(g, GRID_WIDTH+1, 1, GRID_WIDTH+1, GRID_HEIGHT, sgA,  &
                                          1, 1,            1, GRID_HEIGHT, sgA, 0)
 
        dist = distribution_new("dist")
        !call distribution_applyFillBlock(dist, g, 5)
        !call distribution_applyFillBlock(dist, g, 10)
        !call distribution_applyBlockFill(dist, g, 5)
        call distribution_applyBlockCyclic(dist, g, 2,2)

        sched = schedule_new("sched")
        call schedule_calculate(sched, g, dist, 2)

        ! Print the environment
        call environment_print()
        !call schedule_printFortranVersion(sched)
        !call schedule_printGhostsFortranVersion(sched)

        ! Test stencil
        data_in  = data_new(sched)
        call data_initializeSeqVals(data_in)
!!        data_out = data_new(sched)
        call data_print(data_in, 6)
!!        call data_forceUpdate(data_in)
        call data_printForProcs(data_in, 6)

!        call data_apply1(data_out, data_in, addTen)
!        call data_print(data_out, 6)
    end subroutine



    ! **********************************
    ! Construct and test a grid with flipped, periodic, borders
    ! **********************************
    subroutine flippedBorderTest()
        integer, parameter :: GRID_WIDTH  = 10
        integer, parameter :: GRID_HEIGHT = 10

        type(Neighbor)     :: n1, n2, n3, n4, n5, n6
        type(Subgrid)      :: sgA
        type(Grid)         :: g
        type(Distribution) :: dist
        type(Schedule)     :: sched
        type(Grid)         :: g2
        type(DataObj)      :: data_in, data_out
        
        ! Set up environment
        n1 = neighbor_new("neigh1", 0,  1)
        n2 = neighbor_new("neigh2", 1,  1)
        n3 = neighbor_new("neigh3", 1,  0)
        n4 = neighbor_new("neigh4", 0,  -1)
        n5 = neighbor_new("neigh5", -1, -1)
        n6 = neighbor_new("neigh6", -1, 0)
        
        sgA = subgrid_new("sgA", GRID_WIDTH, GRID_HEIGHT)
        
        g = grid_new("g")
        call grid_addSubgrid(g, sgA)

        ! Flip left border
        call grid_addBorder(g,          0, 1,                     0, GRID_HEIGHT, sgA,  &
                               GRID_WIDTH, GRID_HEIGHT,  GRID_WIDTH, 1,           sgA, 1)

        ! Flip right border
        call grid_addBorder(g, GRID_WIDTH+1, 1,            GRID_WIDTH+1, GRID_HEIGHT, sgA,  &
                                          1, GRID_HEIGHT,             1, 1,           sgA, 0)

        ! Flip top border
        call grid_addBorder(g,          1, GRID_HEIGHT+1,  GRID_WIDTH, GRID_HEIGHT+1, sgA,  &
                               GRID_WIDTH, 1,                       1, 1,             sgA, 0)

        ! Flip bottom border
        call grid_addBorder(g,          1, 0,            GRID_WIDTH, 0,           sgA,  &
                               GRID_WIDTH, GRID_HEIGHT,           1, GRID_HEIGHT, sgA, 0)

 
        dist = distribution_new("dist")
        call distribution_applyFillBlock(dist, g, 5)

        sched = schedule_new("sched")
        call schedule_calculate(sched, g, dist, 1)

        ! Print the environment
        call environment_print()
        !call schedule_printFortranVersion(sched)

        ! Test stencil
        data_in  = data_new(sched)
        data_out = data_new(sched)
        call data_print(data_in, 6)
        call data_forceUpdate(data_in)
        call data_printForProcs(data_in, 6)

        call data_apply1(data_out, data_in, addTen)
        call data_print(data_out, 6)
    end subroutine


    ! **********************************
    ! Construct and test the Tripole grid
    ! **********************************
    subroutine tripoleTest()
        integer, parameter :: GRID_WIDTH  = 10
        integer, parameter :: GRID_HEIGHT = 10

        type(Neighbor)     :: n1, n2, n3, n4, n5, n6
        type(Subgrid)      :: sgA
        type(Grid)         :: g
        type(Distribution) :: dist
        type(Schedule)     :: sched
        type(Grid)         :: g2
        type(DataObj)      :: data_in, data_out
        
        ! Set up environment
        n1 = neighbor_new("neigh1", 0,  1)
        n2 = neighbor_new("neigh2", 1,  1)
        n3 = neighbor_new("neigh3", 1,  0)
        n4 = neighbor_new("neigh4", 0,  -1)
        n5 = neighbor_new("neigh5", -1, -1)
        n6 = neighbor_new("neigh6", -1, 0)
        
        sgA = subgrid_new("sgA", GRID_WIDTH, GRID_HEIGHT)
        
        g = grid_new("g")
        call grid_addSubgrid(g, sgA)

        ! Wrap left border to right border
        call grid_addBorder(g,          0, 1,          0, GRID_HEIGHT, sgA,  &
                               GRID_WIDTH, 1, GRID_WIDTH, GRID_HEIGHT, sgA, 0)

        ! Wrap right border to left border
        call grid_addBorder(g, GRID_WIDTH+1, 1, GRID_WIDTH+1, GRID_HEIGHT, sgA,  &
                                          1, 1,            1, GRID_HEIGHT, sgA, 0)

        ! Fold left side of top border to right side
        call grid_addBorder(g,          1, GRID_HEIGHT+1, GRID_WIDTH/2,   GRID_HEIGHT+1, sgA,  &
                               GRID_WIDTH, GRID_HEIGHT,   GRID_WIDTH/2+1, GRID_HEIGHT,   sgA,  &
                               3)

        ! Fold right side of top border to left side
        call grid_addBorder(g, GRID_WIDTH/2+1, GRID_HEIGHT+1, GRID_WIDTH, GRID_HEIGHT+1, sgA,  &
                               GRID_WIDTH/2, GRID_HEIGHT,              1, GRID_HEIGHT,   sgA,  &
                               3)
 
        dist = distribution_new("dist")
        !call distribution_applyFillBlock(dist, g, 5)
        call distribution_applyFillBlock(dist, g, 10)

        sched = schedule_new("sched")
        call schedule_calculate(sched, g, dist, 1)

        ! Print the environment
        call environment_print()
        !call schedule_printFortranVersion(sched)

        ! Test stencil
        data_in  = data_new(sched)
        call data_initializeSeqVals(data_in)
        data_out = data_new(sched)
        call data_print(data_in, 6)
        call data_forceUpdate(data_in)
        call data_printForProcs(data_in, 6)

        call data_apply1(data_out, data_in, addTen)
        call data_print(data_out, 6)
    end subroutine

    !---------------------------------------------------------------------------
    subroutine miniIcosTest()
        integer, parameter :: N  = 3

        type(Neighbor)     :: n1, n2, n3, n4, n5, n6
        type(Subgrid)      :: sg1L, sg2L, sg3L, sg4L, sg5L
        type(Subgrid)      :: sg1R, sg2R, sg3R, sg4R, sg5R
        type(Subgrid)      :: sgNP, sgSP
        type(Grid)         :: g
        type(Distribution) :: dist
        type(Schedule)     :: sched
        type(Grid)         :: g2
        type(DataObj)      :: data_in, data_out

        ! Set up environment
        n1 = neighbor_new("neigh1", 0,  1)
        n2 = neighbor_new("neigh2", 1,  1)
        n3 = neighbor_new("neigh3", 1,  0)
        n4 = neighbor_new("neigh4", 0,  -1)
        n5 = neighbor_new("neigh5", -1, -1)
        n6 = neighbor_new("neigh6", -1, 0)

        sg1L = subgrid_new("sg1L", N, N); sg1R = subgrid_new("sg1R", N, N)
        sg2L = subgrid_new("sg2L", N, N); sg2R = subgrid_new("sg2R", N, N)
        sg3L = subgrid_new("sg3L", N, N); sg3R = subgrid_new("sg3R", N, N)
        sg4L = subgrid_new("sg4L", N, N); sg4R = subgrid_new("sg4R", N, N)
        sg5L = subgrid_new("sg5L", N, N); sg5R = subgrid_new("sg5R", N, N)
        sgNP = subgrid_new("sgNP", 1, 1); sgSP = subgrid_new("sgSP", 1, 1)

        g = grid_new("g")
        call grid_addSubgrid(g, sg1L); call grid_addSubgrid(g, sg1R);
        call grid_addSubgrid(g, sg2L); call grid_addSubgrid(g, sg2R);
        call grid_addSubgrid(g, sg3L); call grid_addSubgrid(g, sg3R);
        call grid_addSubgrid(g, sg4L); call grid_addSubgrid(g, sg4R);
        call grid_addSubgrid(g, sg5L); call grid_addSubgrid(g, sg5R);
        call grid_addSubgrid(g, sgNP); call grid_addSubgrid(g, sgSP)

        ! Connect top and left borders
        call grid_addBorder(g,  1, N+1,  N, N+1,  sg1L,    &
                                1, N,    1, 1,    sg2L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg2L,    &
                                N, N,    1, N,    sg1L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg2L,    &
                                1, N,    1, 1,    sg3L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg3L,    &
                                N, N,    1, N,    sg2L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg3L,    &
                                1, N,    1, 1,    sg4L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg4L,    &
                                N, N,    1, N,    sg3L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg4L,    &
                                1, N,    1, 1,    sg5L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg5L,    &
                                N, N,    1, N,    sg4L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg5L,    &
                                1, N,    1, 1,    sg1L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg1L,    &
                                N, N,    1, N,    sg5L, -1)
        
        ! Connect right and bottom borders
        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg1R,    &
                                  N, 1,    1,   1,  sg2R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg2R,      &
                                  N, N,    N,   1,  sg1R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg2R,    &
                                  N, 1,    1,   1,  sg3R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg3R,      &
                                  N, N,    N,   1,  sg2R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg3R,    &
                                  N, 1,    1,   1,  sg4R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg4R,      &
                                  N, N,    N,   1,  sg3R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg4R,    &
                                  N, 1,    1,   1,  sg5R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg5R,      &
                                  N, N,    N,   1,  sg4R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg5R,    &
                                  N, 1,    1,   1,  sg1R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg1R,      &
                                  N, N,    N,   1,  sg5R, 1)
         
        ! Connect east and west borders
        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg1L,    &
                                  1, 1,    1, N,   sg1R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg1R,    &
                                  N, 1,    N, N,   sg1L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg2L,    &
                                  1, 1,    1, N,   sg2R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg2R,    &
                                  N, 1,    N, N,   sg2L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg3L,    &
                                  1, 1,    1, N,   sg3R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg3R,    &
                                  N, 1,    N, N,   sg3L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg4L,    &
                                  1, 1,    1, N,   sg4R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg4R,    &
                                  N, 1,    N, N,   sg4L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg5L,    &
                                  1, 1,    1, N,   sg5R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg5R,    &
                                  N, 1,    N, N,   sg5L, 0)

        ! Connect north and south borders
        call grid_addBorder(g,   1, N+1,  N, N+1, sg1R,  &
                                 1,   1,  N,   1, sg2L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg2L,  &
                                 1,   N,  N,   N, sg1R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg2R,  &
                                 1,   1,  N,   1, sg3L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg3L,  &
                                 1,   N,  N,   N, sg2R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg3R,  &
                                 1,   1,  N,   1, sg4L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg4L,  &
                                 1,   N,  N,   N, sg3R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg4R,  &
                                 1,   1,  N,   1, sg5L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg5L,  &
                                 1,   N,  N,   N, sg4R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg5R,  &
                                 1,   1,  N,   1, sg1L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg1L,  &
                                 1,   N,  N,   N, sg5R, 0)

        ! Connect to NP
        call grid_addBorder(g,  0, N,  0, N, sg1L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  0, 0,  0, 0, sgNP,    &
                                1, N,  1, N, sg1L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg2L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  1, 0,  1, 0, sgNP,    &
                                1, N,  1, N, sg2L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg3L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  2, 1,  2, 1, sgNP,    &
                                1, N,  1, N, sg3L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg4L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  2, 2,  2, 2, sgNP,    &
                                1, N,  1, N, sg4L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg5L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  1, 2,  1, 2, sgNP,    &
                                1, N,  1, N, sg5L, -1)

        ! Connect to SP
        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg1R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    1, 0,    1, 0, sgSP,    &
                                  N, 1,    N, 1, sg1R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg2R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    0, 0,    0, 0, sgSP,    &
                                  N, 1,    N, 1, sg2R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg3R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    0, 1,    0, 1, sgSP,    &
                                  N, 1,    N, 1, sg3R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg4R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    1, 2,    1, 2, sgSP,    &
                                  N, 1,    N, 1, sg4R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg5R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    2, 2,    2, 2, sgSP,    &
                                  N, 1,    N, 1, sg5R,  1)

        dist = distribution_new("dist")
        call distribution_applyFillBlock(dist, g, 3)

        sched = schedule_new("sched")
        call schedule_calculate(sched, g, dist, 2)

        ! Print the environment
        call environment_print()

        ! Test stencil
        data_in  = data_new(sched)
        call data_initializeSeqVals(data_in)
        call data_forceUpdate(data_in)
        call data_printForProcs(data_in, 6)
!        call data_printForProcs(data_in, 6)
    end subroutine

!===============================================================================
!===============================================================================
!===============================================================================

    subroutine swm_stencil()
        ! ____________________________________ _____________________________
        ! |              number   resolution | |             global number |
        ! | level_max   of cells     (km)    | |             of subdomains |
        ! | ---------------------------------| | sbdmn_iota   (=nsdm_glbl) |
        ! |     5           10242   250.2    | | --------------------------|
        ! |     6           40962   125.1    | |      0             10     |
        ! |     7          163842    62.55   | |      1             40     |
        ! |     8          655362    31.27   | |      2            160     |
        ! |     9         2621442    15.64   | |      3            640     |
        ! |    10        10485762     7.82   | |      4           2560     |
        ! |    11        41943042     3.91   | |      5          10240     |
        ! |    12       167772162     1.95   | |      6          40960     |
        ! |    13       671088642     0.977  | |      7         163840     |
        ! |    14      2684354562     0.487  | -----------------------------
        ! |    15     10737418242     0.244  | 
        ! |    16     42949672962     0.122  | 
        ! ------------------------------------ 
        integer, parameter :: level_max   = 5
        integer, parameter :: sbdmn_iota  = 1

        integer :: cell_max, nsdm_glbl, nsdm_per_sg
        integer :: N, i, blkW, blkH, numIters
        DOUBLE PRECISION :: t1, t2

        type(Neighbor)     :: n1, n2, n3, n4, n5, n6
        type(Subgrid)      :: sg1L, sg2L, sg3L, sg4L, sg5L
        type(Subgrid)      :: sg1R, sg2R, sg3R, sg4R, sg5R
        type(Subgrid)      :: sgNP, sgSP
        type(Grid)         :: g
        type(Distribution) :: dist
        type(Schedule)     :: sched
        type(Grid)         :: g2
        type(DataObj)      :: data_x

        ! Calculate subdomain sizes
        ! cell_max    = 2 + 10*((2**level_max)**2)                        
        ! nsdm_glbl   = 10*2**(2*sbdmn_iota)
        ! nsdm_per_sg = nsdm_glbl / 10
        !         N = 2+2**(level_max-sbdmn_iota)
    
        
        numIters = 10
        N = 10000;  blkW = 50;  blkH = 50

        if(myRank() == 0) then
                print *, "numIters = ", 10
                print *, "N = ", 10000
                print *, blkW
                print *, blkH
        end if

        ! Set up environment
        n1 = neighbor_new("neigh1", 0,  1)
        n2 = neighbor_new("neigh2", 1,  1)
        n3 = neighbor_new("neigh3", 1,  0)
        n4 = neighbor_new("neigh4", 0,  -1)
        n5 = neighbor_new("neigh5", -1, -1)
        n6 = neighbor_new("neigh6", -1, 0)

        sg1L = subgrid_new("sg1L", N, N); sg1R = subgrid_new("sg1R", N, N)
        sg2L = subgrid_new("sg2L", N, N); sg2R = subgrid_new("sg2R", N, N)
        sg3L = subgrid_new("sg3L", N, N); sg3R = subgrid_new("sg3R", N, N)
        sg4L = subgrid_new("sg4L", N, N); sg4R = subgrid_new("sg4R", N, N)
        sg5L = subgrid_new("sg5L", N, N); sg5R = subgrid_new("sg5R", N, N)
        sgNP = subgrid_new("sgNP", 1, 1); sgSP = subgrid_new("sgSP", 1, 1)

        g = grid_new("g")
        call grid_addSubgrid(g, sg1L); call grid_addSubgrid(g, sg1R);
        call grid_addSubgrid(g, sg2L); call grid_addSubgrid(g, sg2R);
        call grid_addSubgrid(g, sg3L); call grid_addSubgrid(g, sg3R);
        call grid_addSubgrid(g, sg4L); call grid_addSubgrid(g, sg4R);
        call grid_addSubgrid(g, sg5L); call grid_addSubgrid(g, sg5R);
        call grid_addSubgrid(g, sgNP); call grid_addSubgrid(g, sgSP)

        ! Connect top and left borders
        call grid_addBorder(g,  1, N+1,  N, N+1,  sg1L,    &
                                1, N,    1, 1,    sg2L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg2L,    &
                                N, N,    1, N,    sg1L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg2L,    &
                                1, N,    1, 1,    sg3L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg3L,    &
                                N, N,    1, N,    sg2L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg3L,    &
                                1, N,    1, 1,    sg4L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg4L,    &
                                N, N,    1, N,    sg3L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg4L,    &
                                1, N,    1, 1,    sg5L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg5L,    &
                                N, N,    1, N,    sg4L, -1)

        call grid_addBorder(g,  1, N+1,  N, N+1,  sg5L,    &
                                1, N,    1, 1,    sg1L, 0)
        call grid_addBorder(g,  0, 0,    0, N-1,  sg1L,    &
                                N, N,    1, N,    sg5L, -1)
        
        ! Connect right and bottom borders
        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg1R,    &
                                  N, 1,    1,   1,  sg2R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg2R,      &
                                  N, N,    N,   1,  sg1R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg2R,    &
                                  N, 1,    1,   1,  sg3R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg3R,      &
                                  N, N,    N,   1,  sg2R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg3R,    &
                                  N, 1,    1,   1,  sg4R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg4R,      &
                                  N, N,    N,   1,  sg3R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg4R,    &
                                  N, 1,    1,   1,  sg5R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg5R,      &
                                  N, N,    N,   1,  sg4R, 1)

        call grid_addBorder(g,  N+1, 2,  N+1, N+1,  sg5R,    &
                                  N, 1,    1,   1,  sg1R, -1)
        call grid_addBorder(g,    1, 0,    N,   0,  sg1R,      &
                                  N, N,    N,   1,  sg5R, 1)
         
        ! Connect east and west borders
        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg1L,    &
                                  1, 1,    1, N,   sg1R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg1R,    &
                                  N, 1,    N, N,   sg1L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg2L,    &
                                  1, 1,    1, N,   sg2R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg2R,    &
                                  N, 1,    N, N,   sg2L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg3L,    &
                                  1, 1,    1, N,   sg3R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg3R,    &
                                  N, 1,    N, N,   sg3L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg4L,    &
                                  1, 1,    1, N,   sg4R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg4R,    &
                                  N, 1,    N, N,   sg4L, 0)

        call grid_addBorder(g,  N+1, 1,  N+1, N,   sg5L,    &
                                  1, 1,    1, N,   sg5R, 0)
        call grid_addBorder(g,    0, 1,    0, N,   sg5R,    &
                                  N, 1,    N, N,   sg5L, 0)

        ! Connect north and south borders
        call grid_addBorder(g,   1, N+1,  N, N+1, sg1R,  &
                                 1,   1,  N,   1, sg2L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg2L,  &
                                 1,   N,  N,   N, sg1R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg2R,  &
                                 1,   1,  N,   1, sg3L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg3L,  &
                                 1,   N,  N,   N, sg2R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg3R,  &
                                 1,   1,  N,   1, sg4L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg4L,  &
                                 1,   N,  N,   N, sg3R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg4R,  &
                                 1,   1,  N,   1, sg5L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg5L,  &
                                 1,   N,  N,   N, sg4R, 0)

        call grid_addBorder(g,   1, N+1,  N, N+1, sg5R,  &
                                 1,   1,  N,   1, sg1L, 0)
        call grid_addBorder(g,   1,   0,  N,   0, sg1L,  &
                                 1,   N,  N,   N, sg5R, 0)

        ! Connect to NP
        call grid_addBorder(g,  0, N,  0, N, sg1L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  0, 0,  0, 0, sgNP,    &
                                1, N,  1, N, sg1L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg2L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  1, 0,  1, 0, sgNP,    &
                                1, N,  1, N, sg2L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg3L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  2, 1,  2, 1, sgNP,    &
                                1, N,  1, N, sg3L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg4L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  2, 2,  2, 2, sgNP,    &
                                1, N,  1, N, sg4L, -1)

        call grid_addBorder(g,  0, N,  0, N, sg5L,    &
                                1, 1,  1, 1, sgNP, 1)
        call grid_addBorder(g,  1, 2,  1, 2, sgNP,    &
                                1, N,  1, N, sg5L, -1)

        ! Connect to SP
        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg1R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    1, 0,    1, 0, sgSP,    &
                                  N, 1,    N, 1, sg1R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg2R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    0, 0,    0, 0, sgSP,    &
                                  N, 1,    N, 1, sg2R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg3R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    0, 1,    0, 1, sgSP,    &
                                  N, 1,    N, 1, sg3R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg4R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    1, 2,    1, 2, sgSP,    &
                                  N, 1,    N, 1, sg4R,  1)

        call grid_addBorder(g,  N+1, 1,  N+1, 1, sg5R,    &
                                  1, 1,    1, 1, sgSP, -1)
        call grid_addBorder(g,    2, 2,    2, 2, sgSP,    &
                                  N, 1,    N, 1, sg5R,  1)

        dist = distribution_new("dist")
        call distribution_applyBlockCyclic(dist, g, blkW, blkH)

        sched = schedule_new("sched")
        !call schedule_calculate(sched, g, dist, 1)
        call schedule_calculate(sched, g, dist, 2)
        !call schedule_calculate(sched, g, dist, 3)

        ! Test non-compact stencil
        data_x  = data_new(sched)
        call data_initializeSeqVals(data_x)

        call turnOffSyntaxHighlighting()

        ! Perform for numIters iterations
        t1 = MPI_WTIME()
        do i=1,numIters
            if(myRank() == 0 .and. mod(i,1000) == 0) &
                print *, "Compute iteration: ", i

            call data_forceUpdate(data_x)
        end do
        t2 = MPI_WTIME()

        if(myRank() == 0) then
            print *, "Ellapsed time = ", (t2 - t1)
        end if
    end subroutine
end program Test
