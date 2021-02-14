module mod_gridweaver
    use mod_utils
    use mod_string

    implicit none

    ! **************************************************************************
    ! ** - [GRID_TYPES] - ******************************************************
    ! **************************************************************************
    type Neighbor
        type(String) :: id
    end type

    type Subgrid
        type(String) :: id
    end type

    type Grid
        type(String) :: id
    end type

    type Distribution
        type(String) :: id
    end type

    ! **************************************************************************
    ! ** - [FUNCTIONS] - *******************************************************
    ! **************************************************************************

    ! The following functions are only run on proc 0
    public :: gridweaver_initialize

    public :: turnOffSyntaxHighlighting

    public :: neighbor_new

    public :: subgrid_new,      &
              subgrid_width,    &
              subgrid_height

    public :: grid_new,                         &
              grid_addSubgrid,                  &
              grid_addBorder,                   &
              grid_placeAdjacentLR,             &
              grid_placeAdjacentRL,             &
              grid_placeAdjacentTB,             &
              grid_placeAdjacentBT,             &
              grid_connectTtoB,                 &
              grid_connectRtoL,                 &
              grid_connectBtoT,                 &
              grid_connectLtoR,                 &
              grid_connectLtoT,                 &
              grid_connectLtoB,                 &
              grid_connectRtoT,                 &
              grid_connectRtoB,                 &
              grid_connectTtoL,                 &
              grid_connectTtoR,                 &
              grid_connectBtoL,                 &
              grid_connectBtoR,                 &
              grid_wrapLR,                      &
              grid_wrapTB,                      &
              grid_mirrorT,                     &
              grid_mirrorB,                     &
              grid_mirrorL,                     &
              grid_mirrorR,                     &
              grid_foldT,                       &
              grid_foldB,                       &
              grid_foldL,                       &
              grid_foldR,                       &
              grid_numSubGrids,                 &
              grid_getSubgrid

    public :: distribution_new,                 &
              distribution_applyFillBlock,      &
              distribution_applyBlockFill,      &
              distribution_applyBlockCyclic,    &
              distribution_visualize,           &
              distribution_width,               &
              distribution_height,              &
              distribution_numLocalBlocks,      &
              distribution_lbid2gbid,           &
              distribution_gbid2lbid,           &
              distribution_gbid2proc,           &
              distribution_blockLowX,           &
              distribution_blockLowY,           &
              distribution_blockHighX,          &
              distribution_blockHighY,          &
              distribution_pos2BlockPos

    public :: environment_print,                            &
              environment_output,                           &
              environment_input,                            &
              environment_clear

  interface
    subroutine cwrap__gridweaver_initialize() &
        bind(C, name="__gridweaver_initialize")
        use iso_c_binding
        integer(c_int)      :: nProcs
    end subroutine

    subroutine cwrap__turnOffSyntaxHighlighting() &
        bind(C, name="__turnOffSyntaxHighlighting")
        use iso_c_binding
    end subroutine

    subroutine cwrap__neighbor_new(name, x, y) &
        bind(C, name="__neighbor_new")
        use iso_c_binding
        character(kind=c_char) :: name(*)
        integer(c_int)      :: x, y
    end subroutine

    subroutine cwrap__subgrid_new(sg, width, height) &
        bind(C, name="__subgrid_new")
        use iso_c_binding
        character(kind=c_char) :: sg(*)
        integer(c_int)      :: width, height
    end subroutine

    subroutine cwrap__subgrid_width(sg, ret) &
        bind(C, name="__subgrid_width")
        use iso_c_binding
        character(kind=c_char) :: sg(*)
        integer(c_int)      :: ret
    end subroutine

    subroutine cwrap__subgrid_height(sg, ret) &
        bind(C, name="__subgrid_height")
        use iso_c_binding
        character(kind=c_char) :: sg(*)
        integer(c_int)      :: ret
    end subroutine
 
    subroutine cwrap__grid_new(g) &
        bind(C, name="__grid_new")
        use iso_c_binding
        character(kind=c_char) :: g(*)
    end subroutine

    subroutine cwrap__grid_addSubgrid(g, sg) &
        bind(C, name="__grid_addSubgrid")
        use iso_c_binding
        character(kind=c_char) :: g(*), sg(*)
    end subroutine

    subroutine cwrap__grid_addBorder(g, srcX1, srcY1, scrX2, scrY2, srcSG,  &
                                        tgtX1, tgtY1, tgtX2, tgtY2, tgtSG,  &
                                        rotation) &
        bind(C, name="__grid_addBorder")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        integer(c_int)      :: srcX1, srcY1, scrX2, scrY2
        character(kind=c_char) :: srcSG(*)
        integer(c_int)      :: tgtX1, tgtY1, tgtX2, tgtY2
        character(kind=c_char) :: tgtSG(*)
        integer(c_int)      :: rotation
    end subroutine
    
    subroutine cwrap__grid_placeAdjacentLR(g, sgL, sgR) &
        bind(C, name="__grid_placeAdjacentLR")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sgL(*)
        character(kind=c_char) :: sgR(*)
    end subroutine

    subroutine cwrap__grid_placeAdjacentRL(g, sgR, sgL) &
        bind(C, name="__grid_placeAdjacentRL")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sgR(*)
        character(kind=c_char) :: sgL(*)
    end subroutine

    subroutine cwrap__grid_placeAdjacentTB(g, sgT, sgB) &
        bind(C, name="__grid_placeAdjacentTB")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sgT(*)
        character(kind=c_char) :: sgB(*)
    end subroutine

    subroutine cwrap__grid_placeAdjacentBT(g, sgB, sgT) &
        bind(C, name="__grid_placeAdjacentBT")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sgB(*)
        character(kind=c_char) :: sgT(*)
    end subroutine

    subroutine cwrap__grid_connectTtoB(g, sg1, sg2) &
        bind(C, name="__grid_connectTtoB")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg1(*)
        character(kind=c_char) :: sg2(*)
    end subroutine

    subroutine cwrap__grid_connectRtoL(g, sg1, sg2) &
        bind(C, name="__grid_connectRtoL")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg1(*)
        character(kind=c_char) :: sg2(*)
    end subroutine

    subroutine cwrap__grid_connectBtoT(g, sg1, sg2) &
        bind(C, name="__grid_connectBtoT")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg1(*)
        character(kind=c_char) :: sg2(*)
    end subroutine

    subroutine cwrap__grid_connectLtoR(g, sg1, sg2) &
        bind(C, name="__grid_connectLtoR")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg1(*)
        character(kind=c_char) :: sg2(*)
    end subroutine

    subroutine cwrap__grid_connectLtoT(g, sg,  sgBL) &
        bind(C, name="__grid_connectLtoT")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
        character(kind=c_char) :: sgBL(*)
    end subroutine

    subroutine cwrap__grid_connectLtoB(g, sg,  sgTL) &
        bind(C, name="__grid_connectLtoB")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
        character(kind=c_char) :: sgTL(*)
    end subroutine

    subroutine cwrap__grid_connectRtoT(g, sg,  sgBR) &
        bind(C, name="__grid_connectRtoT")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
        character(kind=c_char) :: sgBR(*)
    end subroutine

    subroutine cwrap__grid_connectRtoB(g, sg,  sgTR) &
        bind(C, name="__grid_connectRtoB")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
        character(kind=c_char) :: sgTR(*)
    end subroutine

    subroutine cwrap__grid_connectTtoL(g, sg,  sgTR) &
        bind(C, name="__grid_connectTtoL")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
        character(kind=c_char) :: sgTR(*)
    end subroutine

    subroutine cwrap__grid_connectTtoR(g, sg,  sgTL) &
        bind(C, name="__grid_connectTtoR")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
        character(kind=c_char) :: sgTL(*)
    end subroutine

    subroutine cwrap__grid_connectBtoL(g, sg,  sgBR) &
        bind(C, name="__grid_connectBtoL")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
        character(kind=c_char) :: sgBR(*)
    end subroutine

    subroutine cwrap__grid_connectBtoR(g, sg,  sgBL) &
        bind(C, name="__grid_connectBtoR")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
        character(kind=c_char) :: sgBL(*)
    end subroutine

    subroutine cwrap__grid_wrapLR(g, sg) &
        bind(C, name="__grid_wrapLR")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_wrapTB(g, sg) &
        bind(C, name="__grid_wrapTB")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_mirrorT(g, sg) &
        bind(C, name="__grid_mirrorT")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_mirrorB(g, sg) &
        bind(C, name="__grid_mirrorB")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_mirrorL(g, sg) &
        bind(C, name="__grid_mirrorL")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_mirrorR(g, sg) &
        bind(C, name="__grid_mirrorR")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_foldT(g, sg) &
        bind(C, name="__grid_foldT")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_foldB(g, sg) &
        bind(C, name="__grid_foldB")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_foldL(g, sg) &
        bind(C, name="__grid_foldL")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_foldR(g, sg) &
        bind(C, name="__grid_foldR")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: sg(*)
    end subroutine

    subroutine cwrap__grid_numSubgrids(g, ret) &
        bind(C, name="__grid_numSubgrids")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        integer(c_int)         :: ret
    end subroutine

    subroutine cwrap__grid_getSubgrid(g, idx, ret) &
        bind(C, name="__grid_getSubgrid")
        use iso_c_binding
        character(kind=c_char) :: g(*)
        integer(c_int)         :: idx
        character(kind=c_char) :: ret(*)
    end subroutine


    subroutine cwrap__distribution_new(dist) &
        bind(C, name="__distribution_new")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
    end subroutine

    subroutine cwrap__distribution_applyFillBlock( &
        dist, g, nprocs, blockH) &
        bind(C, name="__distribution_applyFillBlock")
        use iso_c_binding
        character(kind=c_char) :: dist(*), g(*)
        integer(c_int)      :: nProcs, blockH
    end subroutine

    subroutine cwrap__distribution_applyBlockFill( &
        dist, g, nprocs, blockW) &
        bind(C, name="__distribution_applyBlockFill")
        use iso_c_binding
        character(kind=c_char) :: dist(*), g(*)
        integer(c_int)      :: nProcs, blockW
    end subroutine

    subroutine cwrap__distribution_applyBlockCyclic( &
        dist, g, nProcs, blkW, blkH) &
        bind(C, name="__distribution_applyBlockCyclic")
        use iso_c_binding
        character(kind=c_char) :: dist(*), g(*)
        integer(c_int)      :: nProcs, blkW, blkH
    end subroutine

    subroutine cwrap__distribution_visualize(dist, dirName) &
        bind(C, name="__distribution_visualize")
        use iso_c_binding
        character(kind=c_char) :: dist(*), dirname(*)
    end subroutine

    subroutine cwrap__distribution_width(dist, ret) &
        bind(C, name="__distribution_width")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_height(dist, ret) &
        bind(C, name="__distribution_height")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_numLocalBlocks(dist, ret) &
        bind(C, name="__distribution_numLocalBlocks")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_lbid2gbid(dist, lbid, ret) &
        bind(C, name="__distribution_lbid2gbid")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int), intent(in) :: lbid
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_gbid2lbid(dist, gbid, ret) &
        bind(C, name="__distribution_gbid2lbid")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int), intent(in) :: gbid
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_gbid2proc(dist, gbid, ret) &
        bind(C, name="__distribution_gbid2proc")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int), intent(in) :: gbid
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_blockLowX(dist, gbid, ret) &
        bind(C, name="__distribution_blockLowX")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int), intent(in) :: gbid
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_blockLowY(dist, gbid, ret) &
        bind(C, name="__distribution_blockLowY")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int), intent(in) :: gbid
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_blockHighX(dist, gbid, ret) &
        bind(C, name="__distribution_blockHighX")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int), intent(in) :: gbid
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_blockHighY(dist, gbid, ret) &
        bind(C, name="__distribution_blockHighY")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int), intent(in) :: gbid
        integer(c_int) :: ret
    end subroutine

    subroutine cwrap__distribution_pos2BlockPos( &
        dist, x, y, sg, blkX, blkY, gbid) &
    !`
        bind(C, name="__distribution_pos2BlockPos")
        use iso_c_binding
        character(kind=c_char) :: dist(*)
        integer(c_int), intent(in) :: x, y
        character(kind=c_char) :: sg(*)
        integer(c_int), intent(inout) :: blkX, blkY, gbid
    end subroutine

    subroutine cwrap__environment_print() &
        bind(C, name="__environment_print")
        use iso_c_binding
    end subroutine

    subroutine cwrap__environment_output(filename) &
        bind(C, name="__environment_output")
        use iso_c_binding
        character(kind=c_char) :: filename(*)
    end subroutine

    subroutine cwrap__environment_input(filename) &
        bind(C, name="__environment_input")
        use iso_c_binding
        character(kind=c_char) :: filename(*)
    end subroutine

    subroutine cwrap__environment_clear() &
        bind(C, name="__environment_clear")
        use iso_c_binding
    end subroutine
  end interface

  contains

! *****************************************************************************

    subroutine gridweaver_initialize()
        call cwrap__gridweaver_initialize()
    end subroutine

    subroutine turnOffSyntaxHighlighting()
        call cwrap__turnOffSyntaxHighlighting()
    end subroutine

    function neighbor_new(name, x, y)
        character(len=*)    :: name
        integer, intent(in) :: x, y
        type(Neighbor)      :: neighbor_new

        ! Have all ranks execute the command
        call cwrap__neighbor_new(name // char(0), cint(x), cint(y))

        call string_init(neighbor_new%id, name)
    end function

    function subgrid_new(sg, width, height)
        character(len=*), intent(in) :: sg
        integer, intent(in)          :: width, height
        type(Subgrid)                :: subgrid_new

        ! Have all ranks execute the command
        call cwrap__subgrid_new(sg // char(0), cint(width), cint(height))

        call string_init(subgrid_new%id, sg)
    end function

    integer function subgrid_width(sg)
        type(Subgrid)   :: sg
        integer(c_int)  :: ret

        call cwrap__subgrid_width(cstr(sg%id), ret)
        subgrid_width = ret
    end function

    integer function subgrid_height(sg)
        type(Subgrid)   :: sg
        integer(c_int)  :: ret

        call cwrap__subgrid_height(cstr(sg%id), ret)
        subgrid_height = ret
    end function
    
    function grid_new(g)
        character(len=*), intent(in) :: g
        type(Grid)                   :: grid_new

        ! Have all ranks execute the command
        call cwrap__grid_new(g // char(0))

        call string_init(grid_new%id, g)
    end function

    subroutine grid_addSubgrid(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        
        ! Have all ranks execute the command
        call cwrap__grid_addSubgrid(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_addBorder(g, srcX1, srcY1, scrX2, scrY2, srcSG,         &
                                        tgtX1, tgtY1, tgtX2, tgtY2, tgtSG,  &
                                        rotation)
      !`
        type(Grid), intent(in)    :: g
        integer, intent(in)       :: srcX1, srcY1, scrX2, scrY2
        type(Subgrid), intent(in) :: srcSG
        integer, intent(in)       :: tgtX1, tgtY1, tgtX2, tgtY2
        type(Subgrid), intent(in) :: tgtSG
        integer, intent(in)       :: rotation
        
        ! Have all ranks execute the command
        call cwrap__grid_addBorder(                                 &
            cstr(g%id),                                             &
            cint(srcX1), cint(srcY1), cint(scrX2), cint(scrY2),     &
            cstr(srcSG%id),                                         &
            cint(tgtX1), cint(tgtY1), cint(tgtX2), cint(tgtY2),     &
            cstr(tgtSG%id),                                         &
            cint(rotation))
    end subroutine

    subroutine grid_placeAdjacentLR(g, sgL, sgR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sgL
        type(Subgrid), intent(in) :: sgR

        ! Have all ranks execute the command
        call cwrap__grid_placeAdjacentLR(cstr(g%id), cstr(sgL%id),      &
            cstr(sgR%id))
    end subroutine

    subroutine grid_placeAdjacentRL(g, sgR, sgL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sgR
        type(Subgrid), intent(in) :: sgL

        ! Have all ranks execute the command
        call cwrap__grid_placeAdjacentRL(cstr(g%id), cstr(sgR%id),      &
            cstr(sgL%id))
    end subroutine

    subroutine grid_placeAdjacentTB(g, sgT, sgB)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sgT
        type(Subgrid), intent(in) :: sgB

        ! Have all ranks execute the command
        call cwrap__grid_placeAdjacentTB(cstr(g%id), cstr(sgT%id),      &
            cstr(sgB%id))
    end subroutine

    subroutine grid_placeAdjacentBT(g, sgB, sgT)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sgB
        type(Subgrid), intent(in) :: sgT

        ! Have all ranks execute the command
        call cwrap__grid_placeAdjacentBT(cstr(g%id), cstr(sgB%id),      &
            cstr(sgT%id))
    end subroutine

    subroutine grid_connectTtoB(g, sg1, sg2)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg1
        type(Subgrid), intent(in) :: sg2

        ! Have all ranks execute the command
        call cwrap__grid_connectTtoB(cstr(g%id), cstr(sg1%id),      &
            cstr(sg2%id))
    end subroutine

    subroutine grid_connectRtoL(g, sg1, sg2)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg1
        type(Subgrid), intent(in) :: sg2

        ! Have all ranks execute the command
        call cwrap__grid_connectRtoL(cstr(g%id), cstr(sg1%id),      &
            cstr(sg2%id))
    end subroutine

    subroutine grid_connectBtoT(g, sg1, sg2)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg1
        type(Subgrid), intent(in) :: sg2

        ! Have all ranks execute the command
        call cwrap__grid_connectBtoT(cstr(g%id), cstr(sg1%id),      &
            cstr(sg2%id))
    end subroutine

    subroutine grid_connectLtoR(g, sg1, sg2)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg1
        type(Subgrid), intent(in) :: sg2

        ! Have all ranks execute the command
        call cwrap__grid_connectLtoR(cstr(g%id), cstr(sg1%id),      &
            cstr(sg2%id))
    end subroutine

    subroutine grid_connectLtoT(g, sg,  sgBL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgBL

        ! Have all ranks execute the command
        call cwrap__grid_connectLtoT(cstr(g%id), cstr(sg%id),      &
            cstr(sgBL%id))
    end subroutine

    subroutine grid_connectLtoB(g, sg,  sgTL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgTL

        ! Have all ranks execute the command
        call cwrap__grid_connectLtoB(cstr(g%id), cstr(sg%id),      &
            cstr(sgTL%id))
    end subroutine

    subroutine grid_connectRtoT(g, sg,  sgBR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgBR

        ! Have all ranks execute the command
        call cwrap__grid_connectRtoT(cstr(g%id), cstr(sg%id),      &
            cstr(sgBR%id))
    end subroutine

    subroutine grid_connectRtoB(g, sg,  sgTR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgTR

        ! Have all ranks execute the command
        call cwrap__grid_connectRtoB(cstr(g%id), cstr(sg%id),      &
            cstr(sgTR%id))
    end subroutine

    subroutine grid_connectTtoL(g, sg,  sgTR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgTR

        ! Have all ranks execute the command
        call cwrap__grid_connectTtoL(cstr(g%id), cstr(sg%id),      &
            cstr(sgTR%id))
    end subroutine

    subroutine grid_connectTtoR(g, sg,  sgTL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgTL

        ! Have all ranks execute the command
        call cwrap__grid_connectTtoR(cstr(g%id), cstr(sg%id),      &
            cstr(sgTL%id))
    end subroutine

    subroutine grid_connectBtoL(g, sg,  sgBR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgBR

        ! Have all ranks execute the command
        call cwrap__grid_connectBtoL(cstr(g%id), cstr(sg%id),      &
            cstr(sgBR%id))
    end subroutine

    subroutine grid_connectBtoR(g, sg,  sgBL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgBL

        ! Have all ranks execute the command
        call cwrap__grid_connectBtoR(cstr(g%id), cstr(sg%id),      &
            cstr(sgBL%id))
    end subroutine

    subroutine grid_wrapLR(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
    
        ! Have all ranks execute the command
        call cwrap__grid_wrapLR(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_wrapTB(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have all ranks execute the command
        call cwrap__grid_wrapTB(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_mirrorT(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have all ranks execute the command
        call cwrap__grid_mirrorT(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_mirrorB(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have all ranks execute the command
        call cwrap__grid_mirrorB(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_mirrorL(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have all ranks execute the command
        call cwrap__grid_mirrorL(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_mirrorR(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have all ranks execute the command
        call cwrap__grid_mirrorR(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_foldT(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have all ranks execute the command
        call cwrap__grid_foldT(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_foldB(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have all ranks execute the command
        call cwrap__grid_foldB(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_foldL(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have all ranks execute the command
        call cwrap__grid_foldL(cstr(g%id), cstr(sg%id))
    end subroutine

    subroutine grid_foldR(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have all ranks execute the command
        call cwrap__grid_foldR(cstr(g%id), cstr(sg%id))
    end subroutine

    integer function grid_numSubgrids(g)
        type(Grid), intent(in) :: g
        integer(c_int)         :: ret

        call cwrap__grid_numSubgrids(cstr(g%id), ret)
        grid_numSubgrids = ret
    end function

    function grid_getSubgrid(g, idx)
        type(Grid), intent(in) :: g
        integer(c_int)         :: idx 
        type(Subgrid) :: grid_getSubgrid
        character(len=256) :: subgridName
        
        call cwrap__grid_getSubgrid(cstr(g%id), idx, subgridName)
        call string_init_from_cstr(grid_getSubgrid%id, subgridName)
    end function

    function distribution_new(dist)
        character(len=*), intent(in) :: dist
        type(Distribution)           :: distribution_new
        
        ! Have all ranks execute the command
        call cwrap__distribution_new(dist // char(0))

        call string_init(distribution_new%id, dist)
    end function

    subroutine distribution_applyFillBlock( &
        dist, g, blockH)
      !`
        type(Distribution), intent(in) :: dist
        type(Grid), intent(in)         :: g
        integer, intent(in)            :: blockH
        integer                        :: nProcs

        ! Have all ranks execute the command
        nProcs = numRanks()
        call cwrap__distribution_applyFillBlock( &
            cstr(dist%id), cstr(g%id), cint(nProcs), cint(blockH))
    end subroutine

    subroutine distribution_applyBlockFill( &
        dist, g, blockW)
      !`
        type(Distribution), intent(in) :: dist
        type(Grid), intent(in)         :: g
        integer, intent(in)            :: blockW
        integer                        :: nProcs

        ! Have all ranks execute the command
        nProcs = numRanks()
        call cwrap__distribution_applyBlockFill( &
            cstr(dist%id), cstr(g%id), cint(nProcs), cint(blockW))
    end subroutine

    subroutine distribution_applyBlockCyclic( &
        dist, g, blkW, blkH)
      !`
        type(Distribution), intent(in) :: dist
        type(Grid), intent(in)         :: g
        integer, intent(in)            :: blkW, blkH
        integer                        :: nProcs
        
        ! Have all ranks execute the command
        nProcs = numRanks()
        call cwrap__distribution_applyBlockCyclic( &
            cstr(dist%id), cstr(g%id),             &
            cint(nProcs), cint(blkW), cint(blkH))
    end subroutine

    subroutine distribution_visualize(dist, dirName)
        type(Distribution), intent(in) :: dist
        character(len=*), intent(in)   :: dirname
        
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__distribution_visualize(cstr(dist%id), &
                                               dirName // char(0))
        end if
    end subroutine

    integer function distribution_width(dist)
        type(Distribution), intent(in) :: dist
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_width(cstr(dist%id), ret)
        distribution_width = ret
    end function

    integer function distribution_height(dist)
        type(Distribution), intent(in) :: dist
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_height(cstr(dist%id), ret)
        distribution_height = ret
    end function

    integer function distribution_numLocalBlocks(dist)
        type(Distribution), intent(in) :: dist
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_numLocalBlocks(cstr(dist%id), ret)
        distribution_numLocalBlocks = ret
    end function

    integer function distribution_lbid2gbid(dist, lbid)
        type(Distribution), intent(in) :: dist
        integer, intent(in) :: lbid
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_lbid2gbid(cstr(dist%id), cint(lbid), ret)
        distribution_lbid2gbid = ret
    end function

    integer function distribution_gbid2lbid(dist, gbid)
        type(Distribution), intent(in) :: dist
        integer, intent(in) :: gbid
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_gbid2lbid(cstr(dist%id), cint(gbid), ret)
        distribution_gbid2lbid = ret
    end function

    integer function distribution_gbid2proc(dist, gbid)
        type(Distribution), intent(in) :: dist
        integer, intent(in) :: gbid
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_gbid2proc(cstr(dist%id), cint(gbid), ret)
        distribution_gbid2proc = ret
    end function

    integer function distribution_blockLowX(dist, gbid)
        type(Distribution), intent(in) :: dist
        integer, intent(in) :: gbid
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_blockLowX(cstr(dist%id), cint(gbid), ret)
        distribution_blockLowX = ret
    end function

    integer function distribution_blockLowY(dist, gbid)
        type(Distribution), intent(in) :: dist
        integer, intent(in) :: gbid
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_blockLowY(cstr(dist%id), cint(gbid), ret)
        distribution_blockLowY = ret
    end function

    integer function distribution_blockHighX(dist, gbid)
        type(Distribution), intent(in) :: dist
        integer, intent(in) :: gbid
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_blockHighX(cstr(dist%id), cint(gbid), ret)
        distribution_blockHighX = ret
    end function

    integer function distribution_blockHighY(dist, gbid)
        type(Distribution), intent(in) :: dist
        integer, intent(in) :: gbid
        integer(c_int) :: ret

        ! Have master rank execute the command
        call cwrap__distribution_blockHighY(cstr(dist%id), cint(gbid), ret)
        distribution_blockHighY = ret
    end function

    subroutine distribution_pos2BlockPos(dist, x, y, sg, blkX, blkY, gbid)
        type(Distribution), intent(in) :: dist
        integer, intent(in) :: x, y
        type(Subgrid), intent(in) :: sg
        integer, intent(inout) :: blkX, blkY, gbid
        integer(c_int) :: blkX_cint, blkY_cint, gbid_cint

        ! Have master rank execute the command
        call cwrap__distribution_pos2BlockPos( &
            cstr(dist%id), cint(x), cint(y), cstr(sg%id), &
            blkX_cint, blkY_cint, gbid_cint)
        blkX = blkX_cint
        blkY = blkY_cint
        gbid = gbid_cint
    end subroutine

    subroutine environment_print()
        ! Have all ranks execute the command
        call cwrap__environment_print()
    end subroutine

    subroutine environment_output(filename)
        character(len=*), intent(in) :: filename
        
        ! Have all ranks execute the command
        if(myRank() == 0) then
            call cwrap__environment_output(filename // char(0))
        end if
    end subroutine

    subroutine environment_input(filename)
        character(len=*), intent(in) :: filename
        
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__environment_input(filename // char(0))
        end if
    end subroutine

    subroutine environment_clear()
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__environment_clear()
        end if
    end subroutine

    end module
