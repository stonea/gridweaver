module mod_wrapper
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

    type Schedule
        type(String) :: id
    end type


    ! **************************************************************************
    ! ** - [FUNCTIONS] - *******************************************************
    ! **************************************************************************

    ! The following functions are only run on proc 0
    public :: gridweaver_initialize

    public :: turnOffSyntaxHighlighting

    public :: neighbor_new

    public :: subgrid_new

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
              !grid_placeAdjacentWithOffsetLR,   &
              !grid_placeAdjacentWithOffsetRL,   &
              !grid_placeAdjacentWithOffsetTB,   &
              !grid_placeAdjacentWithOffsetBT,   &
              grid_mirrorT,                     &
              grid_mirrorB,                     &
              grid_mirrorL,                     &
              grid_mirrorR,                     &
              grid_foldT,                       &
              grid_foldB,                       &
              grid_foldL,                       &
              grid_foldR

    public :: distribution_new,                 &
              distribution_applyFillBlock,      &
              distribution_applyBlockFill,      &
              distribution_applyBlockCyclic,    &
              distribution_visualize

    public :: schedule_new,                                 &
              schedule_calculate

    public :: environment_print,                            &
              environment_output,                           &
              environment_input,                            &
              environment_clear

    ! For schedule_ commands that have a procID parameter the command is
    ! only executed on procID
    public :: schedule_grid,                                &
              schedule_distribution,                        &
              schedule_numMessagesToRecv,                   &
              schedule_msgRecvFrom,                         &
              schedule_transfersInRecvMsg,                  &
              schedule_transferRecvAtLBID,                  &
              schedule_transferRecvRegionLowX,              &
              schedule_transferRecvRegionHighX,             &
              schedule_transferRecvRegionLowY,              &
              schedule_transferRecvRegionHighY,             &
              schedule_transferRecvRegionOrientation,       &
              schedule_numMessagesToSend,                   &
              schedule_msgSendTo,                           &
              schedule_transfersInSendMsg,                  &
              schedule_transferSendAtLBID,                  &
              schedule_transferSendRegionLowX,              &
              schedule_transferSendRegionHighX,             &
              schedule_transferSendRegionLowY,              &
              schedule_transferSendRegionHighY,             &
              schedule_transferSendRegionOrientation

  interface
    subroutine cwrap__gridweaver_initialize() &
        bind(C, name="__gridweaver_initialize")
        use iso_c_binding
        integer(c_size_t)      :: nProcs
    end subroutine

    subroutine cwrap__turnOffSyntaxHighlighting() &
        bind(C, name="__turnOffSyntaxHighlighting")
        use iso_c_binding
    end subroutine

    subroutine cwrap__neighbor_new(name, x, y) &
        bind(C, name="__neighbor_new")
        use iso_c_binding
        character(kind=c_char) :: name(*)
        integer(c_size_t)      :: x, y
    end subroutine

    subroutine cwrap__subgrid_new(sg, width, height) &
        bind(C, name="__subgrid_new")
        use iso_c_binding
        character(kind=c_char) :: sg(*)
        integer(c_size_t)      :: width, height
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
        integer(c_size_t)      :: srcX1, srcY1, scrX2, scrY2
        character(kind=c_char) :: srcSG(*)
        integer(c_size_t)      :: tgtX1, tgtY1, tgtX2, tgtY2
        character(kind=c_char) :: tgtSG(*)
        integer(c_size_t)      :: rotation
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

    !subroutine cwrap__grid_placeAdjacentWithOffsetLR(g, sgL, sgR, shiftUp) &
    !    bind(C, name="__grid_placeAdjacentWithOffsetLR")
    !    use iso_c_binding
    !    character(kind=c_char) :: g(*)
    !    character(kind=c_char) :: sgL(*)
    !    character(kind=c_char) :: sgR(*)
    !    integer(c_size_t)      :: shiftUp
    !end subroutine

    !subroutine cwrap__grid_placeAdjacentWithOffsetRL(g, sgR, sgL, shiftUp) &
    !    bind(C, name="__grid_placeAdjacentWithOffsetRL")
    !    use iso_c_binding
    !    character(kind=c_char) :: g(*)
    !    character(kind=c_char) :: sgL(*)
    !    character(kind=c_char) :: sgR(*)
    !    integer(c_size_t)      :: shiftUp
    !end subroutine

    !subroutine cwrap__grid_placeAdjacentWithOffsetTB(g, sgT, sgB, shiftRight) &
    !    bind(C, name="__grid_placeAdjacentWithOffsetTB")
    !    use iso_c_binding
    !    character(kind=c_char) :: g(*)
    !    character(kind=c_char) :: sgT(*)
    !    character(kind=c_char) :: sgB(*)
    !    integer(c_size_t)      :: shiftRight
    !end subroutine

    !subroutine cwrap__grid_placeAdjacentWithOffsetBT(g, sgB, sgT, shiftRight) &
    !    bind(C, name="__grid_placeAdjacentWithOffsetBT")
    !    use iso_c_binding
    !    character(kind=c_char) :: g(*)
    !    character(kind=c_char) :: sgB(*)
    !    character(kind=c_char) :: sgT(*)
    !    integer(c_size_t)      :: shiftRight
    !end subroutine

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
        integer(c_size_t)      :: nProcs, blockH
    end subroutine

    subroutine cwrap__distribution_applyBlockFill( &
        dist, g, nprocs, blockW) &
        bind(C, name="__distribution_applyBlockFill")
        use iso_c_binding
        character(kind=c_char) :: dist(*), g(*)
        integer(c_size_t)      :: nProcs, blockW
    end subroutine

    subroutine cwrap__distribution_applyBlockCyclic( &
        dist, g, nProcs, blkW, blkH) &
        bind(C, name="__distribution_applyBlockCyclic")
        use iso_c_binding
        character(kind=c_char) :: dist(*), g(*)
        integer(c_size_t)      :: nProcs, blkW, blkH
    end subroutine

    subroutine cwrap__distribution_visualize(dist, dirName) &
        bind(C, name="__distribution_visualize")
        use iso_c_binding
        character(kind=c_char) :: dist(*), dirname(*)
    end subroutine

    subroutine cwrap__schedule_new(sched) &
        bind(C, name="__schedule_new")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
    end subroutine

    subroutine cwrap__schedule_calculate(sched, g, dist) &
        bind(C, name="__schedule_calculate")
        use iso_c_binding
        character(kind=c_char) :: sched(*), g(*), dist(*)
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

    subroutine cwrap__schedule_grid(sched, gridname) &
        bind(C, name="__schedule_grid")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
        character(kind=c_char) :: gridname(*)
    end subroutine

    subroutine cwrap__schedule_distribution(sched, dist) &
        bind(C, name="__schedule_distribution")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
        character(kind=c_char) :: dist(*)
    end subroutine

    subroutine cwrap__schedule_numMessagesToRecv(sched, procID, ret) &
        bind(C, name="__schedule_numMessagesToRecv")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, ret
    end subroutine

    subroutine cwrap__schedule_msgRecvFrom(sched, procID, msgID, ret) &
        bind(C, name="__schedule_msgRecvFrom")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, ret
    end subroutine

    subroutine cwrap__schedule_transfersInRecvMsg(sched, procID, msgID, ret) &
        bind(C, name="__schedule_transfersInRecvMsg")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, ret
    end subroutine

    subroutine cwrap__schedule_transferRecvAtLBID( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferRecvAtLBID")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferRecvRegionLowX( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferRecvRegionLowX")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferRecvRegionHighX( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferRecvRegionHighX")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferRecvRegionLowY( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferRecvRegionLowY")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferRecvRegionHighY( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferRecvRegionHighY")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferRecvRegionOrientation( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferRecvRegionOrientation")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_numMessagesToSend(sched, procID, ret) &
        bind(C, name="__schedule_numMessagesToSend")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, ret
    end subroutine

    subroutine cwrap__schedule_msgSendTo(sched, procID, msgID, ret) &
        bind(C, name="__schedule_msgSendTo")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, ret
    end subroutine

    subroutine cwrap__schedule_transfersInSendMsg(sched, procID, msgID, ret) &
        bind(C, name="__schedule_transfersInSendMsg")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, ret
    end subroutine

    subroutine cwrap__schedule_transferSendAtLBID( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferSendAtLBID")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferSendRegionLowX( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferSendRegionLowX")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferSendRegionHighX( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferSendRegionHighX")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferSendRegionLowY( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferSendRegionLowY")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferSendRegionHighY( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferSendRegionHighY")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
    end subroutine

    subroutine cwrap__schedule_transferSendRegionOrientation( &
        sched, procID, msgID, tid, ret) &
        !`
        bind(C, name="__schedule_transferSendRegionOrientation")
        use iso_c_binding
        character(kind=c_char) :: sched
        integer(c_size_t) :: procID, msgID, tid, ret
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

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__neighbor_new(name // char(0), cint(x), cint(y))
        end if

        call string_init(neighbor_new%id, name)
    end function

    function subgrid_new(sg, width, height)
        character(len=*), intent(in) :: sg
        integer, intent(in)          :: width, height
        type(Subgrid)                :: subgrid_new

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__subgrid_new(sg // char(0), cint(width), cint(height))
        end if

        call string_init(subgrid_new%id, sg)
    end function
    
    function grid_new(g)
        character(len=*), intent(in) :: g
        type(Grid)                   :: grid_new

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_new(g // char(0))
        end if

        call string_init(grid_new%id, g)
    end function

    subroutine grid_addSubgrid(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_addSubgrid(cstr(g%id), cstr(sg%id))
        end if
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
        
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_addBorder(                                 &
                cstr(g%id),                                             &
                cint(srcX1), cint(srcY1), cint(scrX2), cint(scrY2),     &
                cstr(srcSG%id),                                         &
                cint(tgtX1), cint(tgtY1), cint(tgtX2), cint(tgtY2),     &
                cstr(tgtSG%id),                                         &
                cint(rotation))
        end if
    end subroutine

    subroutine grid_placeAdjacentLR(g, sgL, sgR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sgL
        type(Subgrid), intent(in) :: sgR

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_placeAdjacentLR(cstr(g%id), cstr(sgL%id),      &
                cstr(sgR%id))
        end if
    end subroutine

    subroutine grid_placeAdjacentRL(g, sgR, sgL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sgR
        type(Subgrid), intent(in) :: sgL

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_placeAdjacentRL(cstr(g%id), cstr(sgR%id),      &
                cstr(sgL%id))
        end if
    end subroutine

    subroutine grid_placeAdjacentTB(g, sgT, sgB)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sgT
        type(Subgrid), intent(in) :: sgB

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_placeAdjacentTB(cstr(g%id), cstr(sgT%id),      &
                cstr(sgB%id))
        end if
    end subroutine

    subroutine grid_placeAdjacentBT(g, sgB, sgT)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sgB
        type(Subgrid), intent(in) :: sgT

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_placeAdjacentBT(cstr(g%id), cstr(sgB%id),      &
                cstr(sgT%id))
        end if
    end subroutine

    subroutine grid_connectTtoB(g, sg1, sg2)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg1
        type(Subgrid), intent(in) :: sg2

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectTtoB(cstr(g%id), cstr(sg1%id),      &
                cstr(sg2%id))
        end if
    end subroutine

    subroutine grid_connectRtoL(g, sg1, sg2)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg1
        type(Subgrid), intent(in) :: sg2

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectRtoL(cstr(g%id), cstr(sg1%id),      &
                cstr(sg2%id))
        end if
    end subroutine

    subroutine grid_connectBtoT(g, sg1, sg2)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg1
        type(Subgrid), intent(in) :: sg2

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectBtoT(cstr(g%id), cstr(sg1%id),      &
                cstr(sg2%id))
        end if
    end subroutine

    subroutine grid_connectLtoR(g, sg1, sg2)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg1
        type(Subgrid), intent(in) :: sg2

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectLtoR(cstr(g%id), cstr(sg1%id),      &
                cstr(sg2%id))
        end if
    end subroutine

    subroutine grid_connectLtoT(g, sg,  sgBL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgBL

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectLtoT(cstr(g%id), cstr(sg%id),      &
                cstr(sgBL%id))
        end if
    end subroutine

    subroutine grid_connectLtoB(g, sg,  sgTL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgTL

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectLtoB(cstr(g%id), cstr(sg%id),      &
                cstr(sgTL%id))
        end if
    end subroutine

    subroutine grid_connectRtoT(g, sg,  sgBR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgBR

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectRtoT(cstr(g%id), cstr(sg%id),      &
                cstr(sgBR%id))
        end if
    end subroutine

    subroutine grid_connectRtoB(g, sg,  sgTR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgTR

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectRtoB(cstr(g%id), cstr(sg%id),      &
                cstr(sgTR%id))
        end if
    end subroutine

    subroutine grid_connectTtoL(g, sg,  sgTR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgTR

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectTtoL(cstr(g%id), cstr(sg%id),      &
                cstr(sgTR%id))
        end if
    end subroutine

    subroutine grid_connectTtoR(g, sg,  sgTL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgTL

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectTtoR(cstr(g%id), cstr(sg%id),      &
                cstr(sgTL%id))
        end if
    end subroutine

    subroutine grid_connectBtoL(g, sg,  sgBR)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgBR

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectBtoL(cstr(g%id), cstr(sg%id),      &
                cstr(sgBR%id))
        end if
    end subroutine

    subroutine grid_connectBtoR(g, sg,  sgBL)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
        type(Subgrid), intent(in) :: sgBL

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_connectBtoR(cstr(g%id), cstr(sg%id),      &
                cstr(sgBL%id))
        end if
    end subroutine

    subroutine grid_wrapLR(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg
    
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_wrapLR(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    subroutine grid_wrapTB(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_wrapTB(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    !subroutine grid_placeAdjacentWithOffsetLR(g, sgL, sgR, shiftUp)
    !    type(Grid), intent(in)    :: g
    !    type(Subgrid), intent(in) :: sgL
    !    type(Subgrid), intent(in) :: sgR
    !    integer, intent(in)       :: shiftUp

    !    ! Have master rank execute the command
    !    if(myRank() == 0) then
    !        call cwrap__grid_placeAdjacentWithOffsetLR(cstr(g%id),  &
    !           cstr(sgL%id), cstr(sgR%id), cint(shiftUp))
    !    end if
    !end subroutine

    !subroutine grid_placeAdjacentWithOffsetRL(g, sgR, sgL, shiftUp)
    !    type(Grid), intent(in)    :: g
    !    type(Subgrid), intent(in) :: sgR
    !    type(Subgrid), intent(in) :: sgL
    !    integer, intent(in)       :: shiftUp

    !    ! Have master rank execute the command
    !    if(myRank() == 0) then
    !        call cwrap__grid_placeAdjacentWithOffsetRL(cstr(g%id),  &
    !            cstr(sgR%id), cstr(sgL%id), cint(shiftUp))
    !    end if
    !end subroutine

    !subroutine grid_placeAdjacentWithOffsetTB(g, sgT, sgB, shiftRight)
    !    type(Grid), intent(in)    :: g
    !    type(Subgrid), intent(in) :: sgT
    !    type(Subgrid), intent(in) :: sgB
    !    integer, intent(in)       :: shiftRight

    !    ! Have master rank execute the command
    !    if(myRank() == 0) then
    !        call cwrap__grid_placeAdjacentWithOffsetTB(cstr(g%id),  &
    !            cstr(sgT%id), cstr(sgB%id), cint(shiftRight))
    !    end if
    !end subroutine

    !subroutine grid_placeAdjacentWithOffsetBT(g, sgB, sgT, shiftRight)
    !    type(Grid), intent(in)    :: g
    !    type(Subgrid), intent(in) :: sgB
    !    type(Subgrid), intent(in) :: sgT
    !    integer, intent(in)       :: shiftRight

    !    ! Have master rank execute the command
    !    if(myRank() == 0) then
    !        call cwrap__grid_placeAdjacentWithOffsetBT(cstr(g%id),  &
    !            cstr(sgB%id), cstr(sgT%id), cint(shiftRight))
    !    end if
    !end subroutine

    subroutine grid_mirrorT(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_mirrorT(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    subroutine grid_mirrorB(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_mirrorB(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    subroutine grid_mirrorL(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_mirrorL(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    subroutine grid_mirrorR(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_mirrorR(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    subroutine grid_foldT(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_foldT(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    subroutine grid_foldB(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_foldB(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    subroutine grid_foldL(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_foldL(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    subroutine grid_foldR(g, sg)
        type(Grid), intent(in)    :: g
        type(Subgrid), intent(in) :: sg

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__grid_foldR(cstr(g%id), cstr(sg%id))
        end if
    end subroutine

    function distribution_new(dist)
        character(len=*), intent(in) :: dist
        type(Distribution)           :: distribution_new
        
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__distribution_new(dist // char(0))
        end if

        call string_init(distribution_new%id, dist)
    end function

    subroutine distribution_applyFillBlock( &
        dist, g, blockH)
      !`
        type(Distribution), intent(in) :: dist
        type(Grid), intent(in)         :: g
        integer, intent(in)            :: blockH
        integer                        :: nProcs

        ! Have master rank execute the command
        if(myRank() == 0) then
            nProcs = numRanks()
            call cwrap__distribution_applyFillBlock( &
                cstr(dist%id), cstr(g%id), cint(nProcs), cint(blockH))
        end if
    end subroutine

    subroutine distribution_applyBlockFill( &
        dist, g, blockW)
      !`
        type(Distribution), intent(in) :: dist
        type(Grid), intent(in)         :: g
        integer, intent(in)            :: blockW
        integer                        :: nProcs

        ! Have master rank execute the command
        if(myRank() == 0) then
            nProcs = numRanks()
            call cwrap__distribution_applyBlockFill( &
                cstr(dist%id), cstr(g%id), cint(nProcs), cint(blockW))
        end if
    end subroutine

    subroutine distribution_applyBlockCyclic( &
        dist, g, blkW, blkH)
      !`
        type(Distribution), intent(in) :: dist
        type(Grid), intent(in)         :: g
        integer, intent(in)            :: blkW, blkH
        integer                        :: nProcs
        
        ! Have master rank execute the command
        if(myRank() == 0) then
            nProcs = numRanks()
            call cwrap__distribution_applyBlockCyclic( &
                cstr(dist%id), cstr(g%id),             &
                cint(nProcs), cint(blkW), cint(blkH))
        end if
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

    function schedule_new(name)
        character(len=*), intent(in) :: name
        type(Schedule)               :: schedule_new
        
        ! Since schedules are distributed objects have all ranks execute the
        ! command.
        call cwrap__schedule_new(name // char(0))
        
        call string_init(schedule_new%id, name)
    end function

    subroutine schedule_calculate(sched, g, dist)
        type(Schedule), intent(in)     :: sched
        type(Grid), intent(in)         :: g
        type(Distribution), intent(in) :: dist
        
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__schedule_calculate( &
                cstr(sched%id), cstr(g%id), cstr(dist%id))
        end if
    end subroutine

    subroutine environment_print()
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__environment_print()
        end if
    end subroutine

    subroutine environment_output(filename)
        character(len=*), intent(in) :: filename
        
        ! Have master rank execute the command
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

    function schedule_grid(sched)
        type(Schedule), intent(in) :: sched
        type(Grid) :: schedule_grid
        character(len=256) :: gridname

        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__schedule_grid(cstr(sched%id), gridname)
            call string_init_from_cstr(schedule_grid%id, gridname)
        end if
    end function

    function schedule_distribution(sched)
        type(Schedule), intent(in) :: sched
        type(Distribution) :: schedule_distribution
        character(len=256) :: distname
        
        ! Have master rank execute the command
        if(myRank() == 0) then
            call cwrap__schedule_distribution(cstr(sched%id), distname)
            call string_init_from_cstr(schedule_distribution%id, distname)
        end if
    end function

    subroutine schedule_numMessagesToRecv(sched, procID, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_numMessagesToRecv( &
                cstr(sched%id), cint(procID), cint_ret)
        end if
    end subroutine

    subroutine schedule_msgRecvFrom(sched, procID, msgID, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Only have procID execute the command
        if(myRank() == procID) then
            call cwrap__schedule_msgRecvFrom( &
                cstr(sched%id), cint(procID), cint(msgID), cint_ret)
        end if
    end subroutine

    subroutine schedule_transfersInRecvMsg(sched, procID, msgID, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transfersInRecvMsg( &
                cstr(sched%id), cint(procID), cint(msgID), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferRecvAtLBID(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferRecvAtLBID( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferRecvRegionLowX(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferRecvRegionLowX( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferRecvRegionHighX(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferRecvRegionHighX( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferRecvRegionLowY(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferRecvRegionLowY( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferRecvRegionHighY(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferRecvRegionHighY( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferRecvRegionOrientation( &
        sched, procID, msgID, tid, ret)
    !`
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferRecvRegionOrientation( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_numMessagesToSend(sched, procID, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_numMessagesToSend( &
                cstr(sched%id), cint(procID), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_msgSendTo(sched, procID, msgID, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_msgSendTo( &
                cstr(sched%id), cint(procID), cint(msgID), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transfersInSendMsg(sched, procID, msgID, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transfersInSendMsg( &
                cstr(sched%id), cint(procID), cint(msgID), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferSendAtLBID(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferSendAtLBID( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferSendRegionLowX(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferSendRegionLowX( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferSendRegionHighX(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferSendRegionHighX( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferSendRegionLowY(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferSendRegionLowY( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferSendRegionHighY(sched, procID, msgID, tid, ret)
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferSendRegionHighY( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine

    subroutine schedule_transferSendRegionOrientation( &
        sched, procID, msgID, tid, ret)
    !`
        type(Schedule), intent(in) :: sched
        integer, intent(in)        :: procID, msgID, tid
        integer, intent(out)       :: ret
        integer(c_size_t)          :: cint_ret

        ! Have master rank execute the command
        if(myRank() == procID) then
            call cwrap__schedule_transferSendRegionOrientation( &
                cstr(sched%id), cint(procID), cint(msgID), cint(tid), cint_ret)
            ret = cint_ret
        end if
    end subroutine
end module
