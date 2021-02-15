module mod_schedule
    use mod_string
    use mod_utils
    use mod_gridweaver
    use iso_c_binding
    implicit none

    public schedule_new,                        &
           schedule_calculate,                  &
           schedule_calculateGhostNodePlan,     &
           schedule_transferToFortran,          &
           schedule_transferGhostsToFortran,    &
           schedule_printFortranVersion,        &
           schedule_printGhostsFortranVersion,  &
           schedule_sendRegionSize,             &
           schedule_recvRegionSize

    type Schedule
        type(String)        :: id
        type(Grid)          :: grid
        type(Distribution)  :: dist
        integer             :: depth
        
        !-----------------
        ! Halo messages:
        !-----------------
        integer(c_int) :: nMsgsRecv
        integer(c_int), allocatable :: msgRecvFrom(:)
        integer(c_int), allocatable :: recvMsgStart(:)
        integer(c_int), allocatable :: numTransfersInRecvMsg(:)
        integer(c_int), allocatable :: transferRecvAtLBID(:)
        integer(c_int), allocatable :: transferRegionRecvLowX(:)
        integer(c_int), allocatable :: transferRegionRecvLowY(:)
        integer(c_int), allocatable :: transferRegionRecvHighX(:)
        integer(c_int), allocatable :: transferRegionRecvHighY(:)
        integer(c_int), allocatable :: transferRegionRecvOrientation(:)
        
        integer(c_int) :: nMsgsSend
        integer(c_int), allocatable :: msgSendTo(:)
        integer(c_int), allocatable :: sendMsgStart(:)
        integer(c_int), allocatable :: numTransfersInSendMsg(:)
        integer(c_int), allocatable :: transferSendFromLBID(:)
        integer(c_int), allocatable :: transferRegionSendLowX(:)
        integer(c_int), allocatable :: transferRegionSendLowY(:)
        integer(c_int), allocatable :: transferRegionSendHighX(:)
        integer(c_int), allocatable :: transferRegionSendHighY(:)
        integer(c_int), allocatable :: transferRegionSendOrientation(:)
        
        !-----------------
        ! Ghost messages:
        !-----------------
        integer(c_int) :: nGhostMsgsRecv
        integer(c_int), allocatable :: ghostMsgRecvFrom(:)
        integer(c_int), allocatable :: recvGhostMsgStart(:)
        integer(c_int), allocatable :: recvGhostMsgSG(:)
        integer(c_int), allocatable :: recvGhostMsgX(:)
        integer(c_int), allocatable :: recvGhostMsgY(:)

        integer(c_int) :: nGhostMsgsSend
        integer(c_int), allocatable :: ghostMsgSendTo(:)
        integer(c_int), allocatable :: sendGhostMsgStart(:)
        integer(c_int), allocatable :: sendGhostMsgSG(:)
        integer(c_int), allocatable :: sendGhostMsgX(:)
        integer(c_int), allocatable :: sendGhostMsgY(:)
        integer, allocatable :: sendGhostLbid(:)
        integer, allocatable :: sendGhostBlockI(:)
        integer, allocatable :: sendGhostBlockJ(:)
    end type

  interface

    subroutine cwrap__schedule_new(sched) &
        bind(C, name="__schedule_new")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
    end subroutine

    subroutine cwrap__schedule_calculate(sched, g, dist, depth) &
        bind(C, name="__schedule_calculate")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: dist(*)
        integer(c_int)         :: depth
    end subroutine

    subroutine cwrap__schedule_calculateGhostNodePlan(sched, g, dist, depth) &
        bind(C, name="__schedule_calculateGhostNodePlan")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
        character(kind=c_char) :: g(*)
        character(kind=c_char) :: dist(*)
        integer(c_int)         :: depth
    end subroutine

    subroutine cwrap__schedule_transferSizesToFortran(  &
        sched,                          &
        size_msgRecvFrom,               &
        size_recvMsgStart,              &
        size_numTransfersInRecvMsg,     &
        size_transferRecvAtLBID,        &
        size_transferRegionRecvLowX,    &
        size_transferRegionRecvLowY,    &
        size_transferRegionRecvHighX,   &
        size_transferRegionRecvHighY,   &
        size_transferRecvOrientation,   &
        size_msgSendTo,                 &
        size_sendMsgStart,              &
        size_numTransfersInSendMsg,     &
        size_transferSendFromLBID,      &
        size_transferRegionSendLowX,    &
        size_transferRegionSendLowY,    &
        size_transferRegionSendHighX,   &
        size_transferRegionSendHighY,   &
        size_transferSendOrientation)   &
        !`
        bind(C, name="__schedule_transferSizesToFortran")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
        integer(c_int), intent(inout) :: size_msgRecvFrom
        integer(c_int), intent(inout) :: size_recvMsgStart
        integer(c_int), intent(inout) :: size_numTransfersInRecvMsg
        integer(c_int), intent(inout) :: size_transferRecvAtLBID
        integer(c_int), intent(inout) :: size_transferRegionRecvLowX
        integer(c_int), intent(inout) :: size_transferRegionRecvLowY
        integer(c_int), intent(inout) :: size_transferRegionRecvHighX
        integer(c_int), intent(inout) :: size_transferRegionRecvHighY
        integer(c_int), intent(inout) :: size_transferRecvOrientation
        integer(c_int), intent(inout) :: size_msgSendTo
        integer(c_int), intent(inout) :: size_sendMsgStart
        integer(c_int), intent(inout) :: size_numTransfersInSendMsg
        integer(c_int), intent(inout) :: size_transferSendFromLBID
        integer(c_int), intent(inout) :: size_transferRegionSendLowX
        integer(c_int), intent(inout) :: size_transferRegionSendLowY
        integer(c_int), intent(inout) :: size_transferRegionSendHighX
        integer(c_int), intent(inout) :: size_transferRegionSendHighY
        integer(c_int), intent(inout) :: size_transferSendOrientation
    end subroutine

    subroutine cwrap__schedule_transferGhostSizesToFortran(  &
        sched,                  &
        size_ghostMsgRecvFrom,  &
        size_recvGhostMsgStart, &
        size_recvGhostMsgSG,    &
        size_recvGhostMsgX,     &
        size_recvGhostMsgY,     &
        size_ghostMsgSendTo,    &
        size_sendGhostMsgStart, &
        size_sendGhostMsgSG,    &
        size_sendGhostMsgX,     &
        size_sendGhostMsgY)     &
        !`
        bind(C, name="__schedule_transferGhostSizesToFortran")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
        integer(c_int), intent(inout) :: size_ghostMsgRecvFrom
        integer(c_int), intent(inout) :: size_recvGhostMsgStart
        integer(c_int), intent(inout) :: size_recvGhostMsgSG
        integer(c_int), intent(inout) :: size_recvGhostMsgX
        integer(c_int), intent(inout) :: size_recvGhostMsgY
        integer(c_int), intent(inout) :: size_ghostMsgSendTo
        integer(c_int), intent(inout) :: size_sendGhostMsgStart
        integer(c_int), intent(inout) :: size_sendGhostMsgSG
        integer(c_int), intent(inout) :: size_sendGhostMsgX
        integer(c_int), intent(inout) :: size_sendGhostMsgY
    end subroutine

    subroutine cwrap__schedule_transferToFortran( &
        sched,                         &
        nMsgsRecv,                     &
        msgRecvFrom,                   &
        recvMsgStart,                  &
        numTransfersInRecvMsg,         &
        transferRecvAtLBID,            &
        transferRegionRecvLowX,        &
        transferRegionRecvLowY,        &
        transferRegionRecvHighX,       &
        transferRegionRecvHighY,       &
        transferRecvOrientation,       &
        nMsgsSend,                     &
        msgSendTo,                     &
        sendMsgStart,                  &
        numTransfersInSendMsg,         &
        transferSendFromLBID,          &
        transferRegionSendLowX,        &
        transferRegionSendLowY,        &
        transferRegionSendHighX,       &
        transferRegionSendHighY,       &
        transferSendOrientation)       &
        !`
        bind(C, name="__schedule_transferToFortran")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
        integer(c_int)              , intent(inout) :: nMsgsRecv
        integer(c_int), dimension(*), intent(inout) :: msgRecvFrom(*)
        integer(c_int), dimension(*), intent(inout) :: recvMsgStart(*)
        integer(c_int), dimension(*), intent(inout) :: numTransfersInRecvMsg(*)
        integer(c_int), dimension(*), intent(inout) :: transferRecvAtLBID(*)
        integer(c_int), dimension(*), intent(inout) :: transferRegionRecvLowX(*)
        integer(c_int), dimension(*), intent(inout) :: transferRegionRecvLowY(*)
        integer(c_int), dimension(*), intent(inout) :: transferRegionRecvHighX(*)
        integer(c_int), dimension(*), intent(inout) :: transferRegionRecvHighY(*)
        integer(c_int), dimension(*), intent(inout) :: transferRecvOrientation(*)
        integer(c_int)              , intent(inout) :: nMsgsSend
        integer(c_int), dimension(*), intent(inout) :: msgSendTo(*)
        integer(c_int), dimension(*), intent(inout) :: sendMsgStart(*)
        integer(c_int), dimension(*), intent(inout) :: numTransfersInSendMsg(*)
        integer(c_int), dimension(*), intent(inout) :: transferSendFromLBID(*)
        integer(c_int), dimension(*), intent(inout) :: transferRegionSendLowX(*)
        integer(c_int), dimension(*), intent(inout) :: transferRegionSendLowY(*)
        integer(c_int), dimension(*), intent(inout) :: transferRegionSendHighX(*)
        integer(c_int), dimension(*), intent(inout) :: transferRegionSendHighY(*)
        integer(c_int), dimension(*), intent(inout) :: transferSendOrientation(*)
    end subroutine

    subroutine cwrap__schedule_transferGhostsToFortran( &
        sched,              &
        nGhostMsgsRecv,     &
        ghostMsgRecvFrom,   &
        recvGhostMsgStart,  &
        recvGhostMsgSG,     &
        recvGhostMsgX,      &
        recvGhostMsgY,      &
        nGhostMsgsSend,     &
        ghostMsgSendTo,     &
        sendghostMsgStart,  &
        sendGhostMsgSG,     &
        sendGhostMsgX,      &
        sendGhostMsgY)      &
        !`
        bind(C, name="__schedule_transferGhostsToFortran")
        use iso_c_binding
        character(kind=c_char) :: sched(*)
        integer(c_int)              , intent(inout) :: nGhostMsgsRecv
        integer(c_int), dimension(*), intent(inout) :: ghostMsgRecvFrom(*)
        integer(c_int), dimension(*), intent(inout) :: recvGhostMsgStart(*)
        integer(c_int), dimension(*), intent(inout) :: recvGhostMsgSG(*)
        integer(c_int), dimension(*), intent(inout) :: recvGhostMsgX(*)
        integer(c_int), dimension(*), intent(inout) :: recvGhostMsgY(*)
        integer(c_int),               intent(inout) :: nGhostMsgsSend
        integer(c_int), dimension(*), intent(inout) :: ghostMsgSendTo(*)
        integer(c_int), dimension(*), intent(inout) :: sendghostMsgStart(*)
        integer(c_int), dimension(*), intent(inout) :: sendGhostMsgSG(*)
        integer(c_int), dimension(*), intent(inout) :: sendGhostMsgX(*)
        integer(c_int), dimension(*), intent(inout) :: sendGhostMsgY(*)
    end subroutine

  end interface
  contains

    function schedule_new(name)
        character(len=*), intent(in) :: name
        type(Schedule)               :: schedule_new
        
        ! Since schedules are distributed objects have all ranks execute the
        ! command.
        call cwrap__schedule_new(name // char(0))
        
        call string_init(schedule_new%id, name)
    end function

    subroutine schedule_calculate(sched, g, dist, depth)
        type(Schedule), intent(inout)  :: sched
        type(Grid), intent(in)         :: g
        type(Distribution), intent(in) :: dist
        integer :: depth

        ! Since schedules are distributed objects have all ranks execute the
        ! command.
        call cwrap__schedule_calculate( &
            cstr(sched%id), cstr(g%id), cstr(dist%id), cint(depth))

        ! Transfer data to Fortran object
        call schedule_transferToFortran(sched)
        sched%dist  = dist
        sched%grid  = g
        sched%depth = depth

        if(depth > 1) then
            call schedule_calculateGhostNodePlan(sched, g, dist, depth)
        end if
    end subroutine

    subroutine schedule_calculateGhostNodePlan(sched, g, dist, depth)
        type(Schedule), intent(inout)  :: sched
        type(Grid), intent(in)         :: g
        type(Distribution), intent(in) :: dist
        integer, intent(in)            :: depth

        ! Since schedules are distributed objects have all ranks execute the
        ! command.
        call cwrap__schedule_calculateGhostNodePlan( &
            cstr(sched%id), cstr(g%id), cstr(dist%id), cint(depth))

        ! Transfer data to Fortran object
        call schedule_transferGhostsToFortran(sched)
        sched%dist = dist
        sched%grid = g
    end subroutine

    subroutine schedule_transferToFortran(sched)
        type(Schedule), intent(inout)  :: sched
        integer(c_int) :: size_msgRecvFrom,               &
                          size_recvMsgStart,              &
                          size_numTransfersInRecvMsg,     &
                          size_transferRecvAtLBID,        &
                          size_transferRegionRecvLowX,    &
                          size_transferRegionRecvLowY,    &
                          size_transferRegionRecvHighX,   &
                          size_transferRegionRecvHighY,   &
                          size_transferRecvOrientation,   &
                          size_msgSendTo,                 &
                          size_sendMsgStart,              &
                          size_numTransfersInSendMsg,     &
                          size_transferSendFromLBID,      &
                          size_transferRegionSendLowX,    &
                          size_transferRegionSendLowY,    &
                          size_transferRegionSendHighX,   &
                          size_transferRegionSendHighY,   &
                          size_transferSendOrientation    

        ! Allocate arrays for Fortran version of schedule
        call cwrap__schedule_transferSizesToFortran(    &
            cstr(sched%id),                 &
            size_msgRecvFrom,               &
            size_recvMsgStart,              &
            size_numTransfersInRecvMsg,     &
            size_transferRecvAtLBID,        &
            size_transferRegionRecvLowX,    &
            size_transferRegionRecvLowY,    &
            size_transferRegionRecvHighX,   &
            size_transferRegionRecvHighY,   &
            size_transferRecvOrientation,   &
            size_msgSendTo,                 &
            size_sendMsgStart,              &
            size_numTransfersInSendMsg,     &
            size_transferSendFromLBID,      &
            size_transferRegionSendLowX,    &
            size_transferRegionSendLowY,    &
            size_transferRegionSendHighX,   &
            size_transferRegionSendHighY,   &
            size_transferSendOrientation)
        
        allocate(sched%msgRecvFrom(size_msgRecvFrom))
        allocate(sched%recvMsgStart(size_recvMsgStart))
        allocate(sched%numTransfersInRecvMsg(size_numTransfersInRecvMsg))
        allocate(sched%transferRecvAtLBID(size_transferRecvAtLBID))
        allocate(sched%transferRegionRecvLowX(size_transferRegionRecvLowX))
        allocate(sched%transferRegionRecvLowY(size_transferRegionRecvLowY))
        allocate(sched%transferRegionRecvHighX(size_transferRegionRecvHighX))
        allocate(sched%transferRegionRecvHighY(size_transferRegionRecvHighY))
        allocate(sched%transferRegionRecvOrientation(size_transferRecvOrientation))
        
        allocate(sched%msgSendTo(size_msgSendTo))
        allocate(sched%sendMsgStart(size_sendMsgStart))
        allocate(sched%numTransfersInSendMsg(size_numTransfersInSendMsg))
        allocate(sched%transferSendFromLBID(size_transferSendFromLBID))
        allocate(sched%transferRegionSendLowX(size_transferRegionSendLowX))
        allocate(sched%transferRegionSendLowY(size_transferRegionSendLowY))
        allocate(sched%transferRegionSendHighX(size_transferRegionSendHighX))
        allocate(sched%transferRegionSendHighY(size_transferRegionSendHighY))
        allocate(sched%transferRegionSendOrientation(size_transferSendOrientation))

        ! Transfer data from C++ schedule into fortran schedule
        call cwrap__schedule_transferToFortran(     &
            cstr(sched%id),                         &
            sched%nMsgsRecv,                        &
            sched%msgRecvFrom,                      &
            sched%recvMsgStart,                     &
            sched%numTransfersInRecvMsg,            &
            sched%transferRecvAtLBID,               &
            sched%transferRegionRecvLowX,           &
            sched%transferRegionRecvLowY,           &
            sched%transferRegionRecvHighX,          &
            sched%transferRegionRecvHighY,          &
            sched%transferRegionRecvOrientation,    &
            sched%nMsgsSend,                        &
            sched%msgSendTo,                        &
            sched%sendMsgStart,                     &
            sched%numTransfersInSendMsg,            &
            sched%transferSendFromLBID,             &
            sched%transferRegionSendLowX,           &
            sched%transferRegionSendLowY,           &
            sched%transferRegionSendHighX,          &
            sched%transferRegionSendHighY,          &
            sched%transferRegionSendOrientation)
    end subroutine


    subroutine schedule_transferGhostsToFortran(sched)
        type(Schedule), intent(inout)  :: sched
        integer(c_int) :: size_ghostMsgRecvFrom,  &
                          size_recvGhostMsgStart, &
                          size_recvGhostMsgSG,    &
                          size_recvGhostMsgX,     &
                          size_recvGhostMsgY,     &
                          size_ghostMsgSendTo,    &
                          size_sendGhostMsgStart, &
                          size_sendGhostMsgSG,    &
                          size_sendGhostMsgX,     &
                          size_sendGhostMsgY
        integer :: msg,i, gbid,lbid, x,y
                 
        ! Allocate arrays for Fortran version of schedule
        call cwrap__schedule_transferGhostSizesToFortran(    &
            cstr(sched%id),             &
            size_ghostMsgRecvFrom,      &
            size_recvGhostMsgStart,     &
            size_recvGhostMsgSG,        &
            size_recvGhostMsgX,         &
            size_recvGhostMsgY,         &
            size_ghostMsgSendTo,        &
            size_sendGhostMsgStart,     &
            size_sendGhostMsgSG,        &
            size_sendGhostMsgX,         &
            size_sendGhostMsgY)

        allocate(sched%ghostMsgRecvFrom(size_ghostMsgRecvFrom))
        allocate(sched%recvGhostMsgStart(size_recvGhostMsgStart))
        allocate(sched%recvGhostMsgSG(size_recvGhostMsgSG))
        allocate(sched%recvGhostMsgX(size_recvGhostMsgX))
        allocate(sched%recvGhostMsgY(size_recvGhostMsgY))
        allocate(sched%GhostMsgSendTo(size_ghostMsgSendTo))
        allocate(sched%sendGhostMsgStart(size_sendGhostMsgStart))
        allocate(sched%sendGhostMsgSG(size_sendGhostMsgSG))
        allocate(sched%sendGhostMsgX(size_sendGhostMsgX))
        allocate(sched%sendGhostMsgY(size_sendGhostMsgY))

        allocate(sched%sendGhostLbid(size_sendGhostMsgSG))
        allocate(sched%sendGhostBlockI(size_sendGhostMsgX))
        allocate(sched%sendGhostBlockJ(size_sendGhostMsgY))

        ! Transfer data from C++ schedule into fortran schedule
        call cwrap__schedule_transferGhostsToFortran( &
            cstr(sched%id),             &
            sched%nGhostMsgsRecv,       &
            sched%ghostMsgRecvFrom,     &
            sched%recvGhostMsgStart,    &
            sched%recvGhostMsgSG,       &
            sched%recvGhostMsgX,        &
            sched%recvGhostMsgY,        &
            sched%nGhostMsgsSend,       &
            sched%ghostMsgSendTo,       &
            sched%sendGhostMsgStart,    &
            sched%sendGhostMsgSG,       &
            sched%sendGhostMsgX,        &
            sched%sendGhostMsgY)

        do msg=1,sched%nGhostMsgsSend
            do i=sched%sendGhostMsgStart(msg),sched%sendGhostMsgStart(msg+1)-1
                gbid = distribution_gbidAtSGID(     &
                    sched%dist,                 &
                    sched%sendGhostMsgSG(i),    &
                    sched%sendGhostMsgX(i),     &
                    sched%sendGhostMsgY(i))
                lbid = distribution_gbid2lbid(sched%dist, gbid)
                x = sched%sendGhostMsgX(i) - &
                    distribution_blockLowX(sched%dist, gbid) + 1
                y = sched%sendGhostMsgY(i) - &
                    distribution_blockLowY(sched%dist, gbid) + 1

                sched%sendGhostLbid(i)   = lbid
                sched%sendGhostBlockI(i) = x
                sched%sendGhostBlockJ(i) = y
            end do
        end do
    end subroutine

    subroutine schedule_printFortranVersion(sched)
        include 'mpif.h'
        type(Schedule), intent(in) :: sched
        integer :: i, j,k,msg, err
        
        call MPI_BARRIER(MPI_COMM_WORLD, err)
        do i=0,numRanks()
            ! Print receiving side information
            if(myRank() == i) then
                write(6, '(A,I3)') "Data for rank ", i;
                write(6, '(A)') "========================================="
                write(6, '(A,I0,A,I0)') " nMsgsRecv@", myRank(), " = ", sched%nMsgsRecv
                write(6, '(A,I0,A,I0)', advance='no') " msgRecvFrom@", myRank(), " = ["
                do j=1,sched%nMsgsRecv
                    if(j /= 1)  write(6, '(A)', advance='no') ", "
                    write(6, '(I0)', advance='no') sched%msgRecvFrom(j)
                end do
                write(6, '(A)') "]"
                
                ! Print transferRecvAtLBID
                do j=1,sched%nMsgsRecv
                !`
                    write(6, '(A,I0,A,I0,A)', advance='no') &
                         " mTransferRecvAtLBID[",j,"]@",myRank()," = ["

                    do k=sched%recvMsgStart(j), &
                     sched%recvMsgStart(j) + sched%numTransfersInRecvMsg(j) - 1
                    !`
                        if(k /= sched%recvMsgStart(j)) &
                            write(6, '(A)', advance='no') ", "
                        write(6, '(I0)', advance='no') &
                            sched%transferRecvAtLBID(k)
                    end do
                    write(6, '(A)') "]"
                end do

                ! Print transfer region
                do j=1,sched%nMsgsRecv
                !`
                    do k=sched%recvMsgStart(j), &
                     sched%recvMsgStart(j) + sched%numTransfersInRecvMsg(j) - 1
                    !`
                        write(6, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)', advance='no') &
                            " mTransferRegionRecv[",j,"]@",myRank()," = [(", &
                            sched%transferRegionRecvLowX(k),  ", ",    &
                            sched%transferRegionRecvLowY(k),  " - ",   &
                            sched%transferRegionRecvHighX(k), ", ",    &
                            sched%transferRegionRecvHighY(k), "  "
                            select case (sched%transferRegionRecvOrientation(j))
                                case(0) ; write(6, '(A)', advance='no') "BL"
                                case(1) ; write(6, '(A)', advance='no') "BR"
                                case(2) ; write(6, '(A)', advance='no') "TL"
                                case(3) ; write(6, '(A)', advance='no') "TR"
                            end select
                            write(6, '(A)', advance='no') ")] "
                    end do
                    write(6, '()')
                end do
            end if
            call MPI_BARRIER(MPI_COMM_WORLD, err)
            if(myRank() == 0) write(6, '()')
        end do

        if(myRank() == 0) then
            write(6, '()')
            write(6, '(A)') "                   * * * * * * * * *"
            write(6, '()')
        end if

        do i=0,numRanks()
            ! Print sending side information
            if(myRank() == i) then
                write(6, '(A,I3)') "Data for rank ", i;
                write(6, '(A)') "========================================="
                write(6, '(A,I0,A,I0)') " nMsgsSend@", myRank(), " = ", sched%nMsgsSend
                write(6, '(A,I0,A,I0)', advance='no') " msgSendTo@", myRank(), " = ["
                do j=1,sched%nMsgsSend
                    if(j /= 1) write(6, '(A)', advance='no') ", "
                    write(6, '(I0)', advance='no') sched%msgSendTo(j)
                end do
                write(6, '(A)') "]"
                
                ! Print transferSendFromLBID
                do j=1,sched%nMsgsSend
                !`
                    write(6, '(A,I0,A,I0,A)', advance='no') &
                         " mTransferSendFromLBID[",j,"]@",myRank()," = ["

                    do k=sched%recvMsgStart(j), &
                     sched%recvMsgStart(j) + sched%numTransfersInSendMsg(j) - 1
                    !`
                        if(k /= sched%recvMsgStart(j)) &
                            write(6, '(A)', advance='no') ", "
                        write(6, '(I0)', advance='no') &
                            sched%transferSendFromLBID(k)
                    end do
                    write(6, '(A)') "]"
                end do

                ! Print transfer region
                do j=1,sched%nMsgsSend
                !`
                    do k=sched%recvMsgStart(j), &
                     sched%recvMsgStart(j) + sched%numTransfersInSendMsg(j) - 1
                    !`
                        write(6, '(A,I0,A,I0,A,I0,A,I0,A,I0,A,I0,A)', advance='no') &
                            " mTransferRegionSend[",j,"]@",myRank()," = [(", &
                            sched%transferRegionSendLowX(k),  ", ",    &
                            sched%transferRegionSendLowY(k),  " - ",   &
                            sched%transferRegionSendHighX(k), ", ",    &
                            sched%transferRegionSendHighY(k), "  "
                            select case (sched%transferRegionSendOrientation(j))
                                case(0) ; write(6, '(A)', advance='no') "BL"
                                case(1) ; write(6, '(A)', advance='no') "BR"
                                case(2) ; write(6, '(A)', advance='no') "TL"
                                case(3) ; write(6, '(A)', advance='no') "TR"
                            end select
                            write(6, '(A)', advance='no') ")] "
                    end do
                    write(6, '()')
                end do
            end if
            call MPI_BARRIER(MPI_COMM_WORLD, err)
            if(myRank() == 0) write(6, '()')
        end do
    end subroutine

    subroutine schedule_printGhostsFortranVersion(sched)
        include 'mpif.h'
        type(Schedule), intent(in) :: sched
        integer :: i, j, k, msg, err
        
        call MPI_BARRIER(MPI_COMM_WORLD, err)
        do i=0,numRanks()
            ! Print receiving side information
            if(myRank() == i) then
                write(6, '(A,I3)') "Data for rank ", i;
                write(6, '(A)') "========================================="
                write(6, '(A,I0,A,I0)') " nGhostMsgsRecv@", myRank(), " = ", sched%nGhostMsgsRecv
                write(6, '(A,I0,A,I0)', advance='no') " ghostMsgRecvFrom@", myRank(), " = ["
                do j=1,sched%nGhostMsgsRecv
                    if(j /= 1)  write(6, '(A)', advance='no') ", "
                    write(6, '(I0)', advance='no') sched%ghostMsgRecvFrom(j)
                end do
                write(6, '(A)') "]"

                ! Print coordinates for each message
                do j=1,sched%nMsgsRecv
                !`
                    write(6, '(A,I0,A,I0,A)', advance='no') &
                        " mTransferGhostCoordsRecv[",i,"]@",myRank()," = ["
                    do k=sched%recvGhostMsgStart(j), &
                         sched%recvGhostMsgStart(j+1) - 1
                    !`
                        if(k /= sched%recvGhostMsgStart(j)) &
                            write(6, '(A)', advance='no') ", "
                        write(6, '(A,I0,A,I0,A,I0,A)', advance='no') "(", &
                            sched%recvGhostMsgSG(k),  ", <",    &
                            sched%recvGhostMsgX(k),   ", ",     &
                            sched%recvGhostMsgY(k),   ">)"
                    end do
                    write(6, '(A)') "]"
                end do
            end if
            call MPI_BARRIER(MPI_COMM_WORLD, err)
            if(myRank() == 0) write(6, '()')
        end do

        if(myRank() == 0) then
            write(6, '()')
            write(6, '(A)') "                   * * * * * * * * *"
            write(6, '()')
        end if

        call MPI_BARRIER(MPI_COMM_WORLD, err)
        do i=0,numRanks()
            ! Print sending side information
            if(myRank() == i) then
                write(6, '(A,I3)') "Data for rank ", i;
                write(6, '(A)') "========================================="
                write(6, '(A,I0,A,I0)') " nGhostMsgsSend@", myRank(), " = ", sched%nGhostMsgsSend
                write(6, '(A,I0,A,I0)', advance='no') " ghostMsgSendTo@", myRank(), " = ["
                do j=1,sched%nGhostMsgsSend
                    if(j /= 1)  write(6, '(A)', advance='no') ", "
                    write(6, '(I0)', advance='no') sched%ghostMsgSendTo(j)
                end do
                write(6, '(A)') "]"

                ! Print coordinates for each message
                do j=1,sched%nMsgsSend
                !`
                    write(6, '(A,I0,A,I0,A)', advance='no') &
                        " mTransferGhostCoordsSend[",i,"]@",myRank()," = ["
                    do k=sched%recvGhostMsgStart(j), &
                         sched%recvGhostMsgStart(j+1) - 1
                    !`
                        if(k /= sched%recvGhostMsgStart(j)) &
                            write(6, '(A)', advance='no') ", "
                        write(6, '(A,I0,A,I0,A,I0,A)', advance='no') "(", &
                            sched%recvGhostMsgSG(k),  ", <",    &
                            sched%recvGhostMsgX(k),   ", ",     &
                            sched%recvGhostMsgY(k),   ">)"
                    end do
                    write(6, '(A)') "]"
                end do
            end if
            call MPI_BARRIER(MPI_COMM_WORLD, err)
            if(myRank() == 0) write(6, '()')
        end do
    end subroutine


    integer function schedule_sendRegionSize(self, tr)
        type(Schedule), intent(in)  :: self
        integer, intent(in)         :: tr
        
        schedule_sendRegionSize =                       &
            (self%transferRegionSendHighX(tr) -         &
                self%transferRegionSendLowX(tr) + 1) *  &
            (self%transferRegionSendHighY(tr) -         &
                self%transferRegionSendLowY(tr) + 1)
    end function

    integer function schedule_recvRegionSize(self, tr)
        type(Schedule), intent(in)  :: self
        integer, intent(in)         :: tr
        
        schedule_recvRegionSize =                       &
            (self%transferRegionRecvHighX(tr) -         &
                self%transferRegionRecvLowX(tr) + 1) *  &
            (self%transferRegionRecvHighY(tr) -         &
                self%transferRegionRecvLowY(tr) + 1)
    end function

end module
