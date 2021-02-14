module mod_data
    use mod_gridweaver
    use mod_schedule
    implicit none

    type DataObj
        type(Grid)               :: grd
        type(Distribution)       :: dist
        type(Schedule), pointer  :: sched
        integer, allocatable     :: vals(:, :, :)  ! dimensions: x, y, lbid
    end type

    type(DataObj), pointer :: inputData
    integer :: blkXOffset, blkYOffset, currentLbid

    public data_new,                &
           data_print,              &
           data_printForProcs,      &
           data_printForProc,       &

           data_apply,              &

           data_forceUpdate

  contains
    function data_new(sched)
        type(DataObj) :: data_new
        type(Schedule), target, intent(in) :: sched

        integer :: i, j, lbid, gbid, numLocalBlocks, blkW, blkH
        
        ! Populate values in the new data object
        data_new%grd   =  sched%grid
        data_new%sched => sched
        data_new%dist  =  sched%dist

        numLocalBlocks = distribution_numLocalBlocks(sched%dist)

        blkW = distribution_width(sched%dist)
        blkH = distribution_height(sched%dist)

        allocate(data_new%vals(0 : blkW+1, &
                               0 : blkH+1, &
                               1 : numLocalBlocks))
        
        ! Give initial values
        data_new%vals = 0
        do lbid=1,numLocalBlocks
            do i=1,blkW
                do j=1,blkH
                    gbid = distribution_lbid2gbid(data_new%dist, lbid)
                    data_new%vals(i,j,lbid) = &
                        (distribution_blockLowY(data_new%dist, gbid)+j) * &
                        100 - 100 + &
                        distribution_blocklowX(data_new%dist, gbid) + i-1
                end do
            end do
        end do
    end function

    subroutine data_print(self, out)
        include 'mpif.h'
        type(DataObj), intent(in) :: self
        integer, intent(in) :: out

        type(SubGrid) :: sg
        integer :: blkW, blkH
        integer :: i,x,y,j, blkX,blkY, lbid,gbid, proc, req,ierr
        integer :: stat(MPI_STATUS_SIZE)
        integer, allocatable :: copy(:)

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        if(myRank() == 0) then
            allocate(copy(distribution_width(self%dist)))

            write(out, '(A)')               "DataObj {"
            write(out, '(A)', advance='no') "          grid = "
            write(out, '(A)') tstr(self%grd%id)
            write(out, '(A)', advance='no') "      schedule = "
            write(out, '(A)') tstr(self%sched%id)
            write(out, '(A)', advance='no') "  distribution = "
            write(out, '(A)') tstr(self%dist%id)
            write(out, '(A)')               "       values:"

            ! Iterate through procs
            do i=0,numRanks()-1
                call print_procColor(i)
                write(out, '(AI2A)', advance='no') "Rank ", i, "   "
                call print_resetColor()
            end do
            write(out, '()')
        end if
        
        ! Iterate through subgrids
        do i=1,grid_numSubgrids(self%grd)
            sg = grid_getSubgrid(self%grd, i)
            if(myRank() == 0) then
                call print_bold()
                write(out, '(AA)', advance='no') "Subgrid: ", tstr(sg%id)
                call print_resetColor()
                write(out, '()') 
            end if

            do y=subgrid_height(sg), 1, -1
                do x=1,subgrid_width(sg), blkW
                    ! Determine what block (x, y, sg) exists in
                    call distribution_pos2blockPos( &
                         self%dist, x, y, sg, blkX, blkY, gbid)
                    proc = distribution_gbid2proc(self%dist, gbid)

                    ! If I'm the proc that owns this portion of block send it
                    ! to the master
                    if(myRank() == proc) then
                        lbid = distribution_gbid2lbid(self%dist, gbid)
                        if(myRank() /= 0) then
                            call MPI_SEND(                      &
                                self%vals(blkX, blkY, lbid),    &
                                blkW, MPI_INT, 0, 0,            &
                                MPI_COMM_WORLD, req, ierr)
                        end if
                    end if

                    ! If I'm the master, grab that portion of block and
                    ! print it
                    if(myRank() == 0) then
                        call print_procColor(proc)
                        if(proc /= 0) then
                            call MPI_RECV(              &
                                copy, blkW, MPI_INT,    &
                                proc, 0, MPI_COMM_WORLD, stat, ierr)
                        else
                            do j=0,blkH-1
                                copy(j+1) = self%vals(blkX+j, blkY, lbid)
                            end do
                        end if
                        do j=1,blkW
                            write(out, '(I4A)', advance='no') copy(j), " "
                        end do
                        call print_resetColor()
                    end if
                end do
                if(myRank() == 0) &
                    write(out, '()')
            end do
        end do

        if(myRank() == 0) then
            write(out, '(A)') "}"
            deallocate(copy)
        end if

        !type(Grid), pointer         :: grid
        !type(Schedule), pointer     :: sched
        !type(Distribution), pointer :: dist
        !integer, allocatable        :: vals(:, :, :)
    end subroutine

    subroutine data_printForProcs(self, out)
        type(DataObj), intent(in) :: self
        integer, intent(in) :: out
        integer :: i

        do i=0,numRanks()-1
            call data_printForProc(self, i, out)
            if(myRank() == 0) print *, ""
        end do
    end subroutine

    subroutine data_printForProc(self, proc, out)
        include 'mpif.h'
        type(DataObj), intent(in) :: self
        integer, intent(in) :: proc, out

        type(SubGrid) :: sg
        integer :: numLclBlocks, i,x,y,j, blkX,blkY, lbid,gbid, req,ierr
        integer :: blkW, blkH, blkHWithHalo
        integer :: stat(MPI_STATUS_SIZE)
        integer, allocatable :: copy(:)

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        ! If we're not the proc in question or the master we can exit
        if(myRank() /= 0 .and. myRank() /= proc) &
            return

        ! Send the master the number of local blocks
        if(myRank() == proc) then
            numLclBlocks = distribution_numLocalBlocks(self%dist)
            call MPI_ISEND(numLclBlocks, 1, MPI_INT, 0, 0, &
                MPI_COMM_WORLD, req, ierr)
        end if
        if(myRank() == 0) then
            call MPI_RECV(numLclBlocks, 1, MPI_INT, proc, 0, &
                MPI_COMM_WORLD, stat, ierr)
        endif
        if(myRank() == proc) then
            call MPI_WAIT(req, MPI_STATUS_IGNORE, ierr)
        end if

        ! Determine horizontal size of a block (including halo)
        blkHWithHalo = ubound(self%vals,1) - lbound(self%vals,1) + 1

        ! Have the master allocate space for data it will receive
        if(myRank() == 0) then
            allocate(copy(lbound(self%vals,1):ubound(self%vals,1)))
        end if

        ! Print header
        if(myRank() == 0) then
            call print_invertColor()
            write(out, '(AI2A)',advance='no') "Values on proc ", proc, ":"
            call print_resetColor()
            write(out, '()')
            write(out, '(A)', advance='no') "Legend:"
            call print_procColor(proc)
            write(out, '(A)', advance='no') " internal"
            call print_color(-5)
            write(out, '(A)', advance='no') " halo"
            call print_resetColor()
            write(out, '()')
        end if

        ! Iterate through subgrids
        do i=1,grid_numSubGrids(self%grd)
            sg = grid_getSubGrid(self%grd, i)
            if(myRank() == 0) then
                call print_bold()
                write(out, '(AA)', advance='no') "Subgrid: ", tstr(sg%id)
                call print_resetColor()
                write(out, '()')
            end if

            ! Iterate through local block
            do lbid=1,numLclBlocks
                if(myRank() == 0) then
                    call print_underline()
                    write(out, '(AI2)', advance='no') "Block ", lbid
                    call print_resetColor()
                    write(out, '()')
                end if

                ! Iterate through the local block's rows
                do y=ubound(self%vals,2), lbound(self%vals,2), -1
                    ! Have proc send its row to the master
                    if(myRank() == proc) then
                        call MPI_ISEND(                                     &
                            self%vals(lbound(self%vals,1),y,lbid),          &
                            blkHWithHalo, MPI_INT, 0, 0, MPI_COMM_WORLD,    &
                            req, ierr)
                    end if

                    ! Have the master print it out
                    if(myRank() == 0) then
                        call MPI_RECV(                                  &
                                copy, blkHWithHalo, MPI_INT, proc, 0,   &
                                MPI_COMM_WORLD, stat, ierr)
                        do x=lbound(self%vals,1), ubound(self%vals,1)
                            if(x < 1 .or. x > blkW .or.   &
                               y < 1 .or. y > blkH)       &
                            then
                                call print_color(-5)
                            else
                                call print_procColor(proc)
                            end if
                            write(out, '(I4A)', advance='no') copy(x), " "
                        end do
                        call print_resetColor()
                        write(out, '()')
                    end if

                    ! Wait for communication to finish
                    if(myRank() == proc) then
                        call MPI_WAIT(req, stat)
                    end if
                end do
            end do
        end do

        ! Have master clean up its temporary data
        if(myRank() == 0) then
            deallocate(copy)
        end if
    end subroutine

    integer function AFunc(x,y)
        integer, intent(in) :: x,y
        AFunc = inputData%vals(x - blkXOffset, &
                               y - blkYOffset, &
                               currentLbid)
    end function

    subroutine data_apply(self, outData, func)
        type(DataObj), target, intent(inout) :: self, outData
        interface
            integer function func(A, i, j)
                integer, intent(in) :: i, j
                interface
                    integer function A(x, y)
                        integer, intent(in) :: x, y
                    end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        inputData => self

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    outData%vals(blkI, blkJ, lbid) =    &
                        func(AFunc,                     &
                             blkI + blkXOffset,         &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_forceUpdate(self)
        include 'mpif.h'
        type(DataObj), intent(inout) :: self
        integer, allocatable :: sendBufs(:,:), recvBufs(:,:)
        type(Schedule), pointer :: sched
        integer :: msg, tr, i, x, y, blkX, blkY, req, ierr
        integer :: largestSendMsg, largestRecvMsg, regionSz
        integer :: lbid, msgOff, sz
        integer :: stat(MPI_STATUS_SIZE)
        integer :: rgnLowX, rgnLowY, rgnHighX, rgnHighY
        integer, allocatable :: msgSize(:)

        sched => self%sched

        ! Find the largest size an outgoing message will be
        do msg=1,sched%nMsgsSend
            regionSz = 0
            do i=sched%recvMsgStart(msg),   &
                 sched%recvMsgStart(msg) + sched%numTransfersInRecvMsg(msg) - 1
            !`--
                regionSz = schedule_sendRegionSize(sched, i) + regionSz
            end do
            if(regionSz > largestSendMsg) &
                largestSendMsg = regionSz
        end do

        ! Find the largest size an incoming message will be
        largestRecvMsg = 0
        do msg=1,sched%nMsgsRecv
            regionSz = 0
            do i=sched%sendMsgStart(msg),   &
                 sched%sendMsgStart(msg) + sched%numTransfersInSendMsg(msg) - 1
            !`--
                regionSz = schedule_recvRegionSize(sched, i) + regionSz
            end do
            if(regionSz > largestRecvMsg) &
                largestRecvMsg = regionSz
        end do

        ! Allocate buffers:
        allocate(sendBufs(largestSendMsg, sched%nMsgsSend))
        allocate(recvBufs(largestRecvMsg, sched%nMsgsRecv))

        ! *************************
        ! Post sending messages:
        ! *************************
        allocate(msgSize(sched%nMsgsSend))

        ! Iterate through messages to send
        do msg=1,sched%nMsgsSend
            msgOff = 1
            ! Iterate through transfers for this message
            ! and add them to the sending buffer
            do tr=sched%sendMsgStart(msg),   &
                  sched%sendMsgStart(msg) + sched%numTransfersInSendMsg(msg) - 1
            !`--
                lbid = sched%transferSendFromLBID(tr)
                rgnLowX  = sched%transferRegionSendLowX(tr)
                rgnLowY  = sched%transferRegionSendLowY(tr)
                rgnHighX = sched%transferRegionSendHighX(tr)
                rgnHighY = sched%transferRegionSendHighY(tr)

                !if(myRank() == 0) then
                !    print *, "Send to ", sched%msgSendTo(msg), &
                !        rgnLowX, rgnLowY, rgnHighX, rgnHighY
                !end if

                ! Serialize the region
                do y=rgnLowY,rgnHighY
                    do x=rgnLowX, rgnHighX
                        sendBufs(msgOff, msg) = self%vals(x, y, lbid)
                        msgOff = msgOff + 1
                    end do
                end do
            end do

            ! Post the send
            msgOff = msgOff - 1
            msgSize(msg) = msgOff

            call MPI_ISEND(msgSize(msg),                                &
                           1, MPI_INT, sched%msgSendTo(msg), 0,         &
                           MPI_COMM_WORLD, req, ierr)
            call MPI_ISEND(sendBufs(1, msg),                            &
                           msgOff, MPI_INT, sched%msgSendTo(msg), 0,    &
                           MPI_COMM_WORLD, req, ierr)
        end do

        ! ***************************
        ! Receive incoming messages:
        ! ***************************

        ! Iterate through messages to receive
        do msg=1,sched%nMsgsRecv
            ! receive message into buffer
            call MPI_RECV(sz, 1, MPI_INT, sched%msgRecvFrom(msg), 0,    &
                          MPI_COMM_WORLD, stat, ierr)
            call MPI_RECV(recvBufs(1, msg),                             &
                          sz, MPI_INT, sched%msgRecvFrom(msg), 0,       &
                          MPI_COMM_WORLD, stat, ierr)

            ! Iterate through transfers for this message
            ! and unpack it from the buffer
            msgOff = 1
            do tr=sched%recvMsgStart(msg),   &
                  sched%recvMsgStart(msg) + sched%numTransfersInRecvMsg(msg) - 1
            !`--
                currentLbid = sched%transferRecvAtLBID(tr)
                rgnLowX  = sched%transferRegionRecvLowX(tr)
                rgnLowY  = sched%transferRegionRecvLowY(tr)
                rgnHighX = sched%transferRegionRecvHighX(tr)
                rgnHighY = sched%transferRegionRecvHighY(tr)

                ! Deserialize the region
                do y=rgnLowY, rgnHighY
                    do x=rgnLowX, rgnHighX
                        self%vals(x, y, currentLbid) = recvBufs(msgOff, msg)
                        msgOff = msgOff + 1
                    end do
                end do
            end do
        end do

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)

        deallocate(msgSize)
    end subroutine
end module mod_data
