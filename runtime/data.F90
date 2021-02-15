module mod_data
    include 'grid.h'
    include 'schedule.h'
    use mod_utils
    implicit none

    type Data
        type(Grid), pointer         :: grd
        type(Schedule), pointer     :: sched
        type(Distribution), pointer :: dist
        integer, allocatable        :: vals(:, :, :)
    end type

    public data_new,                &
           data_newMedataCopied,    &
           data_print,              &
           data_printSimp,          &
           data_printForProc,       &

           data_input,              &

           data_apply,              &

           data_add,                &
           data_sub,                &
           data_mult,               &
           data_pow,                &

           data_sum,                &

           data_forceUpdate

  contains
    subroutine data_new(self, grd, sched, dist)
        type(Data), intent(inout)               :: self
        type(Grid), pointer, intent(in)         :: grd
        type(Schedule), pointer, intent(in)     :: sched
        type(Distribution), pointer, intent(in) :: dist
        integer :: i, j, lbid, numLocalBlocks
        
        self%grd   => grd
        self%sched => sched
        self%dist  => dist

        numLocalBlocks = 0
        do i=1,size(dist%gbid2proc)
            if(dist%gbid2proc(i) == myRank()) then
                numLocalBlocks = numLocalBlocks + 1
            end if
        end do

        allocate(self%vals(0 : dist%blockSize(1)+1, &
                           0 : dist%blockSize(2)+1, &
                           1 : numLocalBlocks))

        ! Give initial values
        self%vals = 0
        do lbid=1,numLocalBlocks
            do i=1,dist%blockSize(1)
                do j=1,dist%blockSize(2)
                    self%vals(i,j,lbid) = &
                        (distribution_block_y1(dist, dist%lbid2gbid(lbid))+j)* &
                        100 - 100 + &
                        distribution_block_x1(dist, dist%lbid2gbid(lbid)) + i-1
                end do
            end do
        end do
    end subroutine


    subroutine data_newMedataCopied(self, copy)
        type(Data), intent(inout) :: self
        type(Data), intent(in) :: copy

        self%grd   => copy%grd
        self%sched => copy%sched
        self%dist  => copy%dist
        allocate(self%vals(lbound(copy%vals, 1):ubound(copy%vals,1), &
                           lbound(copy%vals, 2):ubound(copy%vals,2), &
                           lbound(copy%vals, 2):ubound(copy%vals,3)))
    end subroutine


    subroutine data_print(self, out)
        include 'mpif.h'
        type(Data), intent(in) :: self
        integer, intent(in) :: out

        type(SubGrid), pointer :: sg
        integer :: i,x,y,j, blkX,blkY, lbid,gbid, proc, req,ierr
        integer :: stat(MPI_STATUS_SIZE)
        integer, allocatable :: copy(:)

        if(myRank() == 0) then
            allocate(copy(self%dist%blockSize(1)))

            write(out, '(A)')               "Data {"
            write(out, '(A)', advance='no') "          grid = "
            call grid_printSimp(self%grd, out)
            write(out, '()')
            write(out, '(A)', advance='no') "      schedule = "
            call schedule_printSimp(self%sched, out)
            write(out, '()')
            write(out, '(A)', advance='no') "  distribution = "
            call distribution_printSimp(self%dist, out)
            write(out, '()')
            write(out, '(A)')               "       values:"

            ! Iterate through procs
            do i=0,numRanks()-1
                call print_procColor(i)
                write(out, '(AI2A)', advance='no'), "Rank ", i, "   "
                call print_resetColor()
            end do
            write(out, '()')
        end if
        
        ! Iterate through subgrids
        do i=1,grid_numSubGrids(self%grd)
            sg => grid_getSubGrid(self%grd, i)
            if(myRank() == 0) then
                call print_bold()
                write(out, '(AA)', advance='no') "Subgrid: ", tstr(sg%name)
                call print_resetColor()
                write(out, '()') 
            end if

            do y=sg%extents(2),1,-1
                do x=1,sg%extents(1), self%dist%blockSize(1)
                    ! Determine what block (x, y, sg) exists in
                    call distribution_pos2blockPos( &
                         self%dist, x, y, sg, blkX, blkY, gbid)
                    proc = self%dist%gbid2proc(gbid)

                    ! If I'm the proc that owns this portion of block send it
                    ! to the master
                    if(myRank() == proc) then
                        lbid = self%dist%gbid2lbid(gbid)
                        if(myRank() /= 0) then
                        call MPI_SEND(                              &
                            self%vals(blkX, blkY, lbid),            &
                            self%dist%blockSize(1), MPI_INT, 0, 0,  &
                            MPI_COMM_WORLD, req, ierr)
                        end if
                    end if

                    ! If I'm the master, grab that portion of block and
                    ! print it
                    if(myRank() == 0) then
                        call print_procColor(proc)
                        if(proc /= 0) then
                            call MPI_RECV(                              &
                                copy, self%dist%blockSize(1), MPI_INT,  &
                                proc, 0, MPI_COMM_WORLD, stat, ierr)
                        else
                            do j=0,self%dist%blockSize(1)-1
                                copy(j+1) = self%vals(blkX+j, blkY, lbid)
                            end do
                        end if
                        do j=1,self%dist%blockSize(1)
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


    subroutine data_printSimp(self, out)
        type(Data), intent(in) :: self
        integer, intent(in) :: out

        if(myRank() == 0) then
            write(out, '(AAA)', advance='no') &
                "<Data: ", self%grd%name, ">"
        end if
    end subroutine


    subroutine data_printForProc(self, proc, out)
        include 'mpif.h'
        type(Data), intent(in) :: self
        integer, intent(in) :: proc, out

        type(SubGrid), pointer :: sg
        integer :: numLclBlocks, i,x,y,j, blkX,blkY, lbid,gbid, req,ierr, blkH
        integer :: stat(MPI_STATUS_SIZE)
        integer, allocatable :: copy(:)

        ! If we're not the proc in question or the master we can exit
        if(myRank() /= 0 .and. myRank() /= proc) &
            return

        ! Send the master the number of local blocks
        if(myRank() == proc) then
            numLclBlocks = size(self%dist%lbid2gbid)
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
        blkH = ubound(self%vals,1) - lbound(self%vals,1) + 1

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
            call print_color(10)
            write(out, '(A)', advance='no') " halo"
            call print_resetColor()
            write(out, '()')
        end if

        ! Iterate through subgrids
        do i=1,grid_numSubGrids(self%grd)
            sg => grid_getSubGrid(self%grd, i)
            if(myRank() == 0) then
                call print_bold()
                write(out, '(AA)', advance='no') "Subgrid: ", tstr(sg%name)
                call print_resetColor()
                write(out, '()')
            end if

            ! Iterate through local blocks
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
                            blkH, MPI_INT, 0, 0, MPI_COMM_WORLD, req, ierr)
                    end if

                    ! Have the master print it out
                    if(myRank() == 0) then
                        call MPI_RECV(                        &
                                copy, blkH, MPI_INT, proc, 0, &
                                MPI_COMM_WORLD, stat, ierr)
                        do x=lbound(self%vals,1), ubound(self%vals,1)
                            if(x < 1 .or. x > self%dist%blockSize(1) .or.   &
                               y < 1 .or. y > self%dist%blockSize(2))       &
                            then
                                call print_color(10)
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

    subroutine data_input(self, in)
        type(Data), intent(inout) :: self
        type(File), intent(inout) :: in

        print *, "data_input", " is not implemented"
        call errExit()
    end subroutine



    type(Data) function data_apply(self, func)
        type(Data), intent(inout) :: self
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

        type(SubGrid), pointer :: sg
        type(Neighbor), pointer :: n
        integer, allocatable :: neighs(:)
        integer :: lbid, blkI, blkJ, neigh
        integer :: blkXOffset, blkYOffset

        call data_newMedataCopied(data_apply, self)
        !sg => grid_getSubGrid(self%grd, 1)
        !allocate(neighs(size(sg%neighbors)))

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            blkXOffset = distribution_block_x1( &
                self%dist, self%dist%lbid2gbid(lbid)) -1
            blkYOffset = distribution_block_y1( &
                self%dist, self%dist%lbid2gbid(lbid)) -1
            
            do blkJ=1,self%dist%blockSize(2)
                do blkI=1,self%dist%blockSize(1)
                    data_apply%vals(blkI, blkJ, lbid) = &
                        func(AFunc,                     &
                             blkI + blkXOffset,         &
                             blkJ + blkYOffset)
                    !data_apply%vals(blkI, blkJ, lbid) = 123
                end do
            end do
        end do

      contains

        integer function AFunc(x,y)
            integer, intent(in) :: x,y
            AFunc = self%vals(x - blkXOffset, &
                              y - blkYOffset, &
                              lbid)
            
            !AFunc = y
            !%self%vals(1,1,lbid)
           !AFunc = blkYOffset
        end function
    end function


    type(Data) function data_add(lhs, rhs)
        type(Data), intent(in) :: lhs
        type(Data), intent(in) :: rhs

        call data_newMedataCopied(data_add, lhs)
        data_add%vals = lhs%vals + rhs%vals
    end function


    type(Data) function data_sub(lhs, rhs)
        type(Data), intent(in) :: lhs
        type(Data), intent(in) :: rhs

        call data_newMedataCopied(data_sub, lhs)
        data_sub%vals = lhs%vals - rhs%vals
    end function


    type(Data) function data_mult(lhs, rhs)
        type(Data), intent(in) :: lhs
        type(Data), intent(in) :: rhs

        call data_newMedataCopied(data_mult, lhs)
        data_mult%vals = lhs%vals * rhs%vals
    end function


    type(Data) function data_pow(lhs, power)
        type(Data), intent(in) :: lhs
        integer, intent(in) :: power

        print *, "data_pow", " is not implemented"
        call errExit()
    end function



    integer function data_sum(self)
        type(Data), intent(in) :: self

        print *, "data_sum", " is not implemented"
        call errExit()
    end function



    subroutine data_forceUpdate(self)
        include 'mpif.h'
        type(Data), intent(inout) :: self
        integer, allocatable :: sendBufs(:,:), recvBufs(:,:)
        type(Schedule), pointer :: sched
        integer :: msg, tr, i, x, y, blkX, blkY, req, ierr
        integer :: largestSendMsg, largestRecvMsg, regionSz
        integer :: lbid, msgOff, sz
        integer :: stat(MPI_STATUS_SIZE)
        type(Region) :: rgn

        sched => self%sched

        ! Find the largest size an outgoing message will be
        largestSendMsg = 0
        do msg=1,sched%msgSendTo%nIntegers
            regionSz = 0
            do i=sched%transferRegionSend%offsets(msg), &
                 sched%transferRegionSend%offsets(msg+1)-1
            !`--
                regionSz = &
                    region_size(sched%transferRegionSend%values(i)) + &
                    regionSz
            end do
            if(regionSz > largestSendMsg) &
                largestSendMsg = regionSz
        end do

        ! Find the largest size an incoming message will be
        largestRecvMsg = 0
        do msg=1,sched%msgRecvFrom%nIntegers
            regionSz = 0
            do i=sched%transferRegionRecv%offsets(msg), &
                 sched%transferRegionRecv%offsets(msg+1)-1
            !--
                regionSz = &
                    region_size(sched%transferRegionRecv%values(i)) + &
                    regionSz
            end do
            if(regionSz > largestRecvMsg) &
                largestRecvMsg = regionSz
        end do

        ! Allocate buffers:
        allocate(sendBufs(largestSendMsg, sched%msgSendTo%nIntegers))
        allocate(recvBufs(largestRecvMsg, sched%msgRecvFrom%nIntegers))

        ! *************************
        ! Post sending messages:
        ! *************************

        ! Iterate through messages to send
        do msg=1,sched%msgSendTo%nIntegers
            msgOff = 1
            ! Iterate through transfers for this message
            ! and add them to the sending buffer
            do tr=sched%transferRegionSend%offsets(msg), &
                  sched%transferRegionSend%offsets(msg+1)-1
            !--
                lbid = sched%transferSendFromLBID%values(tr)
                rgn = sched%transferRegionSend%values(tr)

                ! Serialize the region
                do y=rgn%Y1, rgn%Y2
                    do x=rgn%X1, rgn%X2
                        sendBufs(msgOff, msg) = self%vals(x, y, lbid)
                        msgOff = msgOff + 1
                    end do
                end do
            end do

            ! Post the send
            msgOff = msgOff - 1

            call MPI_ISEND(msgOff,                                        &
                           1, MPI_INT, sched%msgSendTo%integers(msg), 0,  &
                           MPI_COMM_WORLD, req, ierr)
            call MPI_ISEND(sendBufs(1, msg),                                   &
                           msgOff, MPI_INT, sched%msgSendTo%integers(msg), 0,  &
                           MPI_COMM_WORLD, req, ierr)
        end do

        ! ***************************
        ! Receive incoming messages:
        ! ***************************

        ! Iterate through messages to receive
        do msg=1,sched%msgRecvFrom%nIntegers
            ! receive message into buffer
            call MPI_RECV(sz, 1, MPI_INT, sched%msgRecvFrom%integers(msg), 0,  &
                          MPI_COMM_WORLD, stat, ierr)
            call MPI_RECV(recvBufs(1, msg),                                    &
                          sz, MPI_INT, sched%msgRecvFrom%integers(msg), 0,     &
                          MPI_COMM_WORLD, stat, ierr)

            ! Iterate through transfers for this message
            ! and unpack it from the buffer
            msgOff = 1
            do tr=sched%transferRegionRecv%offsets(msg), &
                  sched%transferRegionRecv%offsets(msg+1)-1
            !--
                lbid = sched%transferRecvAtLBID%values(tr)
                rgn  = sched%transferRegionRecv%values(tr)

                ! Deserialize the region
                
                do y=rgn%Y1, rgn%Y2
                    do x=rgn%X1, rgn%X2
                        self%vals(x, y, lbid) = recvBufs(msgOff, msg)
                        msgOff = msgOff + 1
                    end do
                end do
            end do
        end do

    call MPI_BARRIER(MPI_COMM_WORLD, ierr)

    end subroutine
end module mod_data
