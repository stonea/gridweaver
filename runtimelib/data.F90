module mod_data
    use mod_gridweaver
    use mod_schedule
    implicit none

    type DataObj
        type(Grid)              :: grd
        type(Distribution)      :: dist
        type(Schedule), pointer :: sched
        real(8), allocatable    :: vals(:, :, :)  ! dimensions: x, y, lbid
        real(8), allocatable    :: ghosts(:)
        integer                 :: depth
    end type

    type AccessMethod_relative
        type(DataObj), pointer  :: underlyingData
    endtype

    type AccessMethod_tag
        type(DataObj), pointer  :: underlyingData
    endtype

    type AccessMethod_index
        type(DataObj), pointer  :: underlyingData
    endtype



    type(DataObj), pointer :: &
        gInputData1, gInputData2, gInputData3, gInputData4, gInputData5,  &
        gInputData6, gInputData7, gInputData8, gInputData9, gInputData10
    integer :: blkXOffset, blkYOffset
    integer :: gCurrentLbid, gCurrentGbid

    real(8), allocatable :: sendBufs(:,:), recvBufs(:,:)
    real(8), allocatable :: sendGhostsBufs(:,:), recvGhostsBufs(:,:)

    public rel, &
           idx, &
           tag, &

           data_new,                &
           !data_newMedataCopied,    &
           data_print,              &
           !data_printSimp,          &
           data_printForProcs,      &
           data_printForProc,       &

           !data_input,              &

           data_apply1,              &
           data_apply2,              &
           data_apply3,              &
           data_apply4,              &
           data_apply5,              &
           data_apply6,              &
           data_apply7,              &
           data_apply8,              &
           data_apply9,              &
           data_apply10,             &

           !data_add,                &
           !data_sub,                &
           !data_mult,               &
           !data_pow,                &

           data_sum,                &
           data_copy,               &
           data_add,                &

           data_initializeSeqVals,  &

           data_forceUpdate,        &
           data_forceGhostsUpdate

    interface data_apply_noncompact
        module procedure data_apply_noncompact_r,         &
                         data_apply_noncompact_i,         &
                         data_apply_noncompact_t,         &
                         data_apply_noncompact_trrrrrrrrr
    end interface

  contains
    function rel(data_rel)
        type(AccessMethod_relative) :: rel
        type(DataObj), target, intent(inout) :: data_rel

        rel%underlyingData => data_rel
    end function

    function idx(data_idx)
        type(AccessMethod_index) :: idx
        type(DataObj), target, intent(inout) :: data_idx

        idx%underlyingData => data_idx
    end function

    function tag(data_tag)
        type(AccessMethod_tag)   :: tag
        type(DataObj), target, intent(inout) :: data_tag

        tag%underlyingData => data_tag
    end function


    function data_new(sched)
        type(DataObj) :: data_new
        type(Schedule), target, intent(in) :: sched

        integer :: i, j, lbid, gbid, numLocalBlocks, blkW, blkH
        
        ! Populate values in the new data object
        data_new%grd   =  sched%grid
        data_new%sched => sched
        data_new%dist  =  sched%dist
        data_new%depth =  sched%depth

        numLocalBlocks = distribution_numLocalBlocks(sched%dist)

        blkW = distribution_width(sched%dist)
        blkH = distribution_height(sched%dist)

        allocate(data_new%vals(1-data_new%depth : blkW+data_new%depth, &
                               1-data_new%depth : blkH+data_new%depth, &
                               1 : numLocalBlocks))

        if(allocated(sched%ghostMsgRecvFrom)) then
            allocate(data_new%ghosts(   &
                lbound(sched%recvGhostMsgSG,1):ubound(sched%recvGhostMsgSG,1)))
            data_new%ghosts = 0.0
        end if
        
        ! Give initial values
        data_new%vals = 0
    end function


    subroutine data_print(self, out)
        include 'mpif.h'
        type(DataObj), intent(in) :: self
        integer, intent(in) :: out

        type(SubGrid) :: sg
        integer :: blkW, blkH
        integer :: i,x,y,j, blkX,blkY, lbid,gbid, proc, req,ierr
        integer :: stat(MPI_STATUS_SIZE)
        real(8), allocatable :: copy(:)

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        ! Print header information
        if(myRank() == 0) then
            allocate(copy(blkW))

            write(out, '(A)')               "DataObj {"
            write(out, '(A)', advance='no') "          grid = "
            write(out, '(A)') tstr(self%grd%id)
            write(out, '(A)', advance='no') "      schedule = "
            write(out, '(A)') tstr(self%sched%id)
            write(out, '(A)', advance='no') "  distribution = "
            write(out, '(A)') tstr(self%dist%id)
            write(out, '(A)')               "       values:"

            ! Iterate through procs printing key (associating colors with
            ! ranks)
            do i=0,numRanks()-1
                call print_procColor(i)
                write(out, '(A,I2,A)', advance='no') "Rank ", i, "   "
                call print_resetColor()
            end do
            write(out, '()')
        end if
        
        ! Iterate through subgrids
        do i=1,grid_numSubgrids(self%grd)
            sg = grid_getSubgrid(self%grd, i)

            ! Print header information for subgrid
            if(myRank() == 0) then
                call print_bold()
                write(out, '(A,A)', advance='no') "Subgrid: ", tstr(sg%id)
                call print_resetColor()
                write(out, '()') 
            end if

            ! Iterate through the subgrid values going block by block in each
            ! row
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
                                blkW, MPI_REAL8, 0, 0,          &
                                MPI_COMM_WORLD, req, ierr)
                        end if
                    end if

                    ! If I'm the master, grab that portion of block and
                    ! print it
                    if(myRank() == 0) then
                        call print_procColor(proc)
                        if(proc /= 0) then
                            call MPI_RECV(               &
                                copy, blkW, MPI_REAL8,   &
                                proc, 0, MPI_COMM_WORLD, stat, ierr)
                        else
                            do j=0,MIN(blkW-1,subgrid_width(sg)-1)
                                copy(j+1) = self%vals(blkX+j, blkY, lbid)
                            end do
                        end if
                        do j=1,MIN(blkW,subgrid_width(sg))
                            write(out, '(F6.2,A)', advance='no') copy(j), " "
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
        integer :: numGhosts
        integer :: blkW, blkH, blkHWithHalo, sgW, sgH
        integer :: stat(MPI_STATUS_SIZE)
        real(8), allocatable :: copy(:)
        integer :: reqNumGhosts
        integer :: reqGhostCopy, reqGhostSGCopy, reqGhostXCopy, reqGhostYCopy
        real(8), allocatable :: ghostCopy(:)
        integer, allocatable :: ghostSGCopy(:), ghostXCopy(:), ghostYCopy(:)
        logical :: printedHeader

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
            write(out, '(A,I2,A)',advance='no') "Values on proc ", proc, ":"
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
            printedHeader = .false.

            ! Iterate through blocks in subgrid
            do gbid=distribution_firstGbidInSG(self%dist, sg),  &
                    distribution_lastGbidInSG(self%dist, sg)
            !`
                lbid = distribution_gbid2lbid(self%dist, gbid)

                ! If block is not owned by this proc continue
                if(distribution_gbid2proc(self%dist, gbid) /= proc) then
                    cycle
                end if

                ! If block is not on subgrid continue
                if(distribution_gbid2sg(self%dist, gbid) /= i) then
                    cycle
                end if
                sgW = subgrid_width(sg)
                sgH = subgrid_height(sg)

                ! Print subgrid header if first block
                if(.not. printedHeader) then
                    printedHeader = .true.
                    if(myRank() == 0) then
                        call print_bold()
                        write(out, '(A,A)', advance='no') "Subgrid: ", &
                            tstr(sg%id)
                        call print_resetColor()
                        write(out, '()')
                    end if
                end if
                
                if(myRank() == 0) then
                    call print_underline()
                    write(out, '(A,I2)', advance='no') "Block ", gbid
                    call print_resetColor()
                    write(out, '()')
                end if

                ! Iterate through the local block's rows
                do y=ubound(self%vals,2), lbound(self%vals,2), -1
                    ! Have proc send its row to the master
                    if(myRank() == proc) then
                        call MPI_ISEND(                                     &
                            self%vals(lbound(self%vals,1),y,lbid),          &
                            blkHWithHalo, MPI_REAL8, 0, 0, MPI_COMM_WORLD,  &
                            req, ierr)
                    end if

                    ! Have the master print it out
                    if(myRank() == 0) then
                        call MPI_RECV(                                  &
                                copy, blkHWithHalo, MPI_REAL8, proc, 0, &
                                MPI_COMM_WORLD, stat, ierr)
                        if(y > sgH+1) then
                            cycle
                        end if
                        do x=lbound(self%vals,1), MIN(ubound(self%vals,1),sgW+1)
                            if(x < 1 .or. x > MIN(blkW,sgW) .or.   &
                               y < 1 .or. y > MIN(blkH,sgH))       &
                            then
                                call print_color(-5)
                            else
                                call print_procColor(proc)
                            end if
                            write(out, '(F6.2,A)', advance='no') copy(x), " "
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

        !-----------------------------------------------------------------------

        ! Send master ghost information
        if(myRank() == proc) then
            if(allocated(self%ghosts)) then
                numGhosts = ubound(self%ghosts,1) - lbound(self%ghosts,1) + 1
            else
                numGhosts = 0
            end if
            call MPI_ISEND(numGhosts, 1, MPI_INT, 0, 0, &
                MPI_COMM_WORLD, reqNumGhosts, ierr)
            call MPI_ISEND(self%ghosts, numGhosts, MPI_REAL8, 0, 1, &
                MPI_COMM_WORLD, reqGhostCopy, ierr)

            call MPI_ISEND(self%sched%recvGhostMsgSG, numGhosts, MPI_INT, 0, 2, &
                MPI_COMM_WORLD, reqGhostSGCopy, ierr)
            call MPI_ISEND(self%sched%recvGhostMsgX, numGhosts, MPI_INT, 0, 3, &
                MPI_COMM_WORLD, reqGhostXCopy, ierr)
            call MPI_ISEND(self%sched%recvGhostMsgY, numGhosts, MPI_INT, 0, 4, &
                MPI_COMM_WORLD, reqGhostYCopy, ierr)
        end if
        if(myRank() == 0) then
            call MPI_RECV(numGhosts, 1, MPI_INT, proc, 0, &
                MPI_COMM_WORLD, stat, ierr)
            allocate(ghostCopy(numGhosts))
            allocate(ghostSGCopy(numGhosts))
            allocate(ghostXCopy(numGhosts))
            allocate(ghostYCopy(numGhosts))

            call MPI_RECV(ghostCopy, numGhosts, MPI_REAL8, proc, 1, &
                MPI_COMM_WORLD, stat, ierr)
            call MPI_RECV(ghostSGCopy, &
                numGhosts, MPI_INT, proc, 2, MPI_COMM_WORLD, stat, ierr)
            call MPI_RECV(ghostXCopy, &
                numGhosts, MPI_INT, proc, 3, MPI_COMM_WORLD, stat, ierr)
            call MPI_RECV(ghostYCopy, &
                numGhosts, MPI_INT, proc, 4, MPI_COMM_WORLD, stat, ierr)
        endif
        if(myRank() == proc) then
            call MPI_WAIT(reqNumGhosts, MPI_STATUS_IGNORE, ierr)
            call MPI_WAIT(reqGhostCopy, MPI_STATUS_IGNORE, ierr)
            call MPI_WAIT(reqGhostSGCopy, MPI_STATUS_IGNORE, ierr)
            call MPI_WAIT(reqGhostXCopy, MPI_STATUS_IGNORE, ierr)
            call MPI_WAIT(reqGhostYCopy, MPI_STATUS_IGNORE, ierr)
        end if

        ! Have the master print out ghost information
        if(myRank() == 0) then
            write(out, '()')
            call print_color(-5)
            write(out, '(A)', advance='no') "GHOSTS: "
            call print_resetColor()
            do i=1,numGhosts
                if(i /= 1) then
                    write(out, '(A)', advance='no') ",    "
                    if(mod(i-1,3)==0) then
                        write(out, '()')
                        write(out, '(A)', advance='no') "        "
                    end if
                end if

                write(out, '(A,I2,A,I2,A,I2,A)', advance='no') &
                    "(", ghostSGCopy(i), ", <", ghostXCopy(i), ",", &
                    ghostYCopy(i), ">) = "
                call print_color(-5)
                write(out, '(F6.2)', advance='no') ghostCopy(i)
                call print_resetColor()
            end do
            write(out, '()')

            deallocate(ghostCopy)
            deallocate(ghostSGCopy)
            deallocate(ghostXCopy)
            deallocate(ghostYCopy)
        end if


        ! Have master clean up its temporary data
        if(myRank() == 0) then
            deallocate(copy)
        end if
    end subroutine



    real(8) function AFunc1(x,y)
        integer, intent(in) :: x,y
        AFunc1 = gInputData1%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function

    real(8) function AFunc2(x,y)
        integer, intent(in) :: x,y
        AFunc2 = gInputData2%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function

    real(8) function AFunc3(x,y)
        integer, intent(in) :: x,y
        AFunc3 = gInputData3%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function

    real(8) function AFunc4(x,y)
        integer, intent(in) :: x,y
        AFunc4 = gInputData4%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function

    real(8) function AFunc5(x,y)
        integer, intent(in) :: x,y
        AFunc5 = gInputData5%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function

    real(8) function AFunc6(x,y)
        integer, intent(in) :: x,y
        AFunc6 = gInputData6%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function

    real(8) function AFunc7(x,y)
        integer, intent(in) :: x,y
        AFunc7 = gInputData7%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function

    real(8) function AFunc8(x,y)
        integer, intent(in) :: x,y
        AFunc8 = gInputData8%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function

    real(8) function AFunc9(x,y)
        integer, intent(in) :: x,y
        AFunc9 = gInputData9%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function

    real(8) function AFunc10(x,y)
        integer, intent(in) :: x,y
        AFunc10 = gInputData10%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function


    ! --------------------------------------------------------------------------
    ! - Functions for relative data access -------------------------------------
    ! --------------------------------------------------------------------------

    real(8) function ARelFunc1(x,y)
        integer, intent(in) :: x,y

        ARelFunc1 = gInputData1%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
    end function


    ! --------------------------------------------------------------------------
    ! - Functions for indexed data access --------------------------------------
    ! --------------------------------------------------------------------------

    real(8) function AIdxFunc1(sg,x,y)
        integer, intent(in) :: sg,x,y
        integer :: i
        type(Schedule), pointer :: sched

        ! If (sg,<x,y>) is on the current gbid just access it directly
        if(distribution_gbidAtSGID(gInputData1%dist, sg,x,y) == gCurrentGbid) then
            AIdxFunc1 = gInputData1%vals(x - blkXOffset, y - blkYOffset, gCurrentLbid)
            return
        end if

        ! Otherwise search for its ghost
        sched => gInputData1%sched
        do i=lbound(sched%recvGhostMsgSG,1),ubound(sched%recvGhostMsgSG,1)
            if(sched%recvGhostMsgSG(i) == sg) then
                if(sched%recvGhostMsgX(i) == x .and. &
                   sched%recvGhostMsgY(i) == y) &
                then
                    AIdxFunc1 = gInputData1%ghosts(i)
                    return
                endif
            end if
        end do

        AIdxFunc1 = 0.0
    end function

    ! --------------------------------------------------------------------------
    ! - Functions for tagged data access ---------------------------------------
    ! --------------------------------------------------------------------------

    real(8) function ATagFunc1(tag)
        integer, intent(in) :: tag

        if(tag < 0) then
            ATagFunc1 = gInputData1%ghosts(-tag)
        else
            ATagFunc1 = tag
        end if
    end function




    






    subroutine data_apply1(self, in1, func)
        type(DataObj), target, intent(inout) :: self, in1
        interface
            real(8) function func(A, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A(x, y)
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

        gInputData1 => in1

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =    &
                        func(AFunc1,                 &
                             blkI + blkXOffset,      &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply2( &
        self, in1, in2, func)
        type(DataObj), target, intent(inout) :: &
            self, in1, in2
        interface
            real(8) function func(A1, A2, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A1(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A2(x, y);  integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1
        gInputData2 => in1

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =                            &
                        func(AFunc1, AFunc2,                                 &
                             blkI + blkXOffset,                              &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply3( &
        self, in1, in2, in3, func)
        type(DataObj), target, intent(inout) :: &
            self, in1, in2, in3
        interface
            real(8) function func(A1, A2, A3, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A1(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A2(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A3(x, y);  integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1
        gInputData2  => in2
        gInputData3  => in3

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =                            &
                        func(AFunc1, AFunc2, AFunc3,                         &
                             blkI + blkXOffset,                              &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply4( &
        self, in1, in2, in3, in4, func)
        type(DataObj), target, intent(inout) :: &
            self, in1, in2, in3, in4
        interface
            real(8) function func(A1, A2, A3, A4, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A1(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A2(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A3(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A4(x, y);  integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1
        gInputData2  => in2
        gInputData3  => in3
        gInputData4  => in4

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =                            &
                        func(AFunc1, AFunc2, AFunc3, AFunc4,                 &
                             blkI + blkXOffset,                              &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply5( &
        self, in1, in2, in3, in4, in5, func)
        type(DataObj), target, intent(inout) :: &
            self, in1, in2, in3, in4, in5
        interface
            real(8) function func(A1, A2, A3, A4, A5, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A1(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A2(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A3(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A4(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A5(x, y);  integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1
        gInputData2  => in2
        gInputData3  => in3
        gInputData4  => in4
        gInputData5  => in5

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =                            &
                        func(AFunc1, AFunc2, AFunc3, AFunc4, AFunc5,         &
                             blkI + blkXOffset,                              &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply6( &
        self, in1, in2, in3, in4, in5, in6, func)
        type(DataObj), target, intent(inout) :: &
            self, in1, in2, in3, in4, in5, in6
        interface
            real(8) function func(A1, A2, A3, A4, A5, A6, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A1(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A2(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A3(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A4(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A5(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A6(x, y);  integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1
        gInputData2  => in2
        gInputData3  => in3
        gInputData4  => in4
        gInputData5  => in5
        gInputData6  => in6

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =                            &
                        func(AFunc1, AFunc2, AFunc3, AFunc4, AFunc5, AFunc6, &
                             blkI + blkXOffset,                              &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply7( &
        self, in1, in2, in3, in4, in5, in6, in7, func)
        type(DataObj), target, intent(inout) :: &
            self, in1, in2, in3, in4, in5, in6, in7
        interface
            real(8) function func(A1, A2, A3, A4, A5, A6, A7, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A1(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A2(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A3(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A4(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A5(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A6(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A7(x, y);  integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1
        gInputData2  => in2
        gInputData3  => in3
        gInputData4  => in4
        gInputData5  => in5
        gInputData6  => in6
        gInputData7  => in7

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =                            &
                        func(AFunc1, AFunc2, AFunc3, AFunc4, AFunc5, AFunc6, &
                             AFunc7,                                         &
                             blkI + blkXOffset,                              &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply8( &
        self, in1, in2, in3, in4, in5, in6, in7, in8, func)
        type(DataObj), target, intent(inout) :: &
            self, in1, in2, in3, in4, in5, in6, in7, in8
        interface
            real(8) function func(A1, A2, A3, A4, A5, A6, A7, A8, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A1(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A2(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A3(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A4(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A5(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A6(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A7(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A8(x, y);  integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1
        gInputData2  => in2
        gInputData3  => in3
        gInputData4  => in4
        gInputData5  => in5
        gInputData6  => in6
        gInputData7  => in7
        gInputData8  => in8

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =                            &
                        func(AFunc1, AFunc2, AFunc3, AFunc4, AFunc5, AFunc6, &
                             AFunc7, AFunc8,                                 &
                             blkI + blkXOffset,                              &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply9( &
        self, in1, in2, in3, in4, in5, in6, in7, in8, in9, func)
        type(DataObj), target, intent(inout) :: &
            self, in1, in2, in3, in4, in5, in6, in7, in8, in9
        interface
            real(8) function func(A1, A2, A3, A4, A5, A6, A7, A8, A9, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A1(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A2(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A3(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A4(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A5(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A6(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A7(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A8(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A9(x, y);  integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1
        gInputData2  => in2
        gInputData3  => in3
        gInputData4  => in4
        gInputData5  => in5
        gInputData6  => in6
        gInputData7  => in7
        gInputData8  => in8
        gInputData9  => in9

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =                            &
                        func(AFunc1, AFunc2, AFunc3, AFunc4, AFunc5, AFunc6, &
                             AFunc7, AFunc8, AFunc9,                         &
                             blkI + blkXOffset,                              &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply10( &
        self, in1, in2, in3, in4, in5, in6, in7, in8, in9, in10, func)
        type(DataObj), target, intent(inout) :: &
            self, in1, in2, in3, in4, in5, in6, in7, in8, in9, in10
        interface
            real(8) function func(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, i, j)
                integer, intent(in) :: i, j
                interface
                    real(8) function A1(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A2(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A3(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A4(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A5(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A6(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A7(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A8(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A9(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A10(x, y); integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1  => in1
        gInputData2  => in2
        gInputData3  => in3
        gInputData4  => in4
        gInputData5  => in5
        gInputData6  => in6
        gInputData7  => in7
        gInputData8  => in8
        gInputData9  => in9
        gInputData10 => in10

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gCurrentLbid = lbid
            gbid = distribution_lbid2gbid(self%dist, lbid)
            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1
            
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =                            &
                        func(AFunc1, AFunc2, AFunc3, AFunc4, AFunc5, AFunc6, &
                             AFunc7, AFunc8, AFunc9, AFunc10,                &
                             blkI + blkXOffset,                              &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine


    subroutine data_applyNC1(self, in1, func)
        type(DataObj), target, intent(inout) :: self, in1
        interface
            real(8) function func(AREL1, AABS1, sg, i, j)
                integer, intent(in) :: sg, i, j
                interface
                    real(8) function AREL1(    x, y); integer, intent(in) ::     x, y; end function
                    real(8) function AABS1(sg, x, y); integer, intent(in) :: sg, x, y; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, sgid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gbid = distribution_lbid2gbid(self%dist, lbid)
            sgid = distribution_gbid2sg(self%dist, gbid)
            gCurrentLbid = lbid
            gCurrentGbid = gbid

            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1

            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =    &
                        func(ARelFunc1,              &
                             AIdxFunc1,              &
                             sgid,                   &
                             blkI + blkXOffset,      &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine

    subroutine data_apply_noncompact_r(self, in1_rel, func)
        type(DataObj), target, intent(inout)   :: self
        type(AccessMethod_relative),intent(in) :: in1_rel
        interface
            real(8) function func(A1_REL, sg, i, j)
                integer, intent(in) :: sg, i, j
                interface
                    real(8) function A1_REL(    x, y); integer, intent(in) ::     x, y; end function
                end interface
            end function
        end interface
    end subroutine

    subroutine data_apply_noncompact_i(self, in1_idx, func)
        type(DataObj), target, intent(inout) :: self
        type(AccessMethod_index),intent(in)  :: in1_idx
        interface
            real(8) function func(A1_IDX, sg, i, j)
                integer, intent(in) :: sg, i, j
                interface
                    real(8) function A1_IDX(sg, x, y); integer, intent(in) :: sg, x, y; end function
                end interface
            end function
        end interface
    end subroutine



    subroutine data_apply_noncompact_t(self, in1_tag, func)
        type(DataObj), target, intent(inout) :: self
        type(AccessMethod_tag),intent(in)    :: in1_tag
        interface
            real(8) function func(A1_TAG, sg,i,j)
                integer, intent(in) :: sg,i,j
                interface
                    real(8) function A1_TAG(t); integer, intent(in) :: t; end function
                end interface
            end function
        end interface

        type(SubGrid) :: sg
        type(Neighbor), pointer :: n
        integer :: lbid, gbid, sgid, blkI, blkJ, neigh, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        gInputData1 => in1_tag%underlyingData

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gbid = distribution_lbid2gbid(self%dist, lbid)
            sgid = distribution_gbid2sg(self%dist, gbid)
            gCurrentLbid = lbid; gCurrentGbid = gbid

            blkXOffset = distribution_blockLowX(self%dist, gbid) - 1
            blkYOffset = distribution_blockLowY(self%dist, gbid) - 1

            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI, blkJ, lbid) =    &
                        func(ATagFunc1,              &
                             sgid,                   &
                             blkI + blkXOffset,      &
                             blkJ + blkYOffset)
                end do
            end do
        end do
    end subroutine


    subroutine data_apply_noncompact_trrrrrrrrr(self,   &
        in1_tag, in2_rel, in3_rel, in4_rel,             &
        in5_rel, in6_rel, in7_rel, in8_rel, in9_rel, in10_rel, func)
        !`
        type(DataObj), target, intent(inout)   :: self
        type(AccessMethod_tag),intent(in)      :: in1_tag
        type(AccessMethod_relative),intent(in) :: &
            in2_rel, in3_rel, in4_rel,            &
            in5_rel, in6_rel, in7_rel,            &
            in8_rel, in9_rel, in10_rel
        interface
            real(8) function func(A1_TAG,                               &
                A2_REL, A3_REL, A4_REL, A5_REL, A6_REL, A7_REL, A8_REL, &
                A9_REL, A10_REL, sg, i, j)
            !`
                integer, intent(in) :: sg, i, j
                interface
                    real(8) function A1_TAG(tag);   integer, intent(in) :: tag ; end function
                    real(8) function A2_REL(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A3_REL(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A4_REL(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A5_REL(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A6_REL(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A7_REL(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A8_REL(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A9_REL(x, y);  integer, intent(in) :: x, y; end function
                    real(8) function A10_REL(x, y); integer, intent(in) :: x, y; end function
                end interface
            end function
        end interface
    end subroutine








    real(8) function data_sum(self)
        include 'mpif.h'
        type(DataObj), intent(in) :: self
        integer :: lbid, gbid, i,j, neigh, blkW, blkH, ierr
        real(8) :: lclsum = 0.0, glbsum = 0.0

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            do j=1,blkH
                do i=1,blkW
                    lclsum = lclsum + self%vals(i,j,lbid)
                end do
            end do
        end do

        call MPI_ALLREDUCE( &
            lclSum, glbSum, 1, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
        data_sum = glbsum
    end function

    subroutine data_copy(to, from)
        type(DataObj), intent(inout) :: to
        type(DataObj), intent(in)    :: from
        integer :: lbid, blkI, blkJ, blkW, blkH

        blkW = distribution_width(to%dist)
        blkH = distribution_height(to%dist)

        ! Iterate over all points in all local blocks
        do lbid=lbound(to%vals,3), ubound(to%vals,3)
            do blkJ=1,blkH
                do blkI=1,blkW
                    to%vals(blkI,blkJ,lbid) = from%vals(blkI,blkJ,lbid)
                end do
            end do
        end do
    end subroutine

    subroutine data_add(self, val)
        type(DataObj), intent(inout) :: self
        integer, intent(in)          :: val
        integer :: lbid, blkI, blkJ, blkW, blkH

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            do blkJ=1,blkH
                do blkI=1,blkW
                    self%vals(blkI,blkJ,lbid) = self%vals(blkI,blkJ,lbid) + val
                end do
            end do
        end do
    end subroutine

    subroutine data_initializeSeqVals(self)
        type(DataObj), intent(inout) :: self
        integer :: lbid, gbid, blkI, blkJ, blkW, blkH, n, i
        type(Subgrid) :: sg
        integer :: sgid

        blkW = distribution_width(self%dist)
        blkH = distribution_height(self%dist)

        ! Determine how many points are owned by procs before myRank()
        n=1
        do i=0,myRank()-1
           n = n + distribution_numNodesForProc(self%dist, i) 
        end do

        ! Iterate over all points in all local blocks
        do lbid=lbound(self%vals,3), ubound(self%vals,3)
            gbid = distribution_lbid2gbid(self%dist,lbid)
            sg = grid_getSubgrid(self%grd,distribution_gbid2sg(self%dist,gbid))
            sgid = subgrid_getID(sg)

            do blkJ=1,MIN(blkH,subgrid_width(sg))
                do blkI=1,MIN(blkW,subgrid_height(sg))
                    self%vals(blkI,blkJ,lbid) = n
                    n = n+1
                end do
            end do
        end do
    end subroutine

    subroutine data_initializeToVal(self, val)
        type(DataObj), intent(inout) :: self
        real(8), intent(in) :: val

        self%vals = val
    end subroutine

    subroutine data_forceUpdate(self)
        include 'mpif.h'
        type(DataObj), intent(inout) :: self
        type(Schedule), pointer :: sched
        integer :: msg, tr, i, x, y, blkX, blkY, reqSz, req, ierr
        integer :: largestSendMsg, largestRecvMsg, regionSz
        integer :: currentLbid
        integer :: lbid, msgOff, sz
        integer :: stat(MPI_STATUS_SIZE)
        integer :: rgnLowX, rgnLowY, rgnHighX, rgnHighY
        integer, allocatable :: msgSize(:)

        sched => self%sched

        ! Find the largest size an outgoing message will be
        largestSendMsg = 0
        do msg=1,sched%nMsgsSend
            regionSz = 0
            do i=sched%sendMsgStart(msg),   &
                 sched%sendMsgStart(msg) + sched%numTransfersInSendMsg(msg) - 1
            !`--
                regionSz = regionSz + schedule_sendRegionSize(sched, i)
            end do
            if(regionSz > largestSendMsg) &
                largestSendMsg = regionSz
        end do

        ! Find the largest size an incoming message will be
        largestRecvMsg = 0
        do msg=1,sched%nMsgsRecv
            regionSz = 0
            do i=sched%recvMsgStart(msg),   &
                 sched%recvMsgStart(msg) + sched%numTransfersInRecvMsg(msg) - 1
            !`--
                regionSz = schedule_recvRegionSize(sched, i) + regionSz
            end do
            if(regionSz > largestRecvMsg) &
                largestRecvMsg = regionSz
        end do

        ! Allocate buffers:
        if(.not. allocated(sendBufs)) then
            allocate(sendBufs(largestSendMsg, sched%nMsgsSend))
            allocate(recvBufs(largestRecvMsg, sched%nMsgsRecv))
        end if

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

                ! Determine how to serialize based on orientation
                if(sched%transferRegionSendOrientation(tr) == 0) then ! BL
                    do y=rgnLowY, rgnHighY
                        do x=rgnLowX, rgnHighX
                            sendBufs(msgOff, msg) = self%vals(x, y, lbid)
                            msgOff = msgOff + 1
                        end do
                    end do
                else if(sched%transferRegionSendOrientation(tr) == 1) then ! BR
                    do y=rgnLowY, rgnHighY
                        do x=rgnHighX, rgnLowX, -1
                            sendBufs(msgOff, msg) = self%vals(x, y, lbid)
                            msgOff = msgOff + 1
                        end do
                    end do
                else if(sched%transferRegionSendOrientation(tr) == 2) then !TL
                    do y=rgnHighY, rgnLowY, -1
                        do x=rgnLowX, rgnHighX
                            sendBufs(msgOff, msg) = self%vals(x, y, lbid)
                            msgOff = msgOff + 1
                        end do
                    end do
                else if(sched%transferRegionSendOrientation(tr) == 3) then !TR
                    do y=rgnHighY, rgnLowY, -1
                        do x=rgnHighX, rgnLowX, -1
                            sendBufs(msgOff, msg) = self%vals(x, y, lbid)
                            msgOff = msgOff + 1
                        end do
                    end do
                endif
            end do

            ! Post the send
            msgOff = msgOff - 1
            msgSize(msg) = msgOff

            call MPI_ISEND(msgSize(msg),                                &
                           1, MPI_INT, sched%msgSendTo(msg), 0,         &
                           MPI_COMM_WORLD, reqSz, ierr)
            call MPI_ISEND(sendBufs(1, msg),                            &
                           msgOff, MPI_REAL8, sched%msgSendTo(msg), 0,  &
                           MPI_COMM_WORLD, req, ierr)
            call MPI_REQUEST_FREE(reqSz, ierr)
            call MPI_REQUEST_FREE(req, ierr)
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
                          sz, MPI_REAL8, sched%msgRecvFrom(msg), 0,     &
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
                
                ! Determine how to deserialize based on orientation
                if(sched%transferRegionRecvOrientation(tr) == 0) then ! BL
                    do y=rgnLowY, rgnHighY
                        do x=rgnLowX, rgnHighX
                            self%vals(x, y, currentLbid) = recvBufs(msgOff, msg)
                            msgOff = msgOff + 1
                        end do
                    end do
                else if(sched%transferRegionRecvOrientation(tr) == 1) then !BR
                    do y=rgnLowY, rgnHighY
                        do x=rgnHighX, rgnLowX, -1
                            self%vals(x, y, currentLbid) = recvBufs(msgOff, msg)
                            msgOff = msgOff + 1
                        end do
                    end do
                else if(sched%transferRegionRecvOrientation(tr) == 2) then !TL
                    do y=rgnHighY, rgnLowY, -1
                        do x=rgnLowX, rgnHighX
                            self%vals(x, y, currentLbid) = recvBufs(msgOff, msg)
                            msgOff = msgOff + 1
                        end do
                    end do
                else if(sched%transferRegionRecvOrientation(tr) == 3) then !TR
                    do y=rgnHighY, rgnLowY, -1
                        do x=rgnHighX, rgnLowX, -1
                            self%vals(x, y, currentLbid) = recvBufs(msgOff, msg)
                            msgOff = msgOff + 1
                        end do
                    end do
                endif
            end do
        end do
        
        if(allocated(self%ghosts)) then
            call data_forceGhostsUpdate(self)
        end if

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
        deallocate(msgSize)
    end subroutine

    subroutine data_forceGhostsUpdate(self)
        include 'mpif.h'
        type(DataObj), intent(inout) :: self
        type(Schedule), pointer :: sched
        type(Subgrid) :: sg
        integer :: msg, i, gbid, lbid, x, y, blkX, blkY, reqSize,req, ierr
        integer :: largestSendMsg, largestRecvMsg, regionSz
        integer :: sz, offset
        integer :: stat(MPI_STATUS_SIZE)
        integer :: rgnLowX, rgnLowY, rgnHighX, rgnHighY
        integer, allocatable :: msgSize(:)

        sched => self%sched

        if(.not. allocated(sendGhostsBufs)) then
            ! Find the largest size an outgoing message will be
            largestSendMsg = 0
            do msg=1,sched%nGhostMsgsSend
                if(sched%sendGhostMsgStart(msg+1) - sched%sendGhostMsgStart(msg) > largestSendMsg) &
                then
                    largestSendMsg = sched%sendGhostMsgStart(msg+1) - sched%sendGhostMsgStart(msg)
                end if
            end do

            ! Find the largest size an incoming message will be
            largestRecvMsg = 0
            do msg=1,sched%nGhostMsgsRecv
                if(sched%recvGhostMsgStart(msg+1) - sched%recvGhostMsgStart(msg) > largestRecvMsg) &
                then
                    largestRecvMsg = sched%recvGhostMsgStart(msg+1) - sched%recvGhostMsgStart(msg)
                end if
            end do

            ! Allocate buffers:
            allocate(sendGhostsBufs(largestSendMsg, sched%nGhostMsgsSend))
            allocate(recvGhostsBufs(largestRecvMsg, sched%nGhostMsgsRecv))
        end if

        ! *************************
        ! Post sending messages:
        ! *************************
        allocate(msgSize(sched%nGhostMsgsSend))

        ! Iterate through messages to send
        do msg=1,sched%nGhostMsgsSend
            offset=1
            ! Gather data into buffer
            do i=sched%sendGhostMsgStart(msg),sched%sendGhostMsgStart(msg+1)-1
                lbid = sched%sendGhostLbid(i)
                x = sched%sendGhostBlockI(i)
                y = sched%sendGhostBlockJ(i)

                sendGhostsBufs(offset,msg) = self%vals(x,y,lbid)
                offset = offset + 1
            end do

            ! Post the send
            msgSize(msg) = sched%sendGhostMsgStart(msg+1) - &
                           sched%sendGhostMsgStart(msg)
            call MPI_ISEND(msgSize(msg),                                &
                           1, MPI_INT, sched%ghostMsgSendTo(msg), 0,    &
                           MPI_COMM_WORLD, reqSize, ierr)
            call MPI_ISEND(sendGhostsBufs(1, msg),                      &
                           msgSize(msg),                                &
                           MPI_REAL8, sched%ghostMsgSendTo(msg), 1,     &
                           MPI_COMM_WORLD, req, ierr)
            call MPI_REQUEST_FREE(reqSize, ierr)
            call MPI_REQUEST_FREE(req, ierr)
        end do
        
        ! ***************************
        ! Receive incoming messages:
        ! ***************************

        ! Iterate through messages to receive
        do msg=1,sched%nGhostMsgsRecv
           ! receive message into buffer
            call MPI_RECV(sz, 1, MPI_INT, sched%ghostMsgRecvFrom(msg), 0,   &
                          MPI_COMM_WORLD, stat, ierr)
            call MPI_RECV(recvGhostsBufs(1, msg),                           &
                          sz, MPI_REAL8, sched%ghostMsgRecvFrom(msg), 1,    &
                          MPI_COMM_WORLD, stat, ierr)

            ! Store values
            self%ghosts( &
              sched%recvGhostMsgStart(msg):sched%recvGhostMsgStart(msg+1)-1) = &
              recvGhostsBufs(1:sz, msg)
        end do
        
        deallocate(msgSize)
    end subroutine

end module mod_data
