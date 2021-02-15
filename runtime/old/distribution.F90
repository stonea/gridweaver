! 2D distribution of blocks
module Distribution
    use Utils

    implicit none

!    include 'mpif.h'

!    integer, parameter :: &
!        NEIGH_EAST  = 1, &
!        NEIGH_WEST  = 2, &
!        NEIGH_NORTH = 3, &
!        NEIGH_SOUTH = 4, &
!        NEIGH_SEAST = 5, &
!        NEIGH_SWEST = 6, &
!        NEIGH_NWEST = 7, &
!        NEIGH_NEAST = 8, &
!        NEIGH_NUM_NEIGH = 8
    
    type DistributionT
        integer nProcs
        integer communicator
        integer nBlocks
        integer nDims

        integer, allocatable :: &
            nGhostCells(:, :),  &  ! Specifies number of ghost cells to include
                                   ! in the positive and negative for each
                                   ! dimension of every block.
            lower_idx(:, :),    &  ! Specifies the lower index for the block
                                   ! in each dimension
            upper_idx(:, :),    &  ! Specifies the upper index for the block
                                   ! in each dimension
            proc(:),            &  ! Maps block ID to MPI processor rank
            neigh(:, :),        &  ! Specifies neighboring blocks
            all_block_ij(:, :), &  ! Maps block ID to i and j of bottom-left
                                   ! index.  The second dimension corresponds
                                   ! to i and j idx=1 for i, idx=2 for j
            storageIdx(:),      &  ! Offset where storage for a local block
                                   ! begins
            localBlockID(:)

        ! Local scheduling information
        integer, allocatable :: &
            nSendWindows(:),                &
            nRecvWindows(:),                &
            windowID(:),                    &
            sendWindow_lower_idx(:,:,:),    &
            sendWindow_upper_idx(:,:,:),    &
            recvWindow_lower_idx(:,:,:),    &
            recvWindow_upper_idx(:,:,:),    &
            sendWindowBlock(:,:),           &
            recvWindowBlock(:,:)             

        integer nLocalBlocks
        integer, allocatable :: &
            globalBlockID(:)
    end type
    
    public :: distribution_new_cyclic,          &
              distribution_num_local_blocks,    &
              distribution_block_size,          &
              distribution_block_size_w_ghost_cells, &
              distribution_block_at

  contains

! *****************************************************************************
    subroutine distribution_new_cyclic(self, comm, nBlockX, nBlockY, nPointX, &
        nPointY)

        type(DistributionT), intent(inout) :: self
        integer, intent(in) :: comm, nBlockX, nBlockY, nPointX, nPointY
        integer :: ierr, nBlocks, idxBX, idxBY, blk, lclBlk, proc, &
            nLocalBlocks, myproc
        integer, allocatable :: localCount(:)
        
        call MPI_COMM_SIZE(comm, self%nProcs, ierr)
        call MPI_COMM_RANK(comm, myproc, ierr)
        self%communicator = comm
        nBlocks = nBlockX * nBlockY
        self%nBlocks = nBlocks
        self%nDims = 2 ! TODO: Take out hard-coding
        
        allocate(self%nGhostCells(2,2)) ! TODO: Take out hard-coding
        self%nGhostCells(1, 1) = 1
        self%nGhostCells(1, 2) = 1
        self%nGhostCells(2, 1) = 1
        self%nGhostCells(2, 2) = 1
        
        allocate(self%lower_idx(nBlocks, 2))
        allocate(self%upper_idx(nBlocks, 2))
        allocate(self%proc(nBlocks))
        !allocate(self%neigh(nBlocks, NEIGH_NUM_NEIGH))
        allocate(self%all_block_ij(nBlocks, 2))
        allocate(self%localBlockID(nBlocks))

        allocate(localCount(0:self%nProcs-1))
        localCount = 0
        
        ! Iterate through blocks assigning information in globally replicated
        ! variables
        blk = 1
        proc = 0
        do idxBY=1,nBlockY
            do idxBX=1,nBlockY
                self%all_block_ij(blk, 1) = idxBX
                self%all_block_ij(blk, 2) = idxBY
                self%lower_idx(blk, 1) = (idxBX-1) * nPointX+1
                self%upper_idx(blk, 1) = idxBX * nPointX
                self%lower_idx(blk, 2) = (idxBY-1) * nPointY+1
                self%upper_idx(blk, 2) = idxBY * nPointY
                self%proc(blk) = proc
                self%localBlockID(blk) = localCount(proc) + 1

                localCount(proc) = localCount(proc) + 1
                blk = blk + 1
                proc = proc + 1
                
                if(proc >= self%nProcs) proc = 0
            end do
        end do
        nLocalBlocks = localCount(proc)

        ! Allocate local variables
        allocate(self%globalBlockID(nLocalBlocks))
        allocate(self%storageIdx(nLocalBlocks))
        allocate(self%nSendWindows(nLocalBlocks))
        allocate(self%nRecvWindows(nLocalBlocks))
        !allocate(self%windowID(4))
        allocate(self%sendWindow_lower_idx(nLocalBlocks, 4, 2))
        allocate(self%sendWindow_upper_idx(nLocalBlocks, 4, 2))
        allocate(self%recvWindow_lower_idx(nLocalBlocks, 4, 2))
        allocate(self%recvWindow_upper_idx(nLocalBlocks, 4, 2))
        allocate(self%sendWindowBlock(nLocalBlocks, 4))
        allocate(self%recvWindowBlock(nLocalBlocks, 4))

        ! Iterate through blocks assigning local information
        self%storageIdx(1) = 1
        do blk=1,nBlocks
            if(self%proc(blk) == myRank()) then
                self%globalBlockID(self%localBlockID(blk)) = blk

                if(self%localBlockID(blk) /= 1) then
                    self%storageIdx(self%localBlockID(blk)) = &
                            self%storageIdx(self%localBlockID(blk)-1) + &
                            distribution_block_storage_size(self, blk)
                end if
            end if
        end do

        ! Set communication metadata. With this distribution each block has
        ! four send windows
        self%nSendWindows = 4
        self%nRecvWindows = 4
        do lclBlk=1,nLocalBlocks
            blk = self%globalBlockID(lclBlk)
            
            ! North windows
            self%recvWindow_lower_idx(lclBlk, 1, 1) = &
                self%lower_idx(blk, 1) - 1
            self%recvWindow_lower_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2) + 1
            self%recvWindow_upper_idx(lclBlk, 1, 1) = &
                self%upper_idx(blk, 1) + 1
            self%recvWindow_upper_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2) + 1
            self%recvWindowBlock(lclBlk,1) = blk + nBlockX

            self%sendWindow_lower_idx(lclBlk, 1, 1) = &
                self%lower_idx(blk, 1) - 1
            self%sendWindow_lower_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2)
            self%sendWindow_upper_idx(lclBlk, 1, 1) = &
                self%upper_idx(blk, 1) + 1
            self%sendWindow_upper_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2)
            self%sendWindowBlock(lclBlk,1) = blk + nBlockX

            ! East windows
            self%recvWindow_lower_idx(lclBlk, 2, 1) = &
                self%upper_idx(blk, 1) + 1
            self%recvWindow_lower_idx(lclBlk, 2, 2) = &
                self%lower_idx(blk, 2) - 1
            self%recvWindow_upper_idx(lclBlk, 2, 1) = &
                self%upper_idx(blk, 1) + 1
            self%recvWindow_upper_idx(lclBlk, 2, 2) = &
                self%upper_idx(blk, 2) + 1
            self%recvWindowBlock(lclBlk,2) = blk + 1

            self%sendWindow_lower_idx(lclBlk, 1, 1) = &
                self%lower_idx(blk, 1) - 1
            self%sendWindow_lower_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2)
            self%sendWindow_upper_idx(lclBlk, 1, 1) = &
                self%upper_idx(blk, 1) + 1
            self%sendWindow_upper_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2)
            self%sendWindowBlock(lclBlk,1) = blk + 1

            ! South windows
            self%recvWindow_lower_idx(lclBlk, 3, 1) = &
                self%lower_idx(blk, 1) - 1
            self%recvWindow_lower_idx(lclBlk, 3, 2) = &
                self%lower_idx(blk, 2) - 1
            self%recvWindow_upper_idx(lclBlk, 3, 1) = &
                self%upper_idx(blk, 1) + 1
            self%recvWindow_upper_idx(lclBlk, 3, 2) = &
                self%lower_idx(blk, 2) - 1
            self%recvWindowBlock(lclBlk,3) = blk - nBlockX

            self%sendWindow_lower_idx(lclBlk, 1, 1) = &
                self%lower_idx(blk, 1) - 1
            self%sendWindow_lower_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2)
            self%sendWindow_upper_idx(lclBlk, 1, 1) = &
                self%upper_idx(blk, 1) + 1
            self%sendWindow_upper_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2)
            self%sendWindowBlock(lclBlk,1) = blk + nBlockX

            
            ! West windows
            self%recvWindow_lower_idx(lclBlk, 4, 1) = &
                self%lower_idx(blk, 1) - 1
            self%recvWindow_lower_idx(lclBlk, 4, 2) = &
                self%lower_idx(blk, 2) - 1
            self%recvWindow_upper_idx(lclBlk, 4, 1) = &
                self%lower_idx(blk, 1) - 1
            self%recvWindow_upper_idx(lclBlk, 4, 2) = &
                self%upper_idx(blk, 2) + 1
            self%recvWindowBlock(lclBlk,4) = blk - nBlockX

            self%sendWindow_lower_idx(lclBlk, 1, 1) = &
                self%lower_idx(blk, 1) - 1
            self%sendWindow_lower_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2)
            self%sendWindow_upper_idx(lclBlk, 1, 1) = &
                self%upper_idx(blk, 1) + 1
            self%sendWindow_upper_idx(lclBlk, 1, 2) = &
                self%upper_idx(blk, 2)
            self%sendWindowBlock(lclBlk,1) = blk - 1
        end do

!        nSendWindows(:)
!        nRecvWindows(:)
!        windowID(:)
!        sendWindow_lower_idx(:,:,:)
!        sendWindow_upper_idx(:,:,:)
!        recvWindow_lower_idx(:,:,:)
!        procSend(:,:)
!        procRecv(:,:)

        self%nLocalBlocks = nLocalBlocks

        deallocate(localCount)
    end subroutine

    integer function distribution_num_local_blocks(self)
        type(DistributionT), intent(in) :: self

        distribution_num_local_blocks = self%nLocalBlocks
    end function

    integer function distribution_block_size(self, blockID, dimID)
        type(DistributionT), intent(in) :: self
        integer, intent(in) :: blockID, dimID

        distribution_block_size = &
            self%upper_idx(blockID, dimID) - self%lower_idx(blockID, dimID) + 1
    end function

    integer function distribution_block_size_w_ghost_cells(self, blockID, dimID)
        type(DistributionT), intent(in) :: self
        integer, intent(in) :: blockID, dimID
        
        distribution_block_size_w_ghost_cells = &
            distribution_block_size(self, blockID, dimID) + &
                self%nGhostCells(dimID,1) + self%nGhostCells(dimID,2)
    end function

    integer function distribution_block_storage_size(self, blockID)
        type(DistributionT), intent(in) :: self
        integer, intent(in) :: blockID

        integer ret, i

        ret = 1
        do i=1,self%nDims
            ret = ret * (distribution_block_size(self, blockID, i) + &
                self%nGhostCells(i,1) + self%nGhostCells(i,2))
        end do

        distribution_block_storage_size = ret
    end function

    integer function distribution_total_storage_on_local_proc(self)
        type(DistributionT), intent(in) :: self

        integer res

        res = self%storageIdx(self%nLocalBlocks)
        res = res + distribution_block_storage_size(self, &
            self%globalBlockID(self%nLocalBlocks))

        distribution_total_storage_on_local_proc = res
    end function

    subroutine distribution_print(self)
        type(DistributionT), intent(in) :: self

        integer :: i, j, lclBlk, ierr

        do i=1, self%nBlocks
            call MPI_BARRIER(self%communicator, ierr)
            if(myRank() == self%proc(i)) then
                lclBlk = self%localBlockID(i)
                write(*, '(A,I0)'), " BLOCK: ", i
                print *, "proc: ", self%proc(i)
                print *, "lower_idx:", self%lower_idx(i,:)
                print *, "upper_idx:", self%upper_idx(i,:)
                print *, "nSendWindows:", self%nSendWindows(lclBlk)
                print *, "nRecvWindows:", self%nRecvWindows(lclBlk)
                write(*, '(A)', advance="no"), "sendWindow_lower_idx(X): " 
                do j=1,self%nSendWindows(lclBlk)
                    write(*, '(I3)', advance="no"), &
                        self%sendWindow_lower_idx(lclBlk,j,1)
                end do
                print *, ""
                write(*, '(A)', advance="no"), "sendWindow_lower_idx(Y): " 
                do j=1,self%nSendWindows(lclBlk)
                    write(*, '(I3)', advance="no"), &
                        self%sendWindow_lower_idx(lclBlk,j,2)
                end do
                print *, ""
                write(*, '(A)', advance="no"), "sendWindow_upper_idx(X): " 
                do j=1,self%nSendWindows(lclBlk)
                    write(*, '(I3)', advance="no"), &
                        self%sendWindow_upper_idx(lclBlk,j,1)
                end do
                print *, ""
                write(*, '(A)', advance="no"), "sendWindow_upper_idx(Y): " 
                do j=1,self%nSendWindows(lclBlk)
                    write(*, '(I3)', advance="no"), &
                        self%sendWindow_upper_idx(lclBlk,j,2)
                end do
                print *, ""
                write(*, '(A)', advance="no"), "recvWindow_lower_idx(X): " 
                do j=1,self%nSendWindows(lclBlk)
                    write(*, '(I3)', advance="no"), &
                        self%recvWindow_lower_idx(lclBlk,j,1)
                end do
                print *, ""
                write(*, '(A)', advance="no"), "recvWindow_lower_idx(Y): " 
                do j=1,self%nSendWindows(lclBlk)
                   write(*, '(I3)', advance="no"), &
                        self%recvWindow_lower_idx(lclBlk,j,2)
                end do
                print *, ""
                write(*, '(A)', advance="no"), "recvWindow_upper_idx(X): " 
                do j=1,self%nSendWindows(lclBlk)
                    write(*, '(I3)', advance="no"), &
                        self%recvWindow_upper_idx(lclBlk,j,1)
                end do
                print *, ""
                write(*, '(A)', advance="no"), "recvWindow_upper_idx(Y): " 
                do j=1,self%nSendWindows(lclBlk)
                    write(*, '(I3)', advance="no"), &
                        self%recvWindow_upper_idx(lclBlk,j,2)
                end do
                print *, ""
                print *, "recvWindowBlock:", self%recvWindowBlock(lclBlk,:)
                print *, "sendWindowBlock:", self%sendWindowBlock(lclBlk,:)
                print *, ""
                print *, "------------------"
            end if
            call flush()
            call MPI_BARRIER(self%communicator, ierr)
        end do

        call MPI_BARRIER(self%communicator, ierr)
        do i=0,self%nProcs-1
            if(i == myRank()) then
                print *, "-----------------------", i, "-----------------"
                print *, "globalBlockID: ", self%globalBlockID
                print *, "storageIdx: ", self%storageIdx
                print *, ""
            end if
        call flush()
        call MPI_BARRIER(self%communicator, ierr)
        end do
    end subroutine

    integer function distribution_block_at(self, idx)
        type(DistributionT), intent(in) :: self
        integer, intent(in) :: idx(:)

        integer blk, i

!        print *, "Index: ", idx

        ! Loop through all blocks searching for the one that fits the
        ! specified index
        blkLoop: do blk=1,self%nBlocks
!            print *, "Checking blk", blk
            ! Check each value in the index tuple
            do i=1,self%nDims
                if(.not. (idx(i) >= self%lower_idx(blk, i) .and. &
                          idx(i) <= self%upper_idx(blk, i))) &
                then
!                    print *, "Fail on dim", i
                    cycle blkLoop
                end if
            end do

!            print *, "Match found"
            distribution_block_at = blk
            return
        end do blkLoop
    end function
end module
