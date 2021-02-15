module Data_mod
    use SubGrid_mod
    use Distribution
    use Utils

    implicit none

    type, public :: Data
        real, allocatable :: values(:)
        integer :: localStorageSize
        type(SubGrid), pointer :: grid
        type(DistributionT), pointer :: dist
    end type
    public data_new,      &
        data_input,       &
        data_output,      &
        data_set,         &
        data_applyGrid,   &
        data_add,         &
        data_sub,         &
        data_mult,        &
        data_pow,         &
        data_sum,         &
        data_at,          &
        data_update_halo, &
        data_print

    private data_applyDim
contains
! *****************************************************************************
    subroutine data_new(self, g, dist)
        type(Data), intent(inout) :: self
        type(SubGrid), target, intent(in) :: g
        type(DistributionT), target, intent(in) :: dist

        integer numElements, i

        self%grid => g
        self%dist => dist
        self%localStorageSize = distribution_total_storage_on_local_proc(dist)

        allocate(self%values(self%localStorageSize))
        self%values = 0.0
    end subroutine

    subroutine data_input(self, unitnum)
        type(Data), intent(inout) :: self
        integer, intent(in) :: unitnum

        integer :: i

        read(unitnum, *) self%values
    end subroutine

    subroutine data_output(self, unitnum)
        type(Data), intent(inout) :: self
        integer, intent(in) :: unitnum

        write(unitnum, *) self%values
    end subroutine

    ! TODO: Generalize to n-dimensions
    subroutine data_set(self, lclBlkID, idx, val)
        type(Data), intent(inout) :: self
        integer, intent(in) :: lclBlkID, idx(:)
        real, intent(in) :: val
        
!        print *, "Set",data_at(self,lclBlkID,idx), "to", val
        self%values(data_at(self, lclBlkID, idx)) = val
    end subroutine

    type(Data) function data_applyGrid(self)
        type(Data), intent(inout) :: self

        integer :: idx, lclBlk, blkID

        ! If the data object to return hasn't previously been allocated
        ! allocate it now
        if(.not. allocated(data_applyGrid%values)) then
            call data_new(data_applyGrid, self%grid, self%dist)
        end if

        ! Loop through blocks this process owns
        do lclBlk=1,self%dist%nLocalBlocks
            blkID = self%dist%globalBlockID(lclBlk)
            call data_applyDim(self, blkID, 1, data_applyGrid)
        end do
    end function

    type(Data) function data_add(lhs, rhs)
        type(Data), intent(in) :: lhs
        type(Data), intent(in) :: rhs

        integer :: i

        call data_new(data_add, lhs%grid, lhs%dist)

        do i=0,ubound(lhs%values, 1)
            data_add%values(i) = lhs%values(i) + rhs%values(i)
        end do
    end function

    type(Data) function data_sub(lhs, rhs)
        type(Data), intent(in) :: lhs
        type(Data), intent(in) :: rhs

        integer :: i

        call data_new(data_sub, lhs%grid, lhs%dist)

        do i=0,ubound(lhs%values, 1)
            data_sub%values(i) = lhs%values(i) - rhs%values(i)
        end do
    end function

    type(Data) function data_mult(lhs, rhs)
        type(Data), intent(in) :: lhs
        type(Data), intent(in) :: rhs

        integer :: i

        call data_new(data_mult, lhs%grid, lhs%dist)

        do i=0,ubound(lhs%values, 1)
            data_mult%values(i) = lhs%values(i) * rhs%values(i)
        end do
    end function

    type(Data) function data_pow(lhs, exponent)
        type(Data), intent(in) :: lhs
        integer, intent(in) :: exponent

        integer :: i, j

        call data_new(data_pow, lhs%grid, lhs%dist)

        do j=1,ubound(lhs%values, 1)
            data_pow%values(j) = 1.0
        end do

        do i=1,exponent
            do j=1,ubound(lhs%values, 1)
                data_pow%values(j) = data_pow%values(j) * lhs%values(j)
            end do
        end do
    end function

    real function data_sum(x)
        type(Data), intent(in) :: x

        integer :: i
        real :: res

        res = 0.0

        do i=0,ubound(x%values, 1)
            res = res + x%values(i)
        end do

        data_sum = res
    end function

    ! TODO: Make this function generalized to more than 2 dimensions
    ! This function returns the storage local for the value idx in the local
    ! block lclBlkID
    integer function data_at(self, lclBlkID, idx) result(res)
        type(Data), intent(in) :: self
        integer, intent(in) :: lclBlkID, idx(:)
        integer :: translatedIdx(2), blkID

        integer i, tmp

        blkID = self%dist%globalBlockID(lclBlkID)

        ! Translate idx into a local offset (this accounts for ghost cells)
        do i=1,self%dist%nDims
            translatedIdx(i) = idx(i) - self%dist%lower_idx(blkID, i) + &
                self%dist%nGhostCells(i,1) + 1
        end do

        tmp = (translatedIdx(2)-1) * &
            distribution_block_size_w_ghost_cells(self%dist, blkID, 1) + &
            translatedIdx(1)

        res = self%dist%storageIdx(lclBlkID) + tmp - 1
    end function

    subroutine data_update_halo(self)
        type(Data), intent(in) :: self
        
        integer :: i
        
        i = 3

        ! Loop through local blocks
            ! Post receive

        ! Loop through local blocks
            ! Loop through send windows
                ! Buffer data to be sent
                ! Send data

        ! Loop through receive blocks
            ! Store in halo
    end subroutine

    ! TODO: Generalize to more than two dimensions
    subroutine data_print(self)
        type(Data) :: self

        integer idx_0, idx_1, blk, lclBlk, ierr, stat, req
        real :: val

        do idx_1 = subgrid_bound(self%grid, 2), 1, -1
            do idx_0 = 1, subgrid_bound(self%grid, 1)
                blk = distribution_block_at(self%dist, (/ idx_0, idx_1 /))

                if(myRank() == self%dist%proc(blk)) then
                    lclBlk = self%dist%localBlockID(blk)
                    call MPI_ISend( &
                        self%values(data_at(self, lclBlk, (/ idx_0, idx_1 /))),&
                        1, MPI_REAL, 0, 0, self%dist%communicator, req, ierr)
                end if

                if(myRank() == 0) then
                    call MPI_Recv(val, 1, MPI_REAL, self%dist%proc(blk), 0, &
                        self%dist%communicator, stat, ierr)

                    write (*,"(f12.3)", advance="no") val
                end if

                if(myRank() == self%dist%proc(blk)) then
                    call MPI_WAIT(req, MPI_STATUS_IGNORE, ierr)
                end if

                call MPI_BARRIER(self%dist%communicator, ierr)
            enddo
            if(myRank() == 0) print *, " "
            call MPI_BARRIER(self%dist%communicator, ierr)
        end do
        if(myRank() == 0) print *, " "
    end subroutine

! *****************************************************************************
    recursive subroutine data_applyDim(self, blkID, d, res)
        type(Data), intent(inout) :: self
        integer, intent(in) :: blkID, d
        type(Data), intent(inout) :: res

        integer :: i, j, lclBlk
        real    :: newVal
        integer, allocatable :: idx(:)

        ! If this is the last dimension
        if(d == self%grid%env%nBounds) then
            allocate(idx(d))
            lclBlk = self%dist%localBlockID(blkID)

            ! Iterate through local elements
            do i=self%dist%lower_idx(blkID, d), self%dist%upper_idx(blkID, d)
                self%grid%env%indices(d) = i

                ! Determine what result of applying stencil at current element
                ! is
                newVal = 0.0
                do j=1, self%grid%nEdges
                    call neighbor_apply( &
                        self%grid%neighbors(j), self%grid%env, idx)
!                    if(idx /= 0) then
                        newVal = newVal + &
                            applyExp(self%grid%coefficients(j), self%grid%env) &
                            * self%values(data_at(self, lclBlk, idx))
!                    end if
                end do

                !call neighbor_apply( &
                !        self%grid%neighbors(1), self%grid%env, idx)
                !newVal = idx(2)
                !newVal = applyExp(self%grid%coefficients(2), self%grid%env)
                !newVal = self%grid%env%indices(1) * 10.0 + self%grid%env%indices(2)

                call data_set(res, res%dist%localBlockID(blkID), &
                    self%grid%env%indices, newVal)
            end do
            deallocate(idx)
        ! If we're not calling the last dimension call this function recursively
        ! for each set of elements along the current dimension
        else
            do i=self%dist%lower_idx(blkID, d), self%dist%upper_idx(blkID, d)
                self%grid%env%indices(d) = i
                call data_applyDim(self, blkID, d+1, res)
            end do
        end if
    end subroutine
end module
