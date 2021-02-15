program Test
    use Distribution
    use SubGrid_mod
    use Data_mod
    use Expressions
    use Utils
    
    implicit none

    integer :: ierr
    integer, parameter :: N = 4
    real :: A(N * N)
    integer :: i,j,f,lclBlk, blkID,z
    integer :: mpiSize, mpiRank

    ! -----------------------------------------------------------------------
    type(DistributionT) :: dist
    type(Neighbor) :: center, north, east, south, west
    type(SubGrid) :: g
    !type(Grid) :: g
    type(Data) :: lhs, rhs, res
    
    call MPI_INIT(ierr)
    call MPI_COMM_SIZE(MPI_COMM_WORLD, mpiSize, ierr)
    call MPI_COMM_RANK(MPI_COMM_WORLD, mpiRank, ierr)

    call distribution_new_cyclic( &
        dist, MPI_COMM_WORLD, 2, 2, N/2, N/2)

    call distribution_print(dist)

!    call subgrid_new(g, 2, 4)
!    call subgrid_setBound(g, 1, N);
!    call subgrid_setBound(g, 2, N);
!
!    call neighbor_new(north, "north", 2)
!    call neighbor_setOffset(north, (/ idx_(1), plus_(idx_(2), const_(1)) /))
!    
!    call neighbor_new(south, "south", 2)
!    call neighbor_setOffset(south, (/ idx_(1), minus_(idx_(2), const_(1)) /))
!    
!    call neighbor_new(west, "west", 2)
!    call neighbor_setOffset(west, (/ minus_(idx_(1), const_(1)), idx_(2) /))
!
!    call neighbor_new(east, "east", 2)
!    call neighbor_setOffset(east, (/  plus_(idx_(1), const_(1)), idx_(2) /))
!
!    call subgrid_addEdges(g, north, const_(1))
!    call subgrid_addEdges(g, south, const_(1))
!    call subgrid_addEdges(g, west, const_(1))
!    call subgrid_addEdges(g, east, const_(1))
!
!    call data_new(lhs, g, dist)
!    call data_new(rhs, g, dist)
!
    ! Iterate over local indices
    !GRIDGEN local_loop(blkID, i, j)

!    do z=0,1
!    if(z == mpiRank) then
!    print *, "on proc",z

!    do lclBlk=1,dist%nLocalBlocks
!        blkID = dist%globalBlockID(lclBlk)
!        do j=dist%lower_idx(blkID,2), dist%upper_idx(blkID,2)
!            do i=dist%lower_idx(blkID,1), dist%upper_idx(blkID,1)
!                !call data_set(lhs, lclBlk, (/ i, j /), real(i) * 10.0 + real(j))
!                call data_set(lhs, lclBlk, (/ i, j /), 1.0)
!            end do
!        end do
!    end do

!    end if
!    call MPI_BARRIER(MPI_COMM_WORLD, ierr)
!    end do
!    call MPI_BARRIER(MPI_COMM_WORLD, ierr)

!    !open(unit=f, file='input.dat')
!    !call data_input(lhs, f)
!    !call data_input(rhs, f)
!    !close(unit=f)

!    call data_update_halo(lhs)
!    res = data_applyGrid(lhs)

!    if(mpiRank == 0) print *, "LHS:"
!    call data_print(lhs)
!!    !print *, "RHS:"
!!    !call data_print(rhs)
!    if(mpiRank == 0) print *, "RES:"
!    call data_print(res)
!    !print *, "SUM:", data_sum(res)

!    ! -----------------------------------------------------------------------
!
!    A = 0.0
!    A(1:N) = 3.0
!    
!    print *, "Original Matrix: "
!    call printMatrix(A, N)
!    
!    ! TODO: Pass dist
!    !call grid_apply(g, A)
!    
!    print *, " "
!    print *, "After applying stencil: "
!    call printMatrix(A, N)

    call MPI_BARRIER(MPI_COMM_WORLD, ierr)
    call MPI_FINALIZE()
end program Test
