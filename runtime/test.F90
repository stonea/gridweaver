program Test
    use mod_string
    use mod_io
    use mod_region
    use mod_linkedList
    use mod_data
    include 'environment.h'
    implicit none
    include 'mpif.h'

    type(String) :: filename, gridname
    type(File)   :: f
    integer      :: i, mpierr, ierr
    type(String) :: grid_ID, sched_ID, dist_ID
    type(Data)  :: gridData, data_mod

    type(Distribution), pointer :: ds
    type(Schedule), pointer :: sched

    type(SubGrid), pointer :: sg
    type(Grid), pointer :: gr

    type(VecVecInt)    :: vec
    type(VecVecRegion) :: vecR

    integer, allocatable :: A0(:,:), AN(:,:), AE(:,:), ANE(:,:)
    real*8 :: time_start, time_stop

    integer :: lbid, blkXOffset, blkYOffset, blkJ, blkI, j
    integer :: N, lclBlkSz, ib, ie, jb, je, rep


    ! Initialize MPI and GridLib
    call MPI_INIT(mpierr)
    call environment_init()

    ! Calculate block size for local data
    N = 3600
    lclBlkSz = N / numRanks()
    ib = 1; ie = N
    jb = lclBlkSz * numRanks() + 1
    je = jb + lclBlkSz - 1

    ! Allocate storage for stencil and edge values
    allocate(A0(0:N+1, 0:N+1))
    allocate(AN(0:N+1, 0:N+1))
    allocate(AE(0:N+1, 0:N+1))
    allocate(ANE(0:N+1, 0:N+1))

    ! Fill data with random values
    do j=1,N
        do i=1,N
            AN(i,j)  = int(rand(0)*128)+1
            AE(i,j)  = int(rand(0)*128)+1
            ANE(i,j) = int(rand(0)*128)+1
        end do
    end do

    ! --- [ Construct grid ] ---
    !call subgrid_new(sg, 12, 12)
    call subgrid_new(sg, 3600, 3600)
    call grid_new(gr)
    call grid_placeAdjacentWE(gr, sg, sg, 0)

    ! --- [ Create distribution and data objects ] ---
    allocate(ds)
    call distribution_new_fill_block(ds, gr)
    call distribution_print(ds, 6)
    allocate(sched)
    call schedule_new(sched, ds)

    call schedule_print(sched, 6)

    !call environment_print(6)

    call data_new(gridData, gr, sched, ds)
    call data_new(data_mod, gr, sched, ds)

    !call data_print(gridData, 6)

    !do i=0,numRanks()-1
    !    call data_printForProc(gridData, i, 6)
    !    if(myRank() == 0) &
    !        print *, ""
    !end do

    call data_forceUpdate(gridData)

!    if(myRank() == 0) then
!        print *, "******************************************"
!        print *, "After updating the halo:"
!        print *, "******************************************"
!    endif
!    do i=0,numRanks()-1
!        call data_printForProc(gridData, i, 6)
!        if(myRank() == 0) &
!            print *, ""
!    end do

    ! Apply a summing stencil
    time_start = MPI_WTIME()
    do rep=1,100
        call data_forceUpdate(gridData)
!        data_mod = data_apply(gridData, sumNeighbors)

        do lbid=lbound(gridData%vals,3), ubound(gridData%vals,3)
            blkXOffset = distribution_block_x1( &
                gridData%dist, gridData%dist%lbid2gbid(lbid)) -1
            blkYOffset = distribution_block_y1( &
                gridData%dist, gridData%dist%lbid2gbid(lbid)) -1
            
            j = blkYOffset
            do blkJ=1,gridData%dist%blockSize(2)
                i = blkXOffset
                do blkI=1,gridData%dist%blockSize(1)
                    !data_apply%vals(blkI, blkJ, lbid) = &
                    !    func(AFunc,                     &
                    !         blkI + blkXOffset,         &
                    !         blkJ + blkYOffset)

                    !gridData%vals(blkI, blkJ, lbid) = &
                    !    gridData%vals(blkI, blkJ, lbid) + &
                    !    gridData%vals(blkI-1, blkJ, lbid) + &
                    !    gridData%vals(blkI+1, blkJ, lbid) + &
                    !    gridData%vals(blkI, blkJ-1, lbid) + &
                    !    gridData%vals(blkI, blkJ+1, lbid)

                    gridData%vals(blkI, blkJ, lbid) = &
                        A0 (i  , j  ) * gridData%vals(blkI  , blkJ,   lbid) + &
                        AN (i  , j  ) * gridData%vals(blkI  , blkJ+1, lbid) + &
                        AN (i  , j-1) * gridData%vals(blkI  , blkJ-1, lbid) + &
                        AE (i  , j  ) * gridData%vals(blkI+1, blkJ  , lbid) + &
                        AE (i-1, j  ) * gridData%vals(blkI-1, blkJ  , lbid) + &
                        ANE(i  , j  ) * gridData%vals(blkI+1, blkJ+1, lbid) + &
                        ANE(i  , j-1) * gridData%vals(blkI+1, blkJ-1, lbid) + &
                        ANE(i-1, j  ) * gridData%vals(blkI-1, blkJ+1, lbid) + &
                        ANE(i-1, j-1) * gridData%vals(blkI-1, blkJ-1, lbid)
                    i = i + 1
                end do

                j = j + 1
            end do
        end do
       

    end do
    time_stop = MPI_WTIME()
    if(myRank() == 0) then
        write(*, '(A,F0.2)'), "total time (secs): ", time_stop - time_start
    endif

    !if(myRank() == 0) then
    !    print *, "******************************************"
    !    print *, "After applying stencil:"
    !    print *, "******************************************"
    !endif
!    call data_print(data_mod, 6)


    !call environment_print(6)
    ! Read metadata from input-file
    !call string_init(filename, "input.dat")
    !call file_open(f, filename)
    !
    !call environment_input(f)
    !
    !call environment_print(6)   
    !
    !call file_close(f)


!    ! Create a data object and print it out
!    call string_init(grid_ID, "g")
!    call string_init(sched_ID, "sched")
!    call string_init(dist_ID, "blockedDist")
!
!    call data_new(gridData,                             &
!                  environment_getGrid(grid_ID),         &
!                  environment_getSchedule(sched_ID),    &
!                  environment_getDistribution(dist_ID))
!    call data_print(gridData, 6)
!
!    do i=0,numRanks()-1
!        call data_printForProc(gridData, i, 6)
!        if(myRank() == 0) &
!            print *, ""
!    end do
!
!    call MPI_BARRIER(MPI_COMM_WORLD, ierr)
!
!    ! Update the data's halo and print it out again
!    call data_forceUpdate(gridData)
!    if(myRank() == 0) then
!        print *, "******************************************"
!        print *, "After updating the halo:"
!        print *, "******************************************"
!    endif
!    do i=0,numRanks()-1
!        call data_printForProc(gridData, i, 6)
!        if(myRank() == 0) &
!            print *, ""
!    end do
!
!    ! Apply a summing stencil
!!    data_mod = data_apply(gridData, sumNeighbors)
!!
!!    if(myRank() == 0) then
!!        print *, "******************************************"
!!        print *, "After applying a summing stencil: "
!!        print *, "******************************************"
!!    end if
!!
!!    call data_print(data_mod, 6)
!
!    ! Exit
    call MPI_BARRIER(MPI_COMM_WORLD, mpierr)
    call MPI_FINALIZE(mpierr)

  contains
    !integer function sumNeighbors(neighs)
    !    integer, intent(in) :: neighs(:)
        !sumNeighbors = neighs(1) + neighs(2) + neighs(3) + neighs(4)
    !end function

    integer function sumNeighbors(A, i , j)
        integer, intent(in) :: i, j
        interface
            integer function A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface

        sumNeighbors = &
            (A(i, j) + A(i-1, j) + A(i+1, j) + A(i, j-1) + A(i, j+1))
    end function


    !subroutine gen()
    !    ! Iterate over all points in all local blocks
    !    do lbid=lbound(self%vals,3), ubound(self%vals,3)
    !        do y=1, self%dist%blockSize(2)
    !            do x=1, self%dist%blockSize(2)
    !                data_apply%vals(x,y,lbid) = neighs(1) + neighs(2) + neighs(3) + neighs(4)
    !            end do
    !        end do
    !    end do
    !end subroutine
end program Test

