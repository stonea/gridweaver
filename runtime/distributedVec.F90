module mod_distributedVec
    use mod_utils
    use mod_region
    implicit none

    type VecVecInt
        integer :: size
        integer, allocatable :: offsets(:)
        integer, allocatable :: values(:)
    end type

    type VecVecRegion
        integer :: size
        integer, allocatable :: offsets(:)
        type(Region), allocatable :: values(:)
    end type


    public :: distIntVec_print,         &
              distIntVecVec_print,      &
              distIntVecVec_numVecs,    &

              distRegionVec_print,      &
              distRegionVec_printN,     &
              distRegionVecVec_print,   &
              distRegionVecVec_numVecs

    public :: vecVecInt_new,      &
              vecVecInt_pushInto, &
              vecVecInt_addVec,   &

              vecVecRegion_new,      &
              vecVecRegion_pushInto, &
              vecVecRegion_addVec
  contains

    !**
    !  Print values in a distributed vector of integers
    !**
    subroutine distIntVec_print(vec, rank, out)
        include 'mpif.h'
        integer, intent(in) :: vec(:)
        integer, intent(in) :: rank, out

        integer :: sz, reqSz, req, ierr, stat(MPI_STATUS_SIZE), i
        integer, allocatable :: copy(:)

        ! Post send of vector on specified rank
        if(myRank() == rank) then
            sz = ubound(vec, 1) - lbound(vec, 1) + 1
            call MPI_ISEND(sz, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, reqSz, ierr)
            call MPI_ISEND(vec, sz, MPI_INT, 0, 0, MPI_COMM_WORLD, req, ierr)
        end if
        
        ! Have master rank retreive sent vector and print it
        if(myRank() == 0) then
            call MPI_RECV(sz, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            allocate(copy(sz))
            call MPI_RECV(copy, sz, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            
            write(out, '(A)', advance='no') "["
            do i=1,sz
                if(i /= 1) write(out, '(A)', advance='no') ", "
                write(out, '(I2)', advance='no') copy(i)
            end do
            write(out, '(A)', advance='no') "]"

            deallocate(copy)
        end if

        if(myRank() == rank) then 
            call MPI_Wait(req, MPI_STATUS_IGNORE, ierr)
        end if
    end subroutine

    !**
    !  Print values in a distributed vector-of-vector-of-integers
    !**
    subroutine distIntVecVec_print(vv, rank, i, out)
        include 'mpif.h'
        type(VecVecInt), intent(in) :: vv
        integer, intent(in) :: rank, i, out
        
        integer :: sz, reqSz, req, ierr, stat(MPI_STATUS_SIZE), j
        integer, allocatable :: copy(:)

        ! Post send of vector on specified rank
        if(myRank() == rank) then
            ! If vector i not in the vecvec state that the size is 0
            if(i > vv%size) then
                sz =0
            else
                sz = vv%offsets(i+1) - vv%offsets(i)
            end if
            call MPI_ISEND(sz, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, reqSz, ierr)
            call MPI_ISEND(vv%values(vv%offsets(i):vv%offsets(i+1)-1), &
                sz, MPI_INT, 0, 0, MPI_COMM_WORLD, req, ierr)
        end if
        
        ! Have master rank retreive sent vector and print it
        if(myRank() == 0) then
            call MPI_RECV(sz, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            allocate(copy(sz))
            call MPI_RECV(copy, sz, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            
            write(out, '(A)', advance='no') "["
            do j=1,sz
                if(j /= 1) write(out, '(A)', advance='no') ", "
                write(out, '(I0)', advance='no') copy(j)
            end do
            write(out, '(A)', advance='no') "]"

            deallocate(copy)
        end if

        if(myRank() == rank) then 
            call MPI_Wait(req, MPI_STATUS_IGNORE, ierr)
        end if
    end subroutine

    !**
    !* Returns the number of vectors a given rank owns of a
    !* vector-of-vectors-of-integers.  Value is only returned
    !* on master rank, all other ranks return -1.
    !**
    integer function distIntVecVec_numVecs(vv, rank) result(res)
        include 'mpif.h'
        type(VecVecInt), intent(in) :: vv
        integer, intent(in) :: rank
        integer :: sz, ierr

        ! Post broadcast
        sz = vv%size
        call MPI_Bcast(sz, 1, MPI_INT, rank, MPI_COMM_WORLD, ierr)
        res = sz
    end function


    subroutine distRegionVec_print(vec, rank, out)
        include 'mpif.h'
        type(Region), intent(in) :: vec(:)
        integer, allocatable :: vec_data(:), vec_data_in(:)
        integer, intent(in) :: rank, out

        integer :: sz, reqSz, req, ierr, stat(MPI_STATUS_SIZE), i
        type(Region), allocatable :: copy(:)

        ! Post send of vector on specified rank
        if(myRank() == rank) then
            vec_data = transfer(vec, vec_data)
            sz = size(vec_data)
            call MPI_ISEND(sz, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, reqSz, ierr)
            call MPI_ISEND(vec_data, sz, MPI_INT, 0, 0, MPI_COMM_WORLD, req, ierr)
        end if
        
        ! Have master rank retreive sent vector and print it
        if(myRank() == 0) then
            call MPI_RECV(sz, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            allocate(vec_data_in(sz))
            call MPI_RECV(vec_data_in, sz, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            copy = transfer(vec_data_in, copy)
            
            write(out, '(A)', advance='no') "["
            do i=1,size(copy)
                if(i /= 1) write(out, '(A)', advance='no') ", "
                call region_printSimp(copy(i), out)
            end do
            write(out, '(A)', advance='no') "]"

            deallocate(copy)
        end if

        if(myRank() == rank) then 
            call MPI_Wait(req, MPI_STATUS_IGNORE, ierr)
        end if

        if(allocated(vec_data)) deallocate(vec_data)
        if(allocated(vec_data_in)) deallocate(vec_data_in)
    end subroutine


    subroutine distRegionVec_printN(vec, rank, out, n)
        include 'mpif.h'
        type(Region), intent(in) :: vec(:)
        integer, allocatable :: vec_data(:), vec_data_in(:)
        integer, intent(in) :: rank, out, n

        integer :: sz, reqSz, req, ierr, stat(MPI_STATUS_SIZE), i
        type(Region), allocatable :: copy(:)

        ! Post send of vector on specified rank
        if(myRank() == rank) then
            vec_data = transfer(vec(1:n), vec_data)
            sz = size(vec_data)
            call MPI_ISEND(sz, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, reqSz, ierr)
            call MPI_ISEND(vec_data, sz, MPI_INT, 0, 0, MPI_COMM_WORLD, req, ierr)
        end if
        
        ! Have master rank retreive sent vector and print it
        if(myRank() == 0) then
            call MPI_RECV(sz, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            allocate(vec_data_in(sz))
            call MPI_RECV(vec_data_in, sz, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            copy = transfer(vec_data_in, copy)
            
            write(out, '(A)', advance='no') "["
            do i=1,size(copy)
                if(i /= 1) write(out, '(A)', advance='no') ", "
                call region_printSimp(copy(i), out)
            end do
            write(out, '(A)', advance='no') "]"

            deallocate(copy)
        end if

        if(myRank() == rank) then 
            call MPI_Wait(req, MPI_STATUS_IGNORE, ierr)
        end if

        if(allocated(vec_data)) deallocate(vec_data)
        if(allocated(vec_data_in)) deallocate(vec_data_in)
    end subroutine

    subroutine distRegionVecVec_print(vv, rank, i, out)
        include 'mpif.h'
        type(VecVecRegion), intent(in) :: vv
        integer, intent(in) :: rank, i, out
        
        integer :: sz, reqSz, req, ierr, stat(MPI_STATUS_SIZE), j
        type(Region), allocatable :: copy(:)

        ! Post send of vector on specified rank
        if(myRank() == rank) then
            sz = vv%offsets(i+1) - vv%offsets(i)
            call MPI_ISEND(sz, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, reqSz, ierr)
            do j=vv%offsets(i), vv%offsets(i+1)-1
                call MPI_ISEND( &
                    vv%values(j)%X1, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, ierr)
                call MPI_ISEND( &
                    vv%values(j)%Y1, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, ierr)
                call MPI_ISEND( &
                    vv%values(j)%X2, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, ierr)
                call MPI_ISEND( &
                    vv%values(j)%Y2, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, ierr)
                call MPI_ISEND( &
                    vv%values(j)%empty, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, ierr)
            end do
        end if
        
        ! Have master rank retreive sent vector and print it
        if(myRank() == 0) then
            call MPI_RECV(sz, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            allocate(copy(sz))
            do j=1,sz
                call MPI_RECV( &
                    copy(j)%X1, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
                call MPI_RECV( &
                    copy(j)%Y1, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
                call MPI_RECV( &
                    copy(j)%X2, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
                call MPI_RECV( &
                    copy(j)%Y2, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
                call MPI_RECV( &
                    copy(j)%empty, 1, MPI_INT, rank, 0, MPI_COMM_WORLD, stat, ierr)
            end do
            
            write(out, '(A)', advance='no') "["
            do j=1,sz
                if(j /= 1) write(out, '(A)', advance='no') ", "
                call region_printSimp(copy(j), out)
                !write(out, '(I2)', advance='no') copy(j)
            end do
            write(out, '(A)', advance='no') "]"

            deallocate(copy)
        end if

        if(myRank() == rank) then 
            call MPI_Wait(req, MPI_STATUS_IGNORE, ierr)
        end if
    end subroutine


    integer function distRegionVecVec_numVecs(vv, rank) result(res)
        include 'mpif.h'
        type(VecVecRegion), intent(in) :: vv
        integer, intent(in) :: rank
        integer :: sz, ierr

        ! Post broadcast
        sz = vv%size
        call MPI_Bcast(sz, 1, MPI_INT, rank, MPI_COMM_WORLD, ierr)
        res = sz
    end function

    subroutine vecVecInt_new(self)
        type(VecVecInt), intent(inout) :: self

        self%size = 0
        allocate(self%offsets(1))
        self%offsets(1) = 1
        allocate(self%values(0))
    end subroutine

    ! Push val into the i-th vector
    subroutine vecVecInt_pushInto(self, i, val)
        type(VecVecInt), intent(inout) :: self
        integer, intent(in) :: i, val
        integer :: idx, x
        integer, allocatable :: valuesCopy(:)

        ! If the top-level vector does not already exist allocated room
        ! for it
        do while(i > self%size)
            call vecVecInt_addVec(self)
        end do

        ! Determine index where the new element will be added
        if(i == self%size) then
            idx = size(self%values) + 1
        else
            idx = self%offsets(i+1)
        endif

        ! Push all values right of idx down an element
        do x=i+1,self%size+1
            self%offsets(x) = self%offsets(x)+1
        end do
        allocate(valuesCopy(size(self%values)+1))
        valuesCopy(1:idx-1) = self%values(1:idx-1)
        valuesCopy(idx+1:) = self%values(idx:)
        deallocate(self%values)
        allocate(self%values(size(valuesCopy)))
        self%values = valuesCopy
        deallocate(valuesCopy)

        ! Assign value
        self%values(idx) = val
    end subroutine


    subroutine vecVecInt_addVec(self)
        type(VecVecInt), intent(inout) :: self
        integer, allocatable :: offsetsCopy(:)
        
        ! copy offsets into a new array that's sized to include one additional
        ! element
        allocate(offsetsCopy(self%size+2))
        offsetsCopy(1:self%size+1) = self%offsets(1:self%size+1)

        ! Resize offsets array
        self%size = self%size+1
        deallocate(self%offsets)
        allocate(self%offsets(self%size+1))
        self%offsets = offsetsCopy
        deallocate(offsetsCopy)

        ! Set offset for new vector
        self%offsets(self%size+1) = size(self%values) + 1
    end subroutine


    subroutine vecVecRegion_new(self)
        type(VecVecRegion), intent(inout) :: self

        self%size = 0
        allocate(self%offsets(1))
        self%offsets(1) = 1
        allocate(self%values(0))
    end subroutine

    ! Push val into the i-th vector
    subroutine vecVecRegion_pushInto(self, i, val)
        type(VecVecRegion), intent(inout) :: self
        integer, intent(in) :: i
        type(Region) :: val
        integer :: idx, x
        type(Region), allocatable :: valuesCopy(:)

        ! If the top-level vector does not already exist allocated room
        ! for it
        do while(i > self%size)
            call vecVecRegion_addVec(self)
        end do

        ! Determine index where the new element will be added
        if(i == self%size) then
            idx = size(self%values) + 1
        else
            idx = self%offsets(i+1)
        endif

        ! Push all values right of idx down an element
        do x=i+1,self%size+1
            self%offsets(x) = self%offsets(x)+1
        end do
        allocate(valuesCopy(size(self%values)+1))
        valuesCopy(1:idx-1) = self%values(1:idx-1)
        valuesCopy(idx+1:) = self%values(idx:)
        deallocate(self%values)
        allocate(self%values(size(valuesCopy)))
        self%values = valuesCopy
        deallocate(valuesCopy)

        ! Assign value
        self%values(idx) = val
    end subroutine


    subroutine vecVecRegion_addVec(self)
        type(VecVecRegion), intent(inout) :: self
        integer, allocatable :: offsetsCopy(:)
        
        ! copy offsets into a new array that's sized to include one additional
        ! element
        allocate(offsetsCopy(self%size+2))
        offsetsCopy(1:self%size+1) = self%offsets(1:self%size+1)

        ! Resize offsets array
        self%size = self%size+1
        deallocate(self%offsets)
        allocate(self%offsets(self%size+1))
        self%offsets = offsetsCopy
        deallocate(offsetsCopy)

        ! Set offset for new vector
        self%offsets(self%size+1) = size(self%values) + 1
    end subroutine
end module
