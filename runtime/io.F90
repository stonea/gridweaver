!***
!* This module is used to read data from files.  It links against a series of
!* C routines inorder to perform I/O.
!*
!* I/O functions are passed a read mode that specifies whether the data should
!* be globally replicated, only read by the master, or distributed.
!***
module mod_io
    use mod_string
    use mod_utils
    use mod_distributedVec
    use mod_integerSet
    use mod_region
    use iso_c_binding
    implicit none

    integer, parameter  :: MODE_MASTER      = 1
    integer, parameter  :: MODE_GLOBAL      = 2
    integer, parameter  :: MODE_DISTRIBUTED = 3

    public :: file_open,             &
              file_close,            &
                                    
              file_readInt,          &
              file_readString,       &
              file_readRegion,       &

              file_readIntSet,       &

              file_readIntVec,       &
              file_readIntVecVec,    &
                                    
              file_readRegionVec,    &
              file_readRegionVecVec, &

              print_procColor,       &
              print_color,           &
              print_underline,       &
              print_bold,            &
              print_invertColor,     &
              print_resetColor

    !**
    ! Object that represents a file
    !**
    type File
        integer(c_size_t) :: unit
    end type


    !**
    ! Interfaces to C I/O functions (these functions are used internally by
    ! this module).
    !**
    interface
        !**
        ! Open a file, store handle to the file in unit.
        !**
        subroutine cio_fopen(filename, unit) bind(C, name="__cio_fopen")
            use iso_c_binding
            character(kind=c_char) :: filename(*)
            integer(c_size_t), intent(inout) :: unit
        end subroutine

        !**
        ! Close a file that is already open.
        !**
        subroutine cio_fclose(unit) bind(C, name="__cio_fclose")
            use iso_c_binding
            integer(c_size_t), intent(inout) :: unit
        end subroutine


        !**
        ! Read an integer from an open file.
        !**
        subroutine cio_readInt(unit, val) bind(C, name="__cio_readInt")
            use iso_c_binding
            integer(c_size_t), intent(in) :: unit
            integer(c_size_t), intent(inout) :: val
        end subroutine

        !**
        ! Read a C String from an open file.
        !**
        subroutine cio_readString(unit, val, read) &
          bind(C, name="__cio_readString")
            use iso_c_binding
            integer(c_size_t), intent(in) :: unit
            character(kind=c_char) :: val(*)
            integer(c_size_t), intent(inout) :: read
        end subroutine

        subroutine cio_setColor(color) &
          bind(C, name="__cio_setColor")
            use iso_c_binding
            integer(c_size_t), intent(in) :: color
        end subroutine

    end interface
  contains

! *****************************************************************************
    subroutine file_open(self, filename)
        type(File), intent(inout) :: self
        type(String), intent(in)  :: filename

        ! Have master rank open the file
        if(myRank() == 0) then
            call cio_fopen(cstr(filename), self%unit)
        end if
    end subroutine

    subroutine file_close(self)
        include 'mpif.h'
        type(File), intent(inout) :: self
        integer :: ierr

        ! Have master rank close the file
        if(myRank() == 0) then
            call cio_fclose(self%unit)
        end if
    end subroutine


    recursive subroutine file_readInt(self, val, mode)
        include 'mpif.h'
        type(File), intent(in)        :: self
        integer, intent(inout)        :: val
        integer, optional, intent(in) :: mode
        integer :: m, ierr
        integer(c_size_t) :: valFromC

        ! Determine what read-mode to use
        if(present(mode)) then
            m = mode
        else
            m = MODE_MASTER
        endif

        ! Read integer based on selected read-mode
        select case (m)
            case(MODE_MASTER)
                if(myRank() == 0) then
                    call cio_readInt(self%unit, valFromC)
                    val = valFromC
                end if
            case(MODE_GLOBAL)
                call file_readInt(self, val, MODE_MASTER)
                call MPI_BCAST(val, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
            case(MODE_DISTRIBUTED)
                print *, "MODE_DISTRIBUTED not implemented for file_readInt"
                call errExit()
        end select
    end subroutine


    recursive subroutine file_readString(self, val, mode)
        type(File), intent(in)      :: self
        type(String), intent(inout) :: val
        integer, optional, intent(in) :: mode
        integer(c_size_t) :: nRead
        integer :: m

        ! Determine what read-mode to use
        if(present(mode)) then
            m = mode
        else
            m = MODE_MASTER
        endif

        ! Read integer based on selected read-mode
        select case (m)
            case(MODE_MASTER)
                if(myRank() == 0) then
                    call cio_readString(self%unit, val%val, nRead)
                    val%len = nRead
                end if
            case(MODE_GLOBAL)
                call file_readString(self, val, MODE_MASTER)
                call string_bcast(val)
            case(MODE_DISTRIBUTED)
                print *, "MODE_DISTRIBUTED not implemented for file_readString"
                call errExit()
        end select
    end subroutine


    subroutine file_readRegion(self, r, mode)
        type(File), intent(in)        :: self
        Type(Region), intent(inout)   :: r
        integer, optional, intent(in) :: mode
        integer :: m, isEmpty

        ! Determine what read-mode to use
        if(present(mode)) then
            m = mode
        else
            m = MODE_MASTER
        endif

        ! Read integer based on selected read-mode
        select case (m)
            case(MODE_MASTER)
                if(myRank() == 0) then
                    call file_readInt(self, r%X1, MODE_MASTER)
                    call file_readInt(self, r%Y1, MODE_MASTER)
                    call file_readInt(self, r%X2, MODE_MASTER)
                    call file_readInt(self, r%Y2, MODE_MASTER)
                    call file_readInt(self, r%empty, MODE_MASTER)
                end if
            case(MODE_GLOBAL)
                print *, "MODE_GLOBAL not implemented for file_readRegion"
                call errExit()
            case(MODE_DISTRIBUTED)
                print *, "MODE_DISTRIBUTED not implemented for file_readRegion"
                call errExit()
        end select
    end subroutine

    recursive subroutine file_readIntSet(self, vec, mode)
        type(File), intent(in)       :: self
        type(IntegerSet), intent(inout) :: vec
        integer, optional, intent(in) :: mode
        integer :: m
        integer, allocatable :: vecCopy(:)

        ! Determine what read-mode to use
        if(present(mode)) then
            m = mode
        else
            m = MODE_MASTER
        endif

        call file_readIntVec(self, vecCopy, mode)

        select case (m)
            case(MODE_MASTER)
                if(myRank() == 0) then
                    vec%nIntegers = size(vecCopy)
                    vec%integers(1:vec%nIntegers) = vecCopy
                    deallocate(vecCopy)
                endif
            case(MODE_GLOBAL)
                vec%nIntegers = size(vecCopy)
                vec%integers(1:vec%nIntegers) = vecCopy
                deallocate(vecCopy)
            case(MODE_DISTRIBUTED)
                vec%nIntegers = size(vecCopy)
                vec%integers(1:vec%nIntegers) = vecCopy
                deallocate(vecCopy)
        end select
    end subroutine


    recursive subroutine file_readIntVec(self, vec, mode)
        include 'mpif.h'
        type(File), intent(in)              :: self
        integer, allocatable, intent(inout) :: vec(:)
        integer, optional, intent(in) :: mode
        integer, allocatable :: tmpVec(:)
        integer :: nVecs, sz, i, j, m
        integer :: reqSz, reqVec, ierr, stat(MPI_STATUS_SIZE)

        ! Determine what read-mode to use
        if(present(mode)) then
            m = mode
        else
            m = MODE_MASTER
        endif

        ! Read vector based on selected read-mode
        select case (m)
            case(MODE_MASTER)
                if(myRank() == 0) then
                    ! Read size of vector
                    call file_readInt(self, sz)
                    allocate(vec(sz))

                    ! Read vector elements
                    do j=1,sz
                        call file_readInt(self, vec(j))
                    end do
                end if
            case(MODE_GLOBAL)
                call file_readIntVec(self, vec, MODE_MASTER)
                sz = size(vec)
                call MPI_BCAST(sz, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
                if(myRank() /= 0) allocate(vec(sz))
                call MPI_BCAST(vec, sz, MPI_INT, 0, MPI_COMM_WORLD, ierr)
            case(MODE_DISTRIBUTED)
                if(myRank() == 0) then
                    ! Read number of vectors
                    call file_readInt(self, nVecs)
                    if(nVecs /= numRanks()) then
                        print *, "Distributed vector expects", nVecs, "ranks"
                        call errExit()
                    end if

                    ! Read vectors
                    do i=0,nVecs-1
                        call file_readIntVec(self, tmpVec, MODE_MASTER)
                        sz = size(tmpVec, 1)

                        ! Send vector to appropriate rank
                        if(i == 0) then
                            allocate(vec(sz))
                            vec = tmpVec
                        else
                            call MPI_SEND( &
                                sz, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                            call MPI_BSEND( &
                                tmpVec, sz, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                        end if
                        deallocate(tmpVec)
                    end do
                end if

                ! Post receives for all ranks that are not the master
                if(myRank() /= 0) then
                    call MPI_RECV( &
                        sz, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    allocate(vec(sz))
                    call MPI_RECV( &
                        vec, sz, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                end if
        end select
    end subroutine

    !**
    !* Read a vector of vectors.  In this Fortran library we represent this
    !* as two arrays.  An offsets array (specifying where each vector begins),
    !* and a values array.
    !**
    subroutine file_readIntVecVec(self, vv, mode)
        include 'mpif.h'
        type(File), intent(in)         :: self
        type(VecVecInt), intent(inout) :: vv
        integer, optional, intent(in)  :: mode
        integer, allocatable :: vectorsIOffsets(:), vectorJ(:), vectorsI(:), tmp(:)
        integer :: nVecs, sz, totalSz, i, j, m, last
        integer :: req, reqVec, ierr, stat(MPI_STATUS_SIZE)

        ! Determine what read-mode to use
        if(present(mode)) then
            m = mode
        else
            m = MODE_MASTER
        endif

        ! Read vector based on selected read-mode
        select case (m)
            case(MODE_MASTER)
                print *, "MODE_MASTER not implemented for file_readIntVecVec"
                call errExit()
            case(MODE_GLOBAL)
                print *, "MODE_GLOBAL not implemented for file_readIntVecVec"
                call errExit()
            case(MODE_DISTRIBUTED)
                if(myRank() == 0) then
                    ! Read number of ranks to distribute vectors to
                    call file_readInt(self, nVecs)
                    if(nVecs /= numRanks()) then
                        print *, "Distributed vector expects", nVecs, "ranks"
                        call errExit()
                    end if

                    ! Read vectors
                    do i=0,numRanks()-1
                        ! Read number of internal vectors for rank i
                        call file_readInt(self, nVecs)
                        allocate(vectorsIOffsets(nVecs+1))
                        allocate(vectorsI(0))

                        ! Read internal vector data into temporary arrays
                        last = 1
                        do j=1,nVecs
                            ! Vector j starts at last in tmpValues
                            vectorsIOffsets(j) = last
                            
                            ! Read vector j, then add it to vectorsI
                            call file_readIntVec(self, vectorJ, MODE_MASTER)
                            sz = size(vectorJ, 1)
                            allocate(tmp(last + sz - 1))
                            tmp = reshape(vectorsI, (/ last + sz - 1 /), vectorJ)
                            deallocate(vectorsI)
                            allocate(vectorsI(size(tmp)))
                            vectorsI = tmp
                            deallocate(tmp)
                            deallocate(vectorJ)

                            last = last + sz
                        end do
                        vectorsIOffsets(nVecs+1) = last
                        totalSz = last

                        ! Send values read in from file to rank i
                        if(i == 0) then
                            vv%offsets = vectorsIOffsets
                            vv%size = nVecs
                            vv%values  = vectorsI
                        else
                            ! Send offsets array
                            call MPI_SEND( &
                                nVecs, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                            call MPI_BSEND( &
                                vectorsIOffsets, nVecs+1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)

                            ! Send values array
                            call MPI_SEND( &
                                totalSz, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                            call MPI_BSEND( &
                                vectorsI, totalSz, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                        end if
                        
                        ! Cleanup temporary arrays
                        deallocate(vectorsI)
                        deallocate(vectorsIOffsets)
                    end do
                end if

                ! Post receives for all ranks that are not the master
                if(myRank() /= 0) then
                    ! Receive offsets array
                    call MPI_RECV( &
                        vv%size, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    allocate(vv%offsets(vv%size+1))
                    call MPI_RECV( &
                        vv%offsets, vv%size+1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    
                    ! Receive values array
                    call MPI_RECV( &
                        sz, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    allocate(vv%values(sz))
                    call MPI_RECV( &
                        vv%values, sz, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                end if
        end select
    end subroutine


    recursive subroutine file_readRegionVec(self, vec, mode)
        include 'mpif.h'
        type(File), intent(in)              :: self
        type(Region), allocatable, intent(inout) :: vec(:)
        integer, optional, intent(in) :: mode
        type(Region), allocatable :: tmpVec(:)
        integer :: nVecs, sz, i, j, m
        integer :: reqSz, reqVec, ierr, isEmpty, stat(MPI_STATUS_SIZE)

        ! Determine what read-mode to use
        if(present(mode)) then
            m = mode
        else
            m = MODE_MASTER
        endif

        ! Read vector based on selected read-mode
        select case (m)
            case(MODE_MASTER)
                if(myRank() == 0) then
                    ! Read size of vector
                    call file_readInt(self, sz)
                    allocate(vec(sz))

                    ! Read vector elements
                    do j=1,sz
                        call file_readRegion(self, vec(j))
                    end do
                end if
            case(MODE_GLOBAL)
                call file_readRegionVec(self, vec, MODE_MASTER)
                call regionVec_bcast(vec)
            case(MODE_DISTRIBUTED)
                if(myRank() == 0) then
                    ! Read number of vectors
                    call file_readInt(self, nVecs)
                    if(nVecs /= numRanks()) then
                        print *, "Distributed vector expects", nVecs, "ranks"
                        call errExit()
                    end if

                    ! Read vectors
                    do i=0,nVecs-1
                        call file_readRegionVec(self, tmpVec, MODE_MASTER)
                        sz = size(tmpVec, 1)

                        ! Send vector to appropriate rank
                        if(i == 0) then
                            allocate(vec(sz))
                            vec = tmpVec
                        else
                            call MPI_SEND( &
                                sz, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                            do j=1,sz
                                call MPI_SEND( &
                                    tmpVec(j)%X1, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                                call MPI_SEND( &
                                    tmpVec(j)%Y1, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                                call MPI_SEND( &
                                    tmpVec(j)%X2, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                                call MPI_SEND( &
                                    tmpVec(j)%Y2, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                                call MPI_SEND( &
                                    tmpVec(j)%empty, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                            end do
                        end if
                        deallocate(tmpVec)
                    end do
                end if

                ! Post receives for all ranks that are not the master
                if(myRank() /= 0) then
                    call MPI_RECV( &
                        sz, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    allocate(vec(sz))
                    do j=1,sz
                        call MPI_RECV( &
                            vec(j)%X1, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                        call MPI_RECV( &
                            vec(j)%Y1, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                        call MPI_RECV( &
                            vec(j)%X2, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                        call MPI_RECV( &
                            vec(j)%Y2, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                        call MPI_RECV( &
                            vec(j)%empty, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    end do
                end if
        end select
    end subroutine



    subroutine file_readRegionVecVec(self, vv, mode)
        include 'mpif.h'
        type(File), intent(in)         :: self
        type(VecVecRegion), intent(inout) :: vv
        integer, optional, intent(in)  :: mode
        integer, allocatable :: vectorsIOffsets(:)
        type(Region), allocatable :: vectorJ(:), vectorsI(:), tmp(:)
        integer :: nVecs, sz, totalSz, i, j, m, last
        integer :: req, reqVec, ierr, stat(MPI_STATUS_SIZE)

        ! Determine what read-mode to use
        if(present(mode)) then
            m = mode
        else
            m = MODE_MASTER
        endif

        ! Read vector based on selected read-mode
        select case (m)
            case(MODE_MASTER)
                print *, "MODE_MASTER not implemented for file_readRegionVecVec"
                call errExit()
            case(MODE_GLOBAL)
                print *, "MODE_GLOBAL not implemented for file_readRegionVecVec"
                call errExit()
            case(MODE_DISTRIBUTED)
                if(myRank() == 0) then
                    ! Read number of ranks to distribute vectors to
                    call file_readInt(self, nVecs)
                    if(nVecs /= numRanks()) then
                        print *, "Distributed vector expects", nVecs, "ranks"
                        call errExit()
                    end if

                    ! Read vectors
                    do i=0,numRanks()-1
                        ! Read number of internal vectors for rank i
                        call file_readInt(self, nVecs)

                        allocate(vectorsIOffsets(nVecs+1))
                        allocate(vectorsI(0))

                        ! Read internal vector data into temporary arrays
                        last = 1
                        do j=1,nVecs
                            ! Vector j starts at last in tmpValues
                            vectorsIOffsets(j) = last
                            
                            ! Read vector j, then add it to vectorsI
                            call file_readRegionVec(self, vectorJ, MODE_MASTER)
                            
                            sz = size(vectorJ, 1)
                            allocate(tmp(last + sz - 1))
                            tmp = reshape(vectorsI, (/ last + sz - 1 /), vectorJ)
                            deallocate(vectorsI)
                            allocate(vectorsI(size(tmp)))
                            vectorsI = tmp
                            deallocate(tmp)
                            deallocate(vectorJ)

                            last = last + sz
                        end do
                        vectorsIOffsets(nVecs+1) = last
                        totalSz = last

                        ! Send values read in from file to rank i
                        if(i == 0) then
                            vv%offsets = vectorsIOffsets
                            vv%size = nVecs
                            vv%values  = vectorsI
                        else
                            ! Send offsets array
                            call MPI_SEND( &
                                nVecs, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                            call MPI_BSEND( &
                                vectorsIOffsets, nVecs+1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)

                            ! Send values array
                            call MPI_SEND( &
                                totalSz, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                            do j=1,totalSz
                                call MPI_SEND( &
                                    vectorsI(j)%X1, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                                call MPI_SEND( &
                                    vectorsI(j)%Y1, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                                call MPI_SEND( &
                                    vectorsI(j)%X2, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                                call MPI_SEND( &
                                    vectorsI(j)%Y2, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                                call MPI_SEND( &
                                    vectorsI(j)%empty, 1, MPI_INT, i, 0, MPI_COMM_WORLD, ierr)
                            end do
                        end if
                        
                        ! Cleanup temporary arrays
                        deallocate(vectorsI)
                        deallocate(vectorsIOffsets)
                    end do
                end if

                ! Post receives for all ranks that are not the master
                if(myRank() /= 0) then
                    ! Receive offsets array
                    call MPI_RECV( &
                        vv%size, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    allocate(vv%offsets(vv%size+1))
                    call MPI_RECV( &
                        vv%offsets, vv%size+1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    
                    ! Receive values array
                    call MPI_RECV( &
                        sz, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    allocate(vv%values(sz))
                    do j=1,sz
                        call MPI_RECV( &
                            vv%values(j)%X1, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                        call MPI_RECV( &
                            vv%values(j)%Y1, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                        call MPI_RECV( &
                            vv%values(j)%X2, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                        call MPI_RECV( &
                            vv%values(j)%Y2, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                        call MPI_RECV( &
                            vv%values(j)%empty, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, stat, ierr)
                    end do
                end if
        end select

    end subroutine

    subroutine print_procColor(proc)
        integer(c_size_t) :: color
        integer :: proc
        color = proc + 1
        call cio_setColor(color)
    end subroutine

    subroutine print_color(color)
        integer, intent(in) :: color
        integer(c_size_t) :: ccolor
        ccolor = color
        call cio_setColor(ccolor)
    end subroutine

    subroutine print_resetColor()
        integer(c_size_t) :: color
        color = 0
        call cio_setColor(color)
    end subroutine

    subroutine print_underline()
        integer(c_size_t) :: color
        color = -1
        call cio_setColor(color)
    end subroutine

    subroutine print_bold()
        integer(c_size_t) :: color
        color = -2
        call cio_setColor(color)
    end subroutine

    subroutine print_invertColor()
        integer(c_size_t) :: color
        color = -3
        call cio_setColor(color)
    end subroutine
end module
