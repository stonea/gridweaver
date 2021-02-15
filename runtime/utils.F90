module mod_utils
    use mod_String
    implicit none

    public :: printMatrix,        &
              myRank,             &
              numRanks,           &
              errExit

    integer :: gnID = 0

  contains


    integer function AT(x, y, N) result(res)
        integer, intent(in) :: x, y, N

        res = (y-1) * N + x
    end function

    subroutine printMatrix(A, n)
        real, intent(in) :: A(:)
        integer, intent(in) :: n
        integer idx_0, idx_1

        do idx_0 = 1, n
            do idx_1 = 1, n
                write (*,"(f12.3)", advance="no") A(AT(idx_0, idx_1, n))
            enddo
            print *, " "
        end do
        print *, " "
    end subroutine

    integer function myRank() result(rank)
        include 'mpif.h'
        integer ierr
        
        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    end function

    integer function numRanks() result(ranks)
        include 'mpif.h'
        integer ierr

        call MPI_COMM_SIZE(MPI_COMM_WORLD, ranks, ierr)
    end function

    subroutine errExit()
        include 'mpif.h'
        integer ierr

        print *, "Program exitting..."
        call MPI_FINALIZE(ierr)
        call exit(1)
    end subroutine


    type(String) function newID(prefix)
        include 'mpif.h'
        character(*), intent(in) :: prefix
        integer :: ierr

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)

        call string_init(newID, prefix)
        call string_append(newID, "_")
        call string_append(newID, trim(num2str(gnID)))
        gnid = gnid + 1

        call MPI_BARRIER(MPI_COMM_WORLD, ierr)
    end function


    character(50) function num2str(val)
        integer, intent(in) :: val

        write(num2str, '(i0)' ) val
    end function
end module mod_utils

