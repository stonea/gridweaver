module Utils
    implicit none

    include 'mpif.h'

    public :: printMatrix, &
              myRank

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
        integer ierr
        
        call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)
    end function
end module Utils

