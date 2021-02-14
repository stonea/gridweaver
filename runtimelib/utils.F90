module mod_utils
    use mod_String
    use iso_c_binding
    implicit none

    public :: printMatrix,      &
              myRank,           &
              numRanks,         &
              errExit,          &
              cint
    
    public :: print_procColor,       &
              print_color,           &
              print_underline,       &
              print_bold,            &
              print_invertColor,     &
              print_resetColor

    integer :: gnID = 0

  interface
    subroutine cwrap__setColor(color) &
        bind(C, name="__setColor")
        use iso_c_binding
        integer(c_int) :: color
    end subroutine
  end interface

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

    function cint(val)
        integer, intent(in) :: val
        integer(c_int)   :: cint

        cint = val
    end function

    subroutine print_procColor(proc)
        integer :: proc
        call cwrap__setColor(proc)
    end subroutine

    subroutine print_color(color)
        integer, intent(in) :: color
        call cwrap__setColor(color)
    end subroutine

    subroutine print_resetColor()
        integer :: color
        color = -1
        call cwrap__setColor(color)
    end subroutine

    subroutine print_underline()
        integer :: color
        color = -2
        call cwrap__setColor(color)
    end subroutine

    subroutine print_bold()
        integer :: color
        color = -3
        call cwrap__setColor(color)
    end subroutine

    subroutine print_invertColor()
        integer :: color
        color = -4
        call cwrap__setColor(color)
    end subroutine
end module mod_utils

