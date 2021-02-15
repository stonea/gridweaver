module mod_region
    use mod_utils
    implicit none
    
    type Region
        integer :: X1, Y1, X2, Y2
        integer :: empty
    end type

    integer, parameter :: ORIENT_BTM_LEFT  = 1
    integer, parameter :: ORIENT_BTM_RIGHT = 2
    integer, parameter :: ORIENT_TOP_LEFT  = 3
    integer, parameter :: ORIENT_TOP_RIGHT = 4

    
    public region_new,              &
           region_printSimp,        &
           region_print,            &
           region_bcast,            &
           regionVec_bcast,         &
           region_size,             &
           region_set,              &
           region_orientNormal,     &
           region_intersect,        &
           region_translate,        &
           region_expandToMultiple, &
           region_eq,               &
           region_orient_normal,    &
           region_orient,           &
           region_orientation

  contains

    type(Region) function region_new(X1, Y1, X2, Y2)
        integer, intent(in) :: X1, Y1, X2, Y2

        region_new%empty = 0
        region_new%X1 = X1
        region_new%Y1 = Y1
        region_new%X2 = X2
        region_new%Y2 = Y2
    end function

    subroutine region_printSimp(self, out)
        type(Region), intent(in) :: self
        integer, intent(in)      :: out
        
        write(out, '(AI0AI0AI0AI0A)', advance='no') &
            "(", self%X1, ", ", self%Y1, " - ", self%X2, ", ", self%Y2, ")"
    end subroutine

    subroutine region_print(self, out)
        type(Region), intent(in) :: self
        integer, intent(in)      :: out
        
        write(out, '(A)') "Region {"
        write(out, '(AI2)') "       x1 = ", self%X1
        write(out, '(AI2)') "       y1 = ", self%Y1
        write(out, '(AI2)') "       x2 = ", self%X2
        write(out, '(AI2)') "       y2 = ", self%Y2
        if(self%empty == 0) then
            write(out, '(A)') "  isEmpty =  F"
        else 
            write(out, '(A)') "  isEmpty =  T"
        end if
        write(out, '(A)') "}"
    end subroutine


    subroutine region_bcast(self)
        include 'mpif.h'
        type(Region), intent(inout) :: self
        integer, allocatable :: serialized(:)
        integer :: sz, ierr

        if(myRank() == 0) then
            serialized = transfer(self, serialized)
            sz = size(serialized)
        end if
        call MPI_BCAST(sz, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
        call MPI_BCAST(serialized, sz, MPI_INT, 0, MPI_COMM_WORLD, ierr)
        if(myRank() /= 0) then
            self = transfer(serialized, self)
        end if
        deallocate(serialized)
    end subroutine


    subroutine regionVec_bcast(self)
        include 'mpif.h'
        type(Region), allocatable, intent(inout) :: self(:)
        integer, allocatable :: serialized(:)
        integer :: sz, serialSz, ierr

        ! Have rank 0 serialize and broadcast its data
        if(myRank() == 0) then
            serialized = transfer(self, serialized)
            serialSz = size(serialized)
            sz = size(self)
        end if
        call MPI_BCAST(sz, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
        call MPI_BCAST(serialSz, 1, MPI_INT, 0, MPI_COMM_WORLD, ierr)
        if(myRank() /= 0) then
            allocate(self(sz))
            allocate(serialized(serialSz))
        endif
        call MPI_BCAST(serialized, serialSz, MPI_INT, 0, MPI_COMM_WORLD, ierr)

        ! Deserialize data
        if(myRank() /= 0) then
            self = transfer(serialized, self)
        endif
        deallocate(serialized)
    end subroutine

    ! Return the number of elements covered by the region
    integer function region_size(self)
        type(Region), intent(in) :: self

        region_size = (self%Y2 - self%Y1 + 1) * &
                      (self%X2 - self%X1 + 1)
    end function


    subroutine region_set(self, x1, y1, x2, y2)
        type(Region), intent(inout) :: self
        integer, intent(in) :: x1, y1, x2, y2

        self%empty = 0
        self%X1 = x1
        self%Y1 = y1
        self%X2 = x2
        self%Y2 = y2
    end subroutine


    function region_orientNormal(self)
        type(Region) :: region_orientNormal
        type(Region), intent(in) :: self

        region_orientNormal%empty = self%empty
        region_orientNormal%X1 = MIN(self%X1, self%X2)
        region_orientNormal%Y1 = MIN(self%Y1, self%Y1)
        region_orientNormal%X2 = MAX(self%X1, self%X2)
        region_orientNormal%Y2 = MAX(self%Y2, self%Y2)
    end function


    function region_intersect(lhs, rhs)
        type(Region) :: region_intersect
        type(Region), intent(in) :: lhs, rhs
        type(Region) :: lhs_nrm, rhs_nrm
        
        ! convert lhs and rhs to normal orientation
        lhs_nrm = region_orientNormal(lhs)
        rhs_nrm = region_orientNormal(rhs)

        ! Calculate the intersection
        region_intersect%X1 = MAX(lhs_nrm%X1, rhs_nrm%X1)
        region_intersect%Y1 = MAX(lhs_nrm%Y1, rhs_nrm%Y1)
        region_intersect%X2 = MIN(lhs_nrm%X2, rhs_nrm%X2)
        region_intersect%Y2 = MIN(lhs_nrm%Y2, rhs_nrm%Y2)

        ! Determine if the intersection is empty
        if(region_intersect%X1 > region_intersect%X2 .or. &
           region_intersect%Y2 > region_intersect%Y2 .or. &
           lhs%empty /= 0 .or. rhs%empty /= 0) &
        then
            region_intersect%empty = 1
        else
            region_intersect%empty = 0
        endif
    end function

   
    function region_translate(self, x, y)
        type(Region) :: region_translate
        type(Region), intent(in) :: self
        integer, intent(in) :: x, y

        region_translate%X1 = self%X1 + x
        region_translate%Y1 = self%Y1 + y
        region_translate%X2 = self%X2 + x
        region_translate%Y2 = self%Y2 + y
    end function


    function region_expandToMultiple(self, blkSzH, blkSzV)
        type(Region) :: region_expandToMultiple
        type(Region), intent(in) :: self
        integer, intent(in) :: blkSzH, blkSzV
        
        region_expandToMultiple%empty = 0
        region_expandToMultiple%X1 = round_down_blk(self%X1, blkSzH);
        region_expandToMultiple%Y1 = round_down_blk(self%Y1, blkSzV);
        region_expandToMultiple%X2 = round_up_blk(self%X2, blkSzH);
        region_expandToMultiple%Y2 = round_up_blk(self%Y2, blkSzV);
      contains
        integer function round_down_blk(x,m)
            integer, intent(in) :: x,m

            round_down_blk = floor(real((x-1)) / real(m)) * (m) + 1
        end function

        integer function round_up_blk(x,m)
            integer, intent(in) :: x,m

            round_up_blk = ceiling(real((x)) / real(m)) * (m)
        end function
    end function


    logical function region_eq(self, rhs)
        type(Region), intent(in) :: self, rhs
        
        region_eq = self%X1 == rhs%X1 .and. self%Y1 == rhs%Y1 .and. &
                    self%X2 == rhs%x2 .and. self%Y2 == rhs%y2
    end function

    
    type(Region) function region_orient_normal(self)
        type(Region), intent(in) :: self

        region_orient_normal = &
            region_mult_orientation(self, region_orientation(self))
    end function

    type(Region) function region_orient(self, orientation)
        type(Region), intent(in) :: self
        integer, intent(in) :: orientation

        region_orient = region_orient_normal(self)
        region_orient = region_mult_orientation(region_orient, orientation)
    end function

    type(Region) function region_mult_orientation(self, orientation)
        type(Region), intent(in) :: self
        integer, intent(in) :: orientation
        
        if(orientation == ORIENT_BTM_LEFT) then
            region_mult_orientation%X1 =  1 * self%X1
            region_mult_orientation%Y1 =  1 * self%Y1
            region_mult_orientation%X2 =  1 * self%X2
            region_mult_orientation%Y2 =  1 * self%Y2
        elseif(orientation == ORIENT_BTM_RIGHT) then
            region_mult_orientation%X1 =  1 * self%X1
            region_mult_orientation%Y1 = -1 * self%Y1
            region_mult_orientation%X2 =  1 * self%X2
            region_mult_orientation%Y2 = -1 * self%Y2
        elseif(orientation == ORIENT_TOP_LEFT) then
            region_mult_orientation%X1 = -1 * self%X1
            region_mult_orientation%Y1 =  1 * self%Y1
            region_mult_orientation%X2 = -1 * self%X2
            region_mult_orientation%Y2 =  1 * self%Y2
        elseif(orientation == ORIENT_TOP_RIGHT) then
            region_mult_orientation%X1 = -1 * self%X1
            region_mult_orientation%Y1 = -1 * self%Y1
            region_mult_orientation%X2 = -1 * self%X2
            region_mult_orientation%Y2 = -1 * self%Y2
        endif
    end function

    integer function region_orientation(self)
        type(Region), intent(in) :: self

            if(self%X2 >= self%X1 .and. self%Y2 >= self%Y1) then
                region_orientation = ORIENT_BTM_LEFT

        elseif(self%X2 <= self%X1 .and. self%Y2 >= self%Y1) then
                region_orientation = ORIENT_TOP_LEFT

        elseif(self%X2 <= self%X1 .and. self%Y2 <= self%Y1) then
                region_orientation = ORIENT_TOP_RIGHT

        elseif(self%X2 >= self%X1 .and. self%Y2 <= self%Y1) then
                region_orientation = ORIENT_BTM_RIGHT
        endif
    end function
end module
