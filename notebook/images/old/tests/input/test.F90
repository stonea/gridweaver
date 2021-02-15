program Test

    implicit none

!    public :: printMatrix

    type environmentT
        integer :: nBounds
        integer, pointer :: bounds(:)
        integer, pointer :: indices(:)
    end type

    integer, parameter :: &
        IF    = -1, &
        AND   = -2, &
        EQ    = -3, &
        IDX   = -4, &
        CONST = -5, &
        PARAM = -6, &
        MINUS = -7, &
        PLUS  = -8

    type :: exp
        integer :: command
        type(exp), pointer :: left, &
                              right
    end type

    type(exp), target :: nodePool(10000)
    integer :: nAllocatedNodes = 0

!    public :: if_, and_, eq_, var_, const_
!    public :: printExp
!    public :: applyExp

    type :: Neighbor
        character, pointer :: name(:)
        type(exp), pointer :: val(:)
        integer :: nDims
    end type
!    public :: neighbor_new, &
!              neighbor_setValue, &
!              neighbor_apply

    type :: SubGrid
        type(environmentT) :: env
        integer :: nEdges, nAllocatedEdges
        type(Neighbor), pointer :: neighbors(:)
        type(exp), pointer :: coefficients(:)
    end type
!    public :: subgrid_new,      &
!              subgrid_bound,    &
!              subgrid_numDims,  &
!              subgrid_setBound, &
!              subgrid_addEdges

    type :: Data
        real, pointer :: values(:)
        type(SubGrid), pointer :: grid
        integer :: size
    end type
!    public data_new,      &
!        data_input,       &
!        data_output,      &
!        data_set,         &
!        data_apply_grid,  &
!        data_add,         &
!        data_sub,         &
!        data_mult,        &
!        data_pow,         &
!        data_sum,         &
!        data_at,          &
!        data_print
  

!    integer :: subgrid_bound
!    integer :: subgrid_numDims
    !integer :: data_at
!    type(Data) :: data_apply_grid
!    type(Data) :: data_add
!    type(Data) :: data_sub
!    type(Data) :: data_mult
!    type(Data) :: data_pow
!    real :: data_sum

!****************************************************************************************
!****************************************************************************************
!****************************************************************************************

    integer :: ierr
    integer, parameter :: N = 4
    real :: A(N * N)
    integer :: i,j,f

    ! -----------------------------------------------------------------------
!    type(DistributionT) :: dist
    type(Neighbor) :: center, north, east, south, west
    type(SubGrid) :: g
    !type(Grid) :: g
    type(Data) :: lhs, rhs, res
    
!    call MPI_INIT(ierr)

!!    call distribution_new_cyclic(dist, comm, nBlockX, nBlockY, nPointX, nPointY)
!
    call subgrid_new(g, 2, 4)
    call subgrid_setBound(g, 1, N);
    call subgrid_setBound(g, 2, N);

    call neighbor_new(north, "north", 2)
    call neighbor_setOffset(north, (/ idx_(1), plus_(idx_(2), const_(1)) /))
    
    call neighbor_new(south, "south", 2)
    call neighbor_setOffset(south, (/ idx_(1), minus_(idx_(2), const_(1)) /))
    
    call neighbor_new(west, "west", 2)
    call neighbor_setOffset(west, (/ minus_(idx_(1), const_(1)), idx_(2) /))

    call neighbor_new(east, "east", 2)
    call neighbor_setOffset(east, (/  plus_(idx_(1), const_(1)), idx_(2) /))

    call subgrid_addEdges(g, north, const_(1))
    call subgrid_addEdges(g, south, const_(2))
    call subgrid_addEdges(g, west, const_(1))
    call subgrid_addEdges(g, east, const_(1))

    call data_new(lhs, g)
    call data_new(rhs, g)

    open(unit=f, file='test.dat')
    call data_input(lhs, f)
    call data_input(rhs, f)
    close(unit=f)

    print *, "LHS:"
    call data_print(lhs)
    print *, "RHS:"
    call data_print(rhs)


!****************************************************************************************
!****************************************************************************************
!****************************************************************************************

contains
    function AT(x, y, N) result(res)
        integer, intent(in) :: x, y, N
        integer :: res

!        res = (y-1) * N + x
    end function

    subroutine printMatrix(A, n)
        real, intent(in) :: A(:)
        integer, intent(in) :: n
        integer idx_0, idx_1

!        do idx_0 = 1, n
!            do idx_1 = 1, n
!                write (*,"(f12.3)", advance="no") A(AT(idx_0, idx_1, n))
!            enddo
!            print *, " "
!        end do
!        print *, " "
    end subroutine


! *****************************************************************************
    function val_(val)
        type(exp), pointer :: val_
        integer, intent(in) :: val

!        nodePool(nAllocatedNodes)%command = val
!        nullify(nodePool(nAllocatedNodes)%left)
!        nullify(nodePool(nAllocatedNodes)%right)
!        
!        nAllocatedNodes = nAllocatedNodes + 1
!
!        val_ => nodePool(nAllocatedNodes-1)
    end function

    function if_(left)
        type(exp) :: if_
        type(exp), target, intent(in) :: left
        
!        if_%command = IF
!        if_%left  => left
!        nullify(if_%right)
    end function

    function and_(left, right)
        type(exp) :: and_
        type(exp), target, intent(in) :: left, right
        
!        and_%command = AND
!        and_%left  => left
!        and_%right => right
    end function

    function eq_(left, right)
        type(exp) :: eq_
        type(exp), target, intent(in) :: left, right
        
!        eq_%command = EQ
!        eq_%left  => left
!        eq_%right => right
    end function

    function idx_(idx_val)
        type(exp) :: idx_
        integer, intent(in) :: idx_val
                
!        idx_%command = IDX
!        idx_%left => val_(idx_val)
!        nullify(idx_%right)
    end function

    function const_(const_val)
        type(exp) :: const_
        integer, intent(in) :: const_val

!        const_%command = CONST
!        const_%left => val_(const_val)
!        nullify(const_%right)
    end function

    function param_(param_val)
        type(exp) :: param_
        integer, intent(in) :: param_val

!        param_%command = PARAM
!        param_%left => val_(param_val)
!        nullify(param_%right)
    end function

    function minus_(left, right)
        type(exp) :: minus_
        type(exp), target, intent(in) :: left, right
        
!        minus_%command = MINUS
!        minus_%left  => left
!        minus_%right => right
    end function

    function plus_(left, right)
        type(exp) :: plus_
        type(exp), target, intent(in) :: left, right
        
!        plus_%command = PLUS
!        plus_%left  => left
!        plus_%right => right
    end function


    recursive subroutine printExp(e)
        type(exp) :: e

!        select case(e%command)
!            case (IF)     ; write(*, '(A)', advance='no') "IF("
!            case (AND)    ; write(*, '(A)', advance='no') "AND("
!            case (EQ)     ; write(*, '(A)', advance='no') "EQ("
!            case (IDX)    ; write(*, '(A)', advance='no') "IDX("
!            case (CONST)  ; write(*, '(A)', advance='no') "CONST("
!            case (PARAM)  ; write(*, '(A)', advance='no') "PARAM("
!            case (MINUS)  ; write(*, '(A)', advance='no') "MINUS("
!            case (PLUS)   ; write(*, '(A)', advance='no') "PLUS("
!
!            case default
!                write(*, '(I1)', advance='no') e%command
!        end select
!        
!        if(associated(e%left))  call printExp(e%left)
!        if(associated(e%right)) then
!            write(*, '(A)', advance='no') ", "
!            call printExp(e%right)
!        end if
!        if(e%command .lt. 0) &
!            write(*, '(A)', advance='no') ")"
    end subroutine

    recursive function applyExp(e, env) result(res)
        integer :: res
        type(exp) :: e
        type(environmentT) :: env

!        select case(e%command)
!            case (IF)
!                res = applyExp(e%left, env)
!                
!            case (AND)
!                if(applyExp(e%left, env) == 1 .and. &
!                   applyExp(e%right, env) == 1) then
!                    res = 1
!                else
!                    res = 0
!                end if
!
!            case (EQ)
!                if(applyExp(e%left, env) == applyExp(e%right, env)) then
!                    res = 1
!                else
!                    res = 0
!                end if
!
!            case (IDX)
!                res = env%indices(e%left%command)
!
!            case (CONST)
!                res = e%left%command
!
!            case (PARAM)
!                res = env%bounds(e%left%command)
!
!            case (MINUS)
!                res = applyExp(e%left, env) - applyExp(e%right, env)
!
!            case (PLUS)
!                res = applyExp(e%left, env) + applyExp(e%right, env)
!
!            case default
!                print *, "Unrecognized command type for expression"
!        end select
    end function

! *****************************************************************************
    subroutine str(to, from)
        character(len=*), intent(in) :: from
        character, pointer, intent(inout) :: to(:)
        integer :: i

!        allocate(to(len(from)))
!        do i=1,len(from)
!            to(i) = from(i:i)
!        end do
    end subroutine

! *****************************************************************************
    subroutine neighbor_new(self, name, nDims)
        type(Neighbor), intent(inout) :: self
        character(len=*), intent(in) :: name
        integer, intent(in) :: nDims
        
!        call str(self%name, name)
!        self%nDims = nDims
!        allocate(self%val(nDims))
    end subroutine


    subroutine neighbor_setOffset(self, val)
        type(Neighbor), intent(inout) :: self
        type(exp), intent(in) :: val(:)

!        self%val(:) = val(:self%nDims)
    end subroutine

    ! TODO: Generalize to more than two dimensions
    function neighbor_apply(self, env) result(res)
        type(Neighbor), intent(inout) :: self
        type(EnvironmentT), intent(inout) :: env
        integer :: res, x, y

        integer :: i

!        x = applyExp(self%val(1), env)
!        y = applyExp(self%val(2), env)
!
!        if(x < 1 .or. y < 1 .or. x > env%bounds(1) .or. y > env%bounds(2)) then
!            res = 0
!        else
!            res = (x-1) * env%bounds(1) + y
!        endif
    end function

! *****************************************************************************
    subroutine subgrid_new(self, nBounds, nEdges)
        type(SubGrid), intent(inout) :: self
        integer, intent(in) :: nBounds, nEdges

!        self%env%nBounds = nBounds
!        self%nEdges = nEdges
!
!        self%nAllocatedEdges = 0
!        allocate(self%env%bounds(nBounds))
!        allocate(self%env%indices(nBounds))
!        allocate(self%neighbors(nEdges))
!        allocate(self%coefficients(nEdges))
    end subroutine

    function subgrid_bound(self, idx) result(res)
        type(SubGrid), intent(in) :: self
        integer, intent(in) :: idx
        integer :: res

!        subgrid_bound = self%env%bounds(idx)
    end function

    function subgrid_numDims(self) result(res)
        type(SubGrid), intent(in) :: self
        integer :: res
        
!        subgrid_numDims = self%env%nBounds
    end function

    subroutine subgrid_setBound(self, idx, val)
        type(SubGrid), intent(inout) :: self
        integer, intent(in) :: idx, val

!        self%env%bounds(idx) = val
    end subroutine

    subroutine subgrid_addEdges(self, neigh, weightExp)
        type(SubGrid), intent(inout) :: self
        type(Neighbor), intent(in) :: neigh
        type(exp), intent(in) :: weightExp

!        self%nAllocatedEdges = self%nAllocatedEdges + 1
!        self%neighbors(self%nAllocatedEdges) = neigh
!        self%coefficients(self%nAllocatedEdges) = weightExp
    end subroutine

 ! *****************************************************************************
    subroutine data_new(self, g)
        type(Data), intent(inout) :: self
        type(SubGrid), target, intent(in) :: g

!        integer numElements, i
!
!        self%grid => g
!
!        numElements = 1
!        do i=1,subgrid_numDims(self%grid)
!            numElements = numElements * subgrid_bound(self%grid, i)
!        end do
!
!        allocate(self%values(numElements))
!
!        self%values_size = numElements
    end subroutine

    subroutine data_input(self, unitnum)
        type(Data), intent(inout) :: self
        integer, intent(in) :: unitnum
        real :: vals(self%size)

        integer :: i

        read(unitnum, *) vals
        self%values = vals
    end subroutine

    subroutine data_output(self, unitnum)
        type(Data), intent(inout) :: self
        integer, intent(in) :: unitnum

!        write(unitnum, *) self%values
    end subroutine

    ! TODO: Generalize to n-dimensions
    subroutine data_set(self, idx, val)
        type(Data), intent(inout) :: self
        integer, intent(in) :: idx(:)
        real, intent(in) :: val
        
!        self%values(data_at(self, idx)) = val
    end subroutine

    function data_applyGrid(self) result(res)
        type(Data), intent(inout) :: self
        type(Data) :: res
!        integer :: idx
!
!        if(.not. allocated(data_apply_grid%values)) then
!            call data_new(data_apply_grid, self%grid)
!        end if
!
!        idx = 1
!        call data_applyDim( &
!            self%grid, 1, self%values, data_apply_grid%values, idx)
    end function

    function data_add(lhs, rhs) result(res)
        type(Data), intent(in) :: lhs
        type(Data), intent(in) :: rhs
        type(Data) :: res

!        integer :: i
!
!        call data_new(data_add, lhs%grid)
!
!        do i=0,ubound(lhs%values, 1)
!            data_add%values(i) = lhs%values(i) + rhs%values(i)
!        end do
    end function

    function data_sub(lhs, rhs) result(res)
        type(Data), intent(in) :: lhs
        type(Data), intent(in) :: rhs
        type(Data) :: res

!        integer :: i
!
!        call data_new(data_sub, lhs%grid)
!
!        do i=0,ubound(lhs%values, 1)
!            data_sub%values(i) = lhs%values(i) - rhs%values(i)
!        end do
    end function

    function data_mult(lhs, rhs) result(res)
        type(Data), intent(in) :: lhs
        type(Data), intent(in) :: rhs
        type(Data) :: res

!        integer :: i
!
!        call data_new(data_mult, lhs%grid)
!
!        do i=0,ubound(lhs%values, 1)
!            data_mult%values(i) = lhs%values(i) * rhs%values(i)
!        end do
    end function

    function data_pow(lhs, exponent) result(res)
        type(Data), intent(in) :: lhs
        integer, intent(in) :: exponent
        type(Data) :: res

!        integer :: i, j
!
!        call data_new(data_pow, lhs%grid)
!
!        do j=1,ubound(lhs%values, 1)
!            data_pow%values(j) = 1.0
!        end do
!
!        do i=1,exponent
!            do j=1,ubound(lhs%values, 1)
!                data_pow%values(j) = data_pow%values(j) * lhs%values(j)
!            end do
!        end do
    end function

    function data_sum(x) result(res)
        type(Data), intent(in) :: x
        real :: res

!        integer :: i
!        real :: res
!
!        res = 0.0
!
!        do i=0,ubound(x%values, 1)
!            res = res + x%values(i)
!        end do
!
!        data_sum = res
    end function

    ! TODO: Make this function generalized to more than 2 dimensions
    !function data_at(x, idx)
    !    type(Data), intent(in) :: x
    !    integer, dimension(:), intent(in) :: idx
!
!!        data_at = (idx(2)-1) * subgrid_bound(x%grid, 1) + idx(1)
!    end function

    subroutine data_print(self)
        type(Data) :: self
!
!        integer idx_0, idx_1
!
!        do idx_0 = subgrid_bound(self%grid, 1), 1, -1
!            do idx_1 = 1, subgrid_bound(self%grid, 2)
!                write (*,"(f12.3)", advance="no") &
!                    self%values(data_at(self, (/ idx_0, idx_1 /) ))
!            enddo
!            print *, " "
!        end do
!        print *, " "

    end subroutine

! *****************************************************************************
    ! Applies 
    recursive subroutine data_applyDim(grid, d, A, res, input_idx)
        type(SubGrid), intent(inout) :: grid
        integer, intent(in) :: d
        integer, intent(inout) :: input_idx
        real, intent(inout) :: A(:)
        real, intent(inout) :: res(:)

!        integer :: i, j
!        real :: newVal
!        integer :: idx
!
!        ! If this is the last dimension
!        if(d == grid%env%nBounds) then
!            ! Iterate through elements
!            do i=1, grid%env%bounds(d)
!                grid%env%indices(d) = i
!
!                ! Determine what result of applying stencil at current element is
!                newVal = 0.0
!                do j=1, grid%nEdges
!                    idx = neighbor_apply(grid%neighbors(j), grid%env)
!                    if(idx /= 0) then
!                        newVal = newVal + applyExp(grid%coefficients(j), grid%env) * A(idx)
!                    end if
!                end do
!
!                res(input_idx) = newVal
!                input_idx = input_idx+1
!            end do
!
!        ! If we're not calling the last dimension call this function recursively
!        ! for each set of elements along the current dimension
!        else
!            do i=1, grid%env%bounds(d)
!                grid%env%indices(d) = i
!                call data_applyDim(grid, d+1, A, res, input_idx)
!            end do
!        end if
    end subroutine
 
end program
