program Test

    implicit none

    ! Environment

    type environmentT
        integer :: nBounds
        integer, pointer :: bounds(:)
        integer, pointer :: indices(:)
    end type

    ! Stencil
    type :: OffsetT
        character, pointer :: name(:)
        type(exp), pointer :: &
            default(:), &
            conditions(:)
        type(exp), pointer :: &
            results(:, :)
        integer :: nDims, nConditions, nConditionsSet
    end type

    type :: Grid
        type(environmentT) :: env
        integer :: nEdges, nAllocatedEdges
        type(OffsetT), pointer :: neighbors(:)
        integer, pointer :: coefficients(:)
    end type

    ! Expressions

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

    type(exp), pointer :: val_
    type(exp) :: if_, and_, eq_, idx_, const_, param_, &
        minus_, plus_

    ! Utils

    ! -----------------------------------------------------------------------
    integer, parameter :: N = 4
    real :: A(N * N)

    type(OffsetT) :: center, north, east, south, west
    type(Grid) :: g

    call grid_new(g, 2, 5)
    call grid_setBound(g, 1, N);
    call grid_setBound(g, 2, N);

    call neighbor_new(center, "center", 2, 0)
    call neighbor_setDefault(center, (/ idx_(1), idx_(2) /))

    call neighbor_new(north, "north", 2, 1)
    call neighbor_setDefault(north, (/ minus_(idx_(1), const_(1)), idx_(2) /))
    call neighbor_addCondition(north, &
        if_(eq_(idx_(1), param_(1))), (/ const_(1), idx_(1) /))
    
    call neighbor_new(south, "south", 2, 1)
    call neighbor_setDefault(south, (/ plus_(idx_(1), const_(1)), idx_(2) /))
    call neighbor_addCondition(south, &
        if_(eq_(idx_(1), const_(1))), (/ param_(1), idx_(2) /))
    
    call neighbor_new(west, "west", 2, 1)
    call neighbor_setDefault(west, (/ idx_(1), minus_(idx_(2), const_(1)) /))
    call neighbor_addCondition(west, &
        if_(eq_(idx_(2), const_(1))), (/ idx_(1), param_(2) /))

    call neighbor_new(east, "east", 2, 1)
    call neighbor_setDefault(east, (/ idx_(1), plus_(idx_(2), const_(1)) /))
    call neighbor_addCondition(east, &
        if_(eq_(idx_(2), param_(2))), (/ idx_(1), const_(1) /))

    call grid_addEdge(g, center, 1);
    call grid_addEdge(g, north, 1);
    call grid_addEdge(g, south, 1);
    call grid_addEdge(g, west, 1);
    call grid_addEdge(g, east, 1);
    ! -----------------------------------------------------------------------
    A = 0.0
    A(1:N) = 3.0
    
    print *, "Original Matrix: "
    call printMatrix(A, N)
    
    call grid_apply(g, A)
    
    print *, " "
    print *, "After applying stencil: "
    call printMatrix(A, N)

contains
    

! *****************************************************************************
    subroutine str(to, from)
        character(len=*), intent(in) :: from
        character, pointer, intent(inout) :: to(:)
        integer :: i

        allocate(to(len(from)))
        do i=1,len(from)
            to(i) = from(i:i)
        end do
    end subroutine

    subroutine alloc(var, amt)
        type(exp), allocatable, intent(inout) :: var(:)
        integer :: amt
        
        allocate(var(amt))
    end subroutine
    
! *****************************************************************************
    subroutine neighbor_new(self, name, nDims, nConditions)
        type(OffsetT), intent(inout) :: self
        character(len=*), intent(in) :: name
        integer, intent(in) :: nDims, nConditions
        
        call str(self%name, name)
        self%nDims = nDims
        self%nConditions = nConditions

        self%nConditionsSet = 0
        allocate(self%default(nDims))
        allocate(self%conditions(nConditions))
        allocate(self%results(nConditions, nDims))
    end subroutine

    subroutine neighbor_addCondition(self, cond, neighbors)
        type(OffsetT), intent(inout) :: self
        type(exp), intent(in) :: cond
        type(exp), intent(in) :: neighbors(:)
    
        self%nConditionsSet = self%nConditionsSet + 1
        self%conditions(self%nConditionsSet) = cond
        self%results(self%nConditionsSet, :) = neighbors
    end subroutine

    subroutine neighbor_setDefault(self, neighbors)
        type(OffsetT), intent(inout) :: self
        type(exp), intent(in) :: neighbors(:)

        self%default(:) = neighbors(:self%nDims)
    end subroutine
    
    function neighbor_apply(self, env) result(res)
        type(OffsetT), intent(inout) :: self
        type(EnvironmentT), intent(inout) :: env
        integer :: condEval, res

        integer :: i, j

        do i=1, self%nConditions
            if(self%conditions(i)%command /= IF) then
                print *, "Conidition for neighbor must start with IF"
            else
                condEval = applyExp(self%conditions(i), env)
                if(condEval .eq. 1) then
                    res = 0
                    do j=1,env%nBounds - 1
                        res = res + (applyExp(self%results(i, j), env)-1) * &
                            env%bounds(i)
                    end do
                    res = res + applyExp(self%results(i, env%nbounds), env)

                    return
                end if
            end if
        end do
        
        res = 0
        do j=1,env%nBounds - 1
            res = res + (applyExp(self%default(j), env)-1) * &
                env%bounds(i)
        end do
        res = res + applyExp(self%default(env%nbounds), env)
    end function

! *****************************************************************************
    subroutine grid_new(self, nBounds, nEdges)
        type(Grid), intent(inout) :: self
        integer, intent(in) :: nBounds, nEdges

        self%env%nBounds = nBounds
        self%nEdges = nEdges

        self%nAllocatedEdges = 0
        allocate(self%env%bounds(nBounds))
        allocate(self%env%indices(nBounds))
        allocate(self%neighbors(nEdges))
        allocate(self%coefficients(nEdges))
    end subroutine

    subroutine grid_setBound(self, idx, val)
        type(Grid), intent(inout) :: self
        integer, intent(in) :: idx, val

        self%env%bounds(idx) = val
    end subroutine

    subroutine grid_addEdge(self, neighbor, coefficient)
        type(Grid), intent(inout) :: self
        type(OffsetT), intent(in) :: neighbor
        integer, intent(in) :: coefficient

        self%nAllocatedEdges = self%nAllocatedEdges + 1
        self%neighbors(self%nAllocatedEdges) = neighbor
        self%coefficients(self%nAllocatedEdges) = coefficient
    end subroutine

! *****************************************************************************
    subroutine grid_apply(self, A)
        type(Grid), intent(inout) :: self
        real, intent(inout) :: A(:)
        
        call grid_applyDim(self, 1, A)
    end subroutine

    recursive subroutine grid_applyDim(self, d, A)
        type(Grid), intent(inout) :: self
        integer, intent(in) :: d
        real, intent(inout) :: A(:)

!        integer :: i, j
!        real :: newVal
!        integer :: idx
!        integer, allocatable, save :: neighborIndices(:)
!
!        if(d == 1) then
!            allocate(neighborIndices(self%env%nBounds))
!        end if
!
!        if(d == self%env%nBounds) then
!            do i=1, self%env%bounds(d)
!                self%env%indices(d) = i
!                
!                newVal = 0.0
!                do j=1, self%nEdges
!                    neighborIndices(j) = neighbor_apply(self%neighbors(j), self%env)
!
!                    newVal = newVal + self%coefficients(j) * &
!                        A(neighborIndices(j))
!                end do
!
!                
!                A(neighborIndices(1)) = newVal
!            end do
!        else
!            do i=1, self%env%bounds(d)
!                self%env%indices(d) = i
!                call grid_applyDim(self, d+1, A)
!            end do
!        end if
!
!        if(d == 1) then
!            deallocate(neighborIndices)
!        end if
    end subroutine

! *****************************************************************************
    function val_(val)
        integer, intent(in) :: val

        nodePool(nAllocatedNodes)%command = val
        nullify(nodePool(nAllocatedNodes)%left)
        nullify(nodePool(nAllocatedNodes)%right)
        
        nAllocatedNodes = nAllocatedNodes + 1

        val_ => nodePool(nAllocatedNodes-1)
    end function

    function if_(left)
        type(exp), target, intent(in) :: left
        
!        if_%command = IF
!        if_%left  => left
!        nullify(if_%right)
    end function

    function and_(left, right)
        type(exp), target, intent(in) :: left, right
!        
!        and_%command = AND
!        and_%left  => left
!        and_%right => right
    end function

    function eq_(left, right)
        type(exp), target, intent(in) :: left, right
!        
!        eq_%command = EQ
!        eq_%left  => left
!        eq_%right => right
    end function

    function idx_(idx_val)
        integer, intent(in) :: idx_val
!                
!        idx_%command = IDX
!        idx_%left => val_(idx_val)
!        nullify(idx_%right)
    end function

    function const_(const_val)
        integer, intent(in) :: const_val
        type(exp), allocatable, target :: rhs
!
!        const_%command = CONST
!        const_%left => val_(const_val)
!        nullify(const_%right)
    end function

    function param_(param_val)
        integer, intent(in) :: param_val
        type(exp), allocatable, target :: rhs
!
!        param_%command = PARAM
!        param_%left => val_(param_val)
!        nullify(param_%right)
    end function

    function minus_(left, right)
        type(exp), target, intent(in) :: left, right
!        
!        minus_%command = MINUS
!        minus_%left  => left
!        minus_%right => right
    end function

    function plus_(left, right)
        type(exp), target, intent(in) :: left, right
!        
!        plus_%command = PLUS
!        plus_%left  => left
!        plus_%right => right
    end function


    recursive subroutine printExp(e)
        type(exp) :: e
!
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

    function AT(x, y, N) result(res)
        integer, intent(in) :: x, y, N
        integer :: res

        res = (y-1) * N + x
    end function

    integer function idxAT(dims, bounds)
        integer, intent(in) :: dims(:), bounds(:)
        integer :: res

        integer :: i
        res = 0

        do i=1,ubound(dims,1)-1
            res = res + dims(i) * (bounds(i)-1)
        end do
        res = res + bounds(ubound(bounds,1))

        idxAT = res
    end function

    subroutine printMatrix(A, n)
        real, intent(in) :: A(:)
        integer, intent(in) :: n
        integer idx_0, idx_1

        do idx_0 = 1, n
            do idx_1 = 1, n
                write (*,"(f15.4)", advance="no") A(AT(idx_0, idx_1, n))
            enddo
            print *, ""
        end do
        print *, ""
    end subroutine

end program
