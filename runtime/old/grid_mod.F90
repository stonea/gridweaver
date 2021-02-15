module Subgrid_mod
    use Expressions
    use Environment

    implicit none
     
    type, public :: Neighbor
        character, pointer :: name(:)
        type(exp), pointer :: &
            default(:), &
            subisubconditions(:), &
            results(:, :)
        integer :: nDims, nConditions, nConditionsSet
    end type
    public :: neighbor_new, &
              neighbor_setValue

    type, public :: Grid
        type(environmentT) :: env
        integer :: nEdges, nAllocatedEdges
        type(Neighbor), pointer :: offsets(:)
        integer, pointer :: coefficients(:)
    end type
    public :: subgrid_new,      &
              subgrid_setBound, &
              subgrid_addEdges

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
    subroutine offset_new(self, name, nDims, nConditions)
        type(Neighbor), intent(inout) :: self
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

    subroutine offset_addCondition(self, cond, offsets)
        type(Neighbor), intent(inout) :: self
        type(exp), intent(in) :: cond
        type(exp), intent(in) :: offsets(:)
    
        self%nConditionsSet = self%nConditionsSet + 1
        self%conditions(self%nConditionsSet) = cond
        self%results(self%nConditionsSet, :) = offsets
    end subroutine

    subroutine offset_setDefault(self, offsets)
        type(Neighbor), intent(inout) :: self
        type(exp), intent(in) :: offsets(:)

        self%default(:) = offsets(:self%nDims)
    end subroutine
    
    function offset_apply(self, env) result(res)
        type(Neighbor), intent(inout) :: self
        type(EnvironmentT), intent(inout) :: env
        integer :: condEval, res

        integer :: i, j

        do i=1, self%nConditions
            if(self%conditions(i)%command /= IF) then
                print *, "Conidition for offset must start with IF"
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
        allocate(self%offsets(nEdges))
        allocate(self%coefficients(nEdges))
    end subroutine

    subroutine grid_setBound(self, idx, val)
        type(Grid), intent(inout) :: self
        integer, intent(in) :: idx, val

        self%env%bounds(idx) = val
    end subroutine

    subroutine grid_addEdge(self, offset, coefficient)
        type(Grid), intent(inout) :: self
        type(Neighbor), intent(in) :: offset
        integer, intent(in) :: coefficient

        self%nAllocatedEdges = self%nAllocatedEdges + 1
        self%offsets(self%nAllocatedEdges) = offset
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

        integer :: i, j
        real :: newVal
        integer :: idx
        integer, allocatable, save :: offsetIndices(:)

        if(d == 1) then
            allocate(offsetIndices(self%env%nBounds))
        end if

        if(d == self%env%nBounds) then
            do i=1, self%env%bounds(d)
                self%env%indices(d) = i
                
                newVal = 0.0
                do j=1, self%nEdges
                    offsetIndices(j) = offset_apply(self%offsets(j), self%env)

                    newVal = newVal + self%coefficients(j) * &
                        A(offsetIndices(j))
                end do
                
                A(offsetIndices(1)) = newVal
            end do
        else
            do i=1, self%env%bounds(d)
                self%env%indices(d) = i
                call grid_applyDim(self, d+1, A)
            end do
        end if

        if(d == 1) then
            deallocate(offsetIndices)
        end if
    end subroutine
end module Subgrid_mod
