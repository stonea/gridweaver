module Subgrid_mod
    use Expressions
    use Environment

    implicit none
     
    type, public :: Neighbor
        character, pointer :: name(:)
        type(exp), pointer :: offset(:)
        integer :: nDims
    end type
    public :: neighbor_new, &
              neighbor_setOffset, &
              neighbor_apply

    type, public :: SubGrid
        type(environmentT) :: env
        integer :: nEdges, nAllocatedEdges
        type(Neighbor), pointer :: neighbors(:)
        type(exp), pointer :: coefficients(:)
    end type
    public :: subgrid_new,      &
              subgrid_bound,    &
              subgrid_numDims,  &
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
    subroutine neighbor_new(self, name, nDims)
        type(Neighbor), intent(inout) :: self
        character(len=*), intent(in) :: name
        integer, intent(in) :: nDims
        
        call str(self%name, name)
        self%nDims = nDims
        allocate(self%offset(nDims))
    end subroutine


    subroutine neighbor_setOffset(self, val)
        type(Neighbor), intent(inout) :: self
        type(exp), intent(in) :: val(:)

        self%offset(:) = val(:self%nDims)
    end subroutine

    subroutine neighbor_apply(self, env, res)
        type(Neighbor), intent(inout) :: self
        type(EnvironmentT), intent(inout) :: env
        integer, intent(inout) :: res(:)

        integer :: i

        do i=1,ubound(res,1)
            res(i) = applyExp(self%offset(i), env)
        end do
    end subroutine

! *****************************************************************************
    subroutine subgrid_new(self, nBounds, nEdges)
        type(SubGrid), intent(inout) :: self
        integer, intent(in) :: nBounds, nEdges

        self%env%nBounds = nBounds
        self%nEdges = nEdges

        self%nAllocatedEdges = 0
        allocate(self%env%bounds(nBounds))
        allocate(self%env%indices(nBounds))
        allocate(self%neighbors(nEdges))
        allocate(self%coefficients(nEdges))
    end subroutine

    integer function subgrid_bound(self, idx)
        type(SubGrid), intent(in) :: self
        integer, intent(in) :: idx

        subgrid_bound = self%env%bounds(idx)
    end function

    integer function subgrid_numDims(self)
        type(SubGrid), intent(in) :: self
        
        subgrid_numDims = self%env%nBounds
    end function

    subroutine subgrid_setBound(self, idx, val)
        type(SubGrid), intent(inout) :: self
        integer, intent(in) :: idx, val

        self%env%bounds(idx) = val
    end subroutine

    subroutine subgrid_addEdges(self, neigh, weightExp)
        type(SubGrid), intent(inout) :: self
        type(Neighbor), intent(in) :: neigh
        type(exp), intent(in) :: weightExp

        self%nAllocatedEdges = self%nAllocatedEdges + 1
        self%neighbors(self%nAllocatedEdges) = neigh
        self%coefficients(self%nAllocatedEdges) = weightExp
    end subroutine

end module Subgrid_mod
