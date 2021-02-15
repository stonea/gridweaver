module mod_linkedList
    implicit none

    ! A MOLD for transfer() of linked list data
    integer, dimension(:), allocatable :: list_mold

    public LinkedList
    type LinkedList
        integer, dimension(:), pointer :: val => null()
        type(LinkedList), pointer      :: next => null()
    end type
    public linkedList_new,  &
           linkedList_free, &
           linkedList_push, &
           linkedList_size, &
           linkedList_get

  contains
    subroutine linkedList_new(self)
        type(LinkedList), pointer  :: self

        allocate(self)
        nullify(self%next)
        nullify(self%val)
    end subroutine


    subroutine linkedList_free(self)
        type(LinkedList), pointer, intent(inout)  :: self
        type(LinkedList), pointer :: cur, next

        cur => self
        do while(associated(cur))
            ! Recall what the next node is (need to do this since we're about
            ! to delete this information).
            next => cur%next

            ! clear out data for current node
            if(associated(cur%val)) then
                deallocate(cur%val)
                nullify(cur%val)
            end if

            ! Remove current node
            deallocate(cur)
            nullify(cur)

            ! Traverse
            cur => next
        end do
    end subroutine


    subroutine linkedList_push(self, val)
        type(LinkedList), pointer      :: self
        integer, dimension(:), intent(in) :: val
        type(LinkedList), pointer :: cur
        
        ! move cur to the last element in the list
        cur => self
        do while(associated(cur%next))
            cur => cur%next
        end do

        ! associate the value into the list
        if(associated(cur%val)) then
            call linkedList_new(cur%next)
            allocate(cur%next%val(size(val)))
            cur%next%val = val
        else
            allocate(cur%val(size(val)))
            cur%val = val
        endif
    end subroutine

    integer recursive function linkedList_size(self) result(res)
        type(LinkedList), pointer :: self

        ! Recursively compute the size of the list (by traversing it)
        if(.not. associated(self%val)) then
            res = 0
        else if(.not. associated(self%next)) then
            res = 1
        else
            res = 1 + linkedList_size(self%next)
        end if
    end function


    !** Return the n-th element of this linked list **
    recursive function linkedList_get(self, n) result(res)
        type(LinkedList), pointer :: res
        type(LinkedList), pointer :: self
        integer, intent(in) :: n

        if(n == 1) then
            res => self
        else
            res => linkedList_get(self%next, n-1)
        endif
    end function
end module mod_linkedList
