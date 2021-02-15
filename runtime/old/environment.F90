module Environment
    implicit none
    
    type environmentT
        integer :: nBounds
        integer, pointer :: bounds(:)
        integer, pointer :: indices(:)
    end type
end module
