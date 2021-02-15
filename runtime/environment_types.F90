module submod_environment_types
    use mod_linkedList

    implicit none

    type Environment
        type(LinkedList), pointer :: neighbors
        type(LinkedList), pointer :: subgrids
        type(LinkedList), pointer :: grids
        type(LinkedList), pointer :: distributions
        type(LinkedList), pointer :: schedules
    end type

    type(Environment) :: env
   
  contains
end module submod_environment_types
