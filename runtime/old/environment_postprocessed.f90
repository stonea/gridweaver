# 1 "/s/chopin/l/grad/stonea/projects/gridgen/runtime/envir1toLk5.F90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/s/chopin/l/grad/stonea/projects/gridgen/runtime/envir1toLk5.F90"
module Environment
    implicit none
    
    type environmentT
        integer :: nBounds
        integer, pointer :: bounds(:)
        integer, pointer :: indices(:)
    end type
end module
