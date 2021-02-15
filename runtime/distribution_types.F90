module submod_distribution_types
    use mod_string
    use submod_grid_types
    implicit none

    type Distribution
        type(string)         :: name
        type(Grid), pointer  :: grd
        integer              :: nProcs
        integer, allocatable :: blockSize(:)
        integer, allocatable :: gbid2lbid(:)
        integer, allocatable :: gbid2proc(:)
        integer, allocatable :: lbid2gbid(:)
        integer, allocatable :: sg2gbid(:)

        integer, allocatable :: sgBlocksH(:)
        integer, allocatable :: sgBlocksV(:)
    end type
    type DistributionPtr
        type(Distribution), pointer :: val
    end type

  contains
end module submod_distribution_types
