module submod_grid_types
    use mod_string
    use mod_region
    use mod_linkedlist
    implicit none

    type Neighbor
        type(String)         :: name
        integer, allocatable :: offsets(:)
    end type
    type NeighborPtr
        type(Neighbor), pointer :: val
    end type


    type SubGrid
        type(string)                   :: name
        integer                        :: id
        integer                        :: ndims
        integer, allocatable           :: extents(:)
        type(NeighborPtr), allocatable :: neighbors(:)
    end type
    type SubGridPtr
        type(SubGrid), pointer :: val
    end type

    type Grid
        type(String) name
        integer nDims
        type(LinkedList), pointer :: subgrids

        integer nBMaps
        type(Region), allocatable      :: borderSrcRegions(:)
        type(SubGridPtr),  allocatable :: borderSrcSubgrids(:)
        type(Region), allocatable      :: borderTgtRegions(:)
        type(SubGridPtr), allocatable  :: borderTgtSubgrids(:)
        
        type(NeighborPtr), allocatable :: borderSrcNeighbors(:)
        type(NeighborPtr), allocatable :: borderTgtNeighbors(:)
    end type
    type GridPtr
        type(Grid), pointer :: val
    end type

  contains
! *****************************************************************************
end module submod_grid_types
