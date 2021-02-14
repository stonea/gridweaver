! Tell VIM to hihlight: vim: syntax=fortran

! ******************************************************************************
! ** - [UTILS] - ***************************************************************
! ******************************************************************************
    external :: printMatrix,        &
                !myRank,             &
                !numRanks,           &
                errExit

    interface
        integer function myRank()
        end function
        integer function numRanks()
        end function
    end interface


! ******************************************************************************
! ** - [LINKEDLIST] - **********************************************************
! ******************************************************************************
    integer, dimension(:), allocatable :: list_mold

    type LinkedList
        integer, dimension(:), pointer :: val => null()
        type(LinkedList), pointer      :: next => null()
    end type
    external linkedList_new,  &
             linkedList_free, &
             linkedList_push

! ******************************************************************************
! ** - [STRING] - **************************************************************
! ******************************************************************************
type String
    integer             :: len
    character(len=80)   :: val
end type

external :: string_new,    &
            string_size,   &
            string_append, &
            string_equals, &
            string_bcast,  &
            tstr,          &
            cstr

! ******************************************************************************
! ** - [REGION] - **************************************************************
! ******************************************************************************
    type Region
        integer :: X1, Y1, X2, Y2
        integer :: empty
    end type
    
    external region_printSimp,  &
             region_print,      &
             region_bcast,      &
             regionVec_bcast!,   &
             !region_size

    interface
        integer function region_size(self)
        end function
    end interface

! ******************************************************************************
! ** - [DISTRIBUTEDVEC] - ******************************************************
! ******************************************************************************
    type VecVecInt
        integer :: size
        integer, allocatable :: offsets(:)
        integer, allocatable :: values(:)
    end type

    type VecVecRegion
        integer :: size
        integer, allocatable :: offsets(:)
        type(Region), allocatable :: values(:)
    end type


    external :: distIntVec_print,         &
                distIntVecVec_print,      &
                !distIntVecVec_numVecs,    &

                distRegionVec_print,      &
                distRegionVecVec_print!,   &
                !distRegionVecVec_numVecs

    interface
        integer function distIntVecVec_numVecs(vv, rank) 
        end function

        integer function distRegionVecVec_numVecs(vv, rank)
        end function
    end interface

! ******************************************************************************
! ** - [IO] - ******************************************************************
! ******************************************************************************

    integer, parameter  :: MODE_MASTER      = 1
    integer, parameter  :: MODE_GLOBAL      = 2
    integer, parameter  :: MODE_DISTRIBUTED = 3

    type File
        !integer(c_size_t) :: unit
        integer :: unit
    end type

    external :: file_open,             &
                file_close,            &
                                    
                file_readInt,          &
                file_readString,       &
                file_readRegion,       &

                file_readIntVec,       &
                file_readIntVecVec,    &
                                    
                file_readRegionVec,    &
                file_readRegionVecVec, &

                print_procColor,       &
                print_color,           &
                print_underline,       &
                print_bold,            &
                print_invertColor,     &
                print_resetColor

! ******************************************************************************
! ** - [GRID_TYPES] - **********************************************************
! ******************************************************************************
    type Neighbor
        type(String)         :: name
        integer, allocatable :: offsets(:)
    end type
    type NeighborPtr
        type(Neighbor), pointer :: val
    end type


    type subgrid
        type(string)                   :: name
        integer                        :: ndims
        integer, allocatable           :: extents(:)
        type(NeighborPtr), allocatable :: neighbors(:)
    end type
    type SubGridPtr
        type(SubGrid), pointer :: val
    end type


    type Grid
        type(String) name;
        integer nDims;
        type(SubGridPtr), allocatable  :: subgrids(:)
        type(Region), allocatable      :: borderSrcRegions(:)
        type(NeighborPtr), allocatable :: borderSrcNeighbors(:)
        type(SubGridPtr),  allocatable :: borderSrcSubgrids(:)
        type(Region), allocatable      :: borderTgtRegions(:)
        type(NeighborPtr), allocatable :: borderTgtNeighbors(:)
        type(SubGridPtr), allocatable  :: borderTgtSubgrids(:)
    end type
    type GridPtr
        type(Grid), pointer :: val
    end type

! ******************************************************************************
! ** - [GRID_MAIN] - ***********************************************************
! ******************************************************************************
    external :: neighbor_print,       &
                neighbor_printSimp,   &
                neighborVec_print,    &
                neighborPtrVec_print

    external :: subgrid_print,       &
                subgrid_printSimp,   &
                subgridPtrVec_print

    external :: grid_print,       &
                grid_printSimp!,   &

                !grid_numSubGrids

    interface
        integer function grid_numSubGrids(self)
        end function
    end interface

! ******************************************************************************
! ** - [DISTRIBUTION_TYPES] - **************************************************
! ******************************************************************************
    type Distribution
        type(string)         :: name
        type(Grid), pointer  :: grd
        integer              :: nProcs
        integer, allocatable :: blockSize(:)
        integer, allocatable :: gbid2lbid(:)
        integer, allocatable :: gbid2proc(:)
        integer, allocatable :: lbid2gbid(:)
        integer, allocatable :: sg2gbid(:)
   end type
    type DistributionPtr
        type(Distribution), pointer :: val
    end type

! ******************************************************************************
! ** - [DISTRIBUTION_MAIN] - ***************************************************
! ******************************************************************************
    external :: distribution_print,       &
                distribution_printSimp,   &

                distribution_pos2blockPos

! ******************************************************************************
! ** - [SCHEDULE] - ************************************************************
! ******************************************************************************
    type Schedule
        Type(String) :: gridID
        Type(String) :: distID
        
        integer, allocatable :: msgRecvFrom(:)
        type(VecVecInt)      :: transferRecvAtLBID
        type(VecVecRegion)   :: transferRegionRecv
        
        integer, allocatable :: msgSendTo(:)
        type(VecVecInt)      :: transferSendFromLBID
        type(VecVecRegion)   :: transferRegionSend
    end type

    external schedule_newFromFile, &
             schedule_input,       &
             schedule_print,       &
             schedule_printSimp

! ******************************************************************************
! ** - [ENVIRONMENT_TYPES] - ***************************************************
! ******************************************************************************
    type Environment
        type(LinkedList), pointer :: neighbors
        type(LinkedList), pointer :: subgrids
        type(LinkedList), pointer :: grids
        type(LinkedList), pointer :: distributions
    end type

    type(Environment) :: env

! ******************************************************************************
! ** - [ENVIRONMENT_GET] - *****************************************************
! ******************************************************************************
    !external environment_getNeighbor,     &
    !         environment_getSubGrid,      &
    !         environment_getGrid,         &
    !         environment_getDistribution, &
    !         environment_getSchedule    

    interface
        function environment_getNeighbor(id)
            import Neighbor !, String
            type(Neighbor), pointer :: environment_getNeighbor
!            type(String), intent(in) :: id
        end function

        function environment_getSubGrid(id)
            import SubGrid !, String
            type(SubGrid), pointer   :: environment_getSubGrid
!            type(String), intent(in) :: id
        end function

        function environment_getGrid(id)
            import Grid, String
            type(Grid), pointer :: environment_getGrid
            type(String), intent(in) :: id
        end function

        function environment_getDistribution(id)
            import Distribution, String
            type(Distribution), pointer :: environment_getDistribution
            type(String), intent(in)    :: id
        end function

        function environment_getSchedule(id)
            import Schedule, String
            type(Schedule), pointer :: environment_getSchedule
            type(String), intent(in)    :: id
        end function
    end interface

! ******************************************************************************
! ** - [GRID_INPUT] - **********************************************************
! ******************************************************************************
    external :: neighbor_input,       &
                neighborvec_input,    &
                neighborptrvec_input

    external :: subgrid_input,       &
                subgridPtrVec_input

    external :: grid_input

! ******************************************************************************
! ** - [DISTRIBUTION_INPUT] - **************************************************
! ******************************************************************************
    external :: distribution_input

! ******************************************************************************
! ** - [ENVIRONMENT_MAIN] - ****************************************************
! ******************************************************************************
!    external environment_init,            &
!             !environment_newNeighbor,     &
!             !environment_newSubGrid,      &
!             !environment_newGrid,         &
!             !environment_newDistribution, &
!             environment_print,           &
!             environment_printSimp,       &
!             environment_input
!
!    interface
!        function environment_newNeighbor()
!        end function
!
!        function environment_newSubGrid()
!        end function
!
!        function environment_newGrid()
!        end function
!
!        function environment_newDistribution()
!        end function
!    end interface


! ******************************************************************************
! ** - [DATA] - ****************************************************************
! ******************************************************************************
    type Data
        type(Grid), pointer         :: grd
        type(Schedule), pointer     :: sched
        type(Distribution), pointer :: dist
        integer, allocatable        :: vals(:, :, :)
    end type

    external data_new,                &
             data_newMedataCopied,    &
             data_print,              &
             data_printSimp,          &
             data_printForProc,       &

             data_input,              &

             !data_apply,             &

             !data_add,                &
             !data_sub,                &
             !data_mult,               &
             !data_pow,                &

             !data_sum,                &

             data_forceUpdate

    interface
        function data_apply(self, func)
            import Data
            type(Data) :: data_apply

            type(Data) :: self
            interface
                integer function func(neighs)
                    integer, intent(in) :: neighs(:)
                end function
            end interface
        end function

        function data_add(lhs, rhs)
            import :: Data
            type(Data) :: data_add
        end function

        function data_sub(lhs, rhs)
            import Data
            type(Data) :: data_sub
        end function

        function data_mult(lhs, rhs)
            import Data
            type(Data) :: data_mult
        end function

        function data_pow(lhs, power)
            import Data
            type(Data) :: data_pow
        end function

        function data_sum(self)
            integer :: data_sum
        end function
    end interface

