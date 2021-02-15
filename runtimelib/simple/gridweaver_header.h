! Tell VIM to hihlight: vim: syntax=fortran

! ******************************************************************************
! ** - [GRIDWEAVER VARIABLES] - ************************************************
! ******************************************************************************

integer :: gw____lbid, gw____blkI, gw____blkJ
integer :: gw____blkW, gw____blkH

! ******************************************************************************
! ** - [STRING] - **************************************************************
! ******************************************************************************
type String
    integer             :: len
    character(len=80)   :: val
end type

external :: string_new,              &
            string_init,             &
            string_init_from_cstr,   &
            string_size,             &
            string_append,           &
            string_equals,           &
            string_bcast,            &
            tstr,                    &
            cstr

! **************************************************************************
! ** - [GRID_TYPES] - ******************************************************
! **************************************************************************
type Neighbor
    type(String) :: id
end type

type Subgrid
    type(String) :: id
end type

type Grid
    type(String) :: id
end type

type Distribution
    type(String) :: id
end type

external :: gridweaver_initialize

external :: turnOffSyntaxHighlighting

!NEIGHBOR FUNCTIONS:
!   public :: neighbor_new

!SUBGRID FUNCTIONS:
!    public :: subgrid_new,      &
!              subgrid_width,    &
!              subgrid_height

!GRID FUNCTIONS   
!   public :: grid_new,                         &
!             grid_numSubGrids,                 &
!             grid_getSubgrid,                  &
!             grid_orient0,                     &
!             grid_orient45,                    &
!             grid_orient90,                    &
!             grid_orient135,                   &
!             grid_orient180,                   &
!             grid_orient225,                   &
!             grid_orient315,                   &
!             grid_orient360                   

external :: grid_addSubgrid,                  &
            grid_addBorder
            !grid_placeAdjacentLR,             &
            !grid_placeAdjacentRL,             &
            !grid_placeAdjacentTB,             &
            !grid_placeAdjacentBT,             &
            !grid_connectTtoB,                 &
            !grid_connectRtoL,                 &
            !grid_connectBtoT,                 &
            !grid_connectLtoR,                 &
            !grid_connectLtoT,                 &
            !grid_connectLtoB,                 &
            !grid_connectRtoT,                 &
            !grid_connectRtoB,                 &
            !grid_connectTtoL,                 &
            !grid_connectTtoR,                 &
            !grid_connectBtoL,                 &
            !grid_connectBtoR,                 &
            !grid_wrapLR,                      &
            !grid_wrapTB,                      &
            !grid_placeAdjacentWithOffsetLR,   &
            !grid_placeAdjacentWithOffsetRL,   &
            !grid_placeAdjacentWithOffsetTB,   &
            !grid_placeAdjacentWithOffsetBT,   &
            !grid_mirrorT,                     &
            !grid_mirrorB,                     &
            !grid_mirrorL,                     &
            !grid_mirrorR,                     &
            !grid_foldT,                       &
            !grid_foldB,                       &
            !grid_foldL,                       &
            !grid_foldR,                       &

! DISTRIBUTION FUNCTIONS:
!
!    public :: distribution_new,                 &
!              distribution_width,               &
!              distribution_height,              &
!              distribution_numLocalBlocks,      &
!              distribution_lbid2gbid,           &
!              distribution_gbid2lbid,           &
!              distribution_gbid2proc,           &
!              distribution_blockLowX,           &
!              distribution_blockLowY,           &
!              distribution_blockHighX,          &
!              distribution_blockHighY,          &
!              distribution_pos2BlockPos


external :: distribution_applyFillBlock,      &
            distribution_applyBlockFill,      &
            distribution_applyBlockCyclic,    &
            distribution_visualize

external :: environment_print,      &
            environment_output,     &
            environment_input,      &
            environment_clear

! ******************************************************************************
! ** - [SCHEDULE] - ************************************************************
! ******************************************************************************

type Schedule
end type

! SCHEDULE FUNCTIONS:
!
!   public :: schedule_new
!       schedule_sendRegionSize,         &
!       schedule_recvRegionSize

external :: schedule_calculate,             &
            schedule_transferToFortran,     &
            schedule_printFortranVersion

! ******************************************************************************
! ** - [DATA] - ****************************************************************
! ******************************************************************************

type DataObj
    type(Distribution) dist
end type

! DATA FUNCTIONS:
!
!   public :: data_new

external :: data_print,              &
            data_printForProcs,      &
            data_printForProc,       &
            data_apply1,             &
            data_forceUpdate

