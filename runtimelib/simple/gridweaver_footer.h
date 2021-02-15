! Tell VIM to hihlight: vim: syntax=fortran

!*******************************************************************************
!NEIGHBOR FUNCTIONS:
!   public :: neighbor_new

function neighbor_new(name, x, y)
    character(len=*)    :: name
    integer, intent(in) :: x, y
    type(Neighbor)      :: neighbor_new
end function

!*******************************************************************************
!SUBGRID FUNCTIONS:
!    public :: subgrid_new,      &
!              subgrid_width,    &
!              subgrid_height

function subgrid_new(sg, width, height)
    character(len=*), intent(in) :: sg
    integer, intent(in)          :: width, height
    type(Subgrid)                :: subgrid_new
end function

integer function subgrid_width(sg)
    type(Subgrid)   :: sg
end function

integer function subgrid_height(sg)
    type(Subgrid)   :: sg
end function

!*******************************************************************************
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

function grid_new(g)
    character(len=*), intent(in) :: g
    type(Grid)                   :: grid_new
end function

integer function grid_numSubgrids(g)
    type(Grid), intent(in) :: g
    integer                :: ret
end function

function grid_getSubgrid(g, idx)
    type(Subgrid) :: grid_getSubgrid
    type(Grid), intent(in) :: g
    integer                :: idx 
end function

integer function grid_orient0(g)
    type(Grid), intent(in) :: g
end function

integer function grid_orient45(g)
    type(Grid), intent(in) :: g
end function

integer function grid_orient90(g)
    type(Grid), intent(in) :: g
end function

integer function grid_orient135(g)
    type(Grid), intent(in) :: g
end function

integer function grid_orient180(g)
    type(Grid), intent(in) :: g
end function

integer function grid_orient225(g)
    type(Grid), intent(in) :: g
end function

integer function grid_orient315(g)
    type(Grid), intent(in) :: g
end function

integer function grid_orient360(g)
    type(Grid), intent(in) :: g
end function

!*******************************************************************************
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

function distribution_new(dist)
    character(len=*), intent(in) :: dist
    type(Distribution)           :: distribution_new
end function

!*******************************************************************************
! SCHEDULE FUNCTIONS:
!
!   public :: schedule_new
!       schedule_sendRegionSize,         &
!       schedule_recvRegionSize

function schedule_new(name)
    character(len=*), intent(in) :: name
    type(Schedule)               :: schedule_new
end function


!*******************************************************************************
! DATA FUNCTIONS:
!
!   public :: data_new

function data_new(sched)
        type(DataObj) :: data_new
        type(Schedule), target, intent(in) :: sched
end function

