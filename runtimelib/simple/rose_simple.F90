# 1 "/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib/simple/simplmXoISg.F90"
# 1 "<command-line>"
# 1 "/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib/simple/simplmXoISg.F90"
PROGRAM Simple
IMPLICIT NONE
include "/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib/simple/gridweaver_header.h"
include "/usr/include/openmpi-x86_64/mpif.h"
INTEGER, PARAMETER :: GRID_WIDTH = 10
INTEGER, PARAMETER :: GRID_HEIGHT = 10
INTEGER :: mpierr
TYPE ( Neighbor )  :: n1, n2
TYPE ( Subgrid )  :: sgA
TYPE ( Grid )  :: g
TYPE ( Distribution )  :: dist
TYPE ( Schedule )  :: sched
TYPE ( DataObj )  :: data_in, data_out
CALL MPI_INIT(mpierr)
CALL gridweaver_initialize()
n1 = neighbor_new("neigh1",1,1)
n2 = neighbor_new("neigh2",-1,-1)
sgA = subgrid_new("sgA",GRID_WIDTH,GRID_HEIGHT)
g = grid_new("g")
CALL grid_addSubgrid(g,sgA)
CALL grid_addBorder(g,0,1,0,GRID_HEIGHT,sgA,GRID_WIDTH,1,GRID_WIDTH,GRID_HEIGHT,sgA,0)
CALL grid_addBorder(g,GRID_WIDTH + 1,1,GRID_WIDTH + 1,GRID_HEIGHT,sgA,1,1,1,GRID_HEIGHT,sgA,0)
dist = distribution_new("dist")
CALL distribution_applyFillBlock(dist,g,5)
sched = schedule_new("sched")
CALL schedule_calculate(sched,g,dist)
CALL environment_print()
CALL schedule_printFortranVersion(sched)
data_in = data_new(sched)
data_out = data_new(sched)
CALL data_print(data_in,6)
CALL data_forceUpdate(data_in)
CALL data_printForProcs(data_in,6)
! <<<--- GRIDWEAVER REPLACED DATA APPLY HERE --->>>
gw____blkW = distribution_width(data_in%dist)
gw____blkH = distribution_height(data_in%dist)
! Iterate over all points in all local blocks
do gw____lbid=lbound(self%vals,3), ubound(self%vals,3)
    do gw____blkJ=1,gw____blkH
        do gw____blkI=1,gw____blkW
      data_out%vals(gw____blkI, gw____blkJ, gw____lbid) = data_in%vals(gw___blkI,gw___blkJ) + 10
        end do
    end do
end do;CALL data_print(data_out,6)
CALL MPI_BARRIER(MPI_COMM_WORLD,mpierr)
CALL MPI_FINALIZE(mpierr)
CONTAINS
include "/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib/simple/gridweaver_footer.h"
REAL FUNCTION addTen(A,idxI,idxJ)
INTEGER, INTENT(IN) :: idxI, idxJ
INTERFACE 
REAL FUNCTION A(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

END INTERFACE 
addTen = A(idxI,idxJ) + 10
END  FUNCTION 

END PROGRAM Simple

