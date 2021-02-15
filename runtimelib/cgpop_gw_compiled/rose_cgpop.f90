!==============================================================================
! Copyright (C) 2010, University Corporation for Atmospheric Research,
!                     Colorado State University,
!                     Los Alamos National Security, LLC,
!                     United States Department of Energy
!
! All rights reserved.  See ../COPYING for copyright details
!==============================================================================
!>
!! This is the main driver for the standalone Parallel Ocean Program (POP) 
!! conjugate gradient solver.
!<
PROGRAM CGpop
    ! !USES:
USE kinds_mod
USE simple_blocks
USE domain_size, ONLY : nx_global , ny_global , max_blocks_tropic
!    use communicate
!    use simple_domain
!    use solvers
USE constants
!    use timers, only: get_timer,timer_start,timer_stop,timer_print_all, &
!        init_timers
!    use mpi2s_boundary, only: &
!        timer_mpi2s_boundary_create, timer_mpi2s_boundary_2d_dbl
!    use check, only: CheckAnswers
    !include '../gridweaver.h'
IMPLICIT NONE
include "/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib/cgpop_gw_compiled/gridweaver_header.h"
    !-----------------------------------------------------------------------
    !
    !  local variables
    !
    !-----------------------------------------------------------------------
    !***************************************************************************
    !&&&&&&&&&&&&&&&&&&&&&&&&& *************************************************
    !&&& START GW ADDITION &&& *************************************************
    !&&&&&&&&&&&&&&&&&&&&&&&&& *************************************************
    !***************************************************************************
    !***************************************************************************
    !&&&&&&&&&&&&&&&&&&&&&&& ***************************************************
    !&&& END GW ADDITION &&& ***************************************************
    !&&&&&&&&&&&&&&&&&&&&&&& ***************************************************
    !***************************************************************************
    !-----------------------------------------------------------------------
    !  initialize message-passing or other communication protocol
    !-----------------------------------------------------------------------
!    call init_communicate
!    nprocs = get_num_procs()
    !-----------------------------------------------------------------------
    !  initialize constants and i/o stuff
    !-----------------------------------------------------------------------
!    call init_constants
!    !-----------------------------------------------------------------------
!    !  initialize domain and grid
!    !-----------------------------------------------------------------------
!    call init_domain_blocks(reorder)
!    call init_domain_distribution(reorder)
!    boundary_exchange_algorithm = ALG_MPI2S_2D
!    call init_solvers(RHS,PRESSI,PRESSF)
!    
!    !***************************************************************************
!    !&&&&&&&&&&&&&&&&&&&&&&&&& *************************************************
!    !&&& START GW ADDITION &&& *************************************************
!    !&&&&&&&&&&&&&&&&&&&&&&&&& *************************************************
!    !***************************************************************************
!    !-----------------------------------------------------------------------
!    !  Set up grid using GridWeaver
!    !-----------------------------------------------------------------------
!    ! Copy the CGPOP distribution into GridWeaver
!    ! Iterate through blocks
!    do i=1,nblocks_tot
!        call get_block_parameter(i, &
!            cgpop_lbid, ib, ie, jb, je, iblock, jblock, npoints)
!        if(npoints > 0) then
!            gw_gbid = distribution_gbidAt(dist, sg,                 &
!                        (iblock-1) * blkW + 1,  &
!                        (jblock-1) * blkH + 1)
!            gw_proc = distrb_tropic%proc(all_blocks(i)%block_id) - 1
!
!            call distribution_setProcForBlock(dist, gw_gbid, gw_proc)
!        end if
!    end do
!
!    call MPI_BARRIER(MPI_COMM_WORLD, ierr)
!
!    call schedule_calculate(sched, g, dist)
!!    call environment_print()
!
!    ! Create data objects
include "/usr/include/openmpi-x86_64/mpif.h"
REAL(kind=r8) :: PRESSI(nx_block,ny_block,max_blocks_tropic), RHS(nx_block,ny_block,max_blocks_tropic), PRESSF(nx_block,ny_block,max_blocks_tropic), PRESS(nx_block,ny_block,max_blocks_tropic)
REAL(kind=r8) :: sum_diff, gdiff
INTEGER(kind=i4) :: timer_mpi2s_solver_1D, timer_mpi2s_solver_2D
INTEGER(kind=i4), POINTER, DIMENSION(:) :: reorder
INTEGER(kind=i4) :: nscan, nstep
INTEGER(kind=i4) :: ierr
INTEGER(kind=i4) :: n, nprocs
INTEGER(kind=i4) :: timer_esolver
INTEGER(kind=i4) :: timer_solver_init
LOGICAL, PARAMETER :: DO_MPI2S_2D = .TRUE.
INTEGER :: i, j, k, blkW, blkH
TYPE ( Subgrid )  :: sg
TYPE ( Grid )  :: g
TYPE ( Distribution )  :: dist
TYPE ( Schedule )  :: sched
TYPE ( DataObj )  :: data_PRESSI, data_RHS, data_PRESSF, data_PRESS
TYPE ( DataObj )  :: data_A0, data_AN, data_AE, data_ANE, data_RCALCT_B
INTEGER :: cgpop_gbid, cgpop_lbid, gw_gbid, gw_proc, gw_lbid
INTEGER :: ib, jb, ie, je, iblock, jblock, npoints
REAL :: sum1, sum2, sum3, sum4, sum5
REAL(kind=r8) :: rTmp1, rTmp2, rTmp3, rTmp4, rTmp5, rTmp6, rTmp7, rTmp8
TYPE ( DataObj )  :: R, S, Q, Z, AZ, WORK0, A0R
TYPE ( DataObj )  :: WORKJMD
TYPE ( DataObj )  :: WORKN1
TYPE ( DataObj )  :: WORKN2
REAL(kind=r8) :: cg_alpha, cg_beta, cg_sigma, cg_delta, cg_rho_old, cg_rho, rr
CALL gridweaver_initialize()
sg = subgrid_new("sg",nx_global,ny_global)
g = grid_new("g")
CALL grid_addSubgrid(g,sg)
dist = distribution_new("dist")
blkW = nx_block - nghost - nghost
blkH = ny_block - nghost - nghost
CALL distribution_applyBlankDist(dist,g,blkW,blkH)
sched = schedule_new("sched")
data_RHS = data_new(sched)
data_PRESSI = data_new(sched)
data_PRESSF = data_new(sched)
data_PRESS = data_new(sched)
data_A0 = data_new(sched)
data_AN = data_new(sched)
data_AE = data_new(sched)
data_ANE = data_new(sched)
data_RCALCT_B = data_new(sched)
R = data_new(sched)
S = data_new(sched)
Q = data_new(sched)
Z = data_new(sched)
AZ = data_new(sched)
WORK0 = data_new(sched)
A0R = data_new(sched)
WORKJMD = data_new(sched)
WORKN1 = data_new(sched)
WORKN2 = data_new(sched)
CONTAINS
include "/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib/cgpop_gw_compiled/gridweaver_footer.h"
!
!    ! Copy CGPOP data into GridWeaver:
!    ! Iterate through CGPOP blocks; find the associated GW block; copy data
!    rTmp2 = 0.0
!    rTmp3 = 0.0
!    do cgpop_gbid=1,nblocks_tot
!        call get_block_parameter(cgpop_gbid, &
!            cgpop_lbid, ib, ie, jb, je, iblock, jblock, npoints)
!        gw_proc = distrb_tropic%proc(all_blocks(cgpop_gbid)%block_id) - 1
!
!        if(gw_proc == my_task) then
!            cgpop_lbid = distrb_tropic%local_block(cgpop_gbid)
!            gw_gbid = distribution_gbidAt(dist, sg,  &
!                        (iblock-1) * blkW + 1,       &
!                        (jblock-1) * blkH + 1)
!            gw_lbid = distribution_gbid2lbid(dist, gw_gbid)
!
!            ib = all_blocks(cgpop_gbid)%ib-1
!            jb = all_blocks(cgpop_gbid)%jb-1
!           
!            do j=1,blkH
!                do i=1,blkW
!                    data_RHS%vals(i,j,gw_lbid) =  RHS(i+ib,j+jb,cgpop_lbid)
!                    data_PRESSI%vals(i,j,gw_lbid) = PRESSI(i+ib,j+jb,cgpop_lbid)
!                    data_PRESSF%vals(i,j,gw_lbid) = PRESSF(i+ib,j+jb,cgpop_lbid)
!                    
!                    data_A0%vals(i,j,gw_lbid) = A0(i+ib,j+jb,cgpop_lbid)
!                    data_AN%vals(i,j,gw_lbid) = AN(i+ib,j+jb,cgpop_lbid)
!                    data_AE%vals(i,j,gw_lbid) = AE(i+ib,j+jb,cgpop_lbid)
!                    data_ANE%vals(i,j,gw_lbid) = ANE(i+ib,j+jb,cgpop_lbid)
!                    data_RCALCT_B%vals(i,j,gw_lbid) = RCALCT_B(i+ib,j+jb,cgpop_lbid)
!                end do
!            end do
!        end if
!    end do
!    !call data_forceUpdate(data_RHS)
!    !call data_forceUpdate(data_PRESSI)
!    !call data_forceUpdate(data_PRESSF)
!    !call data_forceUpdate(data_A0)
!    !call data_forceUpdate(data_AN)
!    !call data_forceUpdate(data_AE)
!    !call data_forceUpdate(data_ANE)
!    !call data_forceUpdate(data_RCALCT_B)
!
!    !call MPI_BARRIER(MPI_COMM_WORLD, ierr)
!    !rTmp1 = data_sum(DATA_RHS)
!    !rTmp2 = data_sum(data_PRESSI)
!    !rTmp3 = data_sum(data_PRESSF)
!
!    !if(my_task == master_task) then
!    !    print *, "RHS    = ", rTmp1
!    !    print *, "PRESSI = ", rTmp2
!    !    print *, "PRESSF = ", rTmp3
!    !end if
!
!
!    !***************************************************************************
!    !&&&&&&&&&&&&&&&&&&&&&&& ***************************************************
!    !&&& END GW ADDITION &&& ***************************************************
!    !&&&&&&&&&&&&&&&&&&&&&&& ***************************************************
!    !***************************************************************************
!
!    !-----------------------------------------------------------------------
!    !  initialize timers and additional communication routines
!    !-----------------------------------------------------------------------
!    call init_timers()
!    nscan = 0
!
!    !-----------------------------------
!    ! 2 sided MPI with 2D data structure
!    !-----------------------------------
!    call get_timer(timer_mpi2s_solver_2D,'MPI2S_2D',1,nprocs)
!
!    call timer_start(timer_mpi2s_solver_2d)
!    do n=1,ntrials
!        call data_copy(data_PRESS, data_PRESSI)
!        call gw_solver(data_RHS, data_PRESS)
!    end do
!
!    if(my_task == master_task) then 
!        write(*,*) 'Number of trials: ',ntrials
!    endif
!    call timer_print_all()
!
!    call exit_message_environment(ierr)
REAL(kind=8) FUNCTION addTen(A,idxI,idxJ)
INTEGER, INTENT(IN) :: idxI, idxJ
INTERFACE 
REAL(kind=8) FUNCTION A(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

END INTERFACE 
addTen = A(idxI,idxJ) + 10
END  FUNCTION 

REAL(kind=8) FUNCTION diagonal_preconditioner(VAL_A,idxI,idxJ)
INTEGER, INTENT(IN) :: idxI, idxJ
INTERFACE 
REAL(kind=8) FUNCTION VAL_A(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

END INTERFACE 
        ! if (A0(i,j,iblock) /= c0) then
        !     A0R(i,j,iblock) = c1/A0(i,j,iblock)
        ! else
        !     A0R(i,j,iblock) = c0
        ! endif
IF (VAL_A(idxI,idxJ) .NE. c0) THEN
diagonal_preconditioner = c1 / VAL_A(idxI,idxJ)
ELSE
diagonal_preconditioner = c0
END IF
END  FUNCTION 

REAL(kind=8) FUNCTION btrop_operator(VAR_X,VAR_A0,VAR_AN,VAR_AE,VAR_ANE,idxI,idxJ)
INTEGER, INTENT(IN) :: idxI, idxJ
INTERFACE 
REAL(kind=8) FUNCTION VAR_X(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_OLD_X(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_A0(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_AN(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_AE(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_ANE(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

END INTERFACE 
btrop_operator = VAR_A0(idxI,idxJ) * VAR_X(idxI,idxJ) + VAR_AN(idxI,idxJ) * VAR_X(idxI,idxJ + 1) + VAR_AN(idxI,idxJ - 1) * VAR_X(idxI,idxJ - 1) + VAR_AE(idxI,idxJ) * VAR_X(idxI + 1,idxJ) + VAR_AE(idxI - 1,idxJ) * VAR_X(idxI - 1,idxJ) + VAR_ANE(idxI,idxJ) * VAR_X(idxI + 1,idxJ + 1) + VAR_ANE(idxI,idxJ - 1) * VAR_X(idxI + 1,idxJ - 1) + VAR_ANE(idxI - 1,idxJ) * VAR_X(idxI - 1,idxJ + 1) + VAR_ANE(idxI - 1,idxJ - 1) * VAR_X(idxI - 1,idxJ - 1)
END  FUNCTION 

REAL(kind=8) FUNCTION b_minus_Ax(VAR_B,VAR_S,idxI,idxJ)
INTEGER, INTENT(IN) :: idxI, idxJ
INTERFACE 
REAL(kind=8) FUNCTION VAR_B(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_S(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

END INTERFACE 
b_minus_Ax = VAR_B(idxI,idxJ) - VAR_S(idxI,idxJ)
END  FUNCTION 

REAL(kind=8) FUNCTION square(VAR,idxI,idxJ)
INTEGER, INTENT(IN) :: idxI, idxJ
INTERFACE 
REAL(kind=8) FUNCTION VAR(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

END INTERFACE 
square = VAR(idxI,idxJ) * VAR(idxI,idxJ)
END  FUNCTION 

REAL(kind=8) FUNCTION compute_s_and_q(VAR_Z,VAR_S,VAR_AZ,VAR_Q,VAR_X,VAR_R,idxI,idxJ)
INTEGER, INTENT(IN) :: idxI, idxJ
INTERFACE 
REAL(kind=8) FUNCTION VAR_Z(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_S(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_AZ(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_Q(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_X(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

REAL(kind=8) FUNCTION VAR_R(x,y)
INTEGER, INTENT(IN) :: x, y
END  FUNCTION 

END INTERFACE 
REAL(kind=8) :: tmp
tmp = VAR_Z(idxI,idxJ) + cg_beta * VAR_S(idxI,idxJ)
tmp = tmp + VAR_AZ(idxI,idxJ) + cg_beta * VAR_Q(idxI,idxJ)
tmp = tmp + VAR_X(idxI,idxJ) + cg_alpha * VAR_S(idxI,idxJ)
tmp = tmp + VAR_R(idxI,idxJ) - cg_alpha * VAR_Q(idxI,idxJ)
compute_s_and_q = tmp
END  FUNCTION 

SUBROUTINE gw_solver(X,B)
TYPE ( DataObj ) , INTENT(INOUT) :: X, B
        !-------------------------------------------------------------------
CHARACTER(len=char_len) :: noconvrg
INTEGER(kind=i4) :: i, j, m, iblock
        !real (r8), save, dimension(nx_block,ny_block,max_blocks_tropic) :: &
        !    R,            & ! residual (b-Ax)
        !    S,            & ! conjugate direction vector
        !    Q,Z,AZ,WORK0, & ! various cg intermediate results
        !    A0R             ! diagonal preconditioner
        !type(DataObj) :: R, S, Q, Z, AZ, WORK0, A0R
REAL(kind=r8) :: rr0
        !real (r8), dimension(nx_block,ny_block,max_blocks_tropic) :: WORKJMD
        !type(DataObj) :: WORKJMD
        !real (r8), dimension(nx_block,ny_block,2,max_blocks_tropic) :: &
        !    WORKN              ! WORK array
        !type(DataObj) :: WORKN
REAL(kind=r8), DIMENSION(2) :: sumN
INTEGER(kind=i4) :: gid
        !-------------------------------------------------------------------
        !
        !  initialize some scalars
        !
        !-------------------------------------------------------------------
cg_rho = c1
        !solv_sum_iters = solv_max_iters
        !-------------------------------------------------------------------
        !
        !  compute initial residual and initialize other arrays
        !
        !-------------------------------------------------------------------
        !do iblock=1,nblocks_tropic
        !    gid = blocks_tropic(iblock)
        !    R    (:,:,iblock) = c0
        !    S    (:,:,iblock) = c0
        !    Z    (:,:,iblock) = c0
        !    Q    (:,:,iblock) = c0
        !    AZ   (:,:,iblock) = c0
        !    WORK0(:,:,iblock) = c0
        !    WORKN(:,:,:,iblock) = c0
CALL data_initializeToVal(R,c0)
CALL data_initializeToVal(S,c0)
CALL data_initializeToVal(Z,c0)
CALL data_initializeToVal(Q,c0)
CALL data_initializeToVal(AZ,c0)
CALL data_initializeToVal(WORK0,c0)
CALL data_initializeToVal(WORKN1,c0)
CALL data_initializeToVal(WORKN2,c0)
! <<<--- GRIDWEAVER REPLACED DATA APPLY HERE --->>>
gw____blkW = distribution_width(data_A0%dist)
gw____blkH = distribution_height(data_A0%dist)
! Iterate over all points in all local blocks
do gw____lbid=lbound(self%vals,3), ubound(self%vals,3)
    do gw____blkJ=1,gw____blkH
        do gw____blkI=1,gw____blkW
        ! if (A0(i,j,iblock) /= c0) then
        !     A0R(i,j,iblock) = c1/A0(i,j,iblock)
        ! else
        !     A0R(i,j,iblock) = c0
        ! endif
      IF (data_A0%vals(gw___blkI,gw___blkJ) .NE. c0) THEN
      A0R%vals(gw____blkI, gw____blkJ, gw____lbid) = c1 / data_A0%vals(gw___blkI,gw___blkJ)
      ELSE
      A0R%vals(gw____blkI, gw____blkJ, gw____lbid) = c0
      END IF
        end do
    end do
end do
! <<<--- END OF GRIDWEAVER REPLACEMENT --->>>
;
! <<<--- GRIDWEAVER REPLACED DATA APPLY HERE --->>>
gw____blkW = distribution_width(X%dist)
gw____blkH = distribution_height(X%dist)
! Iterate over all points in all local blocks
do gw____lbid=lbound(self%vals,3), ubound(self%vals,3)
    do gw____blkJ=1,gw____blkH
        do gw____blkI=1,gw____blkW
      S%vals(gw____blkI, gw____blkJ, gw____lbid) = data_A0%vals(idxI,idxJ) * X%vals(idxI,idxJ) + data_AN%vals(idxI,idxJ) * X%vals(idxI,idxJ + 1) + data_AN%vals(idxI,idxJ - 1) * X%vals(idxI,idxJ - 1) + data_AE%vals(idxI,idxJ) * X%vals(idxI + 1,idxJ) + data_AE%vals(idxI - 1,idxJ) * X%vals(idxI - 1,idxJ) + data_AN%valsE(idxI,idxJ) * X%vals(idxI + 1,idxJ + 1) + data_AN%valsE(idxI,idxJ - 1) * X%vals(idxI + 1,idxJ - 1) + data_AN%valsE(idxI - 1,idxJ) * X%vals(idxI - 1,idxJ + 1) + data_AN%valsE(idxI - 1,idxJ - 1) * X%vals(idxI - 1,idxJ - 1)
        end do
    end do
end do
! <<<--- END OF GRIDWEAVER REPLACEMENT --->>>
;
! <<<--- GRIDWEAVER REPLACED DATA APPLY HERE --->>>
gw____blkW = distribution_width(B%dist)
gw____blkH = distribution_height(B%dist)
! Iterate over all points in all local blocks
do gw____lbid=lbound(self%vals,3), ubound(self%vals,3)
    do gw____blkJ=1,gw____blkH
        do gw____blkI=1,gw____blkW
      R%vals(gw____blkI, gw____blkJ, gw____lbid) = B%vals(gw___blkJ,idxJ) - S%vals(gw___blkJ,idxJ)
        end do
    end do
end do
! <<<--- END OF GRIDWEAVER REPLACEMENT --->>>
;
! <<<--- GRIDWEAVER REPLACED DATA APPLY HERE --->>>
gw____blkW = distribution_width(R%dist)
gw____blkH = distribution_height(R%dist)
! Iterate over all points in all local blocks
do gw____lbid=lbound(self%vals,3), ubound(self%vals,3)
    do gw____blkJ=1,gw____blkH
        do gw____blkI=1,gw____blkW
      WORKJMD%vals(gw____blkI, gw____blkJ, gw____lbid) = R%vals(gw___blkI,gw___blkJ) * R%vals(gw___blkI,gw___blkJ)
        end do
    end do
end do
! <<<--- END OF GRIDWEAVER REPLACEMENT --->>>
;
! <<<--- GRIDWEAVER REPLACED DATA APPLY HERE --->>>
gw____blkW = distribution_width(Z%dist)
gw____blkH = distribution_height(Z%dist)
! Iterate over all points in all local blocks
do gw____lbid=lbound(self%vals,3), ubound(self%vals,3)
    do gw____blkJ=1,gw____blkH
        do gw____blkI=1,gw____blkW
      tmp = Z%vals(idxI,idxJ) + cg_beta * S%vals(idxI,idxJ)
      tmp = tmp + AZ%vals(idxI,idxJ) + cg_beta * Q%vals(idxI,idxJ)
      tmp = tmp + X%vals(idxI,idxJ) + cg_alpha * S%vals(idxI,idxJ)
      tmp = tmp + R%vals(idxI,idxJ) - cg_alpha * Q%vals(idxI,idxJ)
      Z%vals(gw____blkI, gw____blkJ, gw____lbid) = tmp
        end do
    end do
end do
! <<<--- END OF GRIDWEAVER REPLACEMENT --->>>
;
!        end do iter_loop
!
!        !rms_residual = sqrt(rr*resid_norm)
END SUBROUTINE gw_solver

END PROGRAM CGpop

