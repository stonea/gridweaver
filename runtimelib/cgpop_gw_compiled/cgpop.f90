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
program CGpop
    ! !USES:
    use kinds_mod
    use simple_blocks
    use domain_size, only: nx_global,ny_global, max_blocks_tropic
    use communicate
    use simple_domain
    use solvers
    use constants
    use timers, only: get_timer,timer_start,timer_stop,timer_print_all, &
        init_timers
    use mpi2s_boundary, only: &
        timer_mpi2s_boundary_create, timer_mpi2s_boundary_2d_dbl
    use check, only: CheckAnswers

    include '../gridweaver.h'

    implicit none
    include 'mpif.h'

    real(r8), dimension(nx_block,ny_block,max_blocks_tropic) :: &
        PRESSI, RHS, PRESSF, PRESS

    real(r8) :: sum_diff,gdiff

    integer(i4) :: timer_mpi2s_solver_1D, timer_mpi2s_solver_2D

    integer(i4), pointer :: reorder(:)

    !-----------------------------------------------------------------------
    !
    !  local variables
    !
    !-----------------------------------------------------------------------
    integer (i4) :: nscan,nstep
    integer (i4) :: ierr
    integer (i4) :: n,nprocs

    integer(i4) :: timer_esolver
    integer(i4) :: timer_solver_init

    logical, parameter :: DO_MPI2S_2D = .TRUE.

    !***************************************************************************
    !&&&&&&&&&&&&&&&&&&&&&&&&& *************************************************
    !&&& START GW ADDITION &&& *************************************************
    !&&&&&&&&&&&&&&&&&&&&&&&&& *************************************************
    !***************************************************************************

    integer :: i, j, k, blkW, blkH
    type(Subgrid)      :: sg
    type(Grid)         :: g
    type(Distribution) :: dist
    type(Schedule)     :: sched
    type(DataObj)      :: data_PRESSI, data_RHS, data_PRESSF, data_PRESS
    type(DataObj)      :: data_A0, data_AN, data_AE, data_ANE, data_RCALCT_B
    integer :: cgpop_gbid, cgpop_lbid, gw_gbid, gw_proc, gw_lbid
    integer :: ib, jb, ie, je, iblock, jblock, npoints
    real :: sum1, sum2, sum3, sum4, sum5
    real(r8) :: rTmp1, rTmp2, rTmp3, rTmp4, rTmp5, rTmp6, rTmp7, rTmp8

    type(DataObj) :: R, S, Q, Z, AZ, WORK0, A0R
    type(DataObj) :: WORKJMD
    type(DataObj) :: WORKN1
    type(DataObj) :: WORKN2

    real (r8) :: & ! scalar results
        cg_alpha, cg_beta, cg_sigma, cg_delta, cg_rho_old, cg_rho, rr

    !***************************************************************************
    !&&&&&&&&&&&&&&&&&&&&&&& ***************************************************
    !&&& END GW ADDITION &&& ***************************************************
    !&&&&&&&&&&&&&&&&&&&&&&& ***************************************************
    !***************************************************************************

    !-----------------------------------------------------------------------
    !  initialize message-passing or other communication protocol
    !-----------------------------------------------------------------------
    call init_communicate
    nprocs = get_num_procs()

    !-----------------------------------------------------------------------
    !  initialize constants and i/o stuff
    !-----------------------------------------------------------------------
    call init_constants

    !-----------------------------------------------------------------------
    !  initialize domain and grid
    !-----------------------------------------------------------------------
    call init_domain_blocks(reorder)
    call init_domain_distribution(reorder)
    boundary_exchange_algorithm = ALG_MPI2S_2D
    call init_solvers(RHS,PRESSI,PRESSF)
    
    !***************************************************************************
    !&&&&&&&&&&&&&&&&&&&&&&&&& *************************************************
    !&&& START GW ADDITION &&& *************************************************
    !&&&&&&&&&&&&&&&&&&&&&&&&& *************************************************
    !***************************************************************************

    !-----------------------------------------------------------------------
    !  Set up grid using GridWeaver
    !-----------------------------------------------------------------------
    call gridweaver_initialize()
    sg = subgrid_new("sg", nx_global, ny_global)
    g = grid_new("g")
    call grid_addSubgrid(g, sg)
    dist = distribution_new("dist")
    blkW = nx_block - nghost - nghost
    blkH = ny_block - nghost - nghost
    call distribution_applyBlankDist(dist, g, blkW, blkH)
    sched = schedule_new("sched")

    ! Copy the CGPOP distribution into GridWeaver
    ! Iterate through blocks
    do i=1,nblocks_tot
        call get_block_parameter(i, &
            cgpop_lbid, ib, ie, jb, je, iblock, jblock, npoints)
        if(npoints > 0) then
            gw_gbid = distribution_gbidAt(dist, sg,                 &
                        (iblock-1) * blkW + 1,  &
                        (jblock-1) * blkH + 1)
            gw_proc = distrb_tropic%proc(all_blocks(i)%block_id) - 1

            call distribution_setProcForBlock(dist, gw_gbid, gw_proc)
        end if
    end do

    call MPI_BARRIER(MPI_COMM_WORLD, ierr)

    call schedule_calculate(sched, g, dist)
    !call environment_print()

    ! Create data objects
    data_RHS    = data_new(sched)
    data_PRESSI = data_new(sched)
    data_PRESSF = data_new(sched)
    data_PRESS  = data_new(sched)

    data_A0 = data_new(sched)
    data_AN = data_new(sched)
    data_AE = data_new(sched)
    data_ANE = data_new(sched)
    data_RCALCT_B = data_new(sched)

    R       = data_new(sched)
    S       = data_new(sched)
    Q       = data_new(sched)
    Z       = data_new(sched)
    AZ      = data_new(sched)
    WORK0   = data_new(sched)
    A0R     = data_new(sched)
    WORKJMD = data_new(sched)
    WORKN1  = data_new(sched)
    WORKN2  = data_new(sched)

    ! Copy CGPOP data into GridWeaver:
    ! Iterate through CGPOP blocks; find the associated GW block; copy data
    do cgpop_gbid=1,nblocks_tot
        call get_block_parameter(cgpop_gbid, &
            cgpop_lbid, ib, ie, jb, je, iblock, jblock, npoints)
        gw_proc = distrb_tropic%proc(all_blocks(cgpop_gbid)%block_id) - 1

        if(gw_proc == my_task) then
            cgpop_lbid = distrb_tropic%local_block(cgpop_gbid)
            gw_gbid = distribution_gbidAt(dist, sg,  &
                        (iblock-1) * blkW + 1,       &
                        (jblock-1) * blkH + 1)
            gw_lbid = distribution_gbid2lbid(dist, gw_gbid)

            ib = all_blocks(cgpop_gbid)%ib-1
            jb = all_blocks(cgpop_gbid)%jb-1
           
            do j=1,blkH
                do i=1,blkW
                    data_RHS%vals(i,j,gw_lbid) =  RHS(i+ib,j+jb,cgpop_lbid)
                    data_PRESSI%vals(i,j,gw_lbid) = PRESSI(i+ib,j+jb,cgpop_lbid)
                    data_PRESSF%vals(i,j,gw_lbid) = PRESSF(i+ib,j+jb,cgpop_lbid)
                    
                    data_A0%vals(i,j,gw_lbid) = A0(i+ib,j+jb,cgpop_lbid)
                    data_AN%vals(i,j,gw_lbid) = AN(i+ib,j+jb,cgpop_lbid)
                    data_AE%vals(i,j,gw_lbid) = AE(i+ib,j+jb,cgpop_lbid)
                    data_ANE%vals(i,j,gw_lbid) = ANE(i+ib,j+jb,cgpop_lbid)
                    data_RCALCT_B%vals(i,j,gw_lbid) = RCALCT_B(i+ib,j+jb,cgpop_lbid)
                end do
            end do
        end if
    end do
    call data_forceUpdate(data_RHS)
    call data_forceUpdate(data_PRESSI)
    call data_forceUpdate(data_PRESSF)
    call data_forceUpdate(data_A0)
    call data_forceUpdate(data_AN)
    call data_forceUpdate(data_AE)
    call data_forceUpdate(data_ANE)
    call data_forceUpdate(data_RCALCT_B)

    !***************************************************************************
    !&&&&&&&&&&&&&&&&&&&&&&& ***************************************************
    !&&& END GW ADDITION &&& ***************************************************
    !&&&&&&&&&&&&&&&&&&&&&&& ***************************************************
    !***************************************************************************

    !-----------------------------------------------------------------------
    !  initialize timers and additional communication routines
    !-----------------------------------------------------------------------
    call init_timers()
    nscan = 0

    !-----------------------------------
    ! 2 sided MPI with 2D data structure
    !-----------------------------------
    call get_timer(timer_mpi2s_solver_2D,'MPI2S_2D',1,nprocs)

    call timer_start(timer_mpi2s_solver_2d)
    do n=1,ntrials
        call data_copy(data_PRESS, data_PRESSI)
        call gw_solver(data_RHS, data_PRESS)
    end do

    if(my_task == master_task) then 
        write(*,*) 'Number of trials: ',ntrials
    endif
    call timer_print_all()

    call exit_message_environment(ierr)

  contains    
    real(8) function diagonal_preconditioner(VAL_A, idxI, idxJ)
        integer, intent(in) :: idxI, idxJ
        interface
            real(8) function VAL_A(x,y)
                integer, intent(in) :: x,y
            end function
        end interface

        ! if (A0(i,j,iblock) /= c0) then
        !     A0R(i,j,iblock) = c1/A0(i,j,iblock)
        ! else
        !     A0R(i,j,iblock) = c0
        ! endif
        if(VAL_A(idxI, idxJ) /= c0) then
            diagonal_preconditioner = c1 / VAL_A(idxI, idxJ)
        else
            diagonal_preconditioner = c0
        end if
    end function

    real(8) function btrop_operator(VAR_X, VAR_A0, VAR_AN, VAR_AE, VAR_ANE, idxI, idxJ)
        integer, intent(in) :: idxI, idxJ
        interface
            real(8) function VAR_X(x,y); integer, intent(in) :: x,y; end function
            real(8) function VAR_OLD_X(x,y); integer, intent(in) :: x,y; end function
            real(8) function VAR_A0(x,y); integer, intent(in) :: x,y; end function
            real(8) function VAR_AN(x,y); integer, intent(in) :: x,y; end function
            real(8) function VAR_AE(x,y); integer, intent(in) :: x,y; end function
            real(8) function VAR_ANE(x,y); integer, intent(in) :: x,y; end function
        end interface

        btrop_operator = &
            VAR_A0 (idxI  ,idxJ  ) * VAR_X(idxI  ,idxJ  ) + &
            VAR_AN (idxI  ,idxJ  ) * VAR_X(idxI  ,idxJ+1) + &
            VAR_AN (idxI  ,idxJ-1) * VAR_X(idxI  ,idxJ-1) + &
            VAR_AE (idxI  ,idxJ  ) * VAR_X(idxI+1,idxJ  ) + &
            VAR_AE (idxI-1,idxJ  ) * VAR_X(idxI-1,idxJ  ) + &
            VAR_ANE(idxI  ,idxJ  ) * VAR_X(idxI+1,idxJ+1) + &
            VAR_ANE(idxI  ,idxJ-1) * VAR_X(idxI+1,idxJ-1) + &
            VAR_ANE(idxI-1,idxJ  ) * VAR_X(idxI-1,idxJ+1) + &
            VAR_ANE(idxI-1,idxJ-1) * VAR_X(idxI-1,idxJ-1)
    end function


    real(8) function subtract(VAR_B, VAR_S, idxI, idxJ)
        integer, intent(in) :: idxI, idxJ
        interface
            real(8) function VAR_B(x,y); integer, intent(in) :: x,y; end function
            real(8) function VAR_S(x,y); integer, intent(in) :: x,y; end function
        end interface

        subtract = VAR_B(idxI, idxJ) - VAR_S(idxI, idxJ)
    end function

    real(8) function square(VAR, idxI, idxJ)
        integer, intent(in) :: idxI, idxJ
        interface
            real(8) function VAR(x,y); integer, intent(in) :: x,y; end function
        end interface

        square = VAR(idxI, idxJ) * VAR(idxI, idxJ)
    end function

    real(8) function mult(LHS, RHS, idxI, idxJ)
        integer, intent(in) :: idxI, idxJ
        interface
            real(8) function LHS(x,y); integer, intent(in) :: x,y; end function
            real(8) function RHS(x,y); integer, intent(in) :: x,y; end function
        end interface

        mult = LHS(idxI, idxJ) * RHS(idxI, idxJ)
    end function



    real(8) function compute_s_and_q( &
        VAR_Z,VAR_S,VAR_AZ,VAR_Q,VAR_X,VAR_R,idxI,idxJ)
    !`
        integer, intent(in) :: idxI, idxJ
        interface
            real(8) function  VAR_Z(x,y); integer, intent(in) :: x,y; end function
            real(8) function  VAR_S(x,y); integer, intent(in) :: x,y; end function
            real(8) function VAR_AZ(x,y); integer, intent(in) :: x,y; end function
            real(8) function  VAR_Q(x,y); integer, intent(in) :: x,y; end function
            real(8) function  VAR_X(x,y); integer, intent(in) :: x,y; end function
            real(8) function  VAR_R(x,y); integer, intent(in) :: x,y; end function
        end interface
        real(8) :: tmp

        tmp =       VAR_Z(idxI,idxJ)  + cg_beta * VAR_S(idxI,idxJ)
        tmp = tmp + VAR_AZ(idxI,idxJ) + cg_beta * VAR_Q(idxI,idxJ)
        tmp = tmp + VAR_X(idxI,idxJ)  + cg_alpha* VAR_S(idxI,idxJ)
        tmp = tmp + VAR_R(idxI,idxJ)  - cg_alpha* VAR_Q(idxI,idxJ)

        compute_s_and_q = tmp
    end function

    subroutine gw_solver(X, B)
        type(DataObj), intent(inout) :: X, B

        !-------------------------------------------------------------------
        character (char_len) :: &
            noconvrg           ! error message for no convergence

        integer (i4) :: &
            i,j,m,      & ! local iteration counter
            iblock        ! local block     counter

        !real (r8), save, dimension(nx_block,ny_block,max_blocks_tropic) :: &
        !    R,            & ! residual (b-Ax)
        !    S,            & ! conjugate direction vector
        !    Q,Z,AZ,WORK0, & ! various cg intermediate results
        !    A0R             ! diagonal preconditioner
        !type(DataObj) :: R, S, Q, Z, AZ, WORK0, A0R
        real (r8) :: rr0

        !real (r8), dimension(nx_block,ny_block,max_blocks_tropic) :: WORKJMD
        !type(DataObj) :: WORKJMD

        !real (r8), dimension(nx_block,ny_block,2,max_blocks_tropic) :: &
        !    WORKN              ! WORK array
        !type(DataObj) :: WORKN

        real (r8), dimension(2) :: &
            sumN               ! global sum results for multiple arrays

        integer (i4) :: gid

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
        call data_initializeToVal(R, c0)
        call data_initializeToVal(S, c0)
        call data_initializeToVal(Z, c0)
        call data_initializeToVal(Q, c0)
        call data_initializeToVal(AZ, c0)
        call data_initializeToVal(WORK0, c0)
        call data_initializeToVal(WORKN1, c0)
        call data_initializeToVal(WORKN2, c0)

        !--- diagonal preconditioner if preconditioner not specified
        !do j=1,ny_block
        !    do i=1,nx_block
        !        if (A0(i,j,iblock) /= c0) then
        !            A0R(i,j,iblock) = c1/A0(i,j,iblock)
        !        else
        !            A0R(i,j,iblock) = c0
        !        endif
        !    end do
        !end do
        call data_apply1(A0R, data_A0, diagonal_preconditioner)

        ! use S as a temp here for Ax
        !    call btrop_operator(S,X,gid,iblock)
        !    R(:,:,iblock) = B(:,:,iblock) - S(:,:,iblock) ! b-Ax
        !    WORKJMD(:,:,iblock) = R(:,:,iblock)*R(:,:,iblock)
        call data_apply5(S, X, data_A0, data_AN, data_AE, data_ANE, btrop_operator)
        call data_apply2(R, B, S, subtract)
        call data_apply1(WORKJMD, R, square)
        !rr0 = sqrt( &
        !    global_sum(WORKJMD, distrb_tropic, field_loc_center, RCALCT_B))
        rr0 = sqrt(data_sum(WORKJMD))

        !call update_ghost_cells(R, bndy_tropic, field_loc_center, &
        !    field_type_scalar)
        call data_forceUpdate(R)

        !-------------------------------------------------------------------
        !
        !    take one pass of standard algorithm
        !
        !-------------------------------------------------------------------

        ! do iblock=1,nblocks_tropic
        !    gid = blocks_tropic(iblock)
        !
        !    !---- calculate (PC)r store in Z
        !    Z(:,:,iblock) = R(:,:,iblock)*A0R(:,:,iblock)
        !
        !    !---- Compute intermediate result for dot product
        !    WORKN(:,:,1,iblock) = R(:,:,iblock)*Z(:,:,iblock)
        !
        !    !---- update conjugate direction vector S
        !    S(:,:,iblock) =  Z(:,:,iblock)
        !
        !    !---- compute Q = A * S
        !    call btrop_operator(Q,S,gid,iblock)
        !
        !    !---- compute intermediate result for dot product
        !    WORKN(:,:,2,iblock) = S(:,:,iblock)*Q(:,:,iblock)
        ! end do
        call data_apply2(Z, R, A0R, mult)
        call data_apply2(WORKN1, R, Z, mult)
        call data_copy(S,R)
        call data_apply5(Q, S, data_A0, data_AN, data_AE, data_ANE, &
                btrop_operator)
        call data_apply2(WORKN2, S, Q, mult)

        !call update_ghost_cells(Q, bndy_tropic, field_loc_center, &
        !    field_type_scalar)
        call data_forceUpdate(Q)

        !---- Form dot products
        ! sumN = global_sum(WORKN, distrb_tropic, field_loc_center, RCALCT_B)
        ! 
        cg_rho_old = sumN(1) !(r,PCr)
        cg_sigma   = sumN(2) !(s,As)
        cg_alpha   = cg_rho_old/cg_sigma
 
        !---- compute first solution and residual
 
        ! do iblock=1,nblocks_tropic
        !     X(:,:,iblock) = X(:,:,iblock) + cg_alpha*S(:,:,iblock)
        !     R(:,:,iblock) = R(:,:,iblock) - cg_alpha*Q(:,:,iblock)
        ! end do
        call data_apply2(X, X, S, mult)
        call data_apply2(R, R, Q, mult)

        !-------------------------------------------------------------------
        !
        !     iterate
        !
        !-------------------------------------------------------------------
        iter_loop: do m = 1, solv_max_iters

            !---------------------------------------------------------------
            !
            !     calculate (PC)r and A*(Pc)r
            !
            !---------------------------------------------------------------
            !do iblock=1,nblocks_tropic
            !    gid = blocks_tropic(iblock)

            !    Z(:,:,iblock) = R(:,:,iblock)*A0R(:,:,iblock)

            !    call btrop_operator(AZ,Z,gid,iblock)

            !    !--- intermediate results for inner products
            !    WORKN(:,:,1,iblock) =  R(:,:,iblock)*Z(:,:,iblock)
            !    WORKN(:,:,2,iblock) = AZ(:,:,iblock)*Z(:,:,iblock)
            !end do
            call data_apply5(AZ, Z, data_A0, data_AN, data_AE, data_ANE, &
                btrop_operator)

             do iblock=1,nblocks_tropic
                 gid = blocks_tropic(iblock)

                 Z%vals(:,:,iblock) = R%vals(:,:,iblock)*A0R%vals(:,:,iblock)

                 !--- intermediate results for inner products
                 WORKN1%vals(:,:,iblock) =  R%vals(:,:,iblock)*Z%vals(:,:,iblock)
                 WORKN2%vals(:,:,iblock) = AZ%vals(:,:,iblock)*Z%vals(:,:,iblock)
            end do
                
            !call update_ghost_cells(AZ, bndy_tropic, field_loc_center,&
            !    field_type_scalar)
            call data_forceUpdate(AZ)

            !sumN = global_sum(WORKN, distrb_tropic,field_loc_center, RCALCT_B)
            sumN = data_sum(WORKN1)
            
            cg_rho     = sumN(1)   ! (r,(PC)r)
            cg_delta   = sumN(2)   ! (A (PC)r,(PC)r)
            cg_beta    = cg_rho/cg_rho_old
            cg_sigma   = cg_delta - (cg_beta**2)*cg_sigma
            cg_alpha   = cg_rho/cg_sigma
            cg_rho_old = cg_rho

            !---------------------------------------------------------------
            !
            !     compute S and Q
            !     compute next solution and residual
            !
            !---------------------------------------------------------------
            !do iblock=1,nblocks_tropic
            !    S(:,:,iblock) =  Z(:,:,iblock) + cg_beta *S(:,:,iblock)
            !    Q(:,:,iblock) = AZ(:,:,iblock) + cg_beta *Q(:,:,iblock)
            !    X(:,:,iblock) =  X(:,:,iblock) + cg_alpha*S(:,:,iblock)
            !    R(:,:,iblock) =  R(:,:,iblock) - cg_alpha*Q(:,:,iblock)
            !end do
            call data_apply6(Z, Z,S,AZ,Q,X,R, compute_s_and_q)
        end do iter_loop

        !rms_residual = sqrt(rr*resid_norm)
    end subroutine gw_solver

end program CGpop
