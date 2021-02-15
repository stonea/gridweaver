module submod_schedule_main
    use submod_schedule_types
    use mod_io
    use mod_utils
    use mod_string
    use mod_region
    use mod_distributedVec
    use submod_distribution_main
    use mod_regionSet
    use mod_integerList
    use mod_blockSet

    implicit none

    public schedule_new,           &
           schedule_addTransfer,   &
           schedule_print,         &
           schedule_printSimp,     &
           printVV

  contains
    subroutine schedule_new(self, dist)
        type(Schedule), intent(inout)           :: self
        type(Distribution), pointer, intent(in) :: dist

        type(Grid), pointer :: g
        integer :: b_tgt, idxBMap
        type(Region) :: reg, reg_halo, reg_on, reg_border
        integer :: sgid, rank_recv, bx1, by1, bx2, by2, idx_last_reg_added, i
        type(Subgrid), pointer :: sg
        type(RegionSet) :: R_on, R_off_halo, R_off_border, R_off_halo_blked
        type(BlockSet)  :: B_on,             B_off_border
        type(IntegerList) :: bmaps
        
        g => dist%grd
        self%grid => g
        self%dist => dist

        ! Allocate schedules members to be blank
        call integerSet_new(self%msgRecvFrom)
        call vecVecInt_new(self%transferRecvAtLBID)
        call vecVecRegion_new(self%transferRegionRecv)
        call integerSet_new(self%msgSendTo)
        call vecVecInt_new(self%transferSendFromLBID)
        call vecVecRegion_new(self%transferRegionSend)
                
        ! Loop through each block
        do b_tgt = 1, size(dist%gbid2lbid)
            ! Clear out information for previous block
            call blockSet_new(B_on)
            call blockSet_new(B_off_border)
            call regionSet_new(R_on)
            call regionSet_new(R_off_halo)
            call regionSet_new(R_off_border)
            call regionSet_new(R_off_halo_blked)
            call integerList_new(bmaps)

            sgid = distribution_block_sg(dist, b_tgt)
            sg => grid_getSubGrid(g, sgid)
            rank_recv = dist%gbid2proc(b_tgt)

            ! Construct regions that represent the points that will be stored
            ! in block b (i.e. compute reg_halo)
            bx1 = distribution_block_x1(dist, b_tgt)
            by1 = distribution_block_y1(dist, b_tgt)
            bx2 = distribution_block_x2(dist, b_tgt)
            by2 = distribution_block_y2(dist, b_tgt)
            call region_set(reg_halo, bx1-1, by1-1, bx2+1, by2+1)

            ! Determine the on-subgrid blocks that b communicates with
            ! (i.e. populate R_on and B_on)
            reg_on   = region_intersect(reg_halo, subgrid_region(sg))
            call distribution_blocks_in(dist, reg_on, sgid, B_on, R_on)
            call blockSet_remove(b_on, b_tgt)
            call regionSet_remove(r_on, &
                distribution_block_region(dist, b_tgt))
            
            ! Iterate through border mappings to determine regions that lie
            ! off the subgid (i.e. populate R_off_halo)
            do idxBMap=1, g%nBMaps
                ! add intersection of reg_halo and the border-map if not empty
                if(associated(g%borderSrcSubgrids(idxBMap)%val, sg)) then
                    reg = region_intersect( &
                        g%borderSrcRegions(idxBMap), reg_halo)
                    if(reg%empty == 0) then
                        call regionSet_add(R_off_halo, reg)
                        call integerList_add(bmaps, idxBMap)
                    endif
                end if
            end do
            
            ! For each region off the subgrid find the analogous region
            ! that it maps to then determine the blocks that region belongs
            ! to (i.e. compute R_off_border)
            idx_last_reg_added = 0
            do idxBMap=1, bmaps%nIntegers
                reg_border = grid_bmap_analogousRegion( &
                    g, bmaps%values(idxBMap), &
                    R_off_halo%regions(idxBMap))
                call distribution_blocks_in(dist, &
                    reg_border, &
                    g%borderTgtSubgrids(bmaps%values(idxBMap))%val%id, &
                    B_off_border, R_off_border)

               !if(myRank() == 0) then
               !     print "(A,$)", "Analogy of "
               !     call region_printSimp(R_off_halo%regions(idxBMap), 6)
               !     print "(A,$)", "is "
               !     call region_printSimp(reg_border, 6)
               !     print "(AI2,$)", "in ", &
               !         g%borderTgtSubgrids(bmaps%values(idxBMap))%val%id
               !     print *,
               ! end if  

                ! For each region added to R_off_border determine what portion
                ! of the halo it corresponds to (i.e. add to R_off_halo_blked)
                do i=idx_last_reg_added+1, R_off_border%nRegions
                    reg_halo = grid_bmap_analogousRegionInverted( &
                        g, bmaps%values(idxBMap), &
                        R_off_border%regions(i))
                    call regionSet_add(R_off_halo_blked, reg_halo)

                    !if(myRank() == 0) then
                    !    print "(A,$)", "Reverse Analogy of "
                    !    call region_printSimp(R_off_border%regions(i), 6)
                    !    print "(A,$)", "is "
                    !    call region_printSimp(reg_halo, 6)
                    !    print *,
                    !end if  
                enddo
                idx_last_reg_added = R_off_border%nRegions
            end do
            
            !if(myRank() == 0) then
            !    print "(AI3A,$)", "Blocks around  ", b_tgt, ") "
            !    call blockSet_printSimp(B_on, 6)
            !    print *, ""
            !    print "(AI3A,$)", "Regions around ", b_tgt, ") "
            !    call regionSet_printSimp(R_on, 6)
            !    print *, ""

            !    print "(AI3A,$)", "Blocks incoming  ", b_tgt, ") "
            !    call blockSet_printSimp(B_off_border, 6)
            !    print *, ""
            !    print "(AI3A,$)", "Regions incoming ", b_tgt, ") "
            !    call regionSet_printSimp(R_off_border, 6)
            !    print *, ""
            !    print "(AI3A,$)", "Incoming to ", b_tgt, ") "
            !    call regionSet_printSimp(R_off_halo_blked, 6)
            !    print *, ""

            !    !print "(AI3A,$)", "Off_Reg around ", b_tgt, ") "
            !    !call regionSet_printSimp(R_off_border, 6)
            !    !print *, ""
            !    !print "(AI3A,$)", "bmaps   around ", b_tgt, ") "
            !    !call integerList_printSimp(bmaps, 6)
            !    !print *, ""
            !endif

            ! Add messages to communication schedule for on-subgrid transfers
            do i=1,R_on%nRegions
                call schedule_addTransfer(self, &
                    dist%gbid2proc(b_tgt), b_tgt, R_on%regions(i),  &
                    dist%gbid2Proc(B_on%blocks(i)), B_on%blocks(i), &
                    R_on%regions(i))
            end do

            ! Add messages to communication schedule for off-subgrid transfers
            do i=1,R_off_border%nRegions
                call schedule_addTransfer(self, &
                    dist%gbid2proc(b_tgt), b_tgt, R_off_halo_blked%regions(i), &
                    dist%gbid2Proc(B_off_border%blocks(i)),                    &
                    B_off_border%blocks(i),                                    &
                    R_off_border%regions(i))
            end do
            
        !type(RegionSet) :: R_on, R_off_halo, R_off_border, R_off_halo_blked
        !type(BlockSet)  :: B_on,             B_off_border
        end do
    end subroutine

    subroutine schedule_addTransfer(self, &
                                    rankRecv, gbidRecv, regRecv, &
                                    rankSend, gbidSend, regSend)
        !`-
        type(Schedule), intent(inout) :: self
        integer, intent(in)           :: rankRecv, gbidRecv, rankSend, gbidSend
        type(Region), intent(in)      :: regRecv, regSend
        type(Region) :: regRecv_BlkSpace, regSend_BlkSpace
        integer :: msgID

        ! Translate the receiving and sending message to block space
        regRecv_BlkSpace = region_translate(regRecv, &
            -1 * distribution_block_x1(self%dist, gbidRecv) + 1, &
            -1 * distribution_block_y1(self%dist, gbidRecv) + 1)

        regSend_BlkSpace = region_translate(regSend, &
            -1 * distribution_block_x1(self%dist, gbidSend) + 1, &
            -1 * distribution_block_y1(self%dist, gbidSend) + 1)

        ! Register receiving side of the message
        if(myRank() == rankRecv) then
            msgID = integerSet_add(self%msgRecvFrom, rankSend)
            call vecVecInt_pushInto(self%transferRecvAtLBID, msgID, &
                                    self%dist%gbid2lbid(gbidRecv))
            call vecVecRegion_pushInto(self%transferRegionRecv, msgID, &
                                       regRecv_BlkSpace)
        end if
        
        ! Register sending side of the message
        if(myRank() == rankSend) then
            msgID = integerSet_add(self%msgSendTo, rankRecv)
            call vecVecInt_pushInto(self%transferSendFromLBID, msgID, &
                                    self%dist%gbid2lbid(gbidSend))
            call vecVecRegion_pushInto(self%transferRegionSend, msgID, &
                                       regSend_BlkSpace)
        end if
    end subroutine



    subroutine schedule_print(self, out)
        type(Schedule), intent(in) :: self
        integer, intent(in)        :: out
        integer :: i, j, nVecs

        if(myRank() == 0) then
            write(out, '(AAA)') "Schedule ", tstr(self%name), " {"
            write(out, '(A)', advance='no') "                       grid = "
            write(out, '(A)') tstr(self%grid%name)
            write(out, '(A)', advance='no') "                       dist = "
            write(out, '(A)') tstr(self%dist%name)
        end if

        ! msgRecvFrom
        do i=0,numRanks()-1
            if(myRank() == 0) &
                write(out, '(AI1A)', advance='no') &
                    "             mMsgRecvFrom@", i, " = "
            call distIntVec_print(self%msgRecvFrom%integers( &
                1:self%msgRecvFrom%nIntegers), i, out)
            if(myRank() == 0) &
                write(out, '()')
        end do

        ! Print mTransferRecvAtLBID and mTransferRegionRecv
        do i=0,numRanks()-1
            if(myRank() == 0) &
                write(out, '()')

            ! Print mTransferRecvAtLBID 
            nVecs = distIntVecVec_numVecs(self%transferRecvAtLBID, i)
            do j=1,nVecs
                if(myRank() == 0) then
                    write(out, '(AI1AI1A)', advance='no') &
                        "   mTransferRecvAtLBID@", i, "[", j, "] = "
                end if
                call distIntVecVec_print(self%transferRecvAtLBID, i, j, out)
                if(myRank() == 0) &
                    write(out, '()')
            end do

            ! Print mTransferRegionRecv
            do j=1,nVecs
                if(myRank() == 0) then
                    write(out, '(AI1AI1A)', advance='no') &
                        "   mTransferRegionRecv@", i, "[", j, "] = "
                end if
                call distRegionVecVec_print(self%transferRegionRecv, i, j, out)
                if(myRank() == 0) &
                    write(out, '()')
            end do
        end do

        ! now printing sending info
        if(myRank() == 0) &
            write(out, '(A)') "  * * * "

        ! msgSendTo
        do i=0,numRanks()-1
            if(myRank() == 0) &
                write(out, '(AI1A)', advance='no') &
                    "               mMsgSendTo@", i, " = "
            call distIntVec_print(self%msgSendTo%integers( &
                1:self%msgSendTo%nIntegers), i, out)
            if(myRank() == 0) &
                write(out, '()')
        end do

        ! Print mTransferSendFromLBID and mTransferRegionSend
        do i=0,numRanks()-1
            if(myRank() == 0) &
                write(out, '()')

            ! Print mTransferSendFromLBID
            nVecs = distIntVecVec_numVecs(self%transferSendFromLBID, i)
            do j=1,nVecs
                if(myRank() == 0) then
                    write(out, '(AI1AI1A)', advance='no') &
                        " mTransferSendFromLBID@", i, "[", j, "] = "
                end if
                call distIntVecVec_print(self%transferSendFromLBID, i, j, out)
                if(myRank() == 0) &
                    write(out, '()')
            end do

            ! Print mTransferRegionSend
            do j=1,nVecs
                if(myRank() == 0) then
                    write(out, '(AI1AI1A)', advance='no') &
                        "   mTransferRegionSend@", i, "[", j, "] = "
                end if
                call distRegionVecVec_print(self%transferRegionSend, i, j, out)
                if(myRank() == 0) &
                    write(out, '()')
            end do
        end do

        
        if(myRank() == 0) then
            write(out, '(A)') "}"
        end if
    end subroutine

    ! DELETE ME
    subroutine printVV(vv, out)
        type(VecVecRegion), intent(in) :: vv
        integer, intent(in)            :: out
        integer :: nvecs, i, j

        do i=0,numRanks()-1
            if(myRank() == 0) &
                write(out, '()')

            ! Print mTransferRecvAtLBID 
            nVecs = distRegionVecVec_numVecs(vv, i)
            do j=1,nVecs
                if(myRank() == 0) then
                    write(out, '(AI1AI1A)', advance='no') &
                        "   mTransferRecvAtLBID@", i, "[", j, "] = "
                end if
                call distRegionVecVec_print(vv, i, j, out)
                if(myRank() == 0) &
                    write(out, '()')
            end do
        end do
    end subroutine


    subroutine schedule_printSimp(self, out)
        type(Schedule), intent(in) :: self
        integer, intent(in)        :: out
        
        if(myRank() == 0) &
            write(out, '(A)', advance='no') "Schedule"
    end subroutine
end module
