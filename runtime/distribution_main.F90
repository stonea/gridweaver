module submod_distribution_main
    use submod_distribution_types
    use mod_string
    use submod_grid_types
    use submod_grid_main
    use mod_blockSet
    use mod_regionSet
    implicit none

    public :: distribution_print,                 &
              distribution_printSimp,             &

              distribution_block_sg,              &
              distribution_block_x1,              &
              distribution_block_y1,              &
              distribution_block_x2,              &
              distribution_block_y2,              &
              distribution_block_region,          &

              distribution_blocks_in,             &

              distribution_pos2blockPos,          &

              distribution_new_fill_fill,         &
              distribution_new_fill_block,        &
              distribution_new_fill_cyclic,       &

              distribution_new_block_fill,        &
              distribution_new_block_block,       &

              distribution_new_cyclic_fill,       &
              distribution_new_cyclic_block,      &

              distribution_new_block_cyclic
  contains

! *****************************************************************************
    subroutine distribution_print(self, out)
        type(Distribution), intent(in) :: self
        integer, intent(in) :: out
        integer :: i

        if(myRank() == 0) then
            write(out, '(AAA)')   "Distribution ", tstr(self%name), " {"
            write(out, '(A)', advance='no')     "           grid = ";
            call grid_printSimp(self%grd, out)
            write(out, '()')
            write(out, '(A)', advance='no')     "      blockSize = ";
            call distIntVec_print(self%blockSize, 0, out)
            write(out, '()')
            write(out, '(A)', advance='no')     "      gbid2lbid = "; 
            call distIntVec_print(self%gbid2lbid, 0, out)
            write(out, '()')
            write(out, '(A)', advance='no')     "      gbid2proc = "; 
            call distIntVec_print(self%gbid2proc, 0, out)
            write(out, '()')
        end if
        do i=0,numRanks()-1
            if(myRank() == 0) then
                write(out, '(AI1A)', advance='no') &
                    "    lbid2gbid@", i, " = "
            end if
            call distIntVec_print(self%lbid2gbid, i, out)
            if(myRank() == 0) &
                write(out, '()')
        end do
        if(myRank() == 0) then
            write(out, '(A)', advance='no') "        sg2gbid = "
            call distIntVec_print(self%sg2gbid, 0, out)
            write(out, '()')
            write(out, '(A)') "}"
        end if
    end subroutine

    subroutine distribution_printSimp(self, out)
        type(Distribution), intent(in) :: self
        integer, intent(in) :: out

        write(out, '(A)', advance='no') tstr(self%name)
    end subroutine


    integer function distribution_block_sg(self, gbid)
        type(Distribution), intent(in) :: self
        integer, intent(in) :: gbid
        integer :: i
 
        ! Loop through subgrids until we find one past this gbid
        do i=2,size(self%sg2gbid)
            if(self%sg2gbid(i) > gbid) then
                distribution_block_sg = i-1
                return
            end if
        end do
        distribution_block_sg = size(self%sg2gbid)
    end function

  
    integer function distribution_block_x1(self, gbid)
        type(Distribution), intent(in) :: self
        integer, intent(in) :: gbid
        integer :: sg, sbid

        sg = distribution_block_sg(self, gbid)
        sbid = gbid - self%sg2gbid(sg) + 1

        distribution_block_x1 = &
            MOD((sbid-1), self%sgBlocksH(sg)) * self%blockSize(1) + 1
    end function

   
    integer function distribution_block_y1(self, gbid)
        type(Distribution), intent(in) :: self
        integer, intent(in) :: gbid
        integer :: sg, sbid

        sg = distribution_block_sg(self, gbid)
        sbid = gbid - self%sg2gbid(sg) + 1

        distribution_block_y1 = &
            ((sbid-1) / self%sgBlocksH(sg)) * self%blockSize(2) + 1
    end function
   

    integer function distribution_block_x2(self, gbid)
        type(Distribution), intent(in) :: self
        integer, intent(in) :: gbid

        distribution_block_x2 = &
            distribution_block_x1(self,gbid) + self%blockSize(1)-1
    end function
   
   
    integer function distribution_block_y2(self, gbid)
        type(Distribution), intent(in) :: self
        integer, intent(in) :: gbid

        distribution_block_y2 = &
            distribution_block_y1(self,gbid) + self%blockSize(2)-1
    end function


    type(Region) function distribution_block_region(self, gbid)
        type(Distribution), intent(in) :: self
        integer, intent(in) :: gbid
        
        call region_set(distribution_block_region,          &
                        distribution_block_x1(self,gbid),   &
                        distribution_block_y1(self,gbid),   &
                        distribution_block_x2(self,gbid),   &
                        distribution_block_y2(self,gbid))
    end function


    subroutine distribution_blocks_in(self, reg, sgid, bs, rs)
        type(Distribution), intent(in) :: self
        type(Region), intent(in) :: reg
        integer, intent(in) :: sgid
        type(BlockSet), intent(inout)  :: bs
        type(RegionSet), intent(inout) :: rs

        type(Region) :: reg_expanded
        integer :: blkX, blkY, startgbid, gbid, x, y
        
        ! Expand reg so it fits along block boundaries
        reg_expanded = region_expandToMultiple(reg, &
            self%blockSize(1), self%blockSize(2))
        
        blkX = reg_expanded%X1 / self%blockSize(1)
        blkY = reg_expanded%Y1 / self%blockSize(2)
        startgbid = self%sg2gbid(sgid) + blkY * self%sgBlocksH(sgid) + blkX
        gbid = startgbid;

        ! Iterate through blocks in the region adding them to bs
        ! Intersect with reg to determine what to add to rs
        do y = reg_expanded%Y1, reg_expanded%Y2-1, self%blockSize(2)
            do x = reg_expanded%X1, reg_expanded%X2-1, self%blockSize(1)
                call blockSet_add(bs, gbid)
                call regionSet_add(rs, region_intersect( &
                    reg, distribution_block_region(self, gbid)))
                gbid = gbid+1
            end do
            gbid = startgbid + self%sgBlocksH(sgid)
            startgbid = gbid
        end do
    end subroutine


    subroutine distribution_pos2blockPos(self, sgX, sgY, sg, blkX, blkY, gbid)
        type(Distribution), intent(in) :: self
        integer, intent(in)    :: sgX, sgY
        type(SubGrid), pointer :: sg
        integer, intent(inout) :: blkX, blkY, gbid
        integer blkH, blkV

        blkH = sg%extents(1) / self%blockSize(1)
        blkV = sg%extents(2) / self%blockSize(2)

        gbid = ((sgY-1) / self%blockSize(2)) * blkH + 1 + &
               ((sgX-1) / self%blockSize(1))

        blkX = MOD((sgX-1), self%blockSize(1)) + 1
        blkY = MOD((sgY-1), self%blockSize(2)) + 1
    end subroutine


    subroutine distribution_new_fill_fill(self, gr)
        type(Distribution), intent(in)  :: self
        type(Grid), pointer, intent(in) :: gr
        
        ! TODO: Implement
    end subroutine


    subroutine distribution_new_fill_block(self, gr)
        type(Distribution), intent(inout)  :: self
        type(Grid), pointer, intent(in) :: gr
        integer :: numSG, i, startBlk
        type(SubGrid), pointer :: sg

        self%name = newID("dist")
        
        ! TODO: Implement to not assume all subgrids are same-sized
        numSG = grid_numSubGrids(gr)
        
        self%grd => gr
        allocate(self%blockSize(2))
        allocate(self%gbid2lbid(numSG * numRanks()))
        allocate(self%gbid2proc(numSG * numRanks()))
        allocate(self%lbid2gbid(numSG))
        allocate(self%sg2gbid(numSG))
        allocate(self%sgBlocksH(numSg))
        allocate(self%sgBlocksV(numSG))

        sg => grid_getSubGrid(gr, 1)
        self%blockSize(1) = sg%extents(1)
        self%blockSize(2) = sg%extents(2) / numRanks()

        self%sgBlocksH = 1
        self%sgBlocksV = numRanks()

        do i=1,numSG
            startBlk = (i-1) * numRanks() + 1
            self%sg2gbid(i) = startBlk
            self%gbid2lbid(startBlk:startBlk+numRanks()-1) = i
            self%lbid2gbid(i) = myRank() + numRanks() * (i-1) + 1
        end do

        do i=1,numSG * numRanks()
            self%gbid2proc(i) = MOD(i-1, numRanks())
        end do
    end subroutine


    subroutine distribution_new_fill_cyclic(self, gr)
        type(Distribution), intent(in)  :: self
        type(Grid), pointer, intent(in) :: gr
        
        ! TODO: Implement
    end subroutine


    subroutine distribution_new_block_fill(self, gr)
        type(Distribution), intent(in)  :: self
        type(Grid), pointer, intent(in) :: gr
        
        ! TODO: Implement
    end subroutine


    subroutine distribution_new_block_block(self, gr)
        type(Distribution), intent(in)  :: self
        type(Grid), pointer, intent(in) :: gr
        
        ! TODO: Implement
    end subroutine


    subroutine distribution_new_cyclic_fill(self, gr)
        type(Distribution), intent(in)  :: self
        type(Grid), pointer, intent(in) :: gr
        
        ! TODO: Implement
    end subroutine


    subroutine distribution_new_cyclic_block(self, gr)
        type(Distribution), intent(in)  :: self
        type(Grid), pointer, intent(in) :: gr
        
        ! TODO: Implement
    end subroutine


    subroutine distribution_new_block_cyclic(self, gr)
        type(Distribution), intent(in)  :: self
        type(Grid), pointer, intent(in) :: gr
        
        ! TODO: Implement
    end subroutine
end module submod_distribution_main
