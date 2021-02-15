module submod_grid_main
    use submod_grid_types
    use mod_string
    use mod_distributedVec
    use mod_linkedList
    use mod_utils
    implicit none

    public :: neighbor_print,       &
              neighbor_printSimp,   &
              neighborVec_print,    &
              neighborPtrVec_print

    public :: subgrid_print,        &
              subgrid_printSimp,    &
              subgridPtrVec_print,  &
              subgridPtrVec_printN, &
              subgridPtrList_print, &
              subgrid_region

    public :: grid_print,           &
              grid_printSimp,       &
              grid_numSubGrids,     &
              grid_addSubGrid,      &
              grid_getSubGrid,      &
              grid_mapBorder,       &
              grid_placeAdjacentWE, &
              grid_placeAdjacentEW, &
              grid_placeAdjacentNS, &
              grid_placeAdjacentSN, &
              grid_wrapNS,          &
              grid_wrapSN,          &
              grid_wrapWE,          &
              grid_wrapEW,          &
              grid_mirrorN,         &
              grid_mirrorS,         &
              grid_mirrorW,         &
              grid_mirrorE,         &
              grid_foldN,           &
              grid_foldS,           &
              grid_foldW,           &
              grid_foldE,           &
              grid_bmap_analogousRegion, &
              grid_bmap_analogousRegionInverted

  contains
! *****************************************************************************

    subroutine neighbor_print(self, out)
        type(Neighbor), intent(in) :: self
        integer, intent(in) :: out

        if(myRank() == 0) then
            write(out, '(AAA)')             "Neighbor ", tstr(self%name), " {"
            write(out, '(A)', advance='no') "    offsets = ";
            call distIntVec_print(self%offsets, 0, out)
            write(out, '()')
            write(out, '(A)') "}"
        end if
    end subroutine
   


    subroutine neighbor_printSimp(self, out)
        type(Neighbor), intent(in) :: self
        integer, intent(in) :: out

        write(out, '(A)', advance='no') tstr(self%name)
    end subroutine



    subroutine neighborVec_print(self, out)
        type(Neighbor), intent(in) :: self(:)
        integer, intent(in) :: out
        integer :: i

        if(myRank() == 0) then
            write(out, '(A)', advance='no') "["
            do i=1,size(self)
                if(i /= 1) write(out, '(A)', advance='no') ", "
                call neighbor_printSimp(self(i), out)
            end do
            write(out, '(A)', advance='no') "]"
        end if
    end subroutine



    subroutine neighborPtrVec_print(self, out)
        type(NeighborPtr), intent(in) :: self(:)
        integer, intent(in) :: out
        integer :: i

        if(myRank() == 0) then
            write(out, '(A)', advance='no') "["
            do i=1,size(self)
                if(i /= 1) write(out, '(A)', advance='no') ", "
                call neighbor_printSimp(self(i)%val, out)
            end do
            write(out, '(A)', advance='no') "]"
        end if
    end subroutine


    subroutine subgrid_print(self, out)
        type(SubGrid), intent(in) :: self
        integer, intent(in) :: out

        if(myRank() == 0) then
            write(out, '(AAA)')             "SubGrid ", tstr(self%name), " {"
            write(out, '(AI1)')             "         nDims = ", self%nDims
            write(out, '(A)', advance='no') "        extent = ";
            call distIntVec_print(self%extents, 0, out)
            write(out, '()')
            write(out, '(A)')               "}"
        end if
    end subroutine



    subroutine subgrid_printSimp(self, out)
        type(SubGrid), intent(in) :: self
        integer, intent(in) :: out
        
        write(out, '(A)', advance='no') tstr(self%name)
    end subroutine



    subroutine subgridPtrVec_print(self, out)
        type(SubGridPtr), intent(in) :: self(:)
        integer, intent(in) :: out
        integer :: i

        if(myRank() == 0) then
            write(out, '(A)', advance='no') "["
            do i=1,size(self)
                if(i /= 1) write(out, '(A)', advance='no') ", "
                call subgrid_printSimp(self(i)%val, out)
            end do
            write(out, '(A)', advance='no') "]"
        end if
    end subroutine


    subroutine subgridPtrVec_printN(self, out, n)
        type(SubGridPtr), intent(in) :: self(:)
        integer, intent(in) :: out, n
        integer :: i

        if(myRank() == 0) then
            write(out, '(A)', advance='no') "["
            do i=1,n
                if(i /= 1) write(out, '(A)', advance='no') ", "
                call subgrid_printSimp(self(i)%val, out)
            end do
            write(out, '(A)', advance='no') "]"
        end if
    end subroutine



    subroutine subgridPtrList_print(self, out)
        type(LinkedList), pointer, intent(in) :: self
        integer, intent(in) :: out
        type(LinkedList), pointer :: ltrav
        type(SubGridPtr) :: sg

        if(myRank() == 0) then
            write(out, '(A)', advance='no') "["
            ltrav => self
            do while(associated(ltrav) .and. associated(ltrav%val))
                sg = transfer(ltrav%val, sg)
                
                if(.not. associated(ltrav, self)) &
                    write(out, '(A)', advance='no') ", "
                call subgrid_printSimp(sg%val, out)

                ltrav => ltrav%next
            end do
            write(out, '(A)', advance='no') "]"
        end if
    end subroutine


    function subgrid_region(self)
        type(SubGrid), intent(in) :: self
        type(Region) :: subgrid_region
        
        subgrid_region%empty = 0
        subgrid_region%x1 = 1
        subgrid_region%y1 = 1
        subgrid_region%x2 = self%extents(1)
        subgrid_region%y2 = self%extents(2)
    end function


    subroutine grid_print(self, out)
        type(Grid), intent(in) :: self
        integer, intent(in) :: out

        if(myRank() == 0) then
            write(out, '(AAA)') "Grid ", tstr(self%name), " {"
            write(out, '(AI2)') "             nSubgrids = ",  grid_numSubGrids(self)
            write(out, '(A)', advance='no') "              subgrids = "
            ! TODO: Print subgrids!!!!!! 
            call subgridPtrList_print(self%subgrids, out)
            write(out, '()')
            write(out, '(AI2)') "        nBorderRegions = ", self%nBMaps
            write(out, '(A)', advance='no') "      borderSrcRegions = "
            call distRegionVec_printN(self%borderSrcRegions, 0, out, self%nBMaps)
            !write(out, '()')
            !write(out, '(A)', advance='no') "    borderSrcNeighbors = "
            !call neighborPtrVec_print(self%borderSrcNeighbors, out)
            write(out, '()')
            write(out, '(A)', advance='no') "     borderSrcSubgrids = "
            call subgridPtrVec_printN(self%borderSrcSubgrids, out, self%nBmaps)
            write(out, '()')
            write(out, '(A)', advance='no') "      borderTgtRegions = "
            call distRegionVec_printN(self%borderTgtRegions, 0, out, &
                self%nBmaps)
            !write(out, '()')
            !write(out, '(A)', advance='no') "    borderTgtNeighbors = "
            !call neighborPtrVec_print(self%borderTgtNeighbors, out)
            write(out, '()')
            write(out, '(A)', advance='no') "     borderTgtSubgrids = ";
            call subgridPtrVec_printN(self%borderTgtSubgrids, out, self%nBmaps)
            write(out, '()')
            write(out, '(A)') "}"
        end if
    end subroutine


    subroutine grid_printSimp(self, out)
        type(Grid), intent(in) :: self
        integer, intent(in) :: out
        
        write(out, '(A)', advance='no') tstr(self%name)
    end subroutine


    integer function grid_numSubGrids(self)
        type(Grid), intent(in) :: self

        grid_numSubGrids = linkedList_size(self%subgrids)
    end function


    subroutine grid_addSubGrid(self, sg)
        type(Grid), intent(in) :: self
        type(SubGrid), pointer, intent(in) :: sg
        type(LinkedList), pointer :: ltrav
        type(SubGridPtr) :: ltravEl

        ! Check to see if sg is already in the subgrids list
        ltrav => self%subgrids
        do while(associated(ltrav) .and. associated(ltrav%val))
            ltravEl = transfer(ltrav%val, ltravEl)
            if(associated(ltravEl%val, sg)) then
                return
            end if
            ltrav => ltrav%next
        end do
        
        ! Add sg to the list
        ltravEl%val => sg
        call linkedList_push(self%subgrids, transfer(ltravEl, list_mold))
        sg%id = grid_numSubGrids(self)
    end subroutine

    !** Return subgrid number n **
    function grid_getSubGrid(self, n)
        type(SubGrid), pointer :: grid_getSubGrid
        type(Grid), intent(in) :: self
        integer, intent(in)    :: n
        type(LinkedList), pointer :: l
        type(SubGridPtr) :: sg

        l => linkedList_get(self%subgrids, n)
        sg = transfer(l%val, sg)
        grid_getSubGrid => sg%val
    end function


    subroutine grid_mapBorder(self,                 &
        srcSG, src_x1, src_y1, src_x2, src_y2,      &
        tgtSG, tgt_x1, tgt_y1, tgt_x2, tgt_y2)
    !`--
        type(Grid), intent(inout) :: self
        type(SubGrid), pointer, intent(in) :: srcSG
        integer, intent(in) :: src_x1, src_y1, src_x2, src_y2
        type(SubGrid), pointer, intent(in) :: tgtSG
        integer, intent(in) :: tgt_x1, tgt_y1, tgt_x2, tgt_y2

        ! TODO: Verify legality

        ! Add the subgrids to the grid
        call grid_addSubGrid(self, srcSG)
        call grid_addSubGrid(self, tgtSG)

        ! Add a new border map to grid's list of bmaps
        !allocate(bmap%val)
        !call linkedList_push(self%borderMaps, transfer(bmap, list_mold))
        self%nBMaps = self%nBMaps + 1

        ! Set the bmap's properties
        call region_set(self%borderSrcRegions(self%nBMaps), &
            src_x1, src_y1, src_x2, src_y2)
        call region_set(self%borderTgtRegions(self%nBMaps), &
            tgt_x1, tgt_y1, tgt_x2, tgt_y2)

        self%borderSrcSubgrids(self%nBmaps)%val => srcSG
        self%borderTgtSubgrids(self%nBmaps)%val => tgtSG

        !bmap%val%srcSubGrid => srcSG
        !bmap%val%tgtSubGrid => tgtSG
        !call region_set(bmap%val%srcRegion, src_x1, src_y1, src_x2, src_y2)
        !call region_set(bmap%val%tgtRegion, tgt_x1, tgt_y1, tgt_x2, tgt_y2)
    end subroutine


    subroutine grid_placeAdjacentWE(self, westSG, eastSG, shift)
        type(Grid), intent(inout) :: self
        type(SubGrid), pointer, intent(in) :: westSG, eastSG
        integer, intent(in) :: shift

        integer :: src_x1, src_y1, src_x2, src_y2
        integer :: tgt_x1, tgt_y1, tgt_x2, tgt_y2

        ! TODO: Handle shift
        if(shift /= 0) then
            print *, "TODO in grid_placeAdjacentWE"
            call errExit()
        endif

        ! map the west sg to the east sg
        src_x1 = westSG%extents(1) + 1;  src_y1 = 1
        src_x2 = westSG%extents(1) + 1;  src_y2 = westSG%extents(2)
        tgt_x1 = 1; tgt_y1 = 1
        tgt_x2 = 1; tgt_y2 = eastSG%extents(2)
        call grid_mapBorder(self, &
            westSG, src_x1, src_y1, src_x2, src_y2, &
            eastSG, tgt_x1, tgt_y1, tgt_x2, tgt_y2)

        ! map the east sg to the east sg
        src_x1 = 0;  src_y1 = 1
        src_x2 = 0;  src_y2 = eastSG%extents(2)
        tgt_x1 = westSG%extents(1); tgt_y1 = 1
        tgt_x2 = westSG%extents(1); tgt_y2 = westSG%extents(2)
        call grid_mapBorder(self, &
            eastSG, src_x1, src_y1, src_x2, src_y2, &
            westSG, tgt_x1, tgt_y1, tgt_x2, tgt_y2)
    end subroutine 


    subroutine grid_placeAdjacentEW(self, eastSG, westSG, shift)
        type(Grid), intent(inout) :: self
        type(SubGrid), pointer, intent(in) :: eastSG, westSG
        integer, intent(in) :: shift
    end


    subroutine grid_placeAdjacentNS(self, northSG, southSG, shift)
        type(Grid), intent(inout) :: self
        type(SubGrid), pointer, intent(in) :: northSG, southSG
        integer, intent(in) :: shift
    end


    subroutine grid_placeAdjacentSN(self, southSG, northSG, shift)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: southSG, northSG
        integer, intent(in) :: shift
    end


    subroutine grid_wrapNS(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_wrapSN(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_wrapWE(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_wrapEW(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_mirrorN(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_mirrorS(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_mirrorW(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_mirrorE(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_foldN(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_foldS(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_foldW(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end


    subroutine grid_foldE(self, sg)
        type(Grid), intent(inout) :: self
        type(SubGrid), intent(in) :: sg
    end

    !> Takes a region in a normally-oriented subgrid space and uses a border
    !! map to convert it to a (potentially non-normally oriented) region in a
    !! (potentially different) subgrid.
    !!
    !! \param self  Grid object
    !! \param bmap  ID of border map to apply.
    !! \param reg   Region to map from.  It is assumed that reg is a 
    !!              normally-oriented region in the subgrid associated with
    !!              bmap.
    !! \returns     An analogous region in the subgrid identified.  The
    !!              resulting region will be in the index space of the border
    !!              map's target-subgrid but will have the orientation of
    !!              the border-map-target)
    type(Region) function grid_bmap_analogousRegion(self, bmap, reg)
        type(Grid), intent(in) :: self
        integer, intent(in) :: bmap
        type(Region), intent(in) :: reg
        type(Region) :: res

        ! In this function reg undergoes a series of transformations to
        ! move from one index space to another.  Specifically these
        ! transformations go through the following:
        !
        !      source subgrid space
        !   -> source border-map space (subgrid-orientation)
        !   -> source border-map space (bmap-orientation)
        !   -> target border-map space
        !   -> target subgrid space

        ! Set res to equal reg in the block-map space
        res%X1 = reg%X1 - self%borderSrcRegions(bmap)%X1
        res%Y1 = reg%Y1 - self%borderSrcRegions(bmap)%Y1
        res%X2 = reg%X2 - self%borderSrcRegions(bmap)%X1
        res%Y2 = reg%Y2 - self%borderSrcRegions(bmap)%Y1
        
        ! Convert res from being in block-map-source space to 
        ! block-map-target space
        res = region_orient( &
            res, region_orientation(self%borderTgtRegions(bmap)))

        ! Convert from being in block-map-target space to the target subgrid's
        ! space
        res%X1 = res%X1 + self%borderTgtRegions(bmap)%X1
        res%Y1 = res%Y1 + self%borderTgtRegions(bmap)%Y1
        res%X2 = res%X2 + self%borderTgtRegions(bmap)%X1
        res%Y2 = res%Y2 + self%borderTgtRegions(bmap)%Y1
        
        grid_bmap_analogousRegion = res
    end function

    ! Like grid_bmap_analogousRegion but going from target to source
    type(Region) function grid_bmap_analogousRegionInverted(self, bmap, reg)
        type(Grid), intent(in) :: self
        integer, intent(in) :: bmap
        type(Region), intent(in) :: reg
        type(Region) :: res

        res%X1 = reg%X1 - self%borderTgtRegions(bmap)%X1
        res%Y1 = reg%Y1 - self%borderTgtRegions(bmap)%Y1
        res%X2 = reg%X2 - self%borderTgtRegions(bmap)%X1
        res%Y2 = reg%Y2 - self%borderTgtRegions(bmap)%Y1
        
        res = region_orient( &
            res, region_orientation(self%borderSrcRegions(bmap)))

        res%X1 = res%X1 + self%borderSrcRegions(bmap)%X1
        res%Y1 = res%Y1 + self%borderSrcRegions(bmap)%Y1
        res%X2 = res%X2 + self%borderSrcRegions(bmap)%X1
        res%Y2 = res%Y2 + self%borderSrcRegions(bmap)%Y1
        
        grid_bmap_analogousRegionInverted = res
    end function

end module submod_grid_main
