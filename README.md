# Grid Weaver
Grid Weaver is an active library for specifying stencil computations on
semiregular-grids.  Done as a PhD dissertation project.

# About this project

Probably the best resources to learn about GridWeaver is my [PhD
dissertation](http://astonewebsite.s3-website-us-west-2.amazonaws.com/works/dissertation.pdf).
You can also see the slides for my defense in
[PDF](http://astonewebsite.s3-website-us-west-2.amazonaws.com/works/dissertation_talk.pdf)
or
[PPTX](http://astonewebsite.s3-website-us-west-2.amazonaws.com/works/dissertation_talk.pptx).

Briefly: GridWeaver is an active library for specifying stencil computations on
semiregular-grids. Semiregular-grids are meshes consisting of a finite number
of regular regions, stored in arrays, that connect to one another in an
irregular fashion. With GridWeaver programmers are able to specify grid
topology separately from the operations that they wish to apply. GridWeaver
uses code generation techniques to replace library calls with more efficient,
inlined, code

The semiregular grid that was of particular interest in the dissertation was
the icosahedral grid.  This grid represents the Earth as an icosahedron
consisting of ten logical arrays.  Each array stores two triangular faces of
the icosahedron.  Here's a 3D representation of the grid showing the Earth
discretized into hexagonal and pentagonal cells.  The highlighted blue cells
are immediate neighbors to the highlighted red cell:

ILLUSTRATION

A common algorithm performed on this type of grid would be to iteratively apply
a stencil computation that updates each cell with a weighted average of its
neighbors.

If we flatten the grid into arrays its connectivity pattern looks like this:

ILLUSTRATION

Each circular point represents a piece of data; the two large circles in the
top-left and bottom-right corners represent data nodes for the north and south
poles. Each of the blue rectangular regions represents a collection of data
that stored in a logical array. In this picture we illustrate six by six nodes
in each non-polar array but in practice these arrays contain thousands of
nodes.  The lines between nodes show connectivity. The lines going into and out
of the red bars connect to each other.

In practice each of the logical arrays would be split up and distributed across
multiple compute nodes.  Writing the MPI code by hand to communicate among all
these would be a pain and if a researcher wanted to experiment with different
grids they would have to rewrite that code.

GridWeaver aims to help with this by having including a library that allows
users to split the grid into subgrids (the blue rectangles in the diagram
above) and then use library calls to describe:

* the connectivity pattern among subgrids
* how to distribute and computation for each subgrid across nodes
* the stencil algorithm to apply to each element in the grid.

The following shows how to use GridWeaver to apply a stencil on the tripole
grid with a block-fill decomposition.  The tripole grid wraps its west and east
boundaries and has a folding pattern pattern along the north boundary.

See the following illustration the connectivity pattern for this grid:

ILLUSTRATION

``` Fortran
module Stencils; contains
    real function fivePtAvgStencil(A, i, j)
        fivePtAvgStencil = 0.2 * &
            (A(i, j) + A(i-1, j) + &
            (i+1, j) + A(i, j-1) + A(i, j+1))
    end function
end module

program StencilOnTripole
    type(SubGrid) :: sg
    type(Grid) :: g
    type(Decomposition) :: dcmp
    type(Data) :: data_in, data_out
    integer :: N, M

    ! Create subgrid
    N = 2048; M = 2048
    call subgrid_new(sg, N, M)

    ! Create a tripole grid
    call grid_new(g)
    call grid_addSubgrid(sg)

    ! Wrap left and right borders
    call grid_addBorder( 0, 1, 0, M, sg, N+1, 1, N+1, M, sg)
    call grid_addBorder(N+1, 1, N+1, M, sg, 1, 1, 1, M, sg);

    ! Fold top border
    call grid_addBorder(1, M+1, N/2, M+1, sg N, M, N/2+1, M, sg)
    call grid_addBorder(N/2+1, M+1, N, M+1, sg, N/2, M, 1, M, sg)

    ! Specify a decomposition and input data
    dcmp = decomposition_new_block_fill(g, 64, 64)
    data_in = data_input("input.dat", dcmp)
    data_out = data_new(dcmp)

    ! Perform stencil operation
    call data_apply(data_out, data_in, fivePtAvgStencil)
end program StencilOnTripole
```

