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

