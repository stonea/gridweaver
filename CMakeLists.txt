#Specify the version being used aswell as the language
cmake_minimum_required(VERSION 2.8.8)

#Name your project here
project(gridgen)

# Include and link to rose
include_directories(/s/chopin/l/grad/stonea/local/include)
link_directories(/s/chopin/l/grad/stonea/local/lib)

#Project executable
add_executable(gridgen
    gridgen.cpp
    environment.cpp
    grid.cpp
    dataObject.cpp
    distribution.cpp
    schedule.cpp
    utils.cpp
    runtimeparams.cpp
    gridLibCall.cpp
    project.cpp
    analyses.cpp
)

#Link to ROSE
target_link_libraries(gridgen rose)

