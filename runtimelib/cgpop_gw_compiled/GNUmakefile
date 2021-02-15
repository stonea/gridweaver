FC = mpif90 
LD = mpif90 
CC = gcc
Cp = /bin/cp
Cpp = /usr/bin/cpp -P -traditional-cpp
AWK = /usr/bin/gawk
COMMDIR = mpi

GRIDLIB_DIR=/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib
FTNFLAGS = -g -O3 -I/usr/include/openmpi-x86_64 -I$(GRIDLIB_DIR)
LDFLAGS=-L$(GRIDLIB_DIR) -L/s/chopin/l/grad/stonea/local/lib -lgridlib -rdynamic -lstdc++ -lmpi_cxx

 
# Adjust these to point to where netcdf is installed
NETCDFINC = -I$(NETCDF_DIR)/include
NETCDFLIB = -L$(NETCDF_DIR)/lib -lnetcdff -lnetcdf

LIBS = $(NETCDFLIB)
LDLIBS = $(LIBS)

OBJS= kinds_mod.o simple_type.o communicate.o constants.o exit_mod.o \
	io_serial.o domain_size.o IOUnitsMod.o simple_domain.o mpi2s_boundary.o \
	simple_blocks.o timers.o reductions.o solvers.o boundary.o broadcast.o \
	check.o cgpop.o 


DRIVER: $(OBJS)
	$(LD) $(LDFLAGS) $(LDLIBS) -o cgpop $(OBJS)

%.o: %.f90
	$(FC) $(NETCDFINC) $(FTNFLAGS) -c $<

clean:
	-rm $(OBJS) cgpop
