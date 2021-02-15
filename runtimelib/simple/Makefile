GRIDLIB_DIR=/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib

FTN=mpif90
FTNFLAGS=-g -I/usr/include/openmpi-x86_64 -I$(GRIDLIB_DIR)
LDFLAGS=-L$(GRIDLIB_DIR) -L/s/chopin/l/grad/stonea/local/lib -lgridlib -rdynamic -lstdc++ -lmpi_cxx

DRIVER=simple
OBJS=simple.o

$(DRIVER): $(OBJS) 
	$(FTN) -o $(DRIVER) $(OBJS) $(LDFLAGS)

%.o: %.F90
	$(FTN) $(FTNFLAGS) -c $<

clean:
	-rm -f $(DRIVER) $(OBJS)
