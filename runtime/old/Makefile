FTN=mpif90
OBJS=utils.o environment.o expressions.o \
	subgrid_mod.o distribution.o data_mod.o test.o
DRIVER=test

$(DRIVER): $(OBJS)
	$(FTN) -o $(DRIVER) $(OBJS)

.SUFFIXES: .F90 .mod

.F90.o:
	$(FTN) -c $<

.F90.mod:
	$(FTN) -c $<

clean:
	-rm $(DRIVER) $(OBJS) *.mod

