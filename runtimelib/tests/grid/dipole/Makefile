include ../../Makefile.inc

DRIVER=dipole
OBJS=dipole.o

$(DRIVER): $(OBJS) 
	$(FTN) -o $(DRIVER) $(OBJS) $(LDFLAGS)

%.o: %.F90
	$(FTN) $(FTNFLAGS) -c $<

clean:
	-rm -f $(DRIVER) $(OBJS)
