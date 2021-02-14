include ../../Makefile.inc

DRIVER=subgrids_construct
OBJS=subgrids_construct.o

$(DRIVER): $(OBJS) 
	$(FTN) -o $(DRIVER) $(OBJS) $(LDFLAGS)

%.o: %.F90
	$(FTN) $(FTNFLAGS) -c $<

clean:
	-rm -f $(DRIVER) $(OBJS)
