include ../../Makefile.inc

DRIVER=subgrids_file
OBJS=subgrids_file.o

$(DRIVER): $(OBJS) 
	$(FTN) -o $(DRIVER) $(OBJS) $(LDFLAGS)

%.o: %.F90
	$(FTN) $(FTNFLAGS) -c $<

clean:
	-rm -f $(DRIVER) $(OBJS)
