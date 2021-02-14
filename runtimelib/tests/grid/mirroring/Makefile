include ../../Makefile.inc

DRIVER=mirroring
OBJS=mirroring.o

$(DRIVER): $(OBJS) 
	$(FTN) -o $(DRIVER) $(OBJS) $(LDFLAGS)

%.o: %.F90
	$(FTN) $(FTNFLAGS) -c $<

clean:
	-rm -f $(DRIVER) $(OBJS)
