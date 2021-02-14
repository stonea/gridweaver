CXX=g++

TEST_DRIVER=test
TEST_DRIVER_FTN=mpif90
TEST_DRIVER_OBJS=test.o
TEST_DRIVER_LDFLAGS=-L. -lgridlib -lstdc++ -lmpi_cxx
TEST_DRIVER_FTNFLAGS=-g -I/usr/include/openmpi-x86_64 -fPIC

LIB_FTN=mpif90
LIB_NAME=libgridlib.so
#LIB_OBJS=string.o utils.o gridweaver.o 
LIB_OBJS=string.o utils.o gridweaver.o schedule.o data.o
LIB_STATICLD=../libgridweaver.a
LIB_LDFLAGS=-L.. -L/s/chopin/l/grad/stonea/local/lib -rdynamic 
LIB_FTNFLAGS=-g -I/usr/include/openmpi-x86_64 -fPIC


all: $(LIB_NAME) $(TEST_DRIVER)


$(LIB_NAME): $(LIB_OBJS)
	$(LIB_FTN) $(LIB_LDFLAGS) -shared -o $(LIB_NAME) $(LIB_OBJS) $(LIB_STATICLD)

$(TEST_DRIVER): $(LIB_NAME) $(TEST_DRIVER_OBJS)
	$(TEST_DRIVER_FTN) $(TEST_DRIVER_LDFLAGS) -o $(TEST_DRIVER) $(TEST_DRIVER_OBJS)

$(LIB_OBJS): %.o: %.F90
	$(LIB_FTN) $(LIB_FTNFLAGS) -c $<

$(TEST_DRIVER_OBJS): %.o: %.F90
	$(TEST_DRIVER_FTN) $(TEST_DRIVER_FTNFLAGS) -c $<

clean:
	-rm $(LIB_NAME) $(LIB_OBJS) test.o *.mod
