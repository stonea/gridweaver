CXX=g++
DRIVER=test
OBJS=utils.o gridLibCall.o project.o analyses.o test.o

#gridgen.o expressions.o environment.o utils.o analyzer.o codegen.o \
#grid.o stencil.o

ROSEDIR=/s/bach/e/proj/oadev/ROSE/rose-0.9.5a-18802/ROSE
#BOOSTDIR=/s/bach/e/proj/oadev/local/lib/boost1.44/boost_1_44_0/installTree
INC=-I$(ROSEDIR)/include
#-I$(BOOSTDIR)/include -I/s/bach/e/proj/oadev/local/include
LIB=-L$(ROSEDIR)/lib -lrose
#-L$(BOOSTDIR)/lib/ -L/s/bach/e/proj/oadev/local/lib \
	#-lboost_wave -lboost_date_time -lboost_filesystem -lboost_regex \
	#-lboost_system -lboost_thread
CXXFLAGS=-g

$(DRIVER): $(OBJS)
	$(CXX) $(LIB) $(INC) $(CXXFLAGS) -o $(DRIVER) $(OBJS)

.cpp.o:
	$(CXX) $(LIB) $(INC) $(CXXFLAGS) -c $<

clean:
	-rm $(OBJS) $(DRIVER) rose_* *postprocessed.f90 ./a.out

