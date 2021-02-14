CXX=mpiCC
#LIB__NAME=libgridweaver.so
LIB__NAME=libgridweaver.a
COMPILER__NAME=gridweaver

LIB__OBJS=error.o iprintable.o svgprinter.o cellfieldpicture.o vispage.o binIO.o \
	utils.o grid.o environment.o distribution.o schedule.o runtimewrapper.o
COMPILER__OBJS=gridweaver.o

LIB__CXXFLAGS=-g -I/s/chopin/l/grad/stonea/local/include -fPIC \
	-I/usr/include/openmpi-x86_64
COMPILER__CXXFLAGS=-g -isystem /s/chopin/l/grad/stonea/local/include \
	-I/usr/include/openmpi-x86_64

LIB__LDFLAGS=-g -L/s/chopin/l/grad/stonea/local/lib -rdynamic -shared
COMPILER__LDFLAGS=-g -L/s/chopin/l/grad/stonea/local/lib -lrose

#analyses.o dataObject.o environment.o grid.o gridLibCall.o \
#runtimeparams.o distribution.o expressions.o \
#gridgen.o project.o schedule.o utils.o svgprinter.o \
#cellFieldPicture.o



all: $(LIB__NAME) $(COMPILER__NAME)

$(LIB__NAME): $(LIB__OBJS)
	ar r $(LIB__NAME) $(LIB__OBJS)

	# $(CXX) $(LIB__LDFLAGS) -o $(LIB__NAME) $(LIB__OBJS)

$(COMPILER__NAME): $(COMPILER__OBJS) $(LIB__NAME)
	$(CXX) $(COMPILER__LDFLAGS) -o $(COMPILER__NAME) $(COMPILER__OBJS) \
		$(LIB__NAME)


$(LIB__OBJS) : %.o : %.cpp
	$(CXX) $(LIB__CXXFLAGS) -c $<

$(COMPILER__OBJS) : %.o : %.cpp
	$(CXX) $(COMPILER__CXXFLAGS) -c $<

clean:
	-rm $(LIB__NAME) $(COMPILER__NAME) $(LIB__OBJS) $(COMPILER__OBJS)

