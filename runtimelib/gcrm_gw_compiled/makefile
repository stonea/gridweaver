#----------------------------------------------------------
#  Configuration options
#----------------------------------------------------------
 LEVEL_MAX  = -DLEVEL_MAX=5
 SBDMN_IOTA = -DSBDMN_IOTA=2
 NPE_WRLD   = -DNPE_WRLD=8

#----------------------------------------------------------
#  paths to the source directories
#----------------------------------------------------------
 SOURCE_SWM      = ../../source/swm
 SOURCE_UTIL     = ../../source/utilities
 SOURCE_PARALLEL = ../../source/parallel
 SOURCE_GRID     = ../../source/grid
#----------------------------------------------------------
#  name the EXECUTABLE
#----------------------------------------------------------
 EXECUTABLE = ./swm
#----------------------------------------------------------
# 
#----------------------------------------------------------
 CPP     = /usr/bin/cpp
 CPPOPTS = -P -traditional $(LEVEL_MAX) $(SBDMN_IOTA) $(NPE_WRLD)
 COMP    = mpif90
 FIX     = 
 FREE    = 
 LOAD    = mpif90

 OPTS    = -c -O4 -I/s/chopin/l/grad/stonea/local/include -I/s/chopin/l/grad/stonea/projects/gridweaver/runtimelib
 LIBS    = -L/usr/local/intel/lib -L/s/chopin/l/grad/stonea/local/lib \
 		   -L../../.. -lnetcdff -lnetcdf -lgridlib -lstdc++ -lmpi_cxx
 INCL    = 

 SUF01   = F90
 SUF02   = f90
#------------------------------------------------------------------
#  precompiler options
#------------------------------------------------------------------
 DMPI = -Dmpi=1

 CPP_OPTS = $(CPPOPTS) $(DMPI)
#------------------------------------------------------------------
#  object list
#------------------------------------------------------------------
OBJS_UTIL =                      \
   kinds.o                       \
   numbers.o                     \
   strings.o                     \
   parallel_include.o            \
   ginput.o                      \
   physical_params.o             \
   parallel_params.o             \
   parallel_utilities.o          \
   utilities_misc.o              \
   utilities_netCDF.o            \
   utilities_linear_algebra.o    \
   utilities_timer.o             \
   grid_params.o                 \
   grid_subdomain.o              \
   grid_utilities.o              \
   grid_connectivity.o           \
   wrap_data.o                   \
   wrp1D_data.o                  \
   grid_metrics.o                \
   utilities_io.o                \
   multigrid.o                   \
   multigrid_2D.o                \
   utilities_advection_horz.o    \
   utilities_interp.o            \
   operators.o

OBJS_SWM =                       \
   swm_params_vertical.o         \
   swm_params_time.o             \
   swm_params.o                  \
   swm_vars_prognostic.o         \
   swm_vars_diagnostic.o         \
   swm_vars_work.o               \
   swm_mss.o                     \
   swm_eta.o                     \
   swm_div.o                     \
   swm_wnd.o                     \
   swm_swm.o                     \
   swm_initialize.o              \
   swm_main.o

#------------------------------------------------------------------
#  compile-link
#------------------------------------------------------------------
$(EXECUTABLE): $(OBJS_UTIL) $(OBJS_SWM)
	$(LOAD) -o $(EXECUTABLE) $(OBJS_UTIL) $(OBJS_SWM)  $(LIBS)

#------------------------------------------------------------------
#  rules for utility files
#------------------------------------------------------------------
kinds.o: $(SOURCE_UTIL)/kinds.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/kinds.$(SUF01) > kinds.$(SUF02)
	$(COMP) $(OPTS) $(FREE) kinds.$(SUF02)

numbers.o: $(SOURCE_UTIL)/numbers.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/numbers.$(SUF01) > numbers.$(SUF02)
	$(COMP) $(OPTS) $(FREE) numbers.$(SUF02)

strings.o: $(SOURCE_UTIL)/strings.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/strings.$(SUF01)  > strings.$(SUF02)
	$(COMP) $(OPTS) $(FREE) strings.$(SUF02)

parallel_include.o: $(SOURCE_PARALLEL)/parallel_include.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_PARALLEL)/parallel_include.$(SUF01) > parallel_include.$(SUF02)
	$(COMP) $(OPTS) $(FREE) parallel_include.$(SUF02)

ginput.o: $(SOURCE_UTIL)/ginput.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/ginput.$(SUF01)  > ginput.$(SUF02)
	$(COMP) $(OPTS) $(FREE) ginput.$(SUF02)

physical_params.o: $(SOURCE_UTIL)/physical_params.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/physical_params.$(SUF01) > physical_params.$(SUF02)
	$(COMP) $(OPTS) $(FREE) physical_params.$(SUF02)

parallel_params.o: $(SOURCE_PARALLEL)/parallel_params.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_PARALLEL)/parallel_params.$(SUF01) > parallel_params.$(SUF02)
	$(COMP) $(OPTS) $(FREE) parallel_params.$(SUF02)

parallel_utilities.o: $(SOURCE_PARALLEL)/parallel_utilities.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_PARALLEL)/parallel_utilities.$(SUF01) > parallel_utilities.$(SUF02)
	$(COMP) $(OPTS) $(FREE) parallel_utilities.$(SUF02)

utilities_misc.o: $(SOURCE_UTIL)/utilities_misc.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/utilities_misc.$(SUF01) > utilities_misc.$(SUF02)
	$(COMP) $(OPTS) $(FREE) utilities_misc.$(SUF02)

utilities_netCDF.o: $(SOURCE_UTIL)/utilities_netCDF.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/utilities_netCDF.$(SUF01)  > utilities_netCDF.$(SUF02)
	$(COMP) $(OPTS) $(FREE) utilities_netCDF.$(SUF02)

utilities_linear_algebra.o: $(SOURCE_UTIL)/utilities_linear_algebra.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/utilities_linear_algebra.$(SUF01)  > utilities_linear_algebra.$(SUF02)
	$(COMP) $(OPTS) $(FREE) utilities_linear_algebra.$(SUF02)

utilities_timer.o: $(SOURCE_UTIL)/utilities_timer.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/utilities_timer.$(SUF01) > utilities_timer.$(SUF02)
	$(COMP) $(OPTS) $(FREE) utilities_timer.$(SUF02)

grid_params.o: $(SOURCE_GRID)/grid_params.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_GRID)/grid_params.$(SUF01) > grid_params.$(SUF02)
	$(COMP) $(OPTS) $(FREE) grid_params.$(SUF02)

grid_subdomain.o: $(SOURCE_GRID)/grid_subdomain.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_GRID)/grid_subdomain.$(SUF01) > grid_subdomain.$(SUF02)
	$(COMP) $(OPTS) $(FREE) grid_subdomain.$(SUF02)

grid_utilities.o: $(SOURCE_GRID)/grid_utilities.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_GRID)/grid_utilities.$(SUF01) > grid_utilities.$(SUF02)
	$(COMP) $(OPTS) $(FREE) grid_utilities.$(SUF02)

grid_connectivity.o: $(SOURCE_GRID)/grid_connectivity.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_GRID)/grid_connectivity.$(SUF01) > grid_connectivity.$(SUF02)
	$(COMP) $(OPTS) $(FREE) grid_connectivity.$(SUF02)

wrap_data.o: $(SOURCE_UTIL)/wrap_data.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/wrap_data.$(SUF01) > wrap_data.$(SUF02)
	$(COMP) $(OPTS) $(FREE) wrap_data.$(SUF02)

wrp1D_data.o: $(SOURCE_UTIL)/wrp1D_data.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/wrp1D_data.$(SUF01) > wrp1D_data.$(SUF02)
	$(COMP) $(OPTS) $(FREE) wrp1D_data.$(SUF02)

grid_metrics.o: $(SOURCE_GRID)/grid_metrics.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_GRID)/grid_metrics.$(SUF01) > grid_metrics.$(SUF02)
	$(COMP) $(OPTS) $(FREE) grid_metrics.$(SUF02)

utilities_io.o: $(SOURCE_UTIL)/utilities_io.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/utilities_io.$(SUF01) > utilities_io.$(SUF02)
	$(COMP) $(OPTS) $(FREE) utilities_io.$(SUF02)

multigrid.o: $(SOURCE_UTIL)/multigrid.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/multigrid.$(SUF01)  > multigrid.$(SUF02)
	$(COMP) $(OPTS) $(FREE) multigrid.$(SUF02)

multigrid_2D.o: $(SOURCE_UTIL)/multigrid_2D.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/multigrid_2D.$(SUF01)  > multigrid_2D.$(SUF02)
	$(COMP) $(OPTS) $(FREE) multigrid_2D.$(SUF02)

utilities_advection_horz.o: $(SOURCE_UTIL)/utilities_advection_horz.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/utilities_advection_horz.$(SUF01)  > utilities_advection_horz.$(SUF02)
	$(COMP) $(OPTS) $(FREE) utilities_advection_horz.$(SUF02)

utilities_interp.o: $(SOURCE_UTIL)/utilities_interp.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/utilities_interp.$(SUF01)  > utilities_interp.$(SUF02)
	$(COMP) $(OPTS) $(FREE) utilities_interp.$(SUF02)

operators.o: $(SOURCE_UTIL)/operators.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_UTIL)/operators.$(SUF01) > operators.$(SUF02)
	$(COMP) $(OPTS) $(FREE) operators.$(SUF02)
#-----------------------------------------------------------------------
#  rules for swm files
#-----------------------------------------------------------------------
swm_params_vertical.o: $(SOURCE_SWM)/swm_params_vertical.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_params_vertical.$(SUF01) > swm_params_vertical.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_params_vertical.$(SUF02)

swm_params_time.o: $(SOURCE_SWM)/swm_params_time.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_params_time.$(SUF01) > swm_params_time.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_params_time.$(SUF02)

swm_params.o: $(SOURCE_SWM)/swm_params.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_params.$(SUF01) > swm_params.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_params.$(SUF02)

swm_vars_prognostic.o: $(SOURCE_SWM)/swm_vars_prognostic.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_vars_prognostic.$(SUF01) > swm_vars_prognostic.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_vars_prognostic.$(SUF02)

swm_vars_diagnostic.o: $(SOURCE_SWM)/swm_vars_diagnostic.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_vars_diagnostic.$(SUF01) > swm_vars_diagnostic.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_vars_diagnostic.$(SUF02)

swm_vars_work.o: $(SOURCE_SWM)/swm_vars_work.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_vars_work.$(SUF01) > swm_vars_work.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_vars_work.$(SUF02)

swm_mss.o: $(SOURCE_SWM)/swm_mss.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_mss.$(SUF01) > swm_mss.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_mss.$(SUF02)

swm_eta.o: $(SOURCE_SWM)/swm_eta.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_eta.$(SUF01) > swm_eta.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_eta.$(SUF02)

swm_div.o: $(SOURCE_SWM)/swm_div.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_div.$(SUF01) > swm_div.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_div.$(SUF02)

swm_wnd.o: $(SOURCE_SWM)/swm_wnd.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_wnd.$(SUF01) > swm_wnd.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_wnd.$(SUF02)

swm_swm.o: $(SOURCE_SWM)/swm_swm.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_swm.$(SUF01) > swm_swm.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_swm.$(SUF02)

swm_initialize.o: $(SOURCE_SWM)/swm_initialize.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_initialize.$(SUF01) > swm_initialize.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_initialize.$(SUF02)

swm_main.o: $(SOURCE_SWM)/swm_main.$(SUF01) 
	$(CPP) $(CPP_OPTS) $(SOURCE_SWM)/swm_main.$(SUF01) > swm_main.$(SUF02)
	$(COMP) $(OPTS) $(FREE) swm_main.$(SUF02)

#------------------------------------------------------------------
#  clean
#------------------------------------------------------------------
clean :
	rm -f *.o  *.F90  *.f90  *.mod  fort.* swm
#------------------------------------------------------------------
#  pure
#------------------------------------------------------------------
pure :
	rm  -f  *.o  *.f90  *.F90  *.mod ZGrd  fort.*  a.out
	rm -f ../../output/swm/*

