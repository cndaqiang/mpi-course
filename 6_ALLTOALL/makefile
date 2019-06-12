#########################################################################################
### Author:       cndaqiang                                                           ###
### ContactMe:    https://cndaqiang.github.io                                         ### 
### Name:         MakeFile                                                            ###
### Last-update:  2019-04-06                                                          ###
### Build-time:   2019-03-29                                                          ###
### What it is:   MakeFile                                                            ###
#########################################################################################

# Remove implicit rules (including for Modula-2, which just confuses matters)
.SUFFIXES:
.SUFFIXES: .f .F .o .a  .f90 .F90


FC=mpif90
FFLAGS=-g -O2 -ffree-line-length-none 
INCFLAGS=
FPPFLAGS=
FPPFLAGS_fixed_F= 
FPPFLAGS_free_F90=
FCFLAGS_fixed_f=
FCFLAGS_free_f90=



.F.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS) $(FPPFLAGS) $(FPPFLAGS_fixed_F)  $< 
.F90.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS) $(FPPFLAGS) $(FPPFLAGS_free_F90) $< 
.f.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS) $(FCFLAGS_fixed_f)  $<
.f90.o:
	$(FC) -c $(FFLAGS) $(INCFLAGS) $(FCFLAGS_free_f90)  $<



default: run #what


TEST=mytest

MATHDIR=/mnt/e/work/CODE/soft/sourceCode/matlib

MATHLIB= -llapack.a  -lrefblas  -lscalapac  -ltmglib

OBJ=myalltoall.o main.o m_mpi_my.o

main.o:m_mpi_my.o
myalltoall.o:m_mpi_my.o


what:
	@echo 
	@echo Please Read ReadMe
	@echo


run:$(OBJ)
	$(FC)  $(FCFLAGE) -o $@  $^
	mpirun -np 4 ./run


clean:
	@rm *.o *.mod
	@rm run mytest



