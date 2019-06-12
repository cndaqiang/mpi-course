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
FCFLAGE=-g -O2 -ffree-line-length-none 


default: run what

MAIN=main.f90

TEST=mytest

MATHDIR=/mnt/e/work/CODE/soft/sourceCode/matlib

MATHLIB= -llapack.a  -lrefblas  -lscalapac  -ltmglib

OBJ=m_matradd.o

m_matradd.o : m_matradd.f90
	        $(FC) $(FCFLAGE) -c -o $@ $^


what:
	@echo 
	@echo Please Read ReadMe
	@echo


run:$(OBJ) $(MAIN)
	$(FC)  $(FCFLAGE) -o $@  $^


clean:
	@rm *.o *.mod
	@rm run mytest



