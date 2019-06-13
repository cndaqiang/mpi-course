new:
	mpif90 -c m_mpi_my.f90
	mpif90 -c LMAT.f90 
	mpif90 -c test.f90
	mpif90 *.o
	mpirun -np 4 ./a.out


clean:
	rm *.o a.out *.mod 
