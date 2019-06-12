module lmat
	use m_mpi_my
	implicit none
	contains
		subroutine sublamt(M,OLDTYPE,olddisp,NEWTYPE)
			INTEGER :: M,OLDTYPE,NEWTYPE
			INTEGER :: olddisp !OLDTYPE字节长度, 使用kind(A(1,1))获得
			INTEGER,allocatable :: DATALEN(:),BLOCKDISP(:) 
			INTEGER :: i
			!MPI_TYPE_INDEXED(count,array_of_blocklengths,array_of_displacemets,oldtype,newtype)
			allocate(BLOCKDISP(M),DATALEN(M))
			
			Do i=1,M
				!DATALEN(i)=M-i
				!BLOCKDISP(i)=(i-1)*olddisp
				!Fortran要考虑到存储的方式
				DATALEN(i)=M-i+1
				BLOCKDISP(i)=(i-1)*M+(i-1)
			END DO
			call MPI_TYPE_INDEXED(M,DATALEN,BLOCKDISP,OLDTYPE,NEWTYPE,mpi_ierr)
			deallocate(BLOCKDISP,DATALEN)
		end subroutine

end module lmat
