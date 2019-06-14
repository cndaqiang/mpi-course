module lmat
	use m_mpi_my
	implicit none
	contains
		subroutine sublamt(N,m,num,OLDTYPE,NEWTYPE)
			INTEGER :: N,m,OLDTYPE,NEWTYPE !矩阵NxN,每个块mxm
			!INTEGER :: olddisp !OLDTYPE字节长度, 使用kind(A(1,1))获得
            INTEGER :: order !第几个nxn
			INTEGER,allocatable :: DATALEN(:),BLOCKDISP(:) 
			INTEGER :: i,num
			!MPI_TYPE_INDEXED(count,array_of_blocklengths,array_of_displacemets,oldtype,newtype)
			allocate(BLOCKDISP(N),DATALEN(N))
			DATALEN=m
			Do i=1,N !i代表列号
                order=(i-1)/m  !取整了 第order+1块
                write(*,*) "order",order
				!DATALEN(i)=N-i
				!BLOCKDISP(i)=(i-1)*olddisp
				!Fortran要考虑到存储的方式
				BLOCKDISP(i)=order*M+(i-1)*N
			END DO
			call MPI_TYPE_INDEXED(num*m,DATALEN,BLOCKDISP,OLDTYPE,NEWTYPE,mpi_ierr)
			deallocate(BLOCKDISP,DATALEN)
		end subroutine

end module lmat
