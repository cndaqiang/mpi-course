module lmat
	use m_mpi_my
	implicit none
	contains
		subroutine sublamt(M,n,OLDTYPE,olddisp,NEWTYPE)
			INTEGER :: M,n,OLDTYPE,NEWTYPE !三角矩阵MxM,每个块nxn
			INTEGER :: olddisp !OLDTYPE字节长度, 使用kind(A(1,1))获得
            INTEGER :: order !第几个nxn
			INTEGER,allocatable :: DATALEN(:),BLOCKDISP(:) 
			INTEGER :: i
			!MPI_TYPE_INDEXED(count,array_of_blocklengths,array_of_displacemets,oldtype,newtype)
			allocate(BLOCKDISP(M),DATALEN(M))
			DATALEN=n
			Do i=1,M !i代表列号
                order=(i-1)/n  !取整了 第order+1块
                write(*,*) "order",order
				!DATALEN(i)=M-i
				!BLOCKDISP(i)=(i-1)*olddisp
				!Fortran要考虑到存储的方式
				BLOCKDISP(i)=order*n+(i-1)*M
			END DO
			call MPI_TYPE_INDEXED(M,DATALEN,BLOCKDISP,OLDTYPE,NEWTYPE,mpi_ierr)
			deallocate(BLOCKDISP,DATALEN)
		end subroutine

end module lmat
