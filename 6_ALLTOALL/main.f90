program test_myalltoall
        use m_mpi_my
        use myalltoall
        integer :: x(4),y(4)
        call mpi_start()
        if(node .eq. 0) x=1
        if(node .eq. 1) y=10
        if(node .ne. 0 ) x=0
        x=node
        
        call alltoall(x, 1, MPI_INTEGER, y, 1, MPI_INTEGER, my_COMM, mpi_ierr )
        !subroutine alltoall(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT, RECVTYPE, COMM, IERROR )
        write(*,*) node,y
        
        
        
        call mpi_end()





end program test_myalltoall