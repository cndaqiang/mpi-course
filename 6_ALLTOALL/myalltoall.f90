module myalltoall
        use m_mpi_my
        contains
        subroutine alltoall(SENDBUF, SENDCOUNT, SENDTYPE, RECVBUF, RECVCOUNT, RECVTYPE, COMM, IERROR )
            !此处使用与传入数据为整型情况
            INTEGER :: SENDBUF(*), RECVBUF(*)
            INTEGER :: SENDCOUNT, SENDTYPE,  RECVCOUNT, RECVTYPE, COMM, IERROR
            !SENDCOUNT,RECVCOUNT,COMM,IERROR,

            INTEGER :: node, np,mpi_ierr,mpi_status(mpi_status_size),my_COMM
            INTEGER :: front_node,next_node,master_node=0
            INTEGER :: sendnode,recvnode
            
            
            
            call MPI_COMM_RANK(my_COMM,node,mpi_ierr)
            call MPI_COMM_SIZE(my_COMM,np,mpi_ierr)

            
            DO sendnode=0,np-1
                Do recvnode=0,np-1
                    TAG=100+10*sendnode+recvnode
                    if(node .eq. sendnode) call MPI_SEND(SENDBUF(recvnode*SENDCOUNT+1),SENDCOUNT, SENDTYPE,recvnode,TAG, COMM, IERROR)
                    if(node .eq. recvnode) call MPI_RECV(RECVBUF(sendnode*RECVCOUNT+1), RECVCOUNT, RECVTYPE, sendnode, TAG, COMM, mpi_status, IERROR)
                END DO
            END DO
        
            
        
        
        
        
        
        end subroutine alltoall

end module myalltoall