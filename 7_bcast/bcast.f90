program mpibcast
      implicit none
      include "mpif.h"
      INTEGER :: node, np,mpi_ierr,mpi_status(mpi_status_size),my_COMM
      INTEGER :: front_node,next_node,master_node=0
      REAL :: a
      call MPI_start()
      a=0
      if(node .eq. 0 ) a=100
      call mpibcastr(a,1,MPI_REAL,0,my_COMM,node,np)
      write(*,*) node,a
      
      call MPI_end()
      
      
      
contains
subroutine mpibcastr( b, n,datatype, root, comm, iam, np )
!include ’mpif.h’
integer ::n, root, comm, iam, np
INTEGER ::datatype,status(mpi_status_size)
real :: b
integer :: ierr, newid, i, des, src, left, mlen, iter
newid = mod( np+iam-root, np ) !相对于root的id，统一了进程
iter = alog(real(np))/alog(2.0)+1.0e-16 !传iter=log2(np)次,加上1.0e-16是为了在整iter情况下多循环一下 
                                        !传1 2 4 8..... 2^n
                                        !np=1:  1,irel 不传node1    
                                        !np=2:  1,1+eps 传1次 0->1
                                        !   3:  1,1.x+eps 1   0->1 
                                        !   4:  1,2+eps 2 |0->1; 0->2,0->3
!write(*,*) iter
mlen = 1  !有数据的节点数，初始root节点，为1
do i=1,iter
if ( left .le. 0 ) return 
des = mod( iam + mlen, np ) !目标0，1，2...mlen->0+mlen,1+mlen,2+mlen...2mlen
src = mod( np + iam - mlen, np ) !源 同理
if( newid .lt. mlen) then !小于mlen，我是源
    call mpi_send( b, n, datatype, des, 1, comm, ierr )
elseif( newid .lt. 2*mlen ) then
    call mpi_recv( b, n, datatype, src, 1, comm, status, ierr )
endif
mlen = 2*mlen !传输一次，meln翻倍
end do

left = np - mlen !剩下的节点
if ( left .le. 0 ) return 
des = mod( iam + mlen, np ) !目标
src = mod( np + iam - mlen, np ) !源
if( newid .lt. left) then !我是源
    call mpi_send( b, n, datatype, des, 1, comm, ierr )
elseif( newid .ge. mlen .and. newid .lt. mlen+left ) then
    call mpi_recv( b, n, datatype, src, 1, comm, status, ierr )
endif
return

end subroutine mpibcastr
      subroutine mpi_start()
              call MPI_INIT(mpi_ierr)
              call MPI_COMM_DUP(MPI_COMM_WORLD,my_COMM,mpi_ierr)
              call MPI_COMM_RANK(my_COMM,node,mpi_ierr)
              call MPI_COMM_SIZE(my_COMM,np,mpi_ierr)

              front_node=mod(node-1,np)
              next_node=mod(node+1,np)
      End subroutine mpi_start

      subroutine mpi_end()
              call MPI_FINALIZE(mpi_ierr)
      End subroutine mpi_end
      
end