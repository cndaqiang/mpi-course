program pi
      implicit none
      include "mpif.h"
      INTEGER :: node, np,mpi_ierr,mpi_status(mpi_status_size),my_COMM
      INTEGER :: front_node,next_node,master_node=0
      REAL :: mypi,allpi,h,a=0,b=1,xi,yi
      INTEGER :: n,i
      !REAL,allocatable :: x
            INTEGER :: p,q
            INTEGER :: rowcomm,colcomm
            INTEGER :: mycol,myrow
            
      call mpi_start()
      
      p=3
      q=2
      call pq(node,np,p,q,mycol,myrow,my_COMM,rowcomm,colcomm)
      write(*,*) node , mycol,myrow
      call MPI_end()
        
      
      
      
      
      
      contains
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
      
      
      subroutine pq(iam,np,p,q,mycol,myrow,allcomm,rowcomm,colcomm)
            INTEGER :: iam,np,p,q
            INTEGER :: allcomm,rowcomm,colcomm
            INTEGER :: mpi_ierr
            INTEGER :: mycol,myrow
            if(iam .eq. 0) then
                if (np .ne. p*q) write(*,*) "p*q < np"
            endif
            myrow=iam/q
            mycol=mod(iam,q)
            !row
            !write(*,*) node,mycol,myrow
            call MPI_COMM_SPLIT(allcomm,myrow,iam,rowcomm,mpi_ierr)
            call MPI_COMM_SPLIT(allcomm,mycol,iam,colcomm,mpi_ierr)
                  
      end subroutine pq
end program pi