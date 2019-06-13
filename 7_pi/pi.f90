program pi
      implicit none
      include "mpif.h"
      INTEGER :: node, np,mpi_ierr,mpi_status(mpi_status_size),my_COMM
      INTEGER :: front_node,next_node,master_node=0
      REAL :: mypi,allpi,h,a=0,b=1,xi,yi
      INTEGER :: n,i
      !REAL,allocatable :: x
      
      call mpi_start()
      mypi=0
      n=100000
      h=(b-a)/n
      !xi,i=0,1,2,3,...n，共n块，步长h
      do i = 1,n-1
        if(mod(i,np) .eq. node ) then
            xi=a+i*h
            call pifun(xi,yi)
            mypi=mypi+yi
        end if
      enddo
      call MPI_REDUCE(mypi,allpi,1,MPI_REAL,MPI_SUM,0,my_COMM,mpi_ierr)
      if(node .eq. 0) then
        mypi=0
        call pifun(a,yi)
        mypi=mypi+yi
        call pifun(b,yi)
        mypi=mypi+yi
        allpi=(mypi*h/2.0+allpi*h)*4
        write(*,*) allpi
      endif
      call MPI_end()
        
      
      
      
      
      
      contains
      subroutine pifun(x,y)
        REAL :: x,y
        y=1.0/(1+x**2)
      
      end subroutine pifun
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
      
end program pi