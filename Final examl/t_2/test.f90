program test
    use lmat
    use m_mpi_my
    implicit none
!    include "mpif.h"
!若m_mpi_my中含有include "mpif.h"，就不用再include了，报错
!而且lmat中也不能引
    REAL :: A(6,6),B(6,6),C(6),D(6)
    INTEGER :: M=6
    INTEGER :: Htype
    call MPI_start()

    call sublamt(M,2,2,MPI_REAL,Htype)
    call MPI_TYPE_COMMIT(Htype,mpi_ierr)
    A=0
    if(node .eq. 0 ) A=2
    B=0
    !if(node .eq. 0 ) call MPI_SEND(A,1,Htype,1,99,my_COMM,mpi_ierr)
    !if(node .eq. 1 ) call MPI_RECV(B,1,Htype,0,99,my_COMM,mpi_status,mpi_ierr)
    !if(node < 2) call MPI_SENDRECV(A,1,Htype,1,99,B,1,Htype,0,99,my_COMM,mpi_status,mpi_ierr)
    !SENDRECV容易卡住，缓存？？？
    call MPI_BCAST(A,1,Htype,0,my_COMM,mpi_ierr)
    if(node .eq. 3) write(*,*) A(1,:)
    if(node .eq. 3) write(*,*) A(2,:)
    if(node .eq. 3) write(*,*) A(3,:)
    if(node .eq. 3) write(*,*) A(4,:)
    if(node .eq. 3) write(*,*) A(5,:)
    if(node .eq. 3) write(*,*) A(6,:)
    !call MPI_TYPE_FREE(Htype,mpi_ierr)



    !c=1
    !d=1
    !call MPI_TYPE_INDEXED(6,c,d,MPI_INTEGER,Htype)
    !if(node .eq. 0) write(*,*) kind(M)
    !MPI_TYPE_INDEXED(count,array_of_blocklengths,array_of_displacemets,oldtype,newtype)
			


    call MPI_END()


end program test
