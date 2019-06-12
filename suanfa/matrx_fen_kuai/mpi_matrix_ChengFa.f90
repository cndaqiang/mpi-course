program cheng
      implicit none
      include 'mpif.h'
      real :: A(9,2),B(2,9),C(9,9)
      real,allocatable :: Bn(:,:),Cn(:,:)
      integer :: node,np,ierr,status(mpi_status_size)
      integer :: source_node
      integer :: i,j,DM,DK,DP,p
	  integer :: howp,startp,endp

      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD,node,ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD,np,ierr)
      
      data((A(i,j),i=1,9),j=1,2) /1,2,3,4,5,6,7,8,9,1,2,3,4,5,6,7,8,9/
	  data((B(i,j),i=1,2),j=1,9) /1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9/
	  DM=size(A,dim=1)
	  DK=size(A,dim=2)
	  DP=size(B,dim=2)
	   
!-------Bn-------	  
	  p=DP/np
	  howp=mod(DP,np)
	  if(node>=howp) then
	  	p=p
	  	startp=(p+1)*howp+p*(node-howp)+1
	  else
	  	p=p+1
	  	startp=p*node+1
	  end if
	  allocate(Bn(DK,p))
	  endp=startp+p-1
	  Bn=B(:,startp:endp)
!-----End Bn------------------------
!-----Cn	  
	  allocate(Cn(DM,p))
!-----End Cn
	  call ab(A,Bn,Cn)
	  deallocate(Bn)
	  if(node.eq.0) then
		!write(*,*) Cn(:,p)
		C(:,1:p)=Cn
		DO i=1,np-1
			source_node=node+i
			p=DP/np
			if(source_node>=howp) then
				p=p
				startp=(p+1)*howp+p*(source_node-howp)+1
			else
				p=p+1
				startp=p*source_node+1
			end if
			
			endp=startp+p-1
			
		
			call MPI_RECV(C(:,startp:endp),p*DM,MPI_REAL,source_node,99,MPI_COMM_WORLD,status,ierr)
		
			! write(*,*) "---------------------"
		end DO
		DO i=1,DM
		 write(*,*) C(i,:)
		end DO

	  else
		call MPI_SEND(Cn,p*DM,MPI_REAL,0,99,MPI_COMM_WORLD,ierr)
	  end if

	  deallocate(Cn)



	call MPI_FINALIZE(ierr)
	  contains
	  subroutine ab(A,B,C)
		implicit none
		integer :: m,k,p
		integer :: mn,kn,pn
		real :: A(:,:),B(:,:),C(:,:)
		m=size(A,dim=1)
		k=size(A,dim=2)
		p=size(B,dim=2)
		C=0
		DO pn=1,p
			DO mn=1,m
				DO kn=1,k
				C(mn,pn)=C(mn,pn)+A(mn,kn)*B(kn,pn)
				END DO
			END DO
		END DO
	end subroutine
		
		
	
end program