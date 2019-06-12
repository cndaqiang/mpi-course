module m_matradd
    implicit none
    contains
    subroutine matradd(m,n,A,B,C)
    implicit none
    integer :: m,n,i,j
    real:: A(:,:),B(:,:),C(:,:)
    if (( m.ne.size(A,dim=1)) .OR. (m.ne.size(B,dim=1)) ) &
        stop "Error:Please make sure the dimenssions of the martix A and B are same"  
    if (( n.ne.size(A,dim=2)) .OR. (n.ne.size(B,dim=2)) ) &
        stop "Error:Please make sure the dimenssions of the martix A and B are same"
    if (( m.ne.size(C,dim=1)) .OR. (n.ne.size(B,dim=2)) ) &
        stop "Error:The dimenssions of C are differ from A and B  "
    !其实fortran直接 C=A+B 就可以
    C=A+B
    ! DO j=1,n
    !     DO i=1,m
    !         C(i,j)=A(i,j)+B(i,j)
    !     end DO
    ! end DO
    end subroutine matradd
end module m_matradd
