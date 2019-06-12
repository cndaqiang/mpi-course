module m_matradd
    implicit none
    contains
    subroutine matradd(m,n,A,B,C,lds)
    implicit none
    integer :: m,n,i,j,lds
    !Fortran支持讲高维矩阵转成一维数组，无须指定ldx
    real:: A(*),B(*),C(*)

    !其实fortran直接 C=A+B 就可以
    !C(1:m*n)=A(1:m*n)+B(1:m*n)
    DO j=0,n-1
        DO i=1,m
            C(i+j*lds)=A(i+j*lds)+B(i+j*lds)
        end DO
    end DO
    end subroutine matradd
end module m_matradd
