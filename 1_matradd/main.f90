program main
        use m_matradd
        implicit none
        real :: A(3,2),B(3,2),C(4,2)
        integer ::i,j
        data((A(i,j),i=1,3),j=1,2) /1,1,1,2,2,2/
        B=A
        call matradd(3,2,A,B,C(1:3,1:2),3)
        Do i=1,3
                write(*,*) A(i,:)
        End Do

        write(*,*) "C++++++++++++++++"
        Do i=1,3
                write(*,*) C(i,:)
        End Do
end program main
