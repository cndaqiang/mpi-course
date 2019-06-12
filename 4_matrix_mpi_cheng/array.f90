program test
        INTEGER :: A(3,3),B(3,3),C(3,3)
        write(*,*) "A"

        Do i=1,3
                Do j=1,3
                        A(i,j)=i+j
                        B(i,j)=i

                end DO
                write(*,*) A(i,:)
        end Do
        C=transpose(A)
        Do i=1,3
                write(*,*) C(i,:)
        end Do


end program
