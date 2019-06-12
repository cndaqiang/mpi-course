program main
      use xn
      implicit none
      integer ::x,n
      do x=3,-1,-1
        do n=2,-1,-1
          write(*,*) x,n,power(x,n)
        end do
      end do
      end program


