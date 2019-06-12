module xn
        implicit none
        contains
        real function power(x,n)
                implicit none
                integer,Intent( IN ) :: n,x
                integer ::fn,xn
                !0^0 Error
                if ((x==0) .AND. (n==0)) STOP ("Error:0^0")
                
                !0^n=1
                power=0
                if (x==0) return
                
                !x^0=1
                power=1
                if (n==0) return
                
                !正常计算开始
                xn=x
                fn=abs(n)
                DO while (fn .ne. 0)
                        if (BTEST(fn,0)) power=power*xn
                        xn=xn*xn
                        fn=ISHFT(fn,-1)
                End DO
                if (n<0) power=1.0/power
                return
        end function power
end module xn
