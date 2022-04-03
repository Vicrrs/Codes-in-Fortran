program formato
    implicit none
    
    integer :: n,m
    double precision :: x,y,z
    
    n = 15
    
    m = 567
    
    x = exp(2.0)
    
    y = log(2.0)
    
    z = exp(-4.0)
    
    open(1,file="numero.txt")
    
    
    write(1,"(i2,a9,3f10.4,i4)") n,"x,y,z =",x,y,z,m
    
     close(1)
    
    
    end program formato