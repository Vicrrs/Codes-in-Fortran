program AV
implicit none

integer :: N,i,j

double precision, allocatable :: A(:,:), v(:), w(:)

N = 3

allocate(A(N,N), v(N), w(N))

v(1) = 1.0
v(2) = -1.0
v(3) = 8.0

A = 0.0

A(1,1) = -1.0
A(2,2) = 2.0
A(3,3) = 1.0

call imprime_matriz(A,N)

call imprime_vetor(v,N)

call multiplicaAv(A, v, w, N)

call imprime_vetor(w,N)

deallocate(A,w,v)

contains

!***********************************

subroutine multiplicaAv(A,v,w,n)

integer :: n,i,j
    
double precision :: A(n,n),v(n), w(n),soma
    
     do i = 1,n
     
      soma = 0.0
      
      do j = 1, n
      
       soma = soma + A(i,j)*v(j)
      
      end do
     
     w(i) = soma
     
     end do 
    
end subroutine multiplicaAv

!*********************************

subroutine imprime_vetor(vetor, n)
    
integer :: i, n
double precision :: vetor(n)

    do i = 1, n
        
        write(*,"(a6,i2,a3,f5.2)") "vetor(",i,")= ", vetor(i)

    end do

    write(*,*) " "

end subroutine imprime_vetor

!**********************************

subroutine imprime_matriz(A,n)

integer :: n, i, j

doubleprecision :: A(n,n)

do i = 1, n


    do j = 1, n
  
        write(*,"(f7.2)",advance="no") A(i,j)
    
    end do 
  
    write(*,*)"  "
  
  end do
  
  write(*,*)" "
  
end subroutine imprime_matriz


!**********************************

end program