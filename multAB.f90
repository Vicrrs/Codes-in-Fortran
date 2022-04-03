program mult
implicit none
    
integer :: i,N
    
double precision, allocatable :: A(:,:), B(:,:), C(:,:), identidade(:,:)
    
double precision :: alfa, beta
    
external DGEMM
    
N = 4
    
allocate(identidade(N,N), A(N,N), B(N,N), C(N,N))
    
alfa = 1.0
beta = 0.0
    
    
identidade = 0
     
    do i=1,N
    
     identidade(i,i) = 1.0
    
    end do 
    
    
A = 3.0 * identidade
B = 5.0 * identidade
    
    
call imprime_matriz(A,N)
    
call imprime_matriz(B,N)
    
call DGEMM('N','N',N,N,N,alfa,A,N,B,N,beta,C,N)
    
! C = alfa*A*B + beta*C
    
    
    
call imprime_matriz(C,N)
    
deallocate(A,B,C,identidade)
    
contains
    
    
subroutine imprime_matriz(A,n)
    
integer :: n,i,j
    
double precision :: A(n,n)
    
    do i = 1, n
    
    
      do j = 1, n
    
          write(*,"(f7.2)",advance="no") A(i,j)
      
      end do 
    
      write(*,*)"  "
    
    end do
    
    write(*,*)" "
    
end subroutine imprime_matriz

!************************************
    
    
end program mult