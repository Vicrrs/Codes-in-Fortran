program prog1
    implicit none
    
    
    character*1 :: JOBZ, UPLO
    integer :: N, LDA, LWORK, INFO
    
    double precision, allocatable :: A(:,:),W(:), WORK(:)
    
    external DSYEV
    
    JOBZ='V'
    UPLO = 'U'
    
    N = 3
    LDA = N
    
    LWORK = 3*N
    
    allocate(A(N,N),W(N),WORK(LWORK))
    
    A(1,:) = (/2,0,0/)
    A(2,:) = (/0,5,0/)
    A(3,:) = (/0,0,9/)
    
    call DSYEV(JOBZ,UPLO,N,A,LDA,W,WORK,LWORK,INFO)
    
    write(*,*) W(1)
    write(*,*) W(2)
    write(*,*) W(3)
    write(*,*) " "
    
    write(*,*) A(1,1)
    write(*,*) A(2,1)
    write(*,*) A(3,1)
    write(*,*) " "
    
    write(*,*) A(1,2)
    write(*,*) A(2,2)
    write(*,*) A(3,2)
    write(*,*) " "
    
    write(*,*) A(1,3)
    write(*,*) A(2,3)
    write(*,*) A(3,3)
    write(*,*) " "
    
    deallocate(A,W,WORK)
    
end program prog1