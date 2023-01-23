MODULE BoothroydDekker
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: dp, b_d_mat, matout

    INTEGER, PARAMETER :: dp = SELECTED_INT_KIND(18)     ! 64-bit

INTERFACE OPERATOR(.UEBER.)
    MODULE PROCEDURE ueber
END INTERFACE 

CONTAINS

    FUNCTION ueber(n,k)                                  ! binom
        INTEGER (KIND = dp), INTENT(IN) :: n, k
        INTEGER (KIND = dp) :: ueber, i

        ueber = 1

        DO i= 1, MIN(k,n-k)
            ueber= (ueber*(n-i+1))/i
        ENDDO

    ENDFUNCTION

    FUNCTION b_d_mat(n, inv)                            ! matrix boothroyd dekker
        INTEGER (KIND = dp), INTENT(IN) :: n 
        LOGICAL, INTENT(IN), OPTIONAL :: inv 
        INTEGER (KIND = dp ), DIMENSION(n,n) :: b, b_d_mat
        INTEGER (KIND = dp) :: i,j
        LOGICAL :: check

        check = .false.

        IF(PRESENT(inv))THEN
            IF(inv)THEN
                check = .true.
            ENDIF
        ENDIF

        IF(check) THEN  ! wenn inv existiert und wahr ist
            DO i= 1, n
                DO j= 1, n
                    b(i,j) = (((n+i-1) .UEBER. (i-1))*((n-1).UEBER.(n-j))*n)/(i+j-1)
                ENDDO
            ENDDO

            DO i= 1, n
                DO j= 1, n
                    b_d_mat(i,j) = ((-1)**(i+j))*b(i,j)
                ENDDO
            ENDDO
        ELSE            ! wenn inv nicht existent oder falsch ist
            DO i= 1, n
                DO j= 1, n
                    b_d_mat(i,j) = (((n+i-1).UEBER.(i-1))*((n-1).UEBER.(n-j))*n)/(i+j-1)
                ENDDO
            ENDDO
        ENDIF


    
    ENDFUNCTION


    SUBROUTINE matout(matrix)                                ! Ausgabe der Matrix
        INTEGER (KIND = dp), DIMENSION(:,:) :: matrix
        INTEGER (KIND = dp) :: i

        DO i=1,SIZE(matrix(:,1))
            WRITE(*,*) matrix(i,:)
        ENDDO
    ENDSUBROUTINE

ENDMODULE

PROGRAM Aufgabe15
    USE BoothroydDekker
    IMPLICIT NONE
    INTEGER (KIND = dp), DIMENSION(:,:), ALLOCATABLE :: boothroyd, dekker, product
    INTEGER ( KIND = dp ) :: n

    DO
        WRITE(*,*) 'Enter the dimension of the matrix.'
        READ(*,*) n
        
        IF( n > 0 ) THEN

            ALLOCATE(boothroyd(n,n), dekker(n,n), product(n,n))

            boothroyd= b_d_mat(n)
            dekker= b_d_mat(n, .true.)
            product= MATMUL(boothroyd,dekker)

            CALL MATOUT(boothroyd)
            WRITE(*,*)
            CALL MATOUT(product)

            DEALLOCATE(boothroyd, dekker, product)

        ELSE
            WRITE(*,*) 'You left the code.'
            EXIT
        ENDIF
    ENDDO

ENDPROGRAM