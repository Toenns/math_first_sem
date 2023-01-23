MODULE mag_quad
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: magic_check, magic_sum, matout, tf_out

CONTAINS

    FUNCTION magic_check(matrix)                ! Bedingungsprüfung für magisches Quadrat
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix
        LOGICAL :: magic_check
    
        magic_check= row_check(matrix) .and. column_check(matrix) .and. trace1_check(matrix) .and. trace2_check(matrix) 
    ENDFUNCTION

    FUNCTION row_check(matrix)                ! Zeilensumme prüfen
        LOGICAL :: row_check
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix

        row_check= magic_sum(matrix)== SUM(matrix(1,:))

    ENDFUNCTION


    FUNCTION column_check(matrix)             ! Spaltensumme prüfen
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix
        LOGICAL :: column_check 

        column_check= magic_sum(matrix) == SUM(matrix(1,:))

    ENDFUNCTION

    FUNCTION magic_sum(matrix)                ! Summe ermitteln
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix
        INTEGER :: n
        INTEGER :: magic_sum

        n= SIZE(matrix,1)
        magic_sum= (n*((n**2)+1))/2

    ENDFUNCTION

    FUNCTION trace1_check(matrix)               ! Diagonale 1 prüfen
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix
        LOGICAL :: trace1_check

        trace1_check= trace1(matrix) == magic_sum(matrix)

    ENDFUNCTION

    FUNCTION trace1(matrix)                     ! Diagonale 1 Summe
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix
        INTEGER :: trace1
        INTEGER :: i

        trace1= 0
        trace1= SUM([(matrix(i,i),i=1,SIZE(matrix,1))])
        
    ENDFUNCTION

    FUNCTION trace2_check(matrix)               ! Diagonale 1 prüfen
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix
        LOGICAL :: trace2_check

        trace2_check= trace2(matrix) == magic_sum(matrix)

    ENDFUNCTION

    FUNCTION trace2(matrix)                      ! Diagonale 2 Summe
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix
        INTEGER :: trace2
        INTEGER :: i, n
        trace2= 0
        n = size(matrix, 1)
        trace2= SUM([(matrix(n+1-i,i),i=1,n)])

        ! trace2= 0                              ! Alternative
        ! n= SIZE(matrix,1)
        ! DO i= 1, n
        !     trace2= trace2 + matrix((n+1)-i,i)
        ! ENDDO

    ENDFUNCTION

    SUBROUTINE matout(matrix)                   ! Ausgabe der Matrix
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix
        INTEGER :: i

        DO i=1,SIZE(matrix(:,1))
            WRITE(*,*) matrix(i,:)
        ENDDO
    ENDSUBROUTINE

    FUNCTION tf_out(matrix)                     ! Ausgabe von geschriebenem true oder false
        CHARACTER(5) :: tf_out
        INTEGER, DIMENSION(:,:), INTENT(IN) :: matrix

        IF(magic_check(matrix)) THEN
            tf_out= 'true'
        ELSE
            tf_out= 'false'
        ENDIF
    ENDFUNCTION

ENDMODULE mag_quad

PROGRAM magischesquad
    USE mag_quad
    IMPLICIT NONE
    INTEGER :: n,i,j,k
    INTEGER, DIMENSION(:,:), ALLOCATABLE :: masq


    DO    
        DO
            WRITE(*,*) 'Enter your dimenson of the magic quad. Possible values [1,181]'         ! Abfrage der Dimension
            READ(*,*) n
            IF(n==0) THEN
                EXIT
            ELSEIF(mod(n,2)==1) THEN                                    ! Ausstieg bei 0 oder 2n-1
                EXIT
            ENDIF
        ENDDO

        IF(n<=0) THEN                                                   ! Ausstieg bei 0
            WRITE(*,*) 'You left the code.'
            EXIT
        ENDIF

        ALLOCATE(masq(n,n))                                             ! Dimensionszuweisung

        masq = 0                                                        ! Erfüllen der Vorgaben
        i = 1
        j = (n+1)/2
        masq(i,j) = 1

        DO k= 2, n**2
            IF((masq(i-1,j-1)/=0) .OR. (i==1) .OR. (j==1)) THEN

                IF((i==1) .and. (j==1)) THEN
                    i=2
                    j=1
                ELSEIF((i==1) .and. (j/=1)) THEN
                    i= n
                    j=j-1
                ELSEIF((i/=1) .and. (j==1)) THEN
                    i= i-1
                    j= n
                ELSEIF((i/=1) .and. (j/=1)) THEN
                    i= i+1
                    j= j
                ENDIF
            ELSE
                i= i-1
                j= j-1
            ENDIF
            masq(i,j)= k

        ENDDO

        CALL matout(masq)                                               ! Ausgabe

        WRITE(*,*) 'It is ',tf_out(masq),' that the initialized matrix is a magic square.'
        WRITE(*,*) 'The sum of the matrix is:',magic_sum(masq)
        WRITE(*,*) 'The sum calculated is:',(n*((n**2)+1))/2 
        
        DEALLOCATE(masq)

    ENDDO

ENDPROGRAM