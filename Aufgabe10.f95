MODULE middel_function
    IMPLICIT NONE

    PRIVATE
    PUBLIC :: H, G, Arith

CONTAINS

    ! Harmonisches Mittel
    FUNCTION H( a, b )
        INTEGER, PARAMETER :: qp= SELECTED_REAL_KIND( P=33, R=4931 )
        REAL ( KIND=qp ) :: a
        REAL ( KIND=qp ) :: b
        REAL ( KIND=qp ) :: H

        H = (2*a*b)/(a+b)

    END FUNCTION H 

    ! Geometrisches Mittel
    FUNCTION G( a, b )
        INTEGER, PARAMETER :: qp= SELECTED_REAL_KIND( P=33, R=4931 )
        REAL ( KIND=qp ) :: a
        REAL ( KIND=qp ) :: b
        REAL ( KIND=qp ) :: G

        G = sqrt(a*b)

    END FUNCTION G

    ! Arithmetisches Mittel
    FUNCTION Arith( a, b )
        INTEGER, PARAMETER :: qp= SELECTED_REAL_KIND( P=33, R=4931 )
        REAL ( KIND=qp ) :: a
        REAL ( KIND=qp ) :: b
        REAL ( KIND=qp ) :: Arith

        Arith = (a+b)/2

    END FUNCTION Arith

END MODULE middel_function

PROGRAM pi
    USE middel_function
    IMPLICIT NONE
    INTEGER, PARAMETER :: qp= SELECTED_REAL_KIND( P=33, R=4931 )    ! Deklaration von quadruple precision
    REAL ( KIND=qp ) :: g0, k0, finish, start, i=0, x=3             ! g0 := großes U Index 0 ; k0 := kleines u Index 0

    CALL CPU_TIME(start)

    g0=2*sqrt(x)                            ! Startwertübergabe
    k0=3

    DO 
        i=i+1                               ! Laufindex  
        g0= H(k0, g0)                       ! Berechnung von g0 mit harmonischem Mittel
        k0= G(k0, g0)                       ! Berechnung von k0 mit geometrisches Reihe

        WRITE(*,*) 'The intervall is, [',k0,g0,'], in the',i,'. round.'
        
        IF(g0-k0<=spacing(k0)) THEN         ! Abbruchkriterium
            EXIT
        ENDIF 
    END DO

    CALL CPU_TIME(finish)

    WRITE(*,*) 'The program lasted',(finish-start),'seconds.'

END PROGRAM pi

! Editors: Anton Buehlmeyer and Christian Taubert

! Es können die ersten 32. Nachkommastellen ermittelt werden. Im 53. Durchlauf erreicht die
! untere Intervallgrenze die 34. Nachkommastellen. 
! Die Rechendauer ist nicht messbar, weswegen wir dem Porgramm eine hohe Effizienz zu sprechen. 
! Wenn man die Programmlaufzeit mit dem historischen Kontext vergleicht, erscheint es etwas makaber, 
! das ein Programm ein Lebenswerk einer Person in weniger als einer Sekunde in den Schatten stellt, auch
! wenn die 35. Nachkommastelle nicht erreicht wurde. 
