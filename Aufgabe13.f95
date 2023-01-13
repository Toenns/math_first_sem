MODULE Spat
    IMPLICIT NONE

    PRIVATE

    PUBLIC :: OPERATOR (.SP.), OPERATOR ( .CP. ), OPERATOR ( .A. ), surface, volume, MAG

    INTERFACE OPERATOR ( .SP. )
        MODULE PROCEDURE scalarproduct
    END INTERFACE

    INTERFACE OPERATOR ( .CP. )
        MODULE PROCEDURE crossproduct
    END INTERFACE

    INTERFACE OPERATOR ( .A. )
        MODULE PROCEDURE area
    END INTERFACE

    INTERFACE MAG
        MODULE PROCEDURE magnitude
    END INTERFACE

    CONTAINS
        FUNCTION scalarproduct(a,b)
            REAL, DIMENSION(3), INTENT(IN) :: a, b
            REAL :: scalarproduct
            scalarproduct =  a(1)*b(1)+a(2)*b(2)+a(3)*b(3)
        END FUNCTION
    
        FUNCTION magnitude(a)
            REAL, DIMENSION(3) :: a
            REAL :: magnitude
            magnitude = sqrt(a .SP. a)
        END FUNCTION
        
        FUNCTION crossproduct(a,b)
            REAL, DIMENSION(3), INTENT(IN) :: a, b
            REAL, DIMENSION(3) :: crossproduct
            crossproduct(1) = a(2)*b(3) - a(3)*b(2)
            crossproduct(2) = a(3)*b(1) - a(1)*b(3)
            crossproduct(3) = a(1)*b(2) - a(2)*b(1)
        END FUNCTION

        FUNCTION area(a,b)
            REAL, DIMENSION(3), INTENT(IN) :: a, b 
            REAL :: area
            area = magnitude(a .CP. b)
        END FUNCTION

        FUNCTION surface(a,b,c)
            REAL, DIMENSION(3), INTENT(IN) :: a, b, c 
            REAL :: surface
            surface = 2 * ((a .A. b) + (b .A. c) + (c .A. a))
        END FUNCTION

        FUNCTION volume(a,b,c)
            REAL, DIMENSION(3), INTENT(IN) :: a, b, c
            REAL :: volume
            volume = MAG((a .CP. b)*c)
        END FUNCTION
END MODULE Spat

PROGRAM Parallel 
    USE Spat
    IMPLICIT NONE
    CHARACTER :: op
    REAL, DIMENSION(3) :: a, b, c
    
    DO 
        
        ! WRITE(*,*) 'Enter vector a (value in cm).'
        ! CALL zerovector(a)
        ! WRITE(*,*) 'Enter vector b (value in cm).'
        ! CALL zerovector(b)
        ! WRITE(*,*) 'Enter vector c (value in cm).'
        ! CALL zerovector(C)


    WRITE(*,*) 'Enter vector a (values in cm).'
    READ(*,*) a 
    IF(ALL(a==0)) THEN 
        WRITE(*,*) "Please don't enter the zero vector."
        EXIT
    END IF 

    WRITE(*,*) 'Enter vector b (values in cm).'
    READ(*,*) b
    IF(ALL(b==0)) THEN 
        WRITE(*,*) "Please don't enter the zero vector."
        EXIT
    END IF 

    WRITE(*,*) 'Enter vector c (values in cm).'
    READ(*,*) c
    IF(ALL(c==0)) THEN 
        WRITE(*,*) "Please don't enter the zero vector."
        EXIT
    END IF    
  
    WRITE(*,*) 'The surface of the parallelepiped is:', surface(a,b,c) ,'cm^2.'
    ! WRITE(*,*) 'The surface of the parallelepiped is:', 2 * ((a .A. b) + (b .A. c) + (c .A. a)) ,'cm^2.'
    WRITE(*,*)
    WRITE(*,*) 'The volume of the parallelepiped is:', volume(a,b,c) ,'cm^3.' 
    ! WRITE(*,*) 'The volume of the parallelepiped is:', MAG((a .CP. b)*c) ,'cm^3.'
    WRITE(*,*) 

    WRITE(*,*) 'If you want to exit press "e".'                ! Ausgangsbbedingung
    READ(*,*) op
    IF(op=="e") THEN 
        EXIT
    ENDIF    

    END DO

CONTAINS
    SUBROUTINE zerovector(x)
        REAL, DIMENSION(3) :: x
   
        READ(*,*) x

        IF(ALL(x==0) ) THEN 
            WRITE(*,*) "Please don't enter the zero vector."
            STOP                                               ! STOP wird hier verwendet, da es in diesem Fall den gleichen Effekt wie EXIT hat
        END IF                                                 ! und somit der Code mithilfe der SUBROUTINE übersichtlicher wird. 
    END SUBROUTINE

END PROGRAM

! Das Definieren eines eigenen Operators ergibt bei binären Operationen Sinn. Für unäre Operationen, kann man genau so gut die Funktion 
! schreiben. Mit dem Interface kann man es verkürzen oder 