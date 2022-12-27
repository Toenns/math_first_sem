MODULE prim_fun
    IMPLICIT NONE
    INTEGER, PARAMETER :: ik= SELECTED_INT_KIND ( 18 )
    INTEGER, PARAMETER :: dk= SELECTED_REAL_KIND( P=15, R=307 )
    INTEGER, PARAMETER :: qk= SELECTED_REAL_KIND( P=18, R=4931 )


CONTAINS

    FUNCTION ist_prim(n)
        LOGICAL :: ist_prim
        INTEGER ( KIND= ik ), INTENT(IN) :: n
        INTEGER ( KIND= ik ) :: b, i
        REAL ( KIND= qk) :: p 

        p=n                                     ! Uebergabe des Schleifenindex
        b=nint(sqrt(p))                         ! Wurzel der Zahl, sowie Runden zur nächsten ganzen Zahl
        ist_prim = .TRUE.                       ! Prim_fun auf true setzen

        DO i=3 , b, 2                           ! Schrittweite 2, da nur ungerade Zahlen
            IF(mod(n, i)==0) THEN               ! Wenn Zahl durch eine Zahl des Intervalls teilbar ist, mit Rest 0 -> keine Primzahl
                ist_prim = .false.
            ENDIF
        END DO

        IF(n==1) THEN                           ! Berücksichtigung der 1, 2, und der geraden Zahlen, auch wenn nicht gefordert. 
            ist_prim= .false.
        ELSEIF(n==2) THEN
            ist_prim= .true.
        ELSEIF(mod(n,2)==0) THEN
            ist_prim= .false.
        end if

    END FUNCTION

    FUNCTION Umkehrzahl(i)
        INTEGER ( KIND= ik), INTENT(IN) :: i
        INTEGER ( KIND= ik) :: x
        INTEGER ( KIND= ik) :: storage
        INTEGER ( KIND= ik) :: Umkehrzahl

        Umkehrzahl=0                                    
        x= i                                            ! Uebergabe der Zahl
        DO
            storage= mod(x,10)                          ! Ermitteln der letzten Stelle
            x= x/10                                     ! "Löschen" der letzten Stelle
            Umkehrzahl= 10*Umkehrzahl+storage           ! Konstruktion der Umkehrzahl
            IF (x==0) THEN                              ! Abbruchbedingung
                EXIT
            END IF
        END DO

    END FUNCTION  

END MODULE prim_fun

PROGRAM primpal
    USE prim_fun
    IMPLICIT NONE
    REAL (KIND= dk) :: finish, start                                                    ! Deklaration der Zeitvariablen
    INTEGER (KIND= ik) :: a, b, i

    a=0
    b=0

    DO WHILE(.not. (2<=a .and. a<=b .and. b<=10**9) )                                   ! Abfrage des geforderten Eingabeintervalls
        WRITE(*,*) 'Enter your numbers a and b (2<=a<=b<=10**9):'
        READ(*,*) a, b
    END DO

    CALL CPU_TIME(start)

    DO i= a, b

        IF(ist_prim(i).and.i/=Umkehrzahl(i).and.ist_prim(Umkehrzahl(i))) THEN           ! Ausgabe der Mirpzahl
            WRITE(*,*) 'The number',i,'is a prime and the emirp is:',Umkehrzahl(i)
            WRITE(*,*)
        ELSEIF(ist_prim(i).and.(Umkehrzahl(i)==i)) THEN                                 ! Ausgabe der Primpalindrome
            WRITE(*,*) 'The number',i,'is a prime and a prime palindrome.'  
        ELSEIF(ist_prim(i)) THEN                                                        ! Ausgabe der Primzahlen
            WRITE(*,*) 'The number',i,'is only a prime.'
        !ELSE
        !   WRITE(*,*) i,'Is no prime.'
        !    WRITE(*,*)
        ENDIF

    ENDDO

    CALL CPU_TIME(finish)

    WRITE(*,*) 'The calculation took',finish-start,'seconds.'                            ! Ausgabe der Berechnungszeit
   
END PROGRAM

! Index     Prim                Palin               Mirp
! k=1       11                  11                  13
! k=2       101                 101                 107
! k=3       1009                                    1009
! k=4       10007               10301               10007
! k=5       100003                                  100049
! k=6       1000003             1003001             1000033
! k=7       10000019                                10000169
! k=8       100000007           100030001           100000007
! k=9       1000000008                              1000000008
! k=10      10000000019         10000500001         10000000207
! k=11      100000000003                            100000000237
! k=12      1000000000039       1000005000001       1000000000091

! Es gibt nur in den Intervallen, in denen die Stellenzahl ungerade ist (mit Außnahme der 11) Primzahlpalindrome.

! Autoren: Anton Bühlmeyer, Christian Taubert