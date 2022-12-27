MODULE  ERRORS                        !!! error handling module !!!
    IMPLICIT NONE                               ! no default typing !
    PRIVATE       ! anything that is not declared PUBLIC is PRIVATE !
    PUBLIC :: ERROR, WARNING
  
   CONTAINS
  
    SUBROUTINE ERROR (TEXT)
      CHARACTER (LEN = *) :: TEXT
      WRITE(*,*) 'ERROR: ', TEXT
      STOP
    END SUBROUTINE ERROR
  
    SUBROUTINE WARNING (TEXT)
      CHARACTER (LEN = *) :: TEXT
      WRITE(*,*) 'Warning: ', TEXT
    END SUBROUTINE WARNING
  
END MODULE  ERRORS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE  IVALMOD                  !!! interval arithmetic module !!!
    ! This module defines interval arithmetic with 2 ulp accuracy. !
    ! For the module to function properly, it is crucial that the  !
    ! real intrinsic operators  +, -, *, /, the real square root,  !
    ! and real input/output conversion be accurate to 1 ulp (unit  !
    ! in the last place of the floating-point mantissa).           !
 
   USE ERRORS, ONLY : ERROR, WARNING       ! import error handling !
 
   IMPLICIT NONE                               ! no default typing !
 
   PRIVATE       ! anything that is not declared PUBLIC is PRIVATE !
   PUBLIC :: INTERVAL, pr, IVAL, INF, SUP, MID, GET, PUT, SQRT,    &
  &          OPERATOR(+), OPERATOR(-), OPERATOR(*), OPERATOR(/),   &
  &          OPERATOR(.ISECT.), OPERATOR(.IHULL.), OPERATOR(.SB.), &
  &          OPERATOR(.SP.), OPERATOR(.DJ.), OPERATOR(.IN.),       &
  &          OPERATOR(==), OPERATOR(/=), IVAL_ZERO, IVAL_ONE
   PUBLIC :: WARNING, ERROR                   ! from module ERRORS !
 
   INTEGER, PARAMETER :: pr = KIND(0.0D0)   ! KIND parameter value !
                                            ! for double precision !
   TYPE INTERVAL
     PRIVATE         ! (internal structure) components are PRIVATE !
     REAL(KIND=pr)  :: INF, SUP
   END TYPE INTERVAL
 
   TYPE(INTERVAL), PARAMETER:: IVAL_ZERO=INTERVAL(0.0_pr, 0.0_pr), &
  &                            IVAL_ONE =INTERVAL(1.0_pr, 1.0_pr)
 
   REAL(KIND=pr), PARAMETER :: up = 1.0_pr, down = -1.0_pr
               ! used for directed roundings upwards and downwards !
 
   INTERFACE IVAL
     MODULE PROCEDURE IVAL
   END INTERFACE
 
   INTERFACE INF
     MODULE PROCEDURE INF
   END INTERFACE
 
   INTERFACE SUP
     MODULE PROCEDURE SUP
   END INTERFACE
 
   INTERFACE MID
     MODULE PROCEDURE MID
   END INTERFACE
 
   INTERFACE GET
     MODULE PROCEDURE GET
   END INTERFACE
 
   INTERFACE PUT
     MODULE PROCEDURE PUT
   END INTERFACE
 
   INTERFACE OPERATOR  ( + )
     MODULE PROCEDURE POS, ADD
   END INTERFACE
 
   INTERFACE OPERATOR  ( - )
     MODULE PROCEDURE NEG, SUB
   END INTERFACE
 
   INTERFACE OPERATOR  ( * )
     MODULE PROCEDURE MUL
   END INTERFACE
 
   INTERFACE OPERATOR  ( / )
     MODULE PROCEDURE DIV
   END INTERFACE
 
   INTERFACE SQRT
     MODULE PROCEDURE ISQRT
   END INTERFACE
 
   INTERFACE OPERATOR  ( .ISECT. )
     MODULE PROCEDURE INTERSECTION
   END INTERFACE
 
   INTERFACE OPERATOR  ( .IHULL. )
     MODULE PROCEDURE INTERVALHULL
   END INTERFACE
 
   INTERFACE OPERATOR  ( .SB. )
     MODULE PROCEDURE SUBSET
   END INTERFACE
 
   INTERFACE OPERATOR  ( .SP. )
     MODULE PROCEDURE SUPERSET
   END INTERFACE
 
   INTERFACE OPERATOR  ( .DJ. )
     MODULE PROCEDURE DISJOINT
   END INTERFACE
 
   INTERFACE OPERATOR  ( .IN. )
     MODULE PROCEDURE IN
   END INTERFACE
 
   INTERFACE OPERATOR( == )
     MODULE PROCEDURE EQUAL
   END INTERFACE
 
   INTERFACE OPERATOR( /= )
     MODULE PROCEDURE NOTEQUAL
   END INTERFACE
 
  CONTAINS  ! guaranteed enclosures computed by rounding outwards, !
            ! i.e. by rounding INF downwards and SUP upwards       !
 
   FUNCTION IVAL (L, R)  ! "constructor" function with 1 or 2 args !
     REAL(KIND=pr), INTENT(IN)           :: L
     REAL(KIND=pr), INTENT(IN), OPTIONAL :: R
     TYPE (INTERVAL)                     :: IVAL
  !  IVAL_ZERO= IVAL_ONE ! assignment to constant will not compile !
     IVAL%INF = L
     IF ( .NOT. PRESENT(R) ) THEN
       IVAL%SUP = L                               ! point interval !
     ELSE IF ( L <= R ) THEN
       IVAL%SUP = R
     ELSE
       CALL WARNING(' Inverted bounds - have been interchanged')
       IVAL = INTERVAL(R, L)
     END IF
   END FUNCTION IVAL
 
   FUNCTION INF (INTVAL)        ! extracts lower bound of interval !
     TYPE (INTERVAL), INTENT(IN)  :: INTVAL
     REAL(KIND=pr)                :: INF
     INF = INTVAL%INF
   END FUNCTION INF
 
   FUNCTION SUP (INTVAL)        ! extracts upper bound of interval !
     TYPE (INTERVAL), INTENT(IN)  :: INTVAL
     REAL(KIND=pr)                :: SUP
     SUP = INTVAL%SUP
   END FUNCTION SUP
 
   FUNCTION MID (INTVAL)   ! computes approx. midpoint of interval !
     TYPE (INTERVAL), INTENT(IN)  :: INTVAL
     REAL(KIND=pr)                :: MID
     MID = 0.5_pr * INTVAL%INF + 0.5_pr * INTVAL%SUP
   END FUNCTION MID
 
   SUBROUTINE GET (INTVAL)        ! reads in lower and upper bound !
     TYPE (INTERVAL), INTENT(OUT) :: INTVAL ! enclosure guaranteed !
     REAL(KIND=pr)                :: I, S
     READ (*,*) I, S
     IF ( I > S ) THEN
       CALL WARNING(' Inverted bounds - have been interchanged')
       INTVAL = INTERVAL( NEAREST(S, down), NEAREST(I, up) )
     ELSE
       INTVAL = INTERVAL( NEAREST(I, down), NEAREST(S, up) )
     END IF
   END SUBROUTINE GET
 
   SUBROUTINE PUT (INTVAL)         ! outputs lower and upper bound !
     TYPE (INTERVAL), INTENT(IN)  :: INTVAL ! enclosure guaranteed !
     WRITE (*,*) '[', NEAREST(INTVAL%INF, down), ',',              &
    &                 NEAREST(INTVAL%SUP, up), ']'
   END SUBROUTINE PUT
 
   FUNCTION POS (INTVAL)                                ! unary +  !
     TYPE (INTERVAL), INTENT(IN) :: INTVAL
     TYPE (INTERVAL)             :: POS
     POS = INTVAL
   END FUNCTION POS
 
   FUNCTION NEG (INTVAL)                                ! unary -  !
     TYPE (INTERVAL), INTENT(IN) :: INTVAL
     TYPE (INTERVAL)             :: NEG
     NEG = INTERVAL( -INTVAL%SUP, -INTVAL%INF )
   END FUNCTION NEG
 
   FUNCTION ADD (L, R)                                 ! binary +  !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     TYPE (INTERVAL)             :: ADD
     ADD%INF = NEAREST(L%INF+R%INF, down)  ! approximation - 1 ulp !
     ADD%SUP = NEAREST(L%SUP+R%SUP, up)    ! approximation + 1 ulp !
   END FUNCTION ADD
 
   FUNCTION SUB (L, R)                                 ! binary -  !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     TYPE (INTERVAL)             :: SUB
     SUB%INF = NEAREST(L%INF-R%SUP, down)  ! approximation - 1 ulp !
     SUB%SUP = NEAREST(L%SUP-R%INF, up)    ! approximation + 1 ulp !
   END FUNCTION SUB
 
   FUNCTION MUL (L, R)                                 ! binary *  !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     TYPE (INTERVAL)             :: MUL
     REAL(KIND=pr)               :: II, IS, SI, SS
     II = L%INF * R%INF ;   IS = L%INF * R%SUP
     SI = L%SUP * R%INF ;   SS = L%SUP * R%SUP
     MUL%INF = NEAREST( MIN(II,IS,SI,SS), down )
     MUL%SUP = NEAREST( MAX(II,IS,SI,SS), up )
   END FUNCTION MUL 
 
   FUNCTION DIV (L, R)                                 ! binary /  !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     TYPE (INTERVAL)             :: DIV
     REAL(KIND=pr)               :: II, IS, SI, SS
     IF ( 0.0_pr .IN. R )                                          &
    &     CALL ERROR(' Division by interval containing zero')
     II = L%INF / R%INF ;   IS = L%INF / R%SUP
     SI = L%SUP / R%INF ;   SS = L%SUP / R%SUP
     DIV%INF = NEAREST( MIN(II,IS,SI,SS), down )
     DIV%SUP = NEAREST( MAX(II,IS,SI,SS), up )
   END FUNCTION DIV
 
   FUNCTION ISQRT (INTVAL)     ! square root function is monotonic !
     TYPE (INTERVAL), INTENT(IN) :: INTVAL
     TYPE (INTERVAL)             :: ISQRT
     IF ( INTVAL%INF < 0.0_pr )                                    &
    &  CALL ERROR(' Square root of interval with negative numbers')
     ISQRT = INTERVAL( NEAREST(SQRT(INTVAL%INF), down),            &
    &                  NEAREST(SQRT(INTVAL%SUP), up) )
   END FUNCTION ISQRT
 
   FUNCTION INTERSECTION (L, R)    ! intersection of two intervals !
     TYPE (INTERVAL), INTENT(IN) :: L, R  ! (must not be disjoint) !
     TYPE (INTERVAL)             :: INTERSECTION
     IF ( .NOT. (L .DJ. R) ) THEN    ! PARENTHESES are NECESSARY!! !
       INTERSECTION = INTERVAL(MAX(L%INF, R%INF), MIN(L%SUP, R%SUP))
     ELSE
       CALL ERROR(' Intersection of disjoint intervals')
     END IF
   END FUNCTION INTERSECTION
 
   FUNCTION INTERVALHULL (L, R)   ! interval hull of two intervals !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     TYPE (INTERVAL)             :: INTERVALHULL
     INTERVALHULL = INTERVAL( MIN(L%INF, R%INF), MAX(L%SUP, R%SUP) )
   END FUNCTION INTERVALHULL
 
   FUNCTION SUBSET (L, R)                  ! Is L a subset of R ?  !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     LOGICAL                     :: SUBSET
     SUBSET = L%INF >= R%INF .AND. L%SUP <= R%SUP
   END FUNCTION SUBSET
 
   FUNCTION SUPERSET (L, R)              ! Is L a superset of R ?  !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     LOGICAL                     :: SUPERSET
     SUPERSET = L%INF <= R%INF .AND. L%SUP >= R%SUP
   END FUNCTION SUPERSET
 
   FUNCTION DISJOINT (L, R)  ! Do L and R have no common points ?  !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     LOGICAL                     :: DISJOINT
     DISJOINT = L%SUP < R%INF .OR. L%INF > R%SUP
   END FUNCTION DISJOINT
 
   FUNCTION IN (POINT, INTVAL)  ! Is POINT an element of INTVAL ?  !
     REAL(KIND=pr), INTENT(IN)   :: POINT
     TYPE (INTERVAL), INTENT(IN) :: INTVAL
     LOGICAL                     :: IN
     IN = POINT >= INTVAL%INF .AND. POINT <= INTVAL%SUP
   END FUNCTION IN
 
   FUNCTION EQUAL (L, R)   ! Are L and R equal (identical sets) ?  !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     LOGICAL                     :: EQUAL
     EQUAL = L%INF == R%INF .AND. L%SUP == R%SUP
   END FUNCTION EQUAL
 
   FUNCTION NOTEQUAL (L, R)      ! Are L and R different (sets) ?  !
     TYPE (INTERVAL), INTENT(IN) :: L, R
     LOGICAL                     :: NOTEQUAL
     NOTEQUAL = L%INF /= R%INF .OR. L%SUP /= R%SUP
   END FUNCTION NOTEQUAL
 
 END MODULE  IVALMOD

 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 PROGRAM ivalarith
    USE IVALMOD
    IMPLICIT NONE
    INTEGER, PARAMETER :: dp= SELECTED_REAL_KIND( P=15, R=307 )   ! Declaration of double precision 
    REAL( KIND= dp ) :: f0, df, b0, db, g0, dg                    ! Declaration of the real variables
    TYPE(INTERVAL) :: G1, G2, F, B                                ! Declaration of the intervals

!----------Prompt of values----------!


    WRITE(*,*) 'Enter the focal length of the lens.'
    READ(*,*) f0
    WRITE(*,*) 'Enter the deviation of the focal lenght.'
    READ(*,*) df
    WRITE(*,*) 'Enter the image distance.'
    READ(*,*) b0
    WRITE(*,*) 'Enter the deviation of the image distance.'
    READ(*,*) db

!----------Calculation----------!

    g0= (1/((1/f0)-(1/b0)))

    dg= (df/((1-f0/b0)*(1-f0/b0))) + (db/((b0/f0-1)*(b0/f0-1)))

    G1= IVAL( g0-dg, g0+dg ) 

    F= IVAL( f0-df, f0+df )

    B= IVAL( b0-db, b0+db )

    G2= (IVAL_ONE/((IVAL_ONE/F)-(IVAL_ONE/B)))
    
!----------Output of the sets----------!

    WRITE(*,*) 'Intervall F:'
    CALL PUT( F )
    WRITE(*,*)
    WRITE(*,*) 'Intervall B:'
    CALL PUT( B )
    WRITE(*,*)
    WRITE(*,*) 'Intervall G1:'
    CALL PUT( G1 )
    WRITE(*,*)
    WRITE(*,*) 'Intervall G2:'
    CALL PUT( G2 )
    WRITE(*,*)

!----------Output of set correlation----------!

    IF( G1==G2 ) THEN
      WRITE(*,*) 'The sets are identic.'
      WRITE(*,*)
    ELSEIF( G1.DJ.G2 ) THEN
      WRITE(*,*) 'There is no set correlation.'
      WRITE(*,*)
    ELSEIF( G1.SB.G2 ) THEN
      WRITE(*,*) 'G1 is a subset of the superset G2.'
      WRITE(*,*)
    ELSEIF( G2.SB.G1 ) THEN
      WRITE(*,*) 'G2 is a subset of the superset G1.'
      WRITE(*,*)
    ELSE
      WRITE(*,*) 'The intersection is:'
      CALL PUT( G1.ISECT.G2 )
      WRITE(*,*)
      WRITE(*,*) 'The set union is:'
      CALL PUT( G1.IHULL.G2 )
      WRITE(*,*)
    ENDIF

!----------Output of Warning in the IF-CASE----------!
    
    IF(.NOT.(G1.SP.G2)) THEN
      CALL WARNING('The set G1 does not read the set, of all possible values of g, correctly.') 
      WRITE(*,*)
    ENDIF

 END PROGRAM

 ! Editors: Anton Buehlmeyer & Christian Taubert