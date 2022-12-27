MODULE FiboModule
    IMPLICIT NONE

    PRIVATE  
    PUBLIC :: fibo_rekursiv, fibo_iterativ

CONTAINS
    RECURSIVE FUNCTION fibo_rekursiv(var1) RESULT (res)                 ! Rekursive Funktion  
        INTEGER, PARAMETER :: ik= SELECTED_INT_KIND( R=18 )
        INTEGER ( KIND=ik ) :: var1                   
        INTEGER ( KIND=ik ) :: res  

        IF(var1==0) THEN                                                !für Variable = 0, ist das Ergebnis = 1
            res = 0
        ELSEIF(var1==1) THEN
            res = 1                                                     !für Variable = 1, ist das Ergebnis = 1
        ELSEIF(var1>=2) THEN
            res = fibo_rekursiv(var1-1) + fibo_rekursiv(var1-2)         !für Variable >= 2 ist das Ergebnis aus der (Funktion von variable -1) + (Funktion von variable-2)
        ENDIF
    END FUNCTION

    FUNCTION fibo_iterativ(var2)                                        !Iterative Funktion
        INTEGER, PARAMETER :: ik= SELECTED_INT_KIND( R=18 )
        INTEGER ( KIND=ik ) :: fibo_iterativ
        INTEGER ( KIND=ik ) :: var2
        INTEGER ( KIND=ik ) :: summand1 = 0
        INTEGER ( KIND=ik ) :: summand2 = 1
        INTEGER ( KIND=ik ) :: summe = 0
        INTEGER ( KIND=ik ) :: i 

        summand1 = 0
        summand2 = 1
        summe = 0
         
        DO i=1,var2                                             
            summand1 = summand2                                 ! Zuweisung des Wertes von Summand2 an Summand1
            summand2 = summe                                    ! Zuweisung der Summe an Summand2
            summe = summand1 + summand2                                                             
        END DO 
        fibo_iterativ = summe  
  
    END FUNCTION

END MODULE FiboModule

PROGRAM Fibonaccifolge
    USE fibomodule
    IMPLICIT NONE
        INTEGER, PARAMETER :: ik= SELECTED_INT_KIND( R=18 )
        INTEGER ( KIND=ik ) :: var3, iterativ, rekursiv
        REAL :: start, finish
        
        DO
            WRITE(*,*) 'Enter your number.'
            READ(*,*) var3
            
            IF(var3<0) THEN
                EXIT
            ENDIF

            CALL CPU_TIME(start)

            iterativ= fibo_iterativ(var3)
            rekursiv= fibo_rekursiv(var3)

            IF(iterativ>0) THEN
                WRITE(*,*) 'The ',var3,'. number of the Fibonacci sequenz (calculated iterative) is:',iterativ   
                WRITE(*,*) 'The ',var3,'. number of the Fibonacci sequenz (calculated recursive) is:',rekursiv   
                WRITE(*,*) 

                CALL CPU_TIME(finish)

                WRITE(*,*) 'The calculation took',(finish-start),'seconds.'  
                WRITE(*,*)

            ELSE
                WRITE(*,*) 'Error due to overload.'
            ENDIF   
        END DO 

        

    END PROGRAM Fibonaccifolge


    ! Editors: Anton Buehlmeyer, Chirstian Taubert

    ! Rekursive Funktionen benötigen mehr Arbeitsspeicher und es kann bei zu hoher Rekursionstiefe zu einem Stakoverflow (Sapelüberlauf) kommen.
    ! Stapelüberlauf ist ein Programmierfehler, auf den ein Thread im Benutzermodus stößt, wenn er versucht, 
    ! mehr Daten zu schreiben, der Speicherblock jedoch nicht mehr genügend Platz hat, um sie zu speichern.
    ! Die rekursive Funktion liefert zuverlässige Ergebnisse, jedoch steigt die Berechnungsdauer mit zunehmender Rekursionstiefe. 
    ! Bis zum 40.ten Wert ist die Laufzeit vertretbar, darüber dauern die Rechenprozesse zu lange an.

    !Es lassen sich Werte bis 103 errechnen. Danach kommt es zum Stapelüberlauf.