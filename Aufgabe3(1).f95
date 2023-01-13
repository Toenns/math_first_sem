PROGRAM zins
    IMPLICIT NONE
    INTEGER, PARAMETER :: real_kind= SELECTED_REAL_KIND(P=15, R=307)
    REAL(KIND=real_kind) :: c,u,r,s,i               ! c:= amount of credits, u:= interest rate to be applied , r:= rate of  return, s:= interest owed, i:= yearly instalment
    INTEGER :: n                                    ! runtime in years
    
    write(*,*) 'Enter your amount of credits (in Euro), your rate of return (in %) and your yearly instalment (in Euro).'
    read(*,*) c,r,i                                 !Einlesen der Werte
    u=c*(r/100)                                     !Berechnung der Zinshöhe des ersten Jahres
    do while(i<=u)                                  !Abfragen, ob Zinshöhe<Jahresrate ist
        write(*,*) 'Increase your yearly pay-off.'
        read(*,*) i
    end do
    n=0
    s=0
    do while(c>0)                                   !Solange die Restschuld > 0 ist wird der Kredit weiter getilgt.
        n=n+1                                       !Laufzeitaufsummierung in Jahren
        u=c*(r/100)                                 !Ermittlung der aktuellen Zinshöhe 
        c=(c+u)-i                                   !Addieren der Zinsen auf die Restschuld und subtrahieren der Jahresrate
        s=s+u                                       !Aufsummieren der gezahlten Jahreszinsen
        if(c<0) then                                !Abfragen, ob die letzte Rate den Kredit getilgt hat. Wenn ja,
        i=i+c                                       !Ermittlung der Höhe der letzten Jahresrate
        write(*,*) 'Last rate:',i                   !Ausgabe der zu letzt zahlenden Rate
        end if                                       
    end do
    write(*,*)'Runtime in years:',n
    write(*,*)'Interest owed in Euro:',s            !Ausgabe der Laufzeit und der Zinssume
end program


