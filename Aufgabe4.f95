PROGRAM zyklus 
    IMPLICIT NONE
    INTEGER, PARAMETER :: real_kind= SELECTED_REAL_KIND(P=15, R=307)
    REAL(KIND=real_kind) :: s=0
    INTEGER :: a=0,n,max,min,i
    
    do while(a<1)                                       !Eingabeschleife für Anzahl der Elemente der Menge
        write(*,*) 'Enter your amount of integer.'      !bis Bedingung erfüllt ist.
        read(*,*) a
    end do

    write(*,*) 'Enter your set element.'                 
    read(*,*) n
    max=n
    min=n
    s=n

    do i=2,a,1                                          !DO-Schleife von 2 bis a in Schrittweite 1                     
        write(*,*) 'Enter your set element.'                 
        read(*,*) n
        
        if(n>max) then                                  !Wenn n> aktuellem Max, neu definieren
            max=n

        else if(n<min) then                                  !Wenn n< aktuellem Min, neu definieren
            min=n
            
        end if

        s=s+n                                           !Aufsummieren der Elemente der Menge
        
    end do 

    write(*,*) 'The maximum of the set is:',max         !Ausgabe der Werte
    write(*,*) 'The minimum of the set is:',min
    write(*,*) 'The sum of the set is:',s
    write(*,*) 'The average of the set is:',(s/a)

end program
