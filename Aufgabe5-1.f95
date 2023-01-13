program spiel2
    implicit none
    integer :: i,l,r,n,l1,r1
    character(len=4) :: op,d

    do
        write(*,*)'Geben Sie ein abgeschlossenes Intervall mit den Intervallgrenzen [l,r] an.'
        read(*,*) l,r
        i=0
        op=''
        r1=r
        l1=l
        n=(r1+l1)/2

        write(*,*) 'Ueberlegen Sie sich eine ganze Zahl in dem Intervall [',l,r,'].&
        & bestaetigen Sie die Zahl mit (=), falls Ihre gedachte Zahl auf dem Bildschirm erscheint.&
        & Ist die ausgegebene Zahl groesser als Ihre gedachte Zahl, schreiben Sie (<) und im andern&
        & Fall (>).'

        do while(op/='=')
            i=i+1
            write(*,*)'Ist Ihre Zahl',n,'?'
            read(*,*) op

            if(op=='<') then
                r1=n
                n=(n+l1)/2

            
            elseif(op=='>') then
                l1=n
                n=(n+r1+1)/2
            
            elseif(op=='=') then
                write(*,*) 'Die Zahl ist',n,'Das Spiel hat',i,'Runden gedauert.'
                exit

            else 
                write(*,*) 'Falscher Operator.'
            
            end if
        end do

        write(*,*)'Moechten Sie ein weitere Runde spielen? Bitte antworten Sie mit ja/nein.'
        read(*,*) d
        if(d=='nein') then
            exit
        end if
    end do
end program
