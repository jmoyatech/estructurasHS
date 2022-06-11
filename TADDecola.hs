--Encolafinal (Encolacabeza 1 Vacia) 2
data Decola a = Vacia|Encolacabeza a (Decola a)|Encolafinal (Decola a) a deriving (Show,Eq)

esvacia::Decola a->Bool
esvacia Vacia =True
esvacia _=False

damecabeza::Eq a=>Decola a ->a
damecabeza Vacia = error "decola vacia"
damecabeza (Encolacabeza c d) = c
damecabeza (Encolafinal d f) 
    |d==Vacia =f
    |otherwise =damecabeza d
    
damefinal::Eq a=>Decola a-> a
damefinal Vacia = error "decola vacia"
damefinal (Encolafinal d f) = f
damefinal (Encolacabeza c d)
    |d==Vacia =c
    |otherwise =damefinal d

desencolacabeza::Eq a=>Decola a->Decola a
desencolacabeza Vacia = Vacia
desencolacabeza (Encolacabeza c Vacia)=(Encolacabeza c Vacia)
desencolacabeza (Encolafinal d f) 
   |d==Vacia =Vacia
   |otherwise =desencolacabeza d

desencolafinal::Eq a=>Decola a->Decola a
desencolafinal Vacia = Vacia
desencolafinal (Encolafinal d f)= d
desencolafinal (Encolacabeza c d) 
   |d==Vacia =Vacia
   |otherwise =desencolafinal d
