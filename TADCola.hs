data Cola a = Vacia|Encola (Cola a) a deriving (Show,Eq)
--(Encola (Encola (Encola (Encola Vacia 1) 2) 2) 1)
--hecha con genericos

esvacia::Cola a->Bool
esvacia Vacia =True
esvacia _=False

final::Eq a=>Cola a->a
final Vacia = error "cola vacia"
final (Encola q x) =x

cabeza::Cola a->a
cabeza Vacia = error "cola vacia"
cabeza (Encola q x)
	|esvacia q =x
	|otherwise =cabeza q 

resto :: Cola a-> Cola a
resto Vacia = error "Cola vacia"
resto (Encola Vacia x)= Vacia
resto (Encola q x) = Encola (resto q) x

estacontenido::Eq a=>a->Cola a->Bool
estacontenido n Vacia = False
estacontenido a (Encola q x)
	|x==a =True
	|otherwise =(estacontenido a q)

concatena::Cola a->Cola a->Cola a
concatena q Vacia =q
concatena q p = concatena (Encola q (cabeza p)) (resto p)

palindromo::Eq a=>Cola a->Bool
palindromo Vacia = True
palindromo (Encola q x)
    |x==(cabeza q) =palindromo (resto q)
    |otherwise =False
    
damemitad::Cola a->a
damemitad Vacia = error "cola vacia"
damemitad (Encola (Encola (Encola Vacia x) y) z) = y
damemitad (Encola q x) = damemitad q

veces::Eq a =>a->Cola a->Integer
veces x Vacia = 0
veces x (Encola q y)
	|x==y =1+ (veces y q)
	|otherwise =veces x q

{-
masrepe::Cola a->a
masrepe Vacia = error "cola vacia"
masrepe (Encola Vacia u) = u
masrepe (Encola (Encola q u) v)
	|(veces v (Encola q u)) +1 >= (veces u q) + 1 =u
	|otherwise =masrepe (Encola q u)
	 
-}

masrepe::Eq a=>Cola a->a
masrepe Vacia = error "cola vacia"
masrepe (Encola Vacia x) = x
masrepe (Encola c e)
	|((veces e c) + 1) >= (veces (masrepe c) c)  =e
	| otherwise =masrepe c

{-
Main> masrepe (Encola (Encola	(Encola	(Encola	(Encola	(Encola	(Encola	(Encola	(Encola	(Encola Vacia 3) 4) 3) 7) 6) 3) 4) 4) 7) 3)
3 :: Integer

masrepe (Encola (Encola Vacia 1) 2)
veces('2')=1 >= veces('1')=1 Sí, se sale con '2'

masrepe (Encola (Encola (Encola Vacia 1) 2) 2)
veces('2')=2 >= veces(veces('2')=1 >= veces('1')=1) = veces('2')=1 y sale con '2' (el primero)

-}
colardetras::Eq a=>Cola a->a->a->Cola a
colardetras Vacia x y = Encola Vacia x
colardetras (Encola q f) x y
	|f==y =(Encola (Encola q x) f)
	|otherwise =Encola (colardetras q x y) f

--colardelante [1,2,3] 6 2 = [1 6 2 3]

encolarprincipio::Cola a->a->Cola a
encolarprincipio Vacia x = Encola Vacia x
encolarprincipio (Encola Vacia a) x = (Encola (Encola Vacia x) a)
encolarprincipio (Encola c a) x = Encola (encolarprincipio c x) a

colarantesde::Eq a=>Cola a->a->a->Cola a
colarantesde Vacia a j = Encola Vacia j
colarantesde (Encola c f) a j
	|(estacontenido a (Encola c f) == False) =encolarprincipio (Encola c f) j
	|otherwise =colardetras (Encola c f) j a
	
{-
Main> colardetras (Encola (Encola (Encola (Encola Vacia 1) 2) 3) 4) 6 4 
Encola (Encola (Encola (Encola (Encola Vacia 1) 2) 3) 6) 4

Main> encolarprincipio (Encola (Encola (Encola (Encola Vacia 1) 2) 3) 4) 5
Encola (Encola (Encola (Encola (Encola Vacia 5) 1) 2) 3) 4

Main> colarantesde (Encola (Encola (Encola (Encola Vacia 1) 2) 3) 4) 3 5
Encola (Encola (Encola (Encola (Encola Vacia 1) 2) 5) 3) 4
-}
	
borraelemento::Eq a=>a->Cola a->Cola a
borraelemento x Vacia = Vacia
borraelemento x c
	|cabeza c == x =resto c
	|cabeza c /= x =concatena (Encola Vacia (cabeza c)) (borraelemento x (resto c))

agruparelemento::Eq a=>a->Cola a->Cola a
agruparelemento x Vacia = Vacia
agruparelemento x (Encola c f)
	|estacontenido x (Encola c f) == False && x == f =(Encola c f)
	|estacontenido x (Encola c f) == False && x /= f =(Encola c f)
	|estacontenido x (Encola c f) == True && x == f = (Encola (Encola (agruparelemento x (borraelemento x c)) x) x)
	|estacontenido x (Encola c f) == True && x /= f =Encola (agruparelemento x c) f
	
{- mejor hacerlo con un metodo auxiliar que haga anadirelenveces
(añadir un elemento n veces al final de la cola), 
asi saldria si tambien haces un borrarelemento recursivo (varios)
aunque la idea en este no sea mala
 -}

 