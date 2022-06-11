---------------------------------------------------------------------------------------

data Pila a = Vacia | Apila a ( Pila a ) | Apila2 Int (Pila a) deriving (Show,Eq)

esvacia :: Pila a ->Bool
esvacia Vacia = True
esvacia ( Apila x p ) = False

tope::Pila a -> a
tope Vacia = error "pila vacia"
tope (Apila x p) = x

desapila :: Pila a -> Pila a
desapila Vacia = error "pila vacia"
desapila (Apila x p) = p

fondo :: Pila a -> a
fondo Vacia = error "pila vacia"
fondo ( Apila x Vacia ) = x
fondo ( Apila x p ) = fondo p

quitafondo :: Pila a -> Pila a
quitafondo (Apila x Vacia) = Vacia
--quitafondo (Apila x (Apila y Vacia)) = (Apila x Vacia)
quitafondo (Apila x p) =(Apila x (quitafondo p))

--concatena (Apila 3 (Apila 2 (Apila 1 Vacia))) (Apila 6 (Apila 5 (Apila 4 Vacia)))
concatena :: Pila a -> Pila a -> Pila a
concatena p1 p2
    |(esvacia p1)==True =p2
    |otherwise =concatena (quitafondo p1) (Apila (fondo p1) p2)

invierte :: Pila a -> Pila a
invierte Vacia = Vacia
invierte (Apila x p) = concatena (invierte p) (Apila x Vacia )

pegapilas :: Pila a -> Pila a -> Pila a
pegapilas Vacia p2 = p2
pegapilas p1 p2 = (pegapilas (desapila p1) (Apila (tope p1) p2))

igual :: Eq a => Pila a -> Pila a -> Bool
igual Vacia Vacia = True
igual Vacia _ = False
igual (Apila x p1) (Apila y p2)
	|x==y = igual p1 p2
	|otherwise = False

espalindromopar :: Eq a => Pila a -> Pila a -> Bool
espalindromopar Vacia _ = True
espalindromopar Vacia Vacia = True
espalindromopar (Apila x1 p1) (Apila x2 p2)
	|x1==x2 && (fondo (Apila x1 p1))/=(fondo (Apila x2 p2)) =(espalindromopar p1 p2)
--	|x1==x2 && (fondo (Apila x1 p1))==(fondo (Apila x2 p2)) =(espalindromopar p1 p2)
	|x1/=x2 && (fondo (Apila x1 p1))==(fondo (Apila x2 p2)) =espalindromopar (quitafondo (Apila x1 p1)) (quitafondo (Apila x2 p2))
	|otherwise =False

{-
Main> espalindromopar (Apila 1 (Apila 2 (Apila 3 Vacia))) (Apila 1 (Apila 2 (Apila 3 (Apila 3 (Apila 2 (Apila 1 Vacia))))))
True :: Bool

Main> espalindromopar (Apila 1 (Apila 2 (Apila 3 Vacia))) (Apila 3 (Apila 2 (Apila 1 (Apila 1 (Apila 2 (Apila 3 Vacia))))))
True :: Bool

funciona con impares: 
Main> espalindromopar (Apila 1 (Apila 2 (Apila 3 Vacia))) (Apila 2 (Apila 1 (Apila 1 (Apila 2 (Apila 3 Vacia)))))
True :: Bool
no funciona:
espalindromopar (Apila 1 (Apila 2 (Apila 3 Vacia))) (Apila 1 (Apila 2 (Apila 3 (Apila 1 (Apila 2 (Apila 3 Vacia))))))
tampoco funciona:
Main> espalindromopar (Apila 1 (Apila 2 (Apila 3 Vacia))) (Apila 1 (Apila 2 (Apila 2 (Apila 1 (Apila 3 (Apila 3 Vacia))))))
True :: Bool
habria que rehacerla
-}

palindroma::Eq a=>Pila a->Bool
palindroma Vacia = True
palindroma p = igual (invierte p) p

sombrero :: Eq a =>Pila a -> Pila a -> Bool
sombrero Vacia p = True
sombrero (Apila x1 p1) (Apila x2 p2)
	|x1==x2 =sombrero p1 p2
	|otherwise = False

essubpila :: Eq a => Pila a -> Pila a -> Bool
essubpila p Vacia = False
essubpila p1 p2
	|sombrero p1 p2 =True
	|otherwise = essubpila p1 (desapila p2)

extrae :: Eq a => Pila a -> a -> Pila a
extrae Vacia x = Vacia
extrae p x
	|(tope p)==x =desapila p
	|otherwise =Apila (tope p) (extrae (desapila p) x )
	
quitasubpila :: Eq a => Pila a -> Pila a -> Pila a
quitasubpila Vacia p2 = p2
quitasubpila p1 p2
	|(essubpila p1 p2) = quitasubpila (desapila p1) (extrae p2 (tope p1))
	|otherwise =p2

borrapilas :: Eq a => Pila a -> Pila a -> Pila a
borrapilas Vacia p2 = p2
borrapilas p1 p2 
	|(essubpila p1 p2) =borrapilas p1 (quitasubpila p1 p2)
	|otherwise =p2
--borrapilas (Apila 5 (Apila 1 (Apila 3 Vacia))) (Apila 2 (Apila 5 (Apila 1 (Apila 3 (Apila 4 (Apila 7 (Apila 5 (Apila 1 (Apila 3 (Apila 6 Vacia))))))))))
--Apila 2 (Apila 4 (Apila 7 (Apila 6 Vacia))) :: Pila Integer

--conway (Apila 3 (Apila 1 (Apila 2 (Apila 2 (Apila 1 (Apila 1 Vacia)))))) 

coincidencias::Eq a=>Pila a->a->Int
coincidencias Vacia x = 0
coincidencias (Apila t p) x 
	|t == x && t /= tope p =1
	|t == x && t == tope p =1 + coincidencias p x
	|t /= x =coincidencias p x
{- -}

veces::Eq a=>Pila a ->a->Int
veces Vacia x = 0
veces p x
	|tope p == x =1 + (veces (desapila p) x)
	|otherwise =0 + (veces (desapila p) x)

desapilanveces::Pila a ->Int->Pila a
desapilanveces Vacia n = Vacia
desapilanveces p n 
	|n==0 =p
	|otherwise =desapilanveces (desapila p) (n-1) 

conway::Eq a=>Pila a->Pila a
conway Vacia = Vacia
conway p = Apila2 (veces p (tope p)) (Apila (tope p) (conway (desapilanveces p (veces p (tope p)))))

sinrepes::Eq a=>Pila a->Pila a
sinrepes Vacia = Vacia
sinrepes (Apila t p) 
	| veces p t == 0 =Apila t (sinrepes p)
	|otherwise = sinrepes p
	
insertaP::Eq a=>a->Pila a->Pila a->Pila a
insertaP x p Vacia = p
insertaP x p (Apila u q) 
	|x==u && esvacia q==False =Apila x (concatena p (insertaP x p q))
	|x==u && esvacia q==True =Apila x p
	|otherwise =Apila u (insertaP x p q)
	
insertaPN::Eq a=>a->Pila a->Pila a->Pila a
insertaPN x p Vacia = p
insertaPN x p q = insertaP x p q