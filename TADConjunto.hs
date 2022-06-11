data Conj =Vacio|Ins Integer Conj deriving Show
estaen:: Integer -> Conj -> Bool
estaen x Vacio=False
estaen x (Ins i m)
	|x==i =True
	|otherwise =estaen x m

union:: Conj->Conj->Conj
union Vacio x=x
union m (Ins i t)
	|not (estaen i m) = Ins i (union m t)
	|otherwise =union m t

intersec:: Conj -> Conj -> Conj
intersec Vacio x = Vacio
intersec x (Ins m t)
	|estaen m x = Ins m (intersec x t)
	|otherwise = intersec x t

extrae::Integer -> Conj -> Conj
extrae x Vacio=Vacio
extrae x (Ins i y)
	|x==i =y
	|otherwise = extrae x y

cardinal::Conj -> Integer
cardinal Vacio = 0
cardinal (Ins x s) = 1 + cardinal s

esvacio:: Conj -> Bool
esvacio Vacio=True
esvacio _=False

igual:: Conj->Conj->Bool
igual Vacio Vacio = True
igual x y
    |esvacio (resta x y) =True
    |otherwise =False
    
lista:: [Integer]->Conj
lista [] =Vacio
lista(x:xs)=Ins x (lista xs)

delista:: Conj->Conj
delista Vacio = Vacio
delista (Ins x t)
    |estaen x t = delista (extrae x t)

rx:: Integer -> Conj -> Conj
rx x Vacio = Vacio
rx x (Ins y ys)
    |x==y = rx x ys
    |otherwise = Ins y (rx x ys)
--la resta producira error si restamos al reves
resta::Conj->Conj->Conj
resta Vacio x = x
resta (Ins x xs) y = resta xs (rx x y)
