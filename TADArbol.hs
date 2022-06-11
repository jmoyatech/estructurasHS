data Arbol a = Vacio | Nodo a [Arbol a] deriving (Show,Eq,Ord)

{- si fuera binario seria con (Arbol a, Arbol a) 
10
22   35 52

15 12   33

map f[] -> [f a]
-}

a1::Arbol Integer
a1 = Nodo 10 [a11,a12,a13]
    where
    a11 = Nodo 22 [Nodo 15 [], Nodo 12 []]
    a12 = Nodo 35 []
    a13 = Nodo 52 [Nodo 33 []]    



raiz::Arbol a->a
raiz Vacio = error 'Nodo vacio'
raiz Nodo a _ = a


tamano::Arbol a->Integer
tamano Vacio = 0
tamano (Nodo _ y) = 1 + sum (map tamano y)

profundidad::Arbol a->Integer
profundidad Vacio = 0
profundidad (Nodo _ []) = 1 
profundidad (Nodo _ y) = 1 + max3 (map profundidad y)

max2::Ord a=>a->a->a
max2 a b
    |a>=b =a
    |otherwise =b

max3::Ord a=>[a]->a
max3 (x:[]) = x
max3 (x:xs) = max2 x (max3 xs)

data ArbolB a = VacioB | NodoB (ArbolB a) a (ArbolB a) deriving (Show,Eq)

esta::Eq a=>a->ArbolB a->Bool
esta x VacioB = False
esta x (NodoB i r d)
    |x==r =True
    |otherwise =(esta x i) || (esta x d)

