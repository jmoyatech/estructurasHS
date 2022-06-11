data Complejo=C Double Double deriving Show

partereal :: Complejo -> Double
partereal(C a b) = a

parteimag :: Complejo -> Double
parteimag(C a b) = b

modulo :: Complejo -> Double
modulo(C a b) = sqrt(a*a+b*b)

argumento :: Complejo -> Double
argumento(C a b) = atan(b/a)

data Mnat=Z|S Mnat deriving Show
--16
suma :: Mnat -> Mnat -> Mnat
suma x Z = x
suma x (S y) = S(suma x y)
--20
cardinal :: Mnat -> Integer
cardinal Z = 0
cardinal (S x) = 1 + cardinal x

igual :: Mnat -> Mnat -> Bool
igual Z Z = True
igual Z (S y) = False
igual (S x) Z = False
igual (S x) (S y) = igual x y

menor :: Mnat -> Mnat -> Bool
menor x Z = False
menor x (S y) = if menor x y == True || igual x Z || igual x y then True else False

espar :: Mnat -> Bool
espar Z = True
espar (S x) =  not (espar x)

