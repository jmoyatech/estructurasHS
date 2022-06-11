-- Empty module to serve as the default current module.
module Hugs where
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n-1)

longitud :: [a] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)

--suma1 :: Integer -> Integer -> Integer
--suma1(a,b) = a + b

--suma2 :: Integer -> Integer
--suma n = if n == 0 then 0 else n + suma (n-1)

suma3 :: [Integer] -> Integer
suma3[] = 0
suma3(x:xs) = x + suma3(xs)

esvacio :: [a] -> Bool
esvacio [] = True
esvacio _ = False

absoluto :: Integer -> Integer
absoluto n = if n > 0 then n else -n

--esmultiplo :: Integer -> Integer -> Boolean
--esmultiplo(a,b) = if

data Sem = Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo deriving Show
esfinde::Sem -> Bool
esfinde Sabado = True
esfinde Domingo = True
esfinde _ = False
