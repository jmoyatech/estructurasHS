data Lista = Vacia|Ann Integer Lista deriving Show

esvacia:: Lista -> Bool
esvacia Vacia = True
esvacia _=False

cabeza::Lista -> Integer
cabeza(Vacia)=error "lista vacia"
cabeza (Ann i m) = i

cola::Lista-> Lista
cola(Vacia)=error "lista vacia"
cola (Ann i m) = m

estaen:: Integer -> Lista -> Bool
estaen x Vacia = False
estaen x (Ann i m)
	|x==i =True
	|otherwise =estaen x m

union:: Lista->Lista->Lista
union Vacia x=x
union m (Ann i t) = Ann i (union m t)

intersec:: Lista -> Lista -> Lista
intersec Vacia x = Vacia
intersec x (Ann m t)
	|estaen m x = Ann m (intersec x t)
	|otherwise = intersec x t

extrae:: Integer->Lista->Lista
extrae x Vacia = Vacia
extrae x (Ann i y)
	|x==i =y
	|otherwise = Ann i (extrae x y)

longitud::Lista -> Integer
longitud Vacia = 0
longitud (Ann x s) = 1 + longitud s

concatena::Lista->Lista->Lista
concatena Vacia x = x
concatena x y = Ann (cabeza x) (concatena (cola x) y)

ultimo::Lista->Integer
ultimo Vacia=error "es una lista vacia"
ultimo (Ann i l)
	|igual l Vacia =i
	|otherwise =ultimo l

invierte::Lista->Lista
invierte Vacia = Vacia
invierte (Ann i l) = concatena ( invierte l ) (Ann i Vacia) 

igual::Lista->Lista->Bool
igual Vacia Vacia = True
igual (Ann i l) (Ann j k)
	|i==j =igual l k
	|otherwise =False
	
{-

ejemplo:1 2 3 4 5

concatena (invierte 2 3 4 5 ) 1 null

concatena ( concatena (invierte 3 4 5 ) 2 null ) 1 null

concatena ( concatena ( concatena (invierte 4 5 ) 3 null ) 2 null) 1 null

concatena ( concatena ( concatena (concatena ( invierte 5 ) 4 null ) 3 null ) 2 null ) 1null

concatena ( concatena ( concatena ( concatena ( concatena ( invierte null ) 5 null ) 4 null ) 3 null ) 2 null ) 1 null

-}

quitaelem::Integer->Lista->Lista
quitaelem x Vacia = Vacia
quitaelem x (Ann i l)
	|x==i =l
	|otherwise =(Ann i (quitaelem x l))

izquierda::Lista -> Integer -> Lista
izquierda Vacia x = Vacia
izquierda l n
	|(n==0) =Vacia
	|(longitud l)<=n =l
	| otherwise =Ann (cabeza l) (izquierda (cola l) (n-1))

reemplazar::Lista->Integer->Integer->Lista
reemplazar Vacia _ _ = Vacia
reemplazar (Ann i l) x y
	|x==i =(Ann y l)
	|otherwise =(Ann i (reemplazar l x y))

cremallera::Lista->Lista->Lista
cremallera x Vacia = x
cremallera (Ann i l) (Ann j m) = (Ann i (Ann j (cremallera l m)))

espar::Lista->Bool
espar Vacia = True
espar (Ann i l) = not (espar l)

esimpar::Lista->Bool
esimpar x = not (espar x)

longitudmedia::Lista->Integer
longitudmedia Vacia = 0
longitudmedia (Ann i Vacia) = 1
longitudmedia (Ann i (Ann j l))=1 + longitudmedia l

derecha::Lista->Integer->Lista
derecha Vacia n = Vacia
derecha x n
        |(longitud x)<n =x
        |(longitud x)>n =derecha (cola x) n
        |otherwise =x
        
barajar::Lista->Integer->Lista
barajar Vacia n = Vacia
barajar x 0 = x
barajar x n = (barajar (cremallera (derecha x (longitudmedia x)) (izquierda x (longitudmedia x))) (n-1))

veces::Integer->Lista->Integer
veces _ Vacia = 0
veces x (Ann i l)
      |x==i =1 + (veces x l)
      |otherwise =(veces x l)

ponelultimo::Lista->Integer->Lista
ponelultimo (Ann i Vacia) x = (Ann i (Ann x Vacia))
ponelultimo (Ann i l ) x = (Ann i (ponelultimo l x))

menor::Lista->Integer
menor Vacia = error "Lista vacia"
menor (Ann i Vacia) = i
menor (Ann i l)
      |i<=cabeza l =menor(Ann i (cola l))
      |otherwise =menor l

ordenar::Lista->Lista
ordenar Vacia = Vacia
ordenar l = Ann (menor l) (ordenar (extrae (menor l) l))

primerrepe::Lista->Integer
primerrepe Vacia = error "lista vacia"
primerrepe (Ann i Vacia) = i
primerrepe (Ann i l)
           |i==(cabeza l) =i
           |otherwise =primerrepe l

longrepes::Lista->Integer
longrepes (Ann i Vacia) = 1
longrepes (Ann i l)
          |i==(primerrepe l) =1+longrepes l
          |otherwise =0+longrepes l
          
intercambiar::Lista->Lista
intercambiar (Ann i Vacia) = (Ann i Vacia)
intercambiar (Ann i l)
             |i<=(cabeza l) =(Ann i (intercambiar l))
             |otherwise =(Ann (cabeza l) (intercambiar (Ann i (cola l))))
             
desorden::Lista->Bool
desorden (Ann i Vacia) = False
desorden (Ann i l)
         |i<=(cabeza l) = desorden l
         |otherwise =True
         
burbuja::Lista->Lista
burbuja Vacia = Vacia
burbuja x
        |desorden x==True =burbuja (intercambiar x)
        |otherwise =x
        
inter::Lista->Integer->Lista
inter Vacia x = Vacia
inter (Ann i l) x 
      |i==x =(Ann (cabeza l) (Ann i (inter (cola l) x)))
      |otherwise =Ann i (inter l x)
      
sublista::Lista->Integer->Integer->Lista
sublista Vacia x y = Vacia
sublista l x y 
	|x>=y =Vacia
	|x==1 = izquierda l y
	|otherwise =sublista (cola l) (x-1) (y-1)
	
--criba de eratostenes [2,3,4,5,6,7,8,9,10,11,12,13,14,15] = [2,3,5,7,11,13]

cribar::Integer->Lista->Lista
cribar x Vacia = Vacia
cribar x (Ann i l)
	|(mod i x)==0 = cribar x l
	|otherwise = (Ann i (cribar x l))
	
eratostenes::Lista->Lista
eratostenes Vacia = Vacia
eratostenes (Ann i l) = (Ann i (eratostenes (cribar i l)))

{-
Main> eratostenes (Ann 2 (Ann 3 (Ann 4 (Ann 5 (Ann 6 (Ann 7 (Ann 8 (Ann 9 (Ann 10 (Ann 11 (Ann 12 (Ann 13 (Ann 14 (Ann 15 Vacia))))))))))))))

Ann 2 (Ann 3 (Ann 5 (Ann 7 (Ann 11 (Ann 13 Vacia))))) :: Lista
-}

par::Integer->Bool
par x = x `mod` 2 == 0
	
sumarpares::Lista->Lista
sumarpares Vacia = Vacia
sumarpares (Ann i l)
	|par i == False = (Ann i (sumarpares l))
	|par i == True && par (cabeza l) == False = (Ann i (sumarpares l))
	|par i == True && par (cabeza l) == True = sumarpares (Ann (i+(cabeza l)) (cola l))
	
	
--(Ann 2 (Ann 1 (Ann 2 (Ann 4 (Ann 5 (Ann 7 (Ann 7 (Ann 8 (Ann 10 (Ann 15 Vacia ))))))))))