{-
Corrección: Reentrega amistosa del 3a. Hacer sin quitarRepetidos

- 1a: Se puede hacer sin la lista [1..n-1]. ¿Se te ocurre cómo?
- 3a: ¿Se te ocurre cómo hacerlo sin tener que sacar repetidos?
-}


--1)  
--a) 
--Funciones auxiliares:
-- Esta función toma una lista y un entero n, y me devuelve los divisores de n que están en la lista 
divisoresDeNEnLista :: [Int] -> Int -> [Int]
divisoresDeNEnLista [] n = []
divisoresDeNEnLista (x:xs) n | n `mod` x /= 0 = divisoresDeNEnLista xs n 
                                    | otherwise = x : divisoresDeNEnLista xs n  
-- Suma los elementos de una lista
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs 

-- fin de funciones auxiliares 

sumaDeDivisoresPropios :: Int -> Int
sumaDeDivisoresPropios n = sumatoria (divisoresDeNEnLista lista_n n)
 where lista_n = [1..n-1]

{- 
MG: Ejercicio 1a está bien.
Es un poco rebuscada la forma, se podría hacer más simple, es decir, sin necesidad de crear una lista con todos los números entre 1 y n-1. 
-}

-- b) 
esPerfecto :: Int -> Bool
esPerfecto n | sumaDeDivisoresPropios n == n = True
             | otherwise = False

{- 
MG: Ejercicio 1b está bien. 
Podés retornar directamente la expresión sumaDeDivisoresPropios n == n, fijate que si eso es True devolves True, y otherwise, o sea si es False, devolves False. Bueno, podes devolver directamente la expresión esa. Sería:

esPerfecto n = sumaDeDivisoresPropios n == n
-}


--2) 
--a) Calcula los primeros k elementos de la alicuota n
listaAlicuotaDeNDeLargo :: Int -> Int -> [Int]
listaAlicuotaDeNDeLargo 1 n = [n]
listaAlicuotaDeNDeLargo k n = n:(listaAlicuotaDeNDeLargo (k-1) (sumaDeDivisoresPropios n))

{- 
MG: Ejercicio 2a está bien. 
-}

--b) 
-- Funciones auxiliares:
-- Me dice si x pertenece o no a una lista l
pertenece :: Int -> [Int] -> Bool
pertenece x l | l == [] = False
              | otherwise = (x == head l) || pertenece x (tail l) 

-- Me dice si hay repetidos en una lista
hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos c | pertenece (head c) (tail c) = True
               | otherwise = hayRepetidos (tail c)

-- Una lista es casi sociable si la suma de divisores propios de a_k es igual a a_k+1 en una lista
casiSociable :: [Int] -> Bool
casiSociable c | tail c == [] = True 
               | otherwise = (sumaDeDivisoresPropios (head c) == head (tail c)) && casiSociable (tail c)
--fin auxiliares               

esSociable :: [Int] -> Bool
esSociable c| (head c == sumaDeDivisoresPropios (last c)) && (casiSociable c) && (not (hayRepetidos c)) = True
            | otherwise = False

{- 
MG: Agregado por el docente para correr los tests. El nombre eran sonSociables. 
-}
sonSociables = esSociable

{- 
MG: Ejercicio 2b está bien. 
Nuevamente podes sacar la guarda del otherwise False y tampoco devolver True en la primera. Dejar solo 
esSociable c = (head c == sumaDeDivisoresPropios (last c)) && (casiSociable c) && (not (hayRepetidos c))
Si toda esa expresión es True vas a devolver eso mismo, True, y si es False devuelve False
-}

-- 3)
--a) 
--Funciones auxiliares:
-- Esta función me devuelve el mínimo de una lista
minimo :: [Int] -> Int
minimo [x] = x
minimo c | head c < (minimo (tail c)) = head c
         | otherwise = minimo (tail c)   

--Quita repetidos de una lista
quitarRepetidos :: [Int] -> [Int]
quitarRepetidos [x] = [x]
quitarRepetidos c | ((head c) `pertenece` (tail c)) = quitarRepetidos (tail c)
                  | otherwise = (head c):(quitarRepetidos (tail c))

{- MG: Ojo, esto puede fallar con la lista vacía. Aunque acá no te van a llamar nunca a esta función con una vacía. Es una observación más en general -}

-- Esta función se fija si en una lista todos los elementos son menores iguales que cierto entero k
tieneTodosLosElementosMenoresIgualesQue :: [Int] -> Int -> Bool
tieneTodosLosElementosMenoresIgualesQue [] k = True
tieneTodosLosElementosMenoresIgualesQue c k = (head c <= k) && (tieneTodosLosElementosMenoresIgualesQue (tail c) k)

-- fin auxiliares

minimosDeKClubesMenoresQue :: Int -> Int -> [Int]
minimosDeKClubesMenoresQue k 1 = []
minimosDeKClubesMenoresQue k a | esSociable (alicuotas) = quitarRepetidos (minimo (alicuotas):(minimosDeKClubesMenoresQue k (a-1)))
                               | otherwise = (minimosDeKClubesMenoresQue k (a-1))
 where alicuotas = listaAlicuotaDeNDeLargo k a

{- 
MG: Ejercicio 3a está bien. 
¿Se te ocurre cómo hacerlo sin tener que sacar repetidos?
-}

-- b)
-- (lista alicuotas es decreciente entonces el primer elemento es el máximo)
listaDeNClubesConNrosMenoresQue :: Int -> Int -> [[Int]]
listaDeNClubesConNrosMenoresQue _ 0 = []
listaDeNClubesConNrosMenoresQue n k | (esSociable (alicuotas)) && (tieneTodosLosElementosMenoresIgualesQue (alicuotas) k) = (alicuotas):(listaDeNClubesConNrosMenoresQue n (k-1))
                                    | otherwise = listaDeNClubesConNrosMenoresQue n (k-1)
 where alicuotas = listaAlicuotaDeNDeLargo n k

 {- 
MG: Ejercicio 3b está bien. 
-}