-- Ejercicio 1
max2 :: (Int,Int) -> Int
max2 (x, y) 
    | x >= y = x
    | otherwise = y

-- No esta currificada
-- currificada : max2 x y = if x >= y then x else y

normaVectorial :: (Float,Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

-- No esta currificada
-- currificada : normaVectorial x y = sqrt (x^2 + y^2)

-- Las versiones currificadas me permiten aplicacion parcial , ej:
-- distanciaAOrigen n = normaVectorial 0 n --> me dice la distancia de n al origen.


subtract1 :: Int -> Int -> Int
subtract1 = flip (-)
-- esta currificada

predecesor :: Int -> Int
predecesor = subtract 1
-- esta currificada

evaluarEnCero :: (Int -> a) -> a
evaluarEnCero = \f -> f 0
-- esta currificada

dosVeces :: (a -> a) -> (a -> a)
dosVeces = \f -> f . f
-- esta currificada

flipAll :: [(a -> b -> c)] -> [(b -> a -> c)]
flipAll = map flip
-- esta currificada

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip
-- esta currificada
-- una version de aplicacion mas clara es: flipRaro x f y = f y x

------------------------------------------------------------------------------------------------------------------------------
-- Ejercicio 2
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a,b) = f a b
--------------------------------------------------------------------------------------------
-- Ejercicio 3

-- I)
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x:xs) = f x (foldr f z xs)

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f z []     = z
-- foldl f z (x:xs) = foldl f (f z x) xs

sumF :: Num a => [a] -> a
sumF = foldr (+) 0

elemF :: Eq a => a -> [a] -> Bool
elemF n = foldr (\x acc -> x == n || acc) False

concatenar :: [a] -> [a] -> [a]
concatenar xs ys = foldr (:) ys xs

filterF :: (a -> Bool) -> [a] -> [a]
filterF f = foldr (\x acc -> if f x then x : acc else acc) [] 

mapF :: (a -> b) -> [a] -> [b]
mapF f = foldr (\x acc -> f x : acc) []

-- II)
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x acc -> if f x acc then x else acc)

-- III)
sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = drop 1 (foldr step [sum xs] xs)
  where
    step :: Num a => a -> [a] -> [a]
    step x (a:acc) = (a - x) : (a:acc)

-- [1,4,-1,0,5] ❀ [1,5,4,4,9].

-- IV)
sumAlt :: Num a => [a] -> a
sumAlt = foldr (-) 0

-- V)
sumAlt2:: [Integer] -> Integer
sumAlt2 xs = foldr (-) 0 (reverse xs)


--------------------------------------------------------------------------------------------
-- Ejercicio 4

permutaciones :: [a] -> [[a]]
permutaciones = foldr step [[]]
  where
    step :: a -> [[a]] -> [[a]]
    step x acc = concatMap (insertarEnCadaPos x) acc

    insertarEnCadaPos :: a -> [a] -> [[a]]
    insertarEnCadaPos x xs = [take i xs ++ [x] ++ drop i xs | i <- [0..length xs]]

-- II)
partes :: [a] -> [[a]]
partes = foldr (\x acc -> acc ++ map (x :) acc) [[]]

-- III)
prefijos :: [a] -> [[a]]
prefijos = foldr (\x acc -> [] : (map (x:) acc)) [[]] 
 
-- IV)
sublistas :: [a] -> [[a]]
sublistas = foldr step [[]]
  where
    step :: a -> [[a]] -> [[a]]
    step x [] = [[x]]
    step x acc =  acc ++ map (x :) (prefijos (last acc)) 

--------------------------------------------------------------------------------------------
-- Ejercicio 5

-- TODO)

--------------------------------------------------------------------------------------------
-- Ejercicio 6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna n = recr (\x xs acc -> if x == n then xs else x : acc ) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado n = recr (\x xs acc -> if n <= x then n : x : xs else x : acc) []

--------------------------------------------------------------------------------------------
-- Ejercicio 7

mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = foldr (\(x,y) acc -> f x y : acc ) [] 

armarPares :: [a] -> [b] -> [(a,b)]
armarPares xs ys = (foldr step (\bs -> []) xs) ys
  where
    step :: a -> ([b] -> [(a,b)]) -> ([b] -> [(a,b)])
    step x acc ys = case ys of
      []      -> []
      (y:ys') -> (x,y) : acc ys'

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = mapPares f (armarPares xs ys)

--------------------------------------------------------------------------------------------
-- Ejercicio 8

sumaMat :: [[Int]] -> [[Int]] -> [[Int]] 
sumaMat xs ys = (foldr step (\bs -> []) xs) ys
  where 
    step :: [Int] -> ([[Int]] -> [[Int]]) -> ([[Int]] -> [[Int]])
    step x acc ys = case ys of
      []      -> []
      (y:ys') -> zipWith (+) x y : acc ys'

trasponer :: [[Int]] -> [[Int]]
trasponer = foldr step []
  where
    step :: [Int] -> [[Int]] -> [[Int]]
    step x [] = map (:[]) x
    step x ys = zipWith (:) x ys

--------------------------------------------------------------------------------------------
-- Ejercicio 9

foldNat :: (Int -> b -> b) -> b -> Int -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

potencia :: Int -> Int -> Int
potencia n = foldNat (\_ acc -> n*acc ) 1

factorial :: Int -> Int
factorial = foldNat (*) 1

--------------------------------------------------------------------------------------------
-- Ejercicio 10

genLista :: a -> (a -> a) -> Int -> [a]
genLista x f = foldNat step [] 
  where 
    step _ acc = case acc of
      [] -> [x]
      ys -> ys ++ [f (last ys)]
      
desdeHasta :: Int -> Int -> [Int]
desdeHasta d h = genLista d (+1) (h-d+1)

--------------------------------------------------------------------------------------------
-- Ejercicio 11
data Polinomio a = X |Cte a |Suma (Polinomio a) (Polinomio a) |Prod (Polinomio a) (Polinomio a)

foldPol :: (b -> b -> b) -> (b -> b -> b) -> (a -> b) -> b -> Polinomio a -> b
foldPol suma prod cte x pol = case pol of
  X -> x
  Cte a -> cte a
  Suma p1 p2 -> suma (foldPol suma prod cte x p1) (foldPol suma prod cte x p2)
  Prod p1 p2 -> prod (foldPol suma prod cte x p1) (foldPol suma prod cte x p2)
  
evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPol (+) (*) id n 

-- Representa el polinomio: 2*X^3 + 3*X + 5
polinomio :: Polinomio Integer
polinomio = Suma (Suma (Prod (Cte 2) (Prod X (Prod X X))) (Prod (Cte 3) X)) (Cte 5)

--------------------------------------------------------------------------------------------
-- Ejercicio 12
data AB a = Nil | Bin (AB a) a (AB a)

-- Arbol ejemplo
arbol :: AB Int
arbol = Bin (Bin (Nil) 3 (Bin (Nil) 4 (Nil))) 5 (Bin (Nil) 10 (Nil))

-- I)
foldAB :: (b -> a -> b -> b) -> b -> AB a -> b
foldAB f z Nil = z
foldAB f z (Bin izq dato der) = f (foldAB f z izq) dato (foldAB f z der)

recAB :: (b -> a -> b -> AB a -> b) -> b -> AB a -> b
recAB f z Nil = z
recAB f z (Bin izq dato der) = f (recAB f z izq) dato (recAB f z der) (Bin izq dato der)

-- II)

esNil :: AB a -> Bool
esNil ab = case ab of
  Nil -> True
  _   -> False

altura :: AB a -> Int
altura = foldAB alt 0
  where
    alt izq _ der = 1 + max izq der

cantNodos :: AB a -> Int 
cantNodos = foldAB conteo 0
  where
    conteo izq _ der = 1 + izq + der

--III)

mejorSegunAb :: (a -> a -> Bool) -> AB a ->  Maybe a
mejorSegunAb f Nil                = Nothing
mejorSegunAb f (Bin izq dato der) = Just (foldAB (step f) dato (Bin izq dato der)) 
  where 
    step :: (a -> a -> Bool) -> a -> a -> a -> a
    step f izq dato der = if f dato (mejor f izq der) then dato else (mejor f izq der)

    mejor :: (a -> a -> Bool) -> a -> a -> a
    mejor f izq der = if f izq der then izq else der

-- IV)

esABB :: Ord a => AB a -> Bool
esABB = recAB step True
  where
    step :: Ord a => Bool -> a -> Bool -> AB a -> Bool
    step izq dato der (Bin i _ d) =
         esMayor dato (mejorSegunAb (>) i)
      && esMenor dato (mejorSegunAb (<) d)
      && izq
      && der

    esMayor :: Ord a => a -> Maybe a -> Bool
    esMayor dato izq = case izq of
      Nothing -> True
      Just i  -> dato >= i

    esMenor :: Ord a => a -> Maybe a -> Bool
    esMenor dato der = case der of
      Nothing -> True
      Just d  -> dato < d

--------------------------------------------------------------------------------------------
-- Ejercicio 13
-- I)

ramas :: AB a -> [[a]]
ramas = foldAB merge []
  where
    merge [] x [] = [[x]]      -- Caso hoja: sin ramas en izquierda ni derecha
    merge izq x der = map (x:) (izq ++ der)

cantHojas :: AB a -> Int
cantHojas = recAB conteo 0
    where
      conteo izq _ der (Bin i _ d) = if esNil i && esNil d then 1 + izq + der else izq + der

espejo :: AB a -> AB a
espejo = foldAB espejar Nil
    where
      espejar izq dato der = Bin der dato izq 

 -- II)
mismaEstructura :: AB a -> AB a -> Bool
mismaEstructura ab bb = (foldAB step base ab) bb
  where
    base :: AB a -> Bool
    base t = case t of
      Nil       -> True
      Bin _ _ _ -> False

    step :: (AB a -> Bool) -> a -> (AB a -> Bool) -> AB a -> Bool 
    step fizq _ fder t = case t of
      Nil       -> False
      Bin i _ d -> fizq i && fder d

--------------------------------------------------------------------------------------------
-- Ejercicio 14
data AIH a = Hoja a | Bin1 (AIH a) (AIH a)

-- I)
foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH f g (Hoja x) = f x
foldAIH f g (Bin1 izq der) = g (foldAIH f g izq) (foldAIH f g der)

-- II)
alturaH :: AIH a -> Integer
alturaH = foldAIH (const 1) (\izq der -> 1 + max izq der) 

tamaño :: AIH a -> Integer
tamaño = foldAIH (const 1) (\izq der -> izq + der)

hojas :: AIH a -> [a]
hojas = foldAIH (\x -> [x]) (\izq der -> izq ++ der)

arbolAIH :: Num a => AIH a
arbolAIH = Bin1 (Hoja 10) (Bin1 (Bin1 (Hoja 2) (Bin1 (Hoja 5) (Bin1 (Hoja 4) (Hoja 3)))) (Bin1 (Hoja 3) (Hoja 4)))

--------------------------------------------------------------------------------------------
-- Ejercicio 15

data RoseTree a = Rose a [RoseTree a]

rose :: RoseTree Int
rose = Rose 1 [Rose 2 [], Rose 3 [Rose 4 []], Rose 5 []]

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose f (Rose x ys) = f x (map (foldRose f) ys)

hojasRose :: RoseTree a -> [a]
hojasRose = foldRose (\hoja acc -> [hoja] ++ concat acc)

distancias :: RoseTree a -> [Int]
distancias = foldRose step
  where
    step :: a -> [[Int]] -> [Int]
    step hoja []  = [0]
    step valor xs = map (+1) (concat xs)

alturaR :: RoseTree a -> Int
alturaR rose = mejorSegun (>) (distancias rose)

alt :: RoseTree a -> Int
alt = foldRose step
  where
    step :: a -> [Int] -> Int
    step hoja []  = 0
    step valor xs = 1 + mejorSegun (>) xs

--------------------------------------------------------------------------------------------
-- Ejercicio 16

data HashSet a = Hash (a -> Integer) (Integer -> [a])

vacio :: (a -> Integer) -> HashSet a
vacio f = Hash f (const [])

pertenece :: Eq a => a -> HashSet a -> Bool
pertenece x (Hash f g) = elem x (g (f x))

agregar :: Eq a => a -> HashSet a -> HashSet a
agregar x (Hash f g) = if pertenece x (Hash f g) then Hash f g else add f g x
  where
    add f g x = Hash f (\y -> if y == f x then x : g y else g y)

interseccion :: Eq a => HashSet a -> HashSet a -> HashSet a
interseccion (Hash f g) (Hash h i) = Hash f (\x -> filter (`pertenece` (Hash h i)) (g x)) 

foldr11 :: (a -> a -> a) -> [a] -> a
foldr11 f []     = error "No lista vacía"
foldr11 f (x:xs) = foldr f x xs

--------------------------------------------------------------------------------------------
-- Ejercicio 17
-- [ x | x <- [1..3], y <- [x..3], (x + y) `mod` 3 == 0 ]
-- [1,3]

--------------------------------------------------------------------------------------------
-- Ejercicio 18
paresDeNat :: [(Int, Int)]
paresDeNat = [(x, s - x) | s <- [1..], x <- [0..s]]

--------------------------------------------------------------------------------------------
-- Ejercicio 19

pitagoricas :: [(Integer, Integer, Integer)]
pitagoricas = [(a, b, c) | c <- [1..], a <- [1..c], b <- [1..a], a*a + b*b == c*c ]

-- Se que a y b estan acotados por c ya que si a o b fueran mas grandes que c no tendria sentido.
-- Ademas haciendo que (b <= a) evito permutaciones redundantes como:
-- (4,3,5) (3,4,5) --> (4*4 + 3*3 = 5*5) == (3*3 + 4*4 = 5*5)

--------------------------------------------------------------------------------------------
-- Ejercicio 20

listasQueSuman:: Int -> [[Int]]
listasQueSuman 0 = []
listasQueSuman n = [[]]


