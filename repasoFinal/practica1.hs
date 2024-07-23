import Data.Foldable (Foldable(fold))
-- import Main (prefijos)
-- Practica 1
-- Ej 2
curry :: ((a,b) -> c) -> a -> b -> c
curry f a b = f (a, b)

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

-- Ej 3
sums :: [Int] -> Int
sums = foldr (+) 0

(+++) :: [a] -> [a] -> [a]
(+++) = flip (foldr (:))


maps f = ((:) . f)

-- III
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\ac x -> if null ac then [x] else ac ++ [last ac + x]) []

--IV
sumaAlt :: [Int] -> Int
sumaAlt = foldr (\ x r -> x - r) 0

-- V
sumaAltl :: [Int] -> Int
sumaAltl = foldl (\ac x -> x - ac) 0

-- 4
-- I 
permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x r -> concatMap (\permutations -> map (insert x permutations) [0..length permutations]) r) [[]]
    where
        insert x xs i = take i xs ++ [x] ++ drop i xs

-- II
partes :: [a] -> [[a]]
partes = foldr (\x r -> r ++ map (x :) r) [[]]

-- III
prefijos :: [a] -> [[a]]
prefijos = foldl (\ac x -> ac ++ [last ac ++ [x]]) [[]]

-- Iv
sublistas :: [a] -> [[a]]
sublistas = recr (\x xs r -> map (x :) (prefijos xs) ++ r) [[]]

-- 5
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) =
    if null xs then [x]
    else x : elementosEnPosicionesPares (tail xs)
-- No es recursión estructural porque en el caso else,
-- la recursión se hace sobre tail xs en vez de xs entero.
-- Se "descartan" elementos en la recursión sobre xs.

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys ->
    if null ys then x : entrelazar xs []
    else x : head ys : entrelazar xs (tail ys)

entrelazar2 :: [a] -> ([a] -> [a])
entrelazar2 = foldr (\x fr ys ->
        if null ys then x : fr []
        else x : head ys : fr (tail ys)
    ) id

-- 6
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna y = recr (\x xs r -> if x == y then xs else x:r) []

-- No podemos implementar sacarUna con foldr porque no hay "memoria".
-- Cada vez que procesamos un elemento, no sabemos si ya sacamos algún
-- elemento anterior o no. Dicho de otra manera, con foldr no podemos
-- hacer un return temprano y "abortar" el resto del fold.
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs r -> if e < x then e:x:xs else x:r) [e]

-- 7
genLista :: a -> (a -> a) -> Int -> [a]
genLista n f 0 = []
genLista n f i = n : genLista (f n) f (i-1)

-- Con funciones del preludio.
-- genLista n f i = take i (iterate f n)

desdeHasta :: Int -> Int -> [Int]
desdeHasta i j = genLista i (+1) (j-i+1)

-- 8
mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = map (uncurry f)

armarPares :: [a] -> [b] -> [(a,b)]
armarPares = foldr (\x r ys -> if null ys then [] else (x, head ys) : r (tail ys)) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x r ys -> f x (head ys) : r (tail ys)) (const [])

-- 9 
sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = mapDoble (mapDoble (+))

-- trasponer [[1,2,3],[4,5,6],[7,8,9]]
-- [1 2 3]  ~>  [1 4 7]
-- [4 5 6]  ~>  [2 5 8]
-- [7 8 9]  ~>  [3 6 9]
-- [[],[],[]]
-- [[],[],[]] + [1,2,3]           ~>  [[1],[2],[3]]
-- [[1],[2],[3]] + [4,5,6]        ~>  [[1,4],[2,5],[3,6]]
-- [[1,4],[2,5],[3,6]] + [7,8,9]  ~>  [[1,4,7],[2,5,8],[3,6,9]]

-- trasponer [[1,2],[4,5],[7,8]]
-- [1 2]  ~>  [1 4 7]
-- [4 5]  ~>  [2 5 8]
-- [7 8]  ~>

trasponer :: [[Int]] -> [[Int]]
trasponer = foldl (\ac x -> zipWith (++) ac (map (:[]) x)) (repeat [])

-- 10
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs
    | stop xs = init xs
    | otherwise = generateFrom stop next (xs ++ [next xs])

generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop base next = generate stop (\l -> if null l then base else next (last l))

factoriales :: Int -> [Int]
factoriales n = generate (\l -> length l > n) (\l -> if null l then 1 else length l * last l)

iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n next x = (generateBase (\l -> length l > n) x next)

--IV Noc como usar stop en este contexto
-- generateFromI :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
-- generateFromI stop next xs = takeWhile stop (iterate next xs)

generateFrom' :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom' stop next xs = last (takeWhile (not . stop) (iterate (\ys -> ys ++ [next ys]) xs))

-- 11
foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat _ z 0 = z
foldNat f z n = f n (foldNat f z (n - 1))

--II
potencia :: Integer -> Integer -> Integer
potencia n = foldNat (\ _ res -> n * res) 1

-- Ej 12
data Polinomio a = X
    | Cte a
    | Suma (Polinomio a) (Polinomio a)
    | Prod (Polinomio a) (Polinomio a)
    deriving(Show)

foldPol :: b -> (a -> b) -> (b -> b-> b) -> (b -> b -> b) -> Polinomio a -> b 
foldPol fx fconst fsum fprod p = case p of
    X -> fx
    Cte t -> fconst t 
    Suma t1 t2 -> fsum (res t1) (res t2)
                where
                    res = foldPol fx fconst fsum fprod
    Prod t1 t2 -> fprod (res t1) (res t2)
                where
                    res = foldPol fx fconst fsum fprod

evaluar :: Num a => a -> Polinomio a -> a
evaluar a = foldPol a id (+) (*)

-- 13
data AB a = Nil | Bin (AB a) a (AB a)
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b 
foldAB cNil cBin t = case t of 
    Nil -> cNil
    Bin i r d -> cBin (res i) r (res d)
        where 
            res = foldAB cNil cBin 

recAB ::
    b                                       -- Nil
    -> (AB a -> a -> AB a -> b -> b -> b)   -- Bin
    -> AB a
    -> b

recAB z f x = case x of
    Nil -> z
    (Bin l v r) -> f l v r (rec l) (rec r)
    where rec = recAB z f

esNil :: AB a -> Bool 
esNil t = case t of 
    Nil -> True 
    Bin _ _ _ -> False 

altura :: AB a -> Int 
altura = foldAB 0 (\ resI _ resD ->  1 + max resI resD)

cantNodos :: AB a -> Int 
cantNodos = foldAB 0 (\ resI _ resD -> resI + 1 + resD)

mejorSegún :: (a -> a -> Bool) -> AB a -> a
mejorSegún f (Bin l v r) = foldAB v (\rl v rr -> (rl `g` v) `g` rr) (Bin l v r)
    where g x y = if f x y then x else y

esABB :: Ord a => AB a -> Bool
esABB = recAB True f
    where
        f l v r rl rr
            | esNil l && esNil r = True
            | esNil r = rl && raíz l <= v
            | esNil l = rr && v < raíz r
            | otherwise = rl && rr && raíz l <= v && v < raíz r

raíz :: AB a -> a
raíz (Bin l v r) = v

-- 14 
ramas :: AB a -> [[a]]
ramas = foldAB [] f
    where
        f rl v rr
            | null rl && null rr = [[v]]
            | null rr = map (v:) rl
            | null rl = map (v:) rr
            | otherwise = map (v:) rl ++ map (v:) rr

cantHojas :: AB a -> Int
cantHojas = recAB 0 (\ i _ d resI resD-> resI + resD + if esNil i && esNil d then 1 else 0)

espejo :: AB a -> AB a
espejo = foldAB Nil (\rl v rr -> Bin rr v rl)

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = recAB esNil f
    where
        f xl _ xr xrl xrr y = case y of
            Nil -> False -- Bin(...) != Nil
            (Bin yl _ yr) -> esNil xl == esNil yl && esNil xr == esNil yr && xrl yl && xrr yr

-- 15
data AIH a = Hoja a | Bin (AIH a) (AIH a)

--a
foldAIH :: (a -> b) -> (b-> b -> b) -> AIH a -> b 
foldAIH cHoja cAIH t = case t of 
    Hoja h -> cHoja h 
    Bin izq der -> cAIH (res izq) (res der) 
                where 
                    res = foldAIH cHoja cAIH

--b Esta mal esto?
altura :: AIH a -> Integer 
altura = foldAIH (const 1) (\ resIzq resDer -> 1 + max resIzq resDer)

tamano :: AIH a -> Integer 
tamano = foldAIH (const 1) (+)

--c
--Ni idea
data RoseTree a = Rose a [RoseTree a]

foldRT :: (a -> [b] -> b) -> RoseTree a -> b
foldRT f (Rose rosa hijos) = f rosa (map (foldRT f) hijos)

---III
--A Esta bien?
hojas :: RoseTree a -> [a]
hojas = foldRT (\ rosa hijos -> if null hijos then [rosa] else concat hijos)

--b 
distancias :: RoseTree a -> [Int]
distancias = foldRT (\ rosa hijos -> if null hijos then [0] else map (1+) (concat hijos))

--c
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\a' r -> if f a' r then a' else r)

--Esta bien?
altura :: RoseTree a -> Int 
altura = foldRT (\ rosa hijos -> if null hijos then 1 else 1 + mejorSegun (>) hijos)


