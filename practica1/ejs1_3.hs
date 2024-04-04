-- Considerar las siguientes definiciones de funciones:
-- - max2 (x, y) | x >= y = x
--               | otherwise = y
-- - normaVectorial (x, y) = sqrt (x^2 + y^2)
-- - subtract = flip (-)
-- - predecesor = subtract 1
-- - evaluarEnCero = \f -> f 0
-- - dosVeces = \f -> f . f
-- - flipAll = map flip
-- - flipRaro = flip flip
-- i. ¿Cuál es el tipo de cada función? (Suponer que todos los números son de tipo Float).
-- ii. Indicar cuáles de las funciones anteriores no están currificadas. Para cada una de ellas, definir la función
-- currificada correspondiente. Recordar dar el tipo de la función.

--i 
max2 :: Ord a => (a, a) -> a
max2 (x, y) | x >= y = x
            | otherwise = y

normaVectorial :: Floating a => (a, a) -> a
normaVectorial (x, y) = sqrt (x^2 + y^2)

subtract :: Float -> Float -> Float
subtract = flip (-)

predecesor :: Float -> Float
predecesor = Main.subtract 1

evaluarEnCero :: (Float -> a) -> a
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> a -> a
dosVeces = \f -> f . f

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = Prelude.map flip

--Y que pasa con tres flips?
flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip

--ii
--Solo las primeras dos no?

-- Ejercicio 2 ⋆
-- i. Definir la función curry, que dada una función de dos argumentos, devuelve su equivalente currificada.
-- ii. Definir la función uncurry, que dada una función currificada de dos argumentos, devuelve su versión no
-- currificada equivalente. Es la inversa de la anterior.
-- iii. ¿Se podría definir una función curryN, que tome una función de un número arbitrario de argumentos y
-- devuelva su versión currificada?
-- Sugerencia: pensar cuál sería el tipo de la función.

curry :: ( (a,b) -> c) -> a -> b  -> c
curry f a b = f (a,b)

--ii
uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b

--iii 
---Consultar?

-- Ejercicio 3 ⋆
-- i. Redefinir usando foldr las funciones sum, elem, (++), filter y map.
-- ii. Definir la función mejorSegún :: (a -> a -> Bool) -> [a] -> a, que devuelve el máximo elemento
-- de la lista según una función de comparación, utilizando foldr1. Por ejemplo, maximum = mejorSegún
-- (>).
-- iii. Definir la función sumasParciales :: Num a => [a] -> [a], que dada una lista de números devuelve
-- otra de la misma longitud, que tiene en cada posición la suma parcial de los elementos de la lista original
-- desde la cabeza hasta la posición actual. Por ejemplo, sumasParciales [1,4,-1,0,5] ❀ [1,5,4,4,9].
-- iv. Definir la función sumaAlt, que realiza la suma alternada de los elementos de una lista. Es decir, da como
-- resultado: el primer elemento, menos el segundo, más el tercero, menos el cuarto, etc. Usar foldr.
-- v. Hacer lo mismo que en el punto anterior, pero en sentido inverso (el último elemento menos el anteúltimo,
-- etc.). Pensar qué esquema de recursión conviene usar en este caso.

--i
sum ::(Foldable t, Num a) => t a -> a
sum = foldr (+) 0

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem b = foldr (\a r -> a == b || r) False

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\a' r -> if f a' then a' : r else r) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\a' r -> f a' : r) []

--ii
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\a' r -> if f a' r then a' else r)

--iii --Tiene q haber una mejor manera
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldl (\ac a -> ac Prelude.++ [last' ac + a]) []

last' :: Num a => [a] -> a
last' [] = 0
last' xs = last xs
--iv 
-- sumaAlt :: [Int] -> Int 
-- sumaAlt as = Prelude.sum (foldr (\a r -> ((-1) ^ (length r)) * a : r) [] as)
sumaAlt :: [Int] -> Int
sumaAlt = foldr (-) 0

--v funca pero no me gusta la idea de depender de la longitud del resto de la lista
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)
sumaAlt' :: [Int] -> Int 
sumaAlt' = recr (\ x xs rec -> x * ((-1)^length xs) + rec) 0