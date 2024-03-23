
-- Ejercicio 1
-- Dar el tipo y describir el comportamiento de las siguientes funciones 
-- del módulo Prelude de Haskell

--null (returns True if a list is empty, otherwise Fals)
--[a] -> Bool 

--head (returns the first item of a list)
-- [a] -> a 

-- tail (it accepts a list and returns the list without its first item)
-- [a] -> [a]

-- init (	it accepts a list and returns the list without its last item)
-- [a] -> [a]

-- last (returns the last item of a list)
-- [a] -> a 

-- take (creates a list, the first argument determines, how many items should be taken from the list passed as the second argument)
-- Int -> [a] -> [a]

-- drop (remove the first n numbers of the list) 
-- Int -> [a] -> [a]

-- concat (accepts a list of lists and concatenates them)
-- [[a]] -> [a]

-- elem (returns True if the list contains an item equal to the first argument
-- )
-- Eq a => a -> [a] -> Bool

-- ++ (appends two lists)
-- [a] -> [a] -> [a]

-- !! (accepts a list and an integer and returns the item in the list at integer position. The numbering starts with 0.
-- )
-- [a] -> Int -> a


-- Ejercicio 2
-- Definir las siguientes funciones:
-- a. valorAbsoluto :: Float → Float, que dado un número devuelve su valor absoluto.
-- b. bisiesto :: Int → Bool, que dado un número que representa un año, indica si el mismo es bisiesto.
-- c. factorial :: Int → Int, definida únicamente para enteros positivos, que computa el factorial.
-- d. cantDivisoresPrimos :: Int → Int, que dado un entero positivo devuelve la cantidad de divisores primos.

--a
valorAbsoluto :: Float -> Float
valorAbsoluto 0 = 0
valorAbsoluto w | w < 0 = 1 + valorAbsoluto (w + 1)
                | otherwise = 1 + valorAbsoluto (w - 1)

--b 
bisiesto :: Int -> Bool
bisiesto x  | mod x 100 == 0 && mod x 4 == 0 && mod x 400 == 0 = True
            | mod x 4 == 0 = True
            | otherwise = False


--c
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

--d Esta bien asi o podria ser mejor?
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos 0 = 0
cantDivisoresPrimos n = length (filter (\x -> mod n x == 0 && esPrimo x) [1 .. n])

esPrimo :: Int -> Bool
esPrimo n = n > 1 && all (\x -> mod n x /= 0) [2..n - 1]

-- Ejercicio 3
-- Contamos con los tipos Maybe y Either definidos como sigue:
-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b
-- a. Definir la función inverso :: Float → Maybe Float que dado un número devuelve su inverso multiplicativo
-- si está definido, o Nothing en caso contrario.
-- b. Definir la función aEntero :: Either Int Bool → Int que convierte a entero una expresión que puede ser
-- booleana o entera. En el caso de los booleanos, el entero que corresponde es 0 para False y 1 para True.


--a
inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1 / x)

--b Esta bien asi o habia otra forma?
aEntero :: Either Int Bool -> Int
aEntero = either id (\b -> if b then 1 else 0)


-- Ejercicio 4
-- Definir las siguientes funciones sobre listas:
-- a. limpiar :: String → String → String, que elimina todas las apariciones de cualquier carácter de la primera
-- cadena en la segunda. Por ejemplo, limpiar ``susto'' ``puerta'' evalúa a ``pera''. Nota: String es un
-- renombre de [Char]. La notación ``hola'' es equivalente a [`h',`o',`l',`a'] y a `h':`o':`l':`a':[].
-- b. difPromedio :: [Float] → [Float] que dada una lista de números devuelve la diferencia de cada uno con el
-- promedio general. Por ejemplo, difPromedio [2, 3, 4] evalúa a [-1, 0, 1].
-- c. todosIguales :: [Int] → Bool que indica si una lista de enteros tiene todos sus elementos iguales.

--a La idea es que usemos estas funciones o que armemos todo desde 0?
limpiar :: String -> String -> String
limpiar s' = filter (`notElem` s')

--b Problemas de tipos?
difPromedio :: [Float] -> [Float]
difPromedio xs = map (\x -> x - (sum xs / lengthF xs)) xs

lengthF :: [a] -> Float
lengthF [] = 0.0
lengthF (_ : xs) = 1.0 + lengthF xs

--c Esto de calcular na variable para cada "iteracion", no jode a la complejidad?
todosIguales :: [Int] -> Bool 
todosIguales xs = all (\x -> x == head xs) xs

-- Ejercicio 5
-- Dado el siguiente modelo para árboles binarios:
-- data AB a = Nil | Bin (AB a) a (AB a)
-- definir las siguientes funciones:
-- a. vacioAB :: AB a → Bool que indica si un árbol es vacío (i.e. no tiene nodos).
-- b. negacionAB :: AB Bool → AB Bool que dado un árbol de booleanos construye otro formado por la negación
-- de cada uno de los nodos.
-- c. productoAB :: AB Int → Int que calcula el producto de todos los nodos del árbol.

data AB a = Nil | Bin (AB a) a (AB a) deriving (Show) --Para poder imprimirlo a consola

--a 
--Esta bien?
vacioAB :: AB a -> Bool 
vacioAB Nil = True
vacioAB _ = False

--b
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin i r d) =  Bin (negacionAB i) (not r) (negacionAB d)

--c 
productoAB :: AB Int -> Int 
productoAB Nil = 1 --Este caso base esta bien?
productoAB (Bin i r d) = productoAB i * r * productoAB d