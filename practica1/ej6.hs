recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)
--a
sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs res -> if x == e then xs else x:res) []

--b
--Debido a la necesidad de usar la cola de la lista
--en el caso de que haya que quitar elemento, sobreescribiendo asi cualquier 
--cambio anterior.

--c
insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs res -> if e > x then x : res else e:(x:xs)) [e]

