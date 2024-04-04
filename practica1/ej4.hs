--I 
recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

permutaciones :: [a] -> [[a]]
permutaciones
  = foldr (\ x res-> concatMap (agregarAdelante x) res) [[]]
    where
        agregarAdelante :: a -> [a] -> [[a]]
        agregarAdelante x l = [take i l ++ x : drop i l | i <- [0..length l]]

--II
partes :: [a] -> [[a]]
partes = foldr (\ x res -> res ++ map (x:) res) [[]]

--III Preguntar
prefijos :: [a] -> [[a]]
prefijos = foldl (\ ac x -> ac ++ [last ac ++ [x]]) [[]]

-- IV consultar
sublistas :: [a] -> [[a]]
sublistas xs =  [take j xs ++ drop i xs| i <- [0..length xs], j <- [0..i]]