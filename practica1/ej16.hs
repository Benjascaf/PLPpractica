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
