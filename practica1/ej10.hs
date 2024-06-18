generate :: ([a] -> Bool) ->([a] -> a) -> [a]
generate stop next = generateFrom stop next []

generateFrom :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])

--I
generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
generateBase stop base next = generate stop (\l -> if null l then base else next (last l)) 

--II
factoriales :: Int -> [Int] 
factoriales n = generate (\l -> length l > n) (\l -> if null l then 1 else length l * last l)

--III
iterateN :: Int -> (a -> a) -> a -> [a]
iterateN n f x = generateBase (\l -> length l > n) x f

--IV Noc como usar stop en este contexto
-- generateFromI :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
-- generateFromI stop next xs = takeWhile stop (iterate next xs)

generateFrom' :: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom' stop next xs = last (takeWhile (not . stop) (iterate (\ys -> ys ++ [next ys]) xs))

