nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : filter (\y -> x /= y) (nub xs)
union :: Eq a => [a] -> [a] -> [a]
union xs ys = nub (xs++ys)
intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = filter (\e -> elem e ys) xs