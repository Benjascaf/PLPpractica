--I
foldNat :: (Integer -> a -> a) -> a -> Integer -> a
foldNat _ z 0 = z
foldNat f z n = f n (foldNat f z (n - 1))

--II
potencia :: Integer -> Integer -> Integer
potencia n = foldNat (\ _ res -> n * res) 1
