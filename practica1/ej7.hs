
--No me gusta asi, calculo la idea era hacer recursion a mano, ya 
-- que no estamos recurriendo sobre una lista
genLista :: a -> (a -> a) -> Int -> [a]
genLista e proximo n =  generar [] (replicate n e)
                    where 
                        generar = foldr (\y res -> if null res 
                                                    then [y] 
                                                    else res ++ [proximo (last res)])

desdeHasta :: Int -> Int -> [Int]
desdeHasta desde hasta = genLista desde (+ 1) hasta
