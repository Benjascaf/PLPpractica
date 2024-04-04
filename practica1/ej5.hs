elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
                                    then [x]
                                    else x : elementosEnPosicionesPares (tail xs)
entrelazar :: [a] -> [a] -> [a] 
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
                    then x : entrelazar xs []
                    else x : head ys : entrelazar xs (tail ys)

--mepa que no, porque en el primero haces uso de xs, y en el segundo tenes dos llamados recursivos