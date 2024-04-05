--I Estan bien los tipos?
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (uncurry f)

--II Noc si entendi bien la idea o podria haberlo hecho mejir?
armarPares :: [a] -> [b] -> [(a,b)]
armarPares = mapDoble (,) 
--III
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f as bs = if null as || null bs then [] else f a b : mapDoble f as' bs'
                    where
                        a = head as 
                        b = head bs 
                        as' = tail as 
                        bs' = tail bs