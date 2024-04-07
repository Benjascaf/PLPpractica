data AIH a = Hoja a | Bin (AIH a) (AIH a)

--a
foldAIH :: (a -> b) -> (b-> b -> b) -> AIH a -> b 
foldAIH cHoja cAIH t = case t of 
    Hoja h -> cHoja h 
    Bin izq der -> cAIH (res izq) (res der) 
                where 
                    res = foldAIH cHoja cAIH

--b Esta mal esto?
altura :: AIH a -> Integer 
altura = foldAIH (const 1) (\ resIzq resDer -> 1 + max resIzq resDer)

tamano :: AIH a -> Integer 
tamano = foldAIH (const 1) (+)

--c
--Ni idea
