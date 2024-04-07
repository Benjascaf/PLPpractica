data AB a = Nil | Bin (AB a) a (AB a)

--I
foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin t = case t of
    Nil -> cNil
    Bin i r d -> cBin (res i) r (res d)
            where
                res = foldAB cNil cBin

--Preguntar si esta bien
recrAB :: b -> (b -> a -> b -> AB a -> AB a-> b) -> AB a -> b
recrAB cNil cBin t = case t of
    Nil -> cNil
    Bin i r d -> cBin (res i) r (res d) i d
                where res = recrAB cNil cBin

esNil :: AB a -> Bool
esNil t = case t of
    Nil -> True
    Bin _ _ _ -> False
--I
ramas :: AB a -> Int
ramas = recrAB 0 (\resI _ resD i d -> resI + resD + contarCaminos i d)
        where
            contarCaminos :: AB a -> AB a -> Int
            contarCaminos Nil Nil = 0
            contarCaminos Nil _ = 1
            contarCaminos _ Nil = 1
            contarCaminos _ _ = 2

cantHojas :: AB a -> Int
cantHojas = recrAB 0 (\ resI _ resD i d-> resI + resD + if esNil i && esNil d then 1 else 0)

--Es asi?
espejo :: AB a -> AB a 
espejo = foldAB Nil (\ resI r resD -> Bin resD r resI)

--II
mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura Nil Nil = True 
mismaEstructura Nil (Bin ib rb db) = False 
mismaEstructura (Bin _ _ _) Nil = False 
mismaEstructura (Bin ia ra da) (Bin ib rb db) = mismaEstructura ia ib && mismaEstructura da db