
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

recrAB1 :: (a -> a -> a -> AB a -> AB a-> a) -> AB a -> a
recrAB1 cBin t = case t of 
    Bin Nil r Nil -> r
    Bin i r d -> cBin (res i) r (res d) i d
                where res = recrAB1 cBin
                
-- Función auxiliar que (creo) recomienda el ejercicio
recAB1 :: (AB a -> a -> AB a -> a -> a -> a) -> AB a -> a
recAB1 _ (Bin Nil nodo Nil) = nodo
recAB1 fBin (Bin izq nodo der) = fBin izq nodo der (recAB1 fBin izq) (recAB1 fBin der)
--II
esNil :: AB a -> Bool 
esNil t = case t of 
    Nil -> True 
    Bin _ _ _ -> False 

altura :: AB a -> Int 
altura = foldAB 0 (\ resI _ resD ->  1 + max resI resD)

cantNodos :: AB a -> Int 
cantNodos = foldAB 0 (\ resI _ resD -> resI + 1 + resD)

--III --Noc que devolver cuando es nil?
mejorSegun :: (a -> a -> Bool) -> AB a -> a 
mejorSegun f = recrAB1 (\ resI r resD i d -> 
    case (esNil i, esNil d) of
        (True, True) -> r 
        (False, True) -> if f r resI then r else resI
        (True, False) -> if f r resD then r else resD
        (False,False) -> if f r resD && f r resI then r else (if f resD resI then resD else resI)
                    )

-- Función que pedía el ejercicio
mejorSegúnAB :: (a -> a -> Bool) -> AB a -> a
mejorSegúnAB comp = recrAB1 (\recIzq nodo recDer izq der ->
        case (esNil izq, esNil der) of
          (True, True) -> nodo
          (True, False) -> if comp nodo recDer then nodo else recDer
          (False, True) -> if comp nodo recIzq then nodo else recIzq
          (False, False) -> if comp nodo recIzq && comp nodo recDer then nodo else (if comp recIzq recDer then recIzq else recDer)
    )

--IV
esABB :: Ord a => AB a -> Bool 
esABB = recrAB True (\ resI r resD i d -> resI && estanOrdenados i r d && resD)
        where 
            estanOrdenados :: Ord a => AB a -> a -> AB a -> Bool 
            estanOrdenados Nil r Nil = True 
            estanOrdenados (Bin _ ri _) r Nil = ri < r 
            estanOrdenados Nil r (Bin _ rd _) = r < rd 
            estanOrdenados (Bin _ ri _) r (Bin _ rd _) = ri < r && r < rd

--V
--Ésta te justifico