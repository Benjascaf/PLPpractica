data Polinomio a = X
                    | Cte a
                    | Suma (Polinomio a) (Polinomio a)
                    | Prod (Polinomio a) (Polinomio a)

foldPol :: b -> (a -> b) -> (b -> b-> b) -> (b -> b -> b) -> Polinomio a -> b 
foldPol fx fconst fsum fprod p = case p of
    X -> fx
    Cte t -> fconst t 
    Suma t1 t2 -> fsum (res t1) (res t2)
                where
                    res = foldPol fx fconst fsum fprod
    Prod t1 t2 -> fprod (res t1) (res t2)
                where
                    res = foldPol fx fconst fsum fprod

evaluar :: Num a => a -> Polinomio a -> a 
evaluar n = foldPol n id (+) (*)