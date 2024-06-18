import GHC.Conc (TVar)
import Data.Maybe (fromJust)
import Data.List (findIndex)
type Codigo = [Ins] --Codigo : lista de instrucciones
data Valor = VClosure [Ins] Env | VBool Bool deriving Show--Valores: booleanos y clausuras 
type Env = [Valor]   --Entornos: lista de valores
type Pila = [Valor]  --Pilas: ista de valores
type Dump = [([Ins], Pila, Env)] -- Dumps

data Ins = ILDB Bool    --LDB(b): carga el booleano b en la pila
         | IMKCLO Codigo --MKCLO(l): carga una clausura con codigo l en la pila
         | ILD Int       --LD(n): carga el n-esimo valor del entorno en la pila
         | IAP            --AP: invoca una funcion
         | IRET           --RET: retorna una funcion
         | ITEST Codigo Codigo --Test(l1, l2): Ejecuta l1 o l2 dependiendo del booleano al tope de la pila 
         deriving Show

--{Terminos del calculo lambda}
type Id = String 
data Term = TVar Id 
          | TAbs Id Term 
          | TApp Term Term 
          | TFalse
          | TTrue 
          | TIf Term Term Term 

compile :: [Id] -> Term -> [Ins]
compile lst (TVar x) = [ILD (fromJust (findIndex (== x) lst))]
compile lst (TAbs x m) = [IMKCLO (compile (x : lst) m ++ [IRET])]
compile lst (TApp m n) = compile lst m ++ compile lst n ++ [IAP]
compile lst TFalse = [ILDB False] 
compile lst TTrue = [ILDB True]
compile lst (TIf m n o) = compile lst m ++ [ITEST (compile lst n) (compile lst o)]

--{Falta interpretacion de la SECD}
