-- Pilha de Elementos

type Pilha t = [t]

data Elemento = Valor Int | Soma | Multiplica deriving (Show)

exemploPilhaElem :: Pilha Elemento
exemploPilhaElem = [Valor 10, Valor 20, Soma, Valor 30, Multiplica]

geraString :: Pilha Elemento -> String
geraString pilha = aux pilha []

aux :: Pilha Elemento -> [String] -> String
aux [] [resultado] = resultado
aux (Valor x : xs) stack = aux xs (show x: stack)
aux (Soma : xs) (y:x:stack) = aux xs (("(" ++ x ++ " + " ++ y ++ ")") : stack)
aux (Multiplica : xs) (y:x:stack) = aux xs (("(" ++ x ++ " * " ++ y ++ ")") : stack)

------------------------------------------------------------------------------------------------------

calcula :: Pilha Elemento -> Int
calcula pilha = auxCalc pilha []

auxCalc :: Pilha Elemento -> [Int] -> Int
auxCalc [] [resultado] = resultado
auxCalc (Valor x : xs) stack = auxCalc xs (x: stack)
auxCalc (Soma : xs) (y:x:stack) = auxCalc xs ((x+y): stack)
auxCalc (Multiplica : xs) (y:x:stack) = auxCalc xs ((x*y): stack)