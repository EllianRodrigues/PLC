--Q1
fibonacci :: [Int]
fibonacci = 0:1: fibonacciaux 0 1

fibonacciaux :: Int -> Int -> [Int]
fibonacciaux a b = a+b : fibonacciaux b (a+b)

--Q2
merge :: Ord t => [t] -> [t] -> [t]
merge xs []  = xs
merge []  ys = ys
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

lista1, lista2, lista3 :: [Int]
lista1 = [1,3..20]
lista2 = [2,4..20]
lista3 = [0,3,2,7,4,9,1,5,6,8]

--Q3
mergesort :: Ord t => [t] -> [t]
mergesort [] = []
mergesort [x] = [x]
mergesort lista = merge (mergesort primeiraMetade) (mergesort segundaMetade)
  where
    (primeiraMetade, segundaMetade) = splitAt (length lista `div` 2) lista

--Q4
type Pilha t = [t]
data Elemento = Valor Int | Soma | Multiplica deriving Show

exemploPilha :: Pilha Elemento
exemploPilha = [Valor 10, Valor 20, Soma, Valor 30, Multiplica]

geraString :: Pilha Elemento -> String
geraString [] = ""
geraString (Valor n : Valor x : Soma : xs) = "(" ++ show n ++ "+" ++ show x ++ ")" ++ geraString xs
geraString (Valor n : Valor x : Multiplica : xs) = "(" ++ show n ++ "*" ++ show x ++ ")" ++ geraString xs
geraString (Valor n : Soma : xs) = show n ++ "+" ++ ")" ++ geraString xs
geraString (Valor n : Multiplica : xs) = "*" ++ show n ++  ")" ++ geraString xs
-- geraString (x:y:t:xs) | x == Valor a && y == Valor b && t == Soma = "(" ++ show a ++ "+" ++ show b ++ ")" ++ geraString xs

--Q5
calcula :: Pilha Elemento -> Int
calcula [] = 0
calcula (Valor n : Valor x : Soma : xs) = calculaaux (n + x) xs
calcula (Valor n : Valor x : Multiplica : xs) = calculaaux (n * x) xs
-- calcula (Valor n : Soma : xs) = 
-- calcula (Valor n : Multiplica : xs) = 

calculaaux :: Int -> Pilha Elemento -> Int
calculaaux acc [] = acc
calculaaux acc (Valor n : Soma : xs) = calculaaux (acc + n) xs
calculaaux acc (Valor n : Multiplica : xs) = calculaaux (acc * n) xs
