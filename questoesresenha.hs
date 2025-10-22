--Q1) Escreva uma função que insira um elemento em uma lista ordenada, mantendo a ordenação.
lista :: [Int]
lista = [1,3,5,7,9]

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (cabeca:cauda) | x <= cabeca = x:cabeca:cauda
                        | otherwise =cabeca: insert x cauda

--Q2) verifica se um numero é primo 

ehPrimo :: Int -> Bool
ehPrimo x | x <= 2 = True
          | otherwise = ehPrimoaux x (x-1)

ehPrimoaux :: Int -> Int -> Bool
ehPrimoaux x 1 = True
ehPrimoaux x y | mod x y == 0 = False
               | otherwise = ehPrimoaux x (y-1)

--Q3) Escreva uma funcão que retorna a soma dos quadrados dos numeros primos existentes entre dois numeros inteiros fornecidos como parametro.
--Q4) Reescreva a função do exercicio anterior utilizando uma expressão lambda.
sumPrimeSquare1 :: Int -> Int -> Int
sumPrimeSquare1 a b =  foldl (+) 0 (map (\x -> x*x) (filter ehPrimo [a..b]))

--Q4) Reescreva a função do exercicio anterior sem utilizar map e filter, usando compreensão de listas.

sumPrimeSquare2 :: Int -> Int -> Int
sumPrimeSquare2 a b = sum [x*x | x <- [a..b], ehPrimo x]