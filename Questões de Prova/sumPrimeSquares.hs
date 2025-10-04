--Soma dos quadrados dos n primeiros números primos
ePrimo :: Int -> Bool
ePrimo n | n <= 2 = True
         | otherwise = ePrimoaux n (n-1)

ePrimoaux :: Int -> Int -> Bool
ePrimoaux n 1 = True
ePrimoaux n divisor | mod n divisor == 0 = False
                    | otherwise = ePrimoaux n (divisor-1)

sumPrimeSquares :: Int -> Int -> Int
sumPrimeSquares a b = foldl1 (+) (map (\x-> x*x) (filter ePrimo [a..b]))

--Sem map e filter, apenas compreensão de listas
sumPrimeSquares2 :: Int -> Int -> Int
sumPrimeSquares2 a b = sum [ x*x | x <- [a..b], ePrimo x]