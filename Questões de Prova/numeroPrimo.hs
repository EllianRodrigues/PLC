--Verificar se um n é primo
ePrimo :: Int -> Bool
ePrimo n | n <= 2 = True
         | otherwise = ePrimoaux n (n-1)

ePrimoaux :: Int -> Int -> Bool
ePrimoaux n 1 = True
ePrimoaux n divisor | mod n divisor == 0 = False
                    | otherwise = ePrimoaux n (divisor-1)