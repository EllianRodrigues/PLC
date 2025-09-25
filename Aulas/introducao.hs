answer :: Int
answer = 72

greater :: Bool
greater = answer > 71

yes :: Bool
yes = True

square :: Int -> Int
square x = x * x

allEqual1 :: Int -> Int -> Int -> Bool
allEqual1 x y z = (x == y) && (y == z)

maxi :: Int -> Int -> Int
maxi a b | a >= b = a
         | otherwise = b

fatorial :: Int -> Int
fatorial n | n == 0  = 1
           | otherwise = fatorial (n-1) * n

fatorialpadrao :: Int -> Int
fatorialpadrao 0 = 1
fatorialpadrao n =  fatorialpadrao (n-1) * n