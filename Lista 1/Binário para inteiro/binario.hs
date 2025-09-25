{- Dada uma string representando um número binário, retorna o inteiro na base 10 dessa string.-}

--binario :: String
--binario = "1011"

btoi :: String -> Int
btoi string | string == "" = 0
            | otherwise = btoiaux string (length string)

btoiaux :: String -> Int -> Int
btoiaux "" _ = 0
btoiaux (x:xs) n | x == '1' = 2^(n-1) + btoiaux xs (n-1)
                 | x == '0' = btoiaux xs (n-1)

main = do
    s <- getLine
    let result = btoi s
    print result