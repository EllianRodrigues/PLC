import Data.Char (isNumber)
-- Recebe uma String referente a fatura anual e retorna uma tupla com o menor e o maior dos valores gastos.

string :: String
string = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

split :: Char -> String -> [String]
split _ "" = []
split sep s =
    let (parte, resto) = break (== sep) s
    in parte : case resto of
        [] -> []
        (_:xs) -> split sep xs

isDouble :: String -> Bool
isDouble s = case reads s :: [(Double, String)] of
    [(n,"")] -> True
    _        -> False

minMaxCartao :: String -> (Double, Double)
minMaxCartao s = (minimum valores, maximum valores)
  where
    partes = split ';' s
    valores = [ read x :: Double | x <- partes ]

main = do
    a <- getLine
    let result = minMaxCartao a
    print result


