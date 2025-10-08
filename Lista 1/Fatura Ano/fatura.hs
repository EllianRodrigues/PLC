import Data.List (foldl')

split :: Char -> String -> [String]
split _ "" = []
split sep s =
  let (parte, resto) = break (== sep) s
  in parte : case resto of
       []     -> []
       (_:xs) -> split sep xs

toRecords :: [String] -> [(String,String,String)]
toRecords [] = []
toRecords (d:t:v:xs) = (d,t,v) : toRecords xs
toRecords _ = []

logMes :: String -> String -> Double
logMes mes fatura = foldl' (+) 0.0 valores
  where
    partes    = split ';' fatura 
    registros = toRecords partes
    valores   = [ read v :: Double | (d,_,v) <- registros, last (words d) == mes ]

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result
