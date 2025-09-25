{-Recebe 2 listas de inteiros e retorna uma lista com o produto elemento por elemento entre as duas listas.-}

mul2 :: [Int] -> [Int] -> [Int]
mul2 [] [] = []
mul2 (cabeca:cauda) [] = map (0*) (cabeca:cauda)
mul2 [] (cabeca2:cauda2) = map (0*) (cabeca2:cauda2)
mul2 (cabeca:cauda) (cabeca2:cauda2) = (cabeca * cabeca2) : mul2 cauda cauda2

main :: IO ()
main = do
    sa <- getLine 
    let lista1 = read sa :: [Int]
    sb <- getLine 
    let lista2 = read sb :: [Int]
    let result = mul2 lista1 lista2
    print result