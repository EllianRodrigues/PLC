-- O input consiste de duas Listas de Strings: a primeira corresponde às notas tiradas por Jeff e a segunda corresponde às notas de corte das faculdades de determinado estado desejadas por ele.
-- O output consiste em um Inteiro: a quantidade de faculdades nas quais Jeff é um potencial candidato válido, ou seja, que sua média reconvertida é maior ou igual que as respectivas notas de corte das faculdades em questão.

--Forma burra mesmo de fazer, mas funciona

notas :: [String] -> [String] -> Int
notas xs ys =  comparacao (notasReverter (notasConverter xs / fromIntegral (length xs))) ys

notasConverter :: [String] -> Float
notasConverter [] = 0
notasConverter (x:xs) | x == "A+" = 9.7 + notasConverter xs
                | x == "A" = 9.3 + notasConverter xs
                | x == "A-" = 9.0 + notasConverter xs
                | x == "B+" = 8.7 + notasConverter xs
                | x == "B" = 8.3 + notasConverter xs
                | x == "B-" = 8.0 + notasConverter xs
                | x == "C+" = 7.7 + notasConverter xs
                | x == "C" = 7.3 + notasConverter xs
                | x == "C-" = 7.0 + notasConverter xs
                | x == "D+" = 6.7 + notasConverter xs
                | x == "D" = 6.3 + notasConverter xs
                | x == "D-" = 6.0 + notasConverter xs
                | otherwise = 5.9 + notasConverter xs

notasReverter :: Float -> String
notasReverter n | n >= 9.7 = "A+"
                | n >= 9.3 = "A"
                | n >= 9.0 = "A-"
                | n >= 8.7 = "B+"
                | n >= 8.3 = "B"
                | n >= 8.0 = "B-"
                | n >= 7.7 = "C+"
                | n >= 7.3 = "C"
                | n >= 7.0 = "C-"
                | n >= 6.7 = "D+"
                | n >= 6.3 = "D"
                | n >= 6.0 = "D-"
                | otherwise = "F"

comparacao :: String -> [String] -> Int
comparacao _ [] = 0
comparacao n (y:ys) | notaParaNumero n >= notaParaNumero y = 1 + comparacao n ys
                    | otherwise = comparacao n ys

notaParaNumero :: String -> Float
notaParaNumero x
  | x == "A+" = 9.7
  | x == "A"  = 9.3
  | x == "A-" = 9.0
  | x == "B+" = 8.7
  | x == "B"  = 8.3
  | x == "B-" = 8.0
  | x == "C+" = 7.7
  | x == "C"  = 7.3
  | x == "C-" = 7.0
  | x == "D+" = 6.7
  | x == "D"  = 6.3
  | x == "D-" = 6.0
  | otherwise = 5.9

lista1, lista2 :: [String] 
lista1 = ["A+", "B+", "D", "D-", "F", "A-", "B-", "A-"]
lista2 = ["A+", "C", "D", "D", "B", "A-"]

contagemNotas :: [String] -> [String] -> Int
contagemNotas _ [] = 0
contagemNotas [] _ = 0
contagemNotas xs ys = notas xs ys

main = do
    a <- getLine
    b <- getLine
    print (contagemNotas (read a) (read b))