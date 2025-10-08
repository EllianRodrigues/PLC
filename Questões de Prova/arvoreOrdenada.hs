data Tree = No Int Tree Tree | Folha Int deriving Show

teste1 = No 50 (No 25 (No 12 (Folha 6) (Folha 13)) 
                      (No 30 (Folha 26) (Folha 32)))
               (Folha 59)

collapse :: Tree -> [Int]
collapse (Folha x) = [x]
collapse (No x n1 n2) = collapse n1 ++ [x] ++ collapse n2 

lista_ordenada :: [Int] -> Bool
lista_ordenada [] = True
lista_ordenada [x] = True
lista_ordenada (x:y:xyz) = x <= y && lista_ordenada (xyz)

ordenada :: Tree -> Bool
ordenada arvore = lista_ordenada (collapse arvore)


