--  recebe uma lista de nomes, e retorna essa mesma lista com os nomes ordenados.

lista :: [String]
lista = ["zab","bza","azb","baz","zba"]

bsort :: [String] -> [String]
bsort [] = []
bsort (cabeca:cauda)  = bsort [  x | x <- cauda, x <= cabeca] ++ [cabeca] ++ bsort [ x | x <- cauda, x > cabeca]

main = do
    a <- getLine
    let lista = bsort (read a :: [String])
    print lista
