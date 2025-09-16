import Data.Char (isDigit)

lista1 :: [Int]
lista1 = [1,2,3,4,5]

lista2 :: [Int]
lista2 = [1,5,6]

listaPares :: [(Int,Int)]
listaPares = [(1,2)]

{-O tamanho da lista-}
contaItens :: [Int] -> Int
contaItens = length

{-Dobra os elementos da lista-}

dobrarElementos :: [Int] -> [Int]
dobrarElementos [] = []
dobrarElementos (cabeca:cauda) = (2*cabeca) : dobrarElementos cauda 
{-Ou-}
dobrarElementos2 :: [Int] -> [Int]
dobrarElementos2 = map (2 *)

{-Se um elemento está na lista-}
elementoNaLista :: [Int] -> Int -> Bool
elementoNaLista [] n = False
elementoNaLista (cabeca:cauda) n | cabeca == n = True
                                 | otherwise = elementoNaLista cauda n

{-Mostrar apenas o numeros de uma string-}
apenasNumeros :: String -> String
apenasNumeros [] = []
apenasNumeros (cabeca:cauda)  | isDigit cabeca = show cabeca ++ apenasNumeros cauda
                              | otherwise = apenasNumeros cauda
{-ou-}
apenasNumeros2 :: String -> String
apenasNumeros2 [] = []
apenasNumeros2 (cabeca:cauda)  | cabeca >= '0' && cabeca <= '9' = show cabeca ++ apenasNumeros2 cauda
                                | otherwise = apenasNumeros2 cauda

{-Soma de uma lista de pares-}
somaPares :: [(Int,Int)] -> Int
somaPares [] = 0
somaPares ((x,y):cauda) = x + y + somaPares cauda

{-Banco de dados-}

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

livros :: BancoDados -> Pessoa -> [Livro]
livros bd pessoa = [ livro | (p,livro) <- bd, p == pessoa]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos bd livro = [ pessoa | (pessoa, l) <- bd, l == livro]

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd pessoa livro = [ (p,l) | (p,l) <- bd, not (p == pessoa && l == livro)]

baseExemplo :: BancoDados
baseExemplo = [("Joao","O Senhor dos Aneis"),("Maria","Harry Potter"),("Joao","A Biblia")]

{-Quick Sort-}
lista :: [Int]
lista = [3,1,4,1,5,9,2,6,5,3,5]

lista3 :: [Int]
lista3 = [10,7,8,9,1,5]

qs :: [Int] -> [Int]
qs [] = []
qs (pivo:cauda) = qs [ x | x <- cauda, x <= pivo ] ++ [pivo] ++ qs [ x | x <- cauda, x > pivo]

