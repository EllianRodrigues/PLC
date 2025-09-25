import Data.Char

{- Função para transformar letras minúsculas em maiúsculas -}
offset:: Int
offset = ord 'A' - ord 'a'

transmaiuscula :: Char -> Char
transmaiuscula letra = chr (ord letra - offset)

{- Adiciona espaços em branco -}
addspace :: Int -> String
addspace n | n == 0 = ""
           | otherwise = " " ++ addspace (n-1)

{-- Adiciona espaços em branco à direita -}
paradireita :: Int -> String -> String
paradireita 0 s = s
paradireita n s = " " ++ paradireita (n-1) s

{-Escreva uma função para retornar, em forma de tabela,
todas as vendas da semana 0 até a semana n, incluindo
o total e a média de vendas no período.-}

{-vendas-}
vendas :: Int -> Int
vendas 0 = 12
vendas 1 = 14
vendas 2 = 15

{-cabeçalho-}
cabecalho :: String
cabecalho = "Semana Venda\n"

{-imprime vendas da semana-}
imprimesemana :: Int -> String
imprimesemana 0 = "0      " ++ show (vendas 0)
imprimesemana n = imprimesemana(n-1) ++ "\n" ++ show n ++ "      " ++ show (vendas n)

{-imprime total de vendas-}
imprimetotal :: Int -> String
imprimetotal n = "\nTotal: " ++ show (somavendas n)

somavendas :: Int -> Int
somavendas 0 = vendas 0
somavendas n = vendas n + somavendas (n-1)

{-imprime media de vendas-}
imprimemedia :: Int -> String
imprimemedia n = "\nMédia: " ++ show (fromIntegral (somavendas n) / fromIntegral (n+1)) 

{-função principal-}
tabelavendas :: Int -> String
tabelavendas n = cabecalho ++ imprimesemana n ++ imprimetotal n ++ imprimemedia n
