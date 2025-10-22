--Q1) Escreva uma função que insira um elemento em uma lista ordenada, mantendo a ordenação.
lista :: [Int]
lista = [1,3,5,7,9]

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (cabeca:cauda) | x <= cabeca = x:cabeca:cauda
                        | otherwise =cabeca: insert x cauda

--Q2) verifica se um numero é primo 

ehPrimo :: Int -> Bool
ehPrimo x | x <= 2 = True
          | otherwise = ehPrimoaux x (x-1)

ehPrimoaux :: Int -> Int -> Bool
ehPrimoaux x 1 = True
ehPrimoaux x y | mod x y == 0 = False
               | otherwise = ehPrimoaux x (y-1)

--Q3) Escreva uma funcão que retorna a soma dos quadrados dos numeros primos existentes entre dois numeros inteiros fornecidos como parametro.
--Q4) Reescreva a função do exercicio anterior utilizando uma expressão lambda.
sumPrimeSquare1 :: Int -> Int -> Int
sumPrimeSquare1 a b =  foldl (+) 0 (map (\x -> x*x) (filter ehPrimo [a..b]))

--Q4) Reescreva a função do exercicio anterior sem utilizar map e filter, usando compreensão de listas.

sumPrimeSquare2 :: Int -> Int -> Int
sumPrimeSquare2 a b = sum [x*x | x <- [a..b], ehPrimo x]

-----------------------------------------------------------------------------------------------------------------------------

{-Q1) Defina uma função que insira uma lista (primeiro parâmetro) em uma outra lista (segundo parâmetro) em uma posição informada (terceiro parâmetro). 
Se a posição por menor ou igual a zero insira a primeira lista no início da segunda lista;
se for igual ou maior que o tamanho da segundo lista, insira ela no final.-}

insert2 :: [t] -> [t] -> Int -> [t]
insert2 xs zs n | n <= 0 = xs ++ zs
                | n > length zs = zs ++ xs
                | otherwise = insert2aux xs zs n 

insert2aux :: [t] -> [t] -> Int -> [t]
insert2aux xs zs 0 = xs ++ zs
insert2aux xs (y:zs) n = y : insert2aux xs zs (n-1)

{- 2) (2.5) Defina uma função que informa em que posição um elemento (primeiro parametro) ocorre um uma lista (segundo parâmetro). 
Caso ele não ocorra você deve retornar o resultado (-1). Considere que primeira posição de uma lista é a posição zero. --}

search :: Eq t => t -> [t] -> Int
search _ [] = -1
search x (y:ys) | x == y = 0
                | otherwise = search2 x ys 1

search2 :: Eq t => t -> [t] -> Int -> Int
search2 _ [] _ = -1
search2 x (y:ys) n | x == y = n
                   | otherwise = search2 x ys (n+1)

{-3) (2.5) Dado o tipo de dados abaixo, que representa as horas do dia em formato de 12h pela manhã (AM) e 12h a partir de meio dia (PM), defina uma função em que sejam
adicionados minutos a uma hora do dia, considerando a possível mudan¸ca de turno. 
assuma que o formato da hora é vaálido e que serão somados no máximo 719 minutos. -}

data TimeOfDay = AM Int Int -- hora e minuto antes do meio dia
                | PM Int Int -- hora e minuto após o meio dia
                deriving (Show)

somaMinutos :: TimeOfDay -> Int -> TimeOfDay
somaMinutos (AM h m) x = somaMinutosaux (AM h (m+x))
somaMinutos (PM h m) x = somaMinutosaux (PM h (m+x))


somaMinutosaux :: TimeOfDay -> TimeOfDay
somaMinutosaux (AM h m) = amouPM (AM (h + div m 60) (mod m 60))
somaMinutosaux (PM h m) = amouPM (PM (h + div m 60) (mod m 60))

amouPM :: TimeOfDay -> TimeOfDay
amouPM (AM h m) | h >= 12 = PM h m
                | otherwise = AM h m
amouPM (PM h m) |h >= 12 = AM h m
                | otherwise = PM h m

{- 4) (2.5) Dado o tipo de dados abaixo, que indica se um valor é vaálido ou inválido (incorreto) defina uma função em que seja passado 
um valor de hora e minutos em formato de 24h e a função converta pra o formato TimeOfDay, se for possível, 
retornando o resultado com o construtor Success e a hora no novo formato, ou retornando Fail (falha/erro)
se o formato for inválido (não for menor que 24 ou os mintutos não forem menores que 60). -}

data Resultado t = Success t | Fail
                    deriving Show

convertTime :: Int -> Int -> Resultado TimeOfDay
convertTime h m | h > 24 = Fail
                | m >= 60 = Fail
                | otherwise = Success (convertTimeaux h m)

convertTimeaux :: Int -> Int -> TimeOfDay
convertTimeaux h m | h >= 12 = PM (h-12) m
                   | otherwise = AM h m

--------------------------------------------------------------------------------------------------------------------------------------

{-Escreva uma função que funcione como uma função "meio" ou "mid" do Excel: ela recebe 3 parâmetros, uma String e dois números ( digamos x e y), e retorna a substring que começa no primeiro índice
 ( o primeiro número, x )e contém y caracteres. A primeira posição na lista tem índice 1 ( um). 
Casos inválidos retornam a lista vazia (por exemplo, números negativos ou inicio fora do tamanho da lista). Se a lista contiver menos caracteres que o solicitado deve ser retornado o que for possível.-}

mid :: String -> Int -> Int -> String
mid str x y | x <= 0 = ""
            | y <= 0 = ""
            | x > length str = ""
            | otherwise = take y (drop (x-1) str)

{-2) (2.5) Escreva uma função localizar, que indica a posição  de início de uma String dentro de outra. 
Caso não seja encontrada ou a String de busca seja vazia, deve ser retornado o valor 0 (zero). 
A contagem das posições começa no valor 1 para o primeiro caracter. Deve ser retornada a posicao da primeira ocorrencia, se houver mais de uma.
Exemplos: localizar "abc" "xyz12abrt" ----> 0
          localizar "abc" "aaabrsabcfr" --> 7
          localizar "aab" "aacrabceaabc" -> 9
          localizar "" "aacrabceaabc" ----> 0-}

localizar :: String -> String -> Int
localizar "" _ = 0
localizar sub str = localizaraux sub str 1

localizaraux :: String -> String -> Int -> Int
localizaraux _ [] _ = 0
localizaraux sub str@(x:xs) n  | take (length sub) str == sub = n
                               | otherwise = localizaraux sub xs (n+1)

{-3) Uma fita infinita de papel pode ser representada por uma lista de caracteres. Esta fita possui uma cabeça de leitura/escrita que le ou escreve na posição atual da fita. 
Esta fita pode ser lida ou escrita (alterada) pelos seguintes comandos:
ParaFrente -- Que recebe um inteiro N como parâmetro e move a posição de leitura N posições para frente;
ParaTras -- que recebe um inteiro N como parâmetro e move a posição de leitura N posições para tras;
Escreva -- que recebe um caracter como parametro e escreve o caracter naquela posicao da fita, substituindo o caracter que estiver naquela posicao;
A fita comeca na posicao de numero 1.
Nao é preciso tratar o caso da fita se mover para uma posicao invalida (menor que 1).

data Comando = ParaFrente Int
             | ParaTras Int
             | Escreva Char
             deriving (Show, Eq)
             
3.1) (2.0) Esreva uma funcao que diga a posicao final da fita, apos receber uma lista de comandos.
Exemplos:
posicaofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y', ParaTras 1] ---> 6
posicaofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y']  --------------> 7
posicaofinal "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1]  ---------------------------> 7
posicaofinal "abcdefghijklmno" [] -------------------------------------------------------------------> 1
posicaofinal :: String -> [Comando] -> Int 

-}

data Comando = ParaFrente Int
             | ParaTras Int
             | Escreva Char
             deriving (Show, Eq)

posicaofinal :: String -> [Comando] -> Int
posicaofinal str comandos  = 1 + posicaofinalaux comandos

posicaofinalaux :: [Comando] -> Int
posicaofinalaux [] = 0
posicaofinalaux (ParaFrente n : cauda) = n + posicaofinalaux cauda
posicaofinalaux (ParaTras n : cauda) = (-n) + posicaofinalaux cauda
posicaofinalaux (Escreva _ : cauda) = posicaofinalaux cauda

{-

3.2) (2.0) Escreva uma funcao que, dada uma lista de comandos, retorne o caracter da posicao final da cabeca de leitura, apos comandos.
Exemplos:
interprete "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y', ParaTras 1] ---> 'x'
interprete "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1, Escreva 'y']  --------------> 'y'
interprete "abcdefghijklmno" [ParaFrente 5, Escreva 'x', ParaFrente 1]  ---------------------------> 'g'
interprete "abcdefghijklmno" [] -------------------------------------------------------------------> 'a'
interprete :: String -> [Comando] -> Char

-}

interprete :: String -> [Comando] -> Char
interprete str comandos = interpreteaux str comandos 1

interpreteaux :: String -> [Comando] -> Int -> Char
interpreteaux str [] pos = head (drop (pos - 1) str)
interpreteaux str (ParaFrente n: cauda) pos = interpreteaux str cauda (pos + n)
interpreteaux str (ParaTras n: cauda) pos = interpreteaux str cauda (pos - n)
interpreteaux str (Escreva c: cauda) pos = interpreteaux (replaceChar str (pos-1) c) cauda pos

replaceChar :: String -> Int -> Char -> String
replaceChar str index newChar = take index str ++ [newChar] ++ drop (index + 1) str