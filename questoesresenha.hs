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

