answer :: Int
answer = 72

greater :: Bool
greater = (answer > 71)

yes :: Bool
yes = True

square :: Int -> Int
square x = x * x

allEqual :: Int -> Int -> Int -> Bool
allEqual x y z = (x == y) && (y == z)

maxi :: Int -> Int -> Int
maxi a b | a >= b = a
         | otherwise = b

{------------------------------------------------}

fatorial :: Int -> Int
fatorial n | n == 0  = 1
           | otherwise = fatorial (n-1) * n

fatorialpadrao :: Int -> Int
fatorialpadrao 0 = 1
fatorialpadrao n =  fatorialpadrao (n-1) * n

{--------------Exercicios------------------------}

{-Defina uma função que, dado um valor inteiro s e
um número de semanas n, retorna quantas
semanas de 0 a n tiveram vendas iguais a s. Para
resolver esta questão, primeiro construa uma
definição simples para vendas.-}

vendas :: Int -> Int
vendas 0 = 10
vendas 1 = 15
vendas 2 = 10
vendas 3 = 20
vendas 4 = 15
vendas 5 = 10

somavendasiguaisa :: Int -> Int -> Int
somavendasiguaisa s 0
    | vendas 0 == s = 1
    | otherwise = 0
somavendasiguaisa s n
    | vendas n == s = 1 + somavendasiguaisa s (n-1)
    | otherwise = somavendasiguaisa s (n-1)

{-Defina uma função que, dado um número inteiro,
determina se ele é primo ou não.-}

verificaprimo :: Int -> Bool
verificaprimo n | n < 2 = False
                | otherwise = verificaprimoaux n (n-1)

verificaprimoaux :: Int -> Int -> Bool
verificaprimoaux n 1 = True
verificaprimoaux n d | mod n d == 0 = False
                     | otherwise =verificaprimoaux n (d-1)

{-Defina uma função que, dados dois números
inteiros x e y, determina se esses números
são primos entre si:-}

primosentresi :: Int -> Int -> Bool
primosentresi x y | mdc x y == 1 = True
                  | otherwise = False

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (mod a b)

{-Defina as seguintes funções:
– fatorial
fat :: Int -> Int
– compara se quatro números são iguais
all4Equal :: Int -> Int -> Int -> Int -> Bool
– Defina all4Equal usando allEqual
– retorna quantos parâmetros são iguais
equalCount :: Int -> Int -> Int -> Int-}


fat :: Int -> Int
fat x | x == 0 = 1
      | otherwise = x * fat (x-1)

allEqual :: Int -> Int -> Bool
allEqual x y | x == y = True
             | otherwise = False

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal a b c d | allEqual a b && allEqual b c && allEqual c d = True
                  | otherwise = False

equalCount :: Int -> Int -> Int -> Int
equalCount a b c | allEqual a b && allEqual b c = 3
                 | allEqual a b || allEqual a c || allEqual b c = 2
                 | otherwise = 1


