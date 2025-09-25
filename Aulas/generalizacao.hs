{-Map: eleva ao quadrado-}

lista :: [Int]
lista = [1,2,3,4,5]

listaNegativa :: [Int]
listaNegativa = [-1,-2,3,-4,5]


elevaAoQuadrado :: Int -> Int
elevaAoQuadrado x = x * x

mapping :: (a -> b) -> [a] -> [b]
mapping funcao [] = []
mapping funcao (cabeca:cauda) = funcao cabeca : mapping funcao cauda

{-Folding: A soma dos quadrados dos itens-}

myfoldr1 :: (t -> t -> t) -> [t] -> t
myfoldr1 funcao [cabeca] = cabeca
myfoldr1 funcao (cabeca:cauda) = funcao cabeca (myfoldr1 funcao cauda)

somaDosQuadrados :: [Int] -> Int
somaDosQuadrados l = myfoldr1 (+) (mapping elevaAoQuadrado l)

{-Filtering: Manter na lista todos os itens maiores que zero-}

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter funcao [] = []
myfilter funcao (cabeca:cauda) | funcao cabeca = cabeca : myfilter funcao cauda
                               | otherwise = myfilter funcao cauda

digit :: [Int] -> [Int]
digit l = myfilter (>0) l



