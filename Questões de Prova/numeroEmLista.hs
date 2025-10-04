--Colocar um n em uma lista ordenada
lista :: [Int]
lista = [1,2,3,5,6,7]

numeroEmLista :: Int -> [Int] -> [Int]
numeroEmLista n [] = [n]
numeroEmLista n (cabeca:resto) | n <= cabeca = n:cabeca:resto
                               | otherwise = cabeca:numeroEmLista n resto