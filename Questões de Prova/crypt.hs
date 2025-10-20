type Chave = [(Char,Char)]

letras :: [Char]
letras = ['A'..'Z']

crie_chave :: Int -> Chave
crie_chave n = zip letras (drop n (cycle letras))

crypt :: Chave -> String -> String
crypt chave msg = [procura c chave | c <- msg]

procura :: Char -> Chave -> Char
procura c [] = c
procura c ((x,y):xs) | c == x = y
                     | otherwise = procura c (xs)