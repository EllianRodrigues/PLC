type Chave = [(Char,Char)]

letras :: [Char]
letras = ['A'..'Z']

crie_chave :: Int -> Chave
crie_chave n = zip letras (drop n (cycle letras))

