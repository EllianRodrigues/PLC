-- calcula o maior diâmetro de uma arvore. o diâmetro de uma árvore e maior distância entre entre dois nós.

data Tree t = Nilt | Node t (Tree t) (Tree t) deriving (Eq, Show, Read)

altura :: Tree t -> Int
altura Nilt = 0
altura (Node _ esquerda direita) = 1 + max (altura esquerda) (altura direita)

maiorDiametro :: Tree t -> Int
maiorDiametro Nilt = 0
maiorDiametro (Node _ esquerda direita) = maximum [maiorDiametro esquerda, maiorDiametro direita, altura esquerda + altura direita + 1]

main = do
       s <- getLine
       let result = maiorDiametro (read s::Tree Int)
       print result