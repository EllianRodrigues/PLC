-- Retorne a altura de uma árvore dada. Lembrando que, a altura da árvore é definida por sua subárvore mais "alta".

data Tree t = Nilt | Node t (Tree t) (Tree t) deriving (Eq, Show, Read)

altura :: Tree t -> Int
altura Nilt = 0
altura (Node _ esquerda direita) = 1 + max (altura esquerda) (altura direita)

main = do
       a <- getLine
       let result = altura (read a::Tree Int)
       print result