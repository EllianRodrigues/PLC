-- Q1
type Chave = [(Char,Char)]

letras :: [Char]
letras = ['A'..'Z']

criaChave :: Int -> Chave
criaChave n = zip letras ( drop n ( cycle letras) )

-- Q2

crypt :: Chave -> String -> String
crypt chave texto = [ procura c chave | c <- texto ]

procura :: Char -> Chave -> Char
procura c [] = c
procura c ((x,y):xs) | c == x    = y
                     | otherwise = procura c xs 

-- Q3

data ChaveTree = Node Char Char ChaveTree ChaveTree | Leaf
                 deriving Show

chaveParcial :: ChaveTree
chaveParcial = Node 'I' 'L' (Node 'A' 'D' Leaf Leaf) (Node 'L' 'O' Leaf Leaf)

cryptT :: ChaveTree -> String -> String
cryptT chave texto = [procuraT c chave | c <- texto]

procuraT :: Char -> ChaveTree -> Char
procuraT c Leaf = c
procuraT c (Node x y left right) | c == x = y
                                 | c <=x = procuraT c left                   
                                 | otherwise = procuraT c right

--Q4

ctree_to_chave :: ChaveTree -> Chave
ctree_to_chave Leaf = []
ctree_to_chave (Node x y left right) = ctree_to_chave left ++ [(x,y)] ++ ctree_to_chave right