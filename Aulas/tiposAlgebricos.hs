-- data Bool = True | False

data Estacao = Inverno | Verao | Outono | Primavera
    deriving Show
data Temp = Frio | Quente
    deriving Show

clima :: Estacao -> Temp
clima Inverno = Frio
clima _ = Quente

-------------------------------------------------------
type Nome = String
type Idade = Int
data Pessoas = Pessoa Nome Idade

maria :: Pessoas
maria = Pessoa "Maria" 22

jose :: Pessoas
jose = Pessoa "Jose" 23

showPerson :: Pessoas -> String
showPerson (Pessoa n a) = n ++ " -- " ++ show a

---------------------------------------------------------
data Shape = Circle Float | Rectangle Float Float | Square Float

circle :: Shape
circle = Circle 4.9

rectangle :: Shape
rectangle = Rectangle 4.2 2.0

square :: Shape 
square = Square 5.2


isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = pi * r*r 
area (Rectangle l1 l2) = l1*l2  
area (Square x) = x*x

---------------------------------------------------------
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mult Expr Expr

eval :: Expr -> Int
eval (Lit n) = n 
eval (Add n1 n2) = (eval n1) + (eval n2)
eval (Sub n1 n2) = (eval n1) - (eval n2)
eval (Mult n1 n2) = (eval n1) * (eval n2)

expressao = Sub (Add (Lit 3) (Lit 4))
                (Add (Lit 9) (Lit 10))

----------------------------------------------------------

{-Questões-}

data StrExpr = Lit2 Int | Add2 StrExpr StrExpr | Sub2 StrExpr StrExpr | Mult2 StrExpr StrExpr

showExpress :: StrExpr -> String
showExpress (Lit2 n) = (show n)
showExpress (Add2 n1 n2) = "(" ++ (showExpress n1) ++ "+" ++ (showExpress n2) ++ ")"
showExpress (Sub2 n1 n2) = "(" ++ (showExpress n1) ++ "-" ++ (showExpress n2) ++ ")"
showExpress (Mult2 n1 n2) = "(" ++ (showExpress n1) ++ "*" ++ (showExpress n2) ++ ")"

expressao2 = Add2 (Lit2 2) (Lit2 5)
expressao3 = Sub2 (Add2 (Lit2 3) (Lit2 4))
                (Add2 (Lit2 9) (Lit2 10))

----------------------------------------------------------

data List t = Nil | Cons t (List t)
    deriving Show

toList :: List t -> [t]
toList Nil = []
toList (Cons x list) = x:toList list

fromList :: [t] -> List t 
fromList [] = Nil
fromList (a:as) = Cons a (fromList as)

-----------------------------------------------------------

