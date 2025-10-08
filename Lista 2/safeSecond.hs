-- Recebe uma lista e retorne Just o segundo elemento dessa lista. Caso esse esse elemento não exista, retorne Nothing.

import Prelude hiding (Maybe(..))

data Maybe a = Just a | Nothing deriving Show

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond [x] = Nothing
safeSecond (x:y:xs) = Just y

main = do
    a <- getLine
    let result = safeSecond (read a ::[Int])
    print result
