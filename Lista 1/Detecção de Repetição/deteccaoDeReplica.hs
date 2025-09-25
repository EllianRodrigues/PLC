{-Recebe uma string, um inteiro x e um char e verifica se essa string é exatamente a repetição do char x vezes seguidas.-}

isReplica :: String -> Int -> Char -> Bool
isReplica string numero char | length string /= numero = False 
                             | otherwise = isReplicaAux string numero char

isReplicaAux :: String -> Int -> Char -> Bool
isReplicaAux [] 0 char = True
isReplicaAux (cabeca:cauda) numero char | cabeca == char = isReplicaAux cauda (numero-1) char
                                        | otherwise = False

main :: IO ()
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    let result = isReplica a (read b) (head c)
    print result