data Cmd = Cursor Int
         | Backspace Int
         | Delete Int
         | Insert String
         deriving (Read, Show)

editText :: String -> [Cmd] -> String
editText texto cmds = aplicaComandos texto 0 cmds

aplicaComandos :: String -> Int -> [Cmd] -> String
aplicaComandos texto _ [] = texto
aplicaComandos texto cursor (c:cs) =
  case c of
    Cursor x ->
      aplicaComandos texto (cursor + x) cs

    Backspace x ->
      let (antes, depois) = splitAt cursor texto
          novoAntes = take (max 0 (length antes - x)) antes
          novoTexto = novoAntes ++ depois
          novoCursor = max 0 (cursor - x)
      in aplicaComandos novoTexto novoCursor cs

    Delete x ->
      let (antes, depois) = splitAt cursor texto
          novoTexto = antes ++ drop x depois
      in aplicaComandos novoTexto cursor cs

    Insert s ->
      let (antes, depois) = splitAt cursor texto
          novoTexto = antes ++ s ++ depois
          novoCursor = cursor 
      in aplicaComandos novoTexto novoCursor cs


main = do
       a <- getLine
       b <- getLine
       let result = editText a (read b)
       print result
