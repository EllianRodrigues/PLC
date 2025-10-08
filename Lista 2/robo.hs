-- Recebe a direção inicial do robô e uma lista de comandos e retorna para qual direção o robô estará voltado depois de executar essa lista de comandos.

data Command = Forward Int | Backward Int | TurnLeft | TurnRight
    deriving (Eq,Show, Read)

data Direction = North | South | West | East deriving (Show, Read)

faces :: Direction -> [Command] -> Direction
faces dir [] = dir
faces dir (Forward _:resto) = faces dir resto
faces dir (Backward _:resto) = faces dir resto 
faces North (TurnLeft:resto) = faces West resto
faces North (TurnRight:resto) = faces East resto
faces South (TurnLeft:resto) = faces East resto
faces South (TurnRight:resto) = faces West resto
faces West (TurnLeft:resto) = faces South resto
faces West (TurnRight:resto) = faces North resto
faces East (TurnLeft:resto) = faces North resto
faces East (TurnRight:resto) = faces South resto

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a ::Direction) (read b ::[Command])
       print result