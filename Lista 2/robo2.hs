data Command = Forward Int | Backward Int | TurnLeft | TurnRight
  deriving (Eq, Show, Read)

destination :: (Int, Int) -> [Command] -> (Int, Int)
destination start cmds =
  fst (foldl apply (start, (0,1)) cmds)

apply :: ((Int, Int), (Int, Int)) -> Command -> ((Int, Int), (Int, Int))
apply ((x, y), (dx, dy)) (Forward n)  = ((x + n * dx, y + n * dy), (dx, dy))
apply ((x, y), (dx, dy)) (Backward n) = ((x - n * dx, y - n * dy), (dx, dy))
apply ((x, y), (dx, dy)) TurnLeft     = ((x, y), (-dy, dx))
apply ((x, y), (dx, dy)) TurnRight    = ((x, y), (dy, -dx))

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  print (destination (read a) (read b))
