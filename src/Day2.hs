module Day2 
    ( part1
    , part2
    )
    where

data Instruction = 
    Up Int |
    Down Int |
    Forward Int

type Position = (Int, Int)

part1 :: IO ()
part1 = do
    lines <- lines <$> readFile "day2.txt"
    let instructions = map toInstruction lines
    let (x, y) = foldl move (0, 0) instructions
    putStrLn $ show (x * y)

toInstruction :: String -> Instruction
toInstruction line =
    case inst of
        "forward" -> Forward val
        "up" -> Up val
        "down" -> Down val
    where
        [inst, second] = words line
        val = read second

move :: Position -> Instruction -> Position 
move (x, y) (Forward dx) = (x + dx, y)
move (x, y) (Up dy) = (x, y - dy)
move (x, y) (Down dy) = (x, y + dy)

type Position2 = (Int, Int, Int)

part2 :: IO ()
part2 = do
    lines <- lines <$> readFile "day2.txt"
    let instructions = map toInstruction lines
    let (x, y, _) = foldl move2 (0, 0, 0) instructions
    putStrLn $ show (x * y)

move2 :: Position2 -> Instruction -> Position2
move2 (x, y, aim) (Forward val) = (x + val, y + aim * val, aim)
move2 (x, y, aim) (Up val) = (x, y, aim - val)
move2 (x, y, aim) (Down val) = (x, y, aim + val)
