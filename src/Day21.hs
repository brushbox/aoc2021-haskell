module Day21
  ( part1, part2)
where

type Dice = [Int]
type Pos = Int
type Score = Int
type Player = (Pos, Score)
type Rolls = Int
type Game = (Player, Player, Dice, Rolls)

part1 :: IO ()
part1 = do
    print "Day 21 part 1"
    let (p1, p2, _, rolls) = playGame $ newGame 9 4
    -- let (p1, p2, _, rolls) = playGame $ newGame 4 8
    let answer = rolls * loserScore p1 p2
    print answer

part2 :: IO ()
part2 = do
    print "Day 21 part 2"


deterministicDice :: [Int]
deterministicDice = cycle [1..100]

newGame :: Int -> Int -> Game
newGame p1Pos p2Pos = ((p1Pos, 0), (p2Pos, 0), deterministicDice, 0)

playTurn :: Game -> Game
playTurn (player1, player2, dice, rolls) = (player1', player2', dice', rolls')
  where
    (p1Pos, p1Score) = player1
    (p2Pos, p2Score) = player2
    (p1Rolls, p2Dice) = splitAt 3 dice
    (p2Rolls, dice') = splitAt 3 p2Dice
    p1Points = sum p1Rolls
    p2Points = sum p2Rolls
    p1Pos' = movePlayer p1Pos p1Points
    p2Pos' = movePlayer p2Pos p2Points
    (player1', rolls2) = if hasWon player1 then 
                            (player1, rolls) 
                         else 
                            ((p1Pos', p1Score + p1Pos'), rolls + 3)
    (player2', rolls') = if hasWon player1' then 
                            (player2, rolls2) 
                         else 
                            ((p2Pos', p2Score + p2Pos'), rolls2 + 3)
    movePlayer p d = (p + d - 1) `mod` 10 + 1

loserScore (_, s1) (_, s2) | s1 >= 1000 = s2
                           | otherwise = s1

playGame :: Game -> Game
playGame g = if gameWon then g' else playGame g'
  where
    g' = playTurn g
    (player1, player2, _, _) = g'
    gameWon = hasWon player1 || hasWon player2

hasWon :: Player -> Bool
hasWon (_, score) = score >= 1000