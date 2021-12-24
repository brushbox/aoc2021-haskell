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

{- 
-- Dirac Dice

We play with quantum dice. The dice can roll a 1,2 or 3.
In fact it rolls all three at once (splitting into 3 separate universes on each roll).
The game continues until one player reaches a score of 21.
We want to find the player that wins in the most universes...and count how many universes they win in.

Since each turn involves rolling the dice 3 times, that would mean 3 * 3 * 3 universes. But they
are not evenly likely. Three dice rolls can have a score of 3,4,5,6,7,8,9 each one occurring 
1,3,6,7,6,3,1 times respectively out of the 27 outcomes.

So, if we pass along the distribution with the "triple value" we can calculate how many universes the progression applies to
_and_ reduce the scale factor from 27 to 7.

That's probably a lot more manageable.

Further to that, there are only 10 squares on the board, and with only 7 possible progressions from each one we have 
70 outcomes. E.g. for pos p and a dice roll of d - the player moves to p' and scores s points.

Can we cache the player's pos and score? Because for any (pos, score) the subsequent splits will progress the same
regardless of how we got to that place (we don't care about the number of turns).

-}
part2 :: IO ()
part2 = do
    print "Day 21 part 2"

deterministicDice :: [Int]
deterministicDice = cycle [1..100]

diracDice :: [(Int, Int)]
diracDice = zip [3..9] [1, 3, 6, 7, 6, 3, 1]

newGame :: Int -> Int -> Game
newGame p1Pos p2Pos = ((p1Pos, 0), (p2Pos, 0), deterministicDice, 0)

playTurn :: Game -> Game
playTurn (player1, player2, dice, rolls) = (player1', player2', dice', rolls')
  where
    (player1', dice1, p1Rolls) = playerTurn player1 dice
    (player2', dice', p2Rolls) = playerTurn player2 dice1
    rolls' = rolls + p1Rolls + p2Rolls

playerTurn :: Player -> Dice -> (Player, Dice, Int)
playerTurn p@(pos, score) d = (p', d', rolls)
  where
    (pRolls, pDice) = splitAt 3 d
    points = sum pRolls
    pos' = movePlayer pos points
    (p', d', rolls) = if hasWon p then
                        (p, d, 0)
                      else
                        ((pos', score + pos'), pDice, 3)
    movePlayer p d = (p + d - 1) `mod` 10 + 1

-- Dirac Dice
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