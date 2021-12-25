module Day21
  ( part1, part2)
where

import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

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

deterministicDice :: [Int]
deterministicDice = cycle [1..100]

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
    (p', d', rolls) = if hasWonDeterministic p then
                        (p, d, 0)
                      else
                        ((pos', score + pos'), pDice, 3)
    movePlayer p d = (p + d - 1) `mod` 10 + 1

loserScore (_, s1) (_, s2) | s1 >= 1000 = s2
                           | otherwise = s1

playGame :: Game -> Game
playGame g = if gameWon then g' else playGame g'
  where
    g' = playTurn g
    (player1, player2, _, _) = g'
    gameWon = hasWonDeterministic player1 || hasWonDeterministic player2

hasWon :: Int -> Player -> Bool
hasWon target (_, score) = score >= target

hasWonDeterministic :: Player -> Bool
hasWonDeterministic = hasWon 1000

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

For each pos and score: there are 10 pos and 21 scores (anything > 21 can be treated as 21)...210 combinations

We have a starting position (Player, Player, UniverseCount) => ((p1, 0), (p2, 0), 0)
For each turn we run through the 7 possible rolls ... for _each_ player

diracPlayer :: Player -> DiracDice -> [(Player, UniverseCount)]

We can do this as a fold by starting with a single player in a list.

But we still need to switch to the other player.

What if we move by turns - where a turn is both players' moves.
So for a single turn we want:
* the dice sum for each player
* the number of universes this applies to
turnRolls = [((d1, d2), u1 + u2)| (d1, u1) <- diracDice, (d2, u2) <- diracDice]

A Player is the same as in the simple game.
A DiracGame is (Player, Player, UniverseCount)

diracTurn :: UniverseMap -> [TurnRoll] -> UniverseMap
-}

type UniverseCount = Int
type DiceRoll = Int
type TurnRoll = (DiceRoll, UniverseCount)
type UniverseMap = Map (Player, Player) UniverseCount
type DiracGame = ((Player, Player), UniverseCount)

part2 :: IO ()
part2 = do
    print "Day 21 part 2"

diracDiceRolls :: [(DiceRoll, UniverseCount)]
diracDiceRolls = zip [3..9] [1, 3, 6, 7, 6, 3, 1]

turnRolls :: [TurnRoll]
turnRolls = [((d1, d2), u1 * u2) | (d1, u1) <- diracDiceRolls, (d2, u2) <- diracDiceRolls]

{-
To play dirac dice I need to have:
* a map of universes after the last turn starts as `Map.singleton ((4, 0), (8, 0)) 1` - for the example
* a count of the p1 wins and p2 wins so far (0, 0)
* then we:
  * if the input map is empty we return the win counts 
  * do a diracTurn on the inputMap to get the outputMap
  * partition the outputMap into finished games and games still in progress
  * tally up the p1 and p2 wins - update the winCounts
  * the games still in progress becomes the input for the next round.
-}

playDiracDice :: (Int, Int)
playDiracDice = diracDice (Map.singleton ((4, 0), (8, 0)) 1) (0, 0)

diracDice :: UniverseMap -> (Int, Int) -> (Int, Int)
diracDice us winCounts@(p1Wins, p2Wins)
  | Map.null us = winCounts
  | otherwise = diracDice stillInProgress (p1Wins', p2Wins')
  where
    outputUs = diracTurn us
    (gamesWon, stillInProgress) = Map.partitionWithKey (curry gameWonDirac) outputUs
    (wonByP1, wonByP2) = foldl (\(w1, w2) ((p1, p2), c) -> (w1 + winCount p1 c, w2 + winCount p2 c)) (0, 0) $ Map.toList gamesWon
    (p1Wins', p2Wins') = (p1Wins + wonByP1, p2Wins + wonByP2)
    winCount p@(_, s) c | hasWonDirac p = c 
                        | otherwise = 0

-- build a new map of Universes by splitting all the current universes by the turnmap
diracTurn :: UniverseMap -> UniverseMap
diracTurn m = foldl splitUniverse Map.empty $ Map.toList m
  where
    splitUniverse m u = insertListWith (*) (applyRolls u turnRolls) m

applyRolls :: DiracGame -> [TurnRoll] -> [DiracGame]
applyRolls game = map (applyTurn game)

applyTurn :: DiracGame -> TurnRoll -> DiracGame
applyTurn g@((p1, p2), universes) ((p1Roll, p2Roll), count)
  | gameWonDirac g = g
  | otherwise = ((p1', p2'), universes'')
  where
   (p1', universes') = movePlayer p1 p1Roll universes
   (p2', universes'') = if hasWonDirac p1 then (p2, universes') else movePlayer p2 p2Roll universes'
   movePlayer (pos, score) roll universes = ((newPos pos roll, score + newPos pos roll), universes * count)
   newPos p d = (p + d - 1) `mod` 10 + 1

gameWonDirac :: DiracGame -> Bool
gameWonDirac ((p1, p2), _) = hasWonDirac p1 || hasWonDirac p2

hasWonDirac :: Player -> Bool
hasWonDirac = hasWon 4

insertListWith :: Ord key
               => (elt -> elt -> elt)
               -> [(key,elt)]
               -> Map key elt
               -> Map key elt
insertListWith f xs m0 = foldl' (\m (k, v) -> Map.insertWith f k v m) m0 xs


-- Here's what my first attempt got (expected vs actual)
-- 444356092776315
-- 5810775830793857067

-- 341960390180808
-- 3519343295535406443
