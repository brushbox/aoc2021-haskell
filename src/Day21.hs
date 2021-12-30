module Day21
  ( part1, part2)
where

import Debug.Trace
import Data.List
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

type Dice = [Int]
type Pos = Int
type Score = Int
-- type Player = (Pos, Score)
type Rolls = Int
type Game = (Player, Player, Dice, Rolls)

data Player = Player Int Int
  deriving Show

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
newGame p1Pos p2Pos = (Player p1Pos 0, Player p2Pos 0, deterministicDice, 0)

-- playTurn :: Game -> Game
-- playTurn (player, p2, dice, rolls) = (p2, player', dice', rolls + rolls')
--   where
--     (player', dice', rolls') = playerTurn player dice

playTurn :: Game -> Game
playTurn (player1, player2, dice, rolls) = (player1', player2', dice', rolls')
  where
    (player1', dice1, p1Rolls) = playerTurn player1 dice
    (player2', dice', p2Rolls) = playerTurn player2 dice1
    rolls' = rolls + p1Rolls + p2Rolls

playerTurn :: Player -> Dice -> (Player, Dice, Int)
playerTurn p@(Player pos score) dice = (p', dice'', rolls')
  where
    (rolls, dice') = splitAt 3 dice
    points = sum rolls
    pos' = movePlayer pos points
    (p', dice'', rolls') = if hasWonDeterministic p then
                        (p, dice, 0)
                      else
                        (Player pos' (score + pos'), dice', 3)
    movePlayer p d = (p + d - 1) `mod` 10 + 1

loserScore :: Player -> Player -> Int
loserScore (Player _ s1) (Player _ s2) = min s1 s2

playGame :: Game -> Game
playGame g = if gameWon then g' else playGame g'
  where
    g' = playTurn g
    (player1, player2, _, _) = g'
    gameWon = hasWonDeterministic player1 || hasWonDeterministic player2

hasWon :: Int -> Player -> Bool
hasWon target (Player _ score) = score >= target

hasWonDeterministic :: Player -> Bool
hasWonDeterministic = hasWon 1000

type UniverseCount = Int
type DiceRoll = (Int, UniverseCount)
type UniverseMap = Map (Player, Player) Int
type DiracGame = ((Player, Player), UniverseCount)

instance Eq Player where
    Player p1 s1 == Player p2 s2 = p1 == p2 && s1 == s2

instance Ord Player where
    Player p1 s1 <= Player p2 s2 = s2 > s1 || (s2 == s1 && p2 >= p1)

part2 :: IO ()
part2 = do
    print "Day 21 part 2"
    let (a, b)= playDiracDice startingGame
    print $ max a b

diracDiceRolls :: [DiceRoll]
diracDiceRolls = zip [3..9] [1, 3, 6, 7, 6, 3, 1]

startingGame :: ((Player, Player), Int)
startingGame = ((Player 9 0, Player 4 0), 1)

exampleGame :: ((Player, Player), Int)
exampleGame = ((Player 4 0, Player 8 0), 1)

-- wincounts for starting pos 4, 8
-- Winning Score	Player 1 Wins	Player 2 Wins	Player 1 Win%
-- 1	27	0	100.00
-- 2	183	156	53.98
-- 3	990	207	82.71
-- 4	2930	971	75.11
-- 5	7907	2728	74.35
-- 6	30498	7203	80.89
-- 7	127019	152976	45.36
-- 8	655661	1048978	38.46
-- 9	4008007	4049420	49.74
-- 10	18973591	12657100	59.98
-- 11	90197150	47304735	65.60
-- 12	454323519	217150220	67.66
-- 13	2159295972	1251104269	63.32
-- 14	9632852745	7543855038	56.08
-- 15	43413388231	37334719860	53.76
-- 16	199092281721	161946691198	55.14
-- 17	903307715712	698632570521	56.39
-- 18	4227532541969	3151502992942	57.29
-- 19	20259464849183	14795269706204	57.79
-- 20	95627706087732	71421811355805	57.25
-- 21	444356092776315	341960390180808	56.51

{-
If we play a single turn with a winning score of 1, then the player who is playing will win all 27 games.

Give a single Game state let's play all the possible dice for a single player and come up with a list of output gamestate/universes.
-}
diracTurn :: DiracGame -> (Int, [DiracGame])
diracTurn ((player, otherPlayer), universes) = (winCount, inPlay)
  where
    winCount = sum $ map snd wins
    (wins, inPlay) = partition (\((_, p), _) -> hasWonDirac p) outcomes
    outcomes = map newUniverse diracDiceRolls
    newUniverse :: (Int, Int) -> DiracGame -- Note: we flip-flop the players here.
    newUniverse (sum, freq) = ((otherPlayer, movePlayer player (sum, freq)), universes * freq)

movePlayer :: Player -> (Int, Int) -> Player
movePlayer (Player pos score) (sum, _) = Player newPos (score + newPos)
  where
    newPos = (pos + sum - 1) `mod` 10 + 1

hasWonDirac :: Player -> Bool
hasWonDirac = hasWon 21

playDiracDice :: DiracGame -> (Int, Int)
playDiracDice (state, count) = diracDice (Map.singleton state count) (0, 0)

diracDice :: UniverseMap -> (Int, Int) -> (Int, Int)
diracDice games (p1Wins, p2Wins)
  | Map.null games = (p1Wins, p2Wins)
  | otherwise = (p1Wins', p2Wins')
  where
    outcomes :: [(Int, [DiracGame])]
    outcomes = map diracTurn $ Map.toList games
    wins = map fst outcomes
    nextGames = map snd outcomes
    gamesInPlay = foldl (flip (insertListWith (+))) Map.empty nextGames
    (p2Wins', p1Wins') = diracDice gamesInPlay (p2Wins, p1Wins + sum wins)

dbg lst = listSize ++ mapSize
  where
    listSize = "List " ++ show (length lst)
    mapSize = " Map " ++ show (Map.size map)
    map = insertListWith (*) lst Map.empty

insertListWith :: Ord key
               => (elt -> elt -> elt)
               -> [(key,elt)]
               -> Map key elt
               -> Map key elt
insertListWith f xs m0 = foldl' (\m (k, v) -> Map.insertWith f k v m) m0 xs


