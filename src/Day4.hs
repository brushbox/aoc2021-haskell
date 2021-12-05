module Day4
    ( part1
    , part2
    ) where

import Data.List.Split
import Data.List

part1 :: IO ()
part1 = do
    putStrLn "Day4: part1"
    lines <- lines <$> readFile "day4.txt"
    let numbersToPlay = toNums $ head lines
    let boardLines = drop 2 lines
    let chunks = chunksOf 6 boardLines
    let boards = map makeBoard chunks

    case (playBingo [] numbersToPlay boards) of
        Nothing -> putStrLn "No Solution Found"
        Just (board, played) -> calculateSolution board played

toNums :: String -> [Int]
toNums lines = map read $ splitOn "," lines

data Board = Board [[Int]]
    deriving Show

makeBoard :: [String] -> Board
makeBoard lines = 
    Board nums
    where
        nums :: [[Int]]
        nums = map lineToNums (take 5 lines)
        lineToNums :: String -> [Int]
        lineToNums line = map read (words line)

boardRows :: Board -> [[Int]]
boardRows (Board rows) = rows

boardColumns :: Board -> [[Int]]
boardColumns (Board rows) = transpose rows
-- boardColumns (Board [a, b, c, d, e]) =
--     map (\(a', b', c', d', e') -> [a', b', c', d', e']) (zip5 a b c d e)

allContained :: [Int] -> [Int] -> Bool
-- allContained set candidates = all (\n -> n `elem` set) candidates
allContained set = all (`elem` set) 

boardSolved :: [Int] -> Board -> Bool
boardSolved set board =
    any (allContained set) (boardRows board) ||
        any (allContained set) (boardColumns board)

findSolution :: [Int] -> [Board] -> Maybe Board
findSolution set boards = find (boardSolved set) boards

playBingo :: [Int] -> [Int] -> [Board] -> Maybe (Board, [Int])
playBingo _ [] boards = Nothing
playBingo played (next:remaining) boards =
    case (findSolution (next:played) boards) of
        Nothing -> playBingo (next:played) remaining boards
        Just board -> Just (board, (next:played))
    
calculateSolution :: Board -> [Int] -> IO ()
calculateSolution (Board rows) played@(last:_) = do
    putStrLn $ "Winning board " ++ (show rows)
    putStrLn $ "Played: " ++ (show played)
    putStrLn (show answer)
    where
        answer = last * sumUnmarked
        sumUnmarked = sum unmarked
        unmarked = unmarkedNumbers played (concat rows)

unmarkedNumbers :: [Int] -> [Int] -> [Int]
unmarkedNumbers played boardNums =
    -- foldl (\result num -> delete num result) (concat boardRows) played
    foldl (flip delete) boardNums played

part2 :: IO ()
part2 = do
    putStrLn "Day4: part2"
    lines <- lines <$> readFile "day4.txt"
    let numbersToPlay = toNums $ head lines
    let boards = map makeBoard (chunksOf 6 (drop 2 lines))
    let winners = findAllWinners numbersToPlay boards
    let (winningBoards,played) = head winners
    calculateSolution (head winningBoards) played

findAllWinners :: [Int] -> [Board] -> [([Board], [Int])]
findAllWinners set boards =
    findAllWinners' [] set boards []

findAllWinners' :: [Int] -> [Int] -> [Board] -> [([Board], [Int])] -> [([Board], [Int])]
findAllWinners' _ _ [] winners = winners
findAllWinners' _ [] _ winners = winners
findAllWinners' played (next:remaining) unsolved winners =
    findAllWinners' (next:played) remaining unsolved' winners'
    where
        unsolved' = findUnsolved (next:played) unsolved
        winners' = case (findSolutions (next:played) unsolved) of
            [] -> winners
            wins -> ((wins, (next:played)):winners)

findSolutions :: [Int] -> [Board] -> [Board]
findSolutions set boards = filter (boardSolved set) boards

findUnsolved :: [Int] -> [Board] -> [Board]
findUnsolved set boards = filter (not . (boardSolved set)) boards
