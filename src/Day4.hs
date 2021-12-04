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
    let cols = boardColumns (last boards)
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
boardColumns (Board [a, b, c, d, e]) =
    map (\(a', b', c', d', e') -> [a', b', c', d', e']) (zip5 a b c d e)

allContained :: [Int] -> [Int] -> Bool
allContained set candidates = all (\n -> n `elem` set) candidates

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
calculateSolution (Board rows) played@(last:_) =
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
