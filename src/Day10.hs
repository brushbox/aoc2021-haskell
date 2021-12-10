module Day10
    ( part1
    , part2
    ) where

import Data.List
import Data.Either

type ParseResult = Either Char String

part1 :: IO ()
part1 = do
    putStrLn "Day 10 part 1"
    contents <- readFile "day10.txt"
    -- contents <- readFile "day10_example.txt"
    let ls = lines contents
    let (result, _) = partitionEithers $ map (parseChunk "") ls
    let answer = sum $ map scoreChar result
    putStrLn $ show $ answer

scoreChar :: Char -> Int
scoreChar ')' = 3
scoreChar ']' = 57
scoreChar '}' = 1197
scoreChar '>' = 25137

part2 :: IO ()
part2 = do
    putStrLn "Day 10 part 2"
    -- contents <- readFile "day10_example.txt"
    contents <- readFile "day10.txt"
    let ls = lines contents
    let (_, stacks) = partitionEithers $ map (parseChunk "") ls
    let scores = sort $ map scoreStack stacks
    let middle = scores !! ((length scores) `div` 2)
    putStrLn $ show middle

getStack :: ParseResult -> String
getStack (Right stack) = stack
getStack _ = error "Can only get the stacks from Good results"

scoreStack :: String -> Int
scoreStack s = 
    foldl (\sum score -> sum * 5 + score) 0 scores
    where
        scores = map scoreStackChar s

inverse :: Char -> Char
inverse '(' = ')'
inverse '[' = ']'
inverse '{' = '}'
inverse '<' = '>'

scoreStackChar :: Char -> Int
scoreStackChar ')' = 1
scoreStackChar ']' = 2
scoreStackChar '}' = 3
scoreStackChar '>' = 4

parseChunk :: String -> String -> ParseResult
parseChunk stack [] = Right stack
parseChunk stack (c:s) | isOpen c = parseChunk (inverse c:stack) s
                       | c == head stack = parseChunk (tail stack) s
                       | otherwise = Left c

isOpen :: Char -> Bool
isOpen = (`elem` "([{<")
