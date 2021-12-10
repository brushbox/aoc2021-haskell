module Day10
    ( part1
    , part2
    ) where

import Data.List

data ParseResult = 
    Good String String |
    Corrupt Char |
    Incomplete String |
    Unexpected Char
    deriving Show

part1 :: IO ()
part1 = do
    putStrLn "Day 10 part 1"
    contents <- readFile "day10.txt"
    -- contents <- readFile "day10_example.txt"
    let ls = lines contents
    let result = map (parseChunk "") ls
    let res' = filter isUnexpected result
    let res'' = map scoreChar res'
    putStrLn $ show $ sum res''

isUnexpected :: ParseResult -> Bool
isUnexpected (Unexpected _) = True
isUnexpected _ = False

scoreChar :: ParseResult -> Int
scoreChar (Unexpected ')') = 3
scoreChar (Unexpected ']') = 57
scoreChar (Unexpected '}') = 1197
scoreChar (Unexpected '>') = 25137
scoreChar _ = error "We can only score Unexpected Chars"

part2 :: IO ()
part2 = do
    putStrLn "Day 10 part 2"
    -- contents <- readFile "day10_example.txt"
    contents <- readFile "day10.txt"
    let ls = lines contents
    let result = map (parseLine) ls
    putStrLn $ show result
    let res' = filter isIncomplete result
    let stacks = map getStack res'
    let scores = sort $ map scoreStack stacks
    putStrLn $ show scores
    let middle = scores !! ((length scores) `div` 2)
    putStrLn $ show middle

isIncomplete :: ParseResult -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _ = False

getStack :: ParseResult -> String
getStack (Incomplete stack) = stack
getStack _ = error "Can only get the stacks from incomplete results"

scoreStack :: String -> Int
scoreStack s = 
    foldl (\sum score -> sum * 5 + score) 0 scores
    where
        scores = map scoreStackChar s

scoreStackChar :: Char -> Int
scoreStackChar '(' = 1
scoreStackChar '[' = 2
scoreStackChar '{' = 3
scoreStackChar '<' = 4

parseLine :: String -> ParseResult
parseLine line =
    case (parseChunk "" line) of
        Good "" "" -> Good "" ""
        Good "" s -> parseLine s
        result -> result

{-
parsing a chunk is going to return a few possible results:
* the chunk was Incomplete (ran out of input and hadn't found the matching closing char)
* the chunk was Corrupt Char (found the wrong sort of match)
* an unknown char was encountered - Unexpected Char (this should never happen for the input)
* the chunk was Good String (the string contains the remainder of the input after the chunk was consumed)
-}
parseChunk :: String -> String -> ParseResult
parseChunk stack [] = Good stack []
parseChunk stack (c:s) =
    case c of
        '(' -> untilCloseParen ('(':stack) s
        '[' -> untilCloseSquare ('[':stack) s
        '{' -> untilCloseBrace ('{':stack) s
        '<' -> untilCloseAngle ('<':stack) s
        otherwise -> Unexpected c

untilClose :: Char -> String -> String -> ParseResult
untilClose close stack [] = Incomplete stack
untilClose close stack s@(c:s') = 
    if c == close then
        Good (tail stack) s'
    else
        case (parseChunk stack s) of
            (Good stack' s') -> untilClose close stack' s'
            result -> result

untilCloseParen :: String -> String -> ParseResult
untilCloseParen = untilClose ')'

untilCloseSquare :: String -> String -> ParseResult
untilCloseSquare = untilClose ']'

untilCloseBrace :: String -> String -> ParseResult
untilCloseBrace = untilClose '}'

untilCloseAngle :: String -> String -> ParseResult
untilCloseAngle = untilClose '>'
