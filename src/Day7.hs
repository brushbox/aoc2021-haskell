module Day7 
    ( part1
    , part2
    ) where

import Data.List
import Data.List.Split

-- Ruby solution for part 1
-- is = File.read("day7.txt").split(",").map(&:to_i)
-- def med(array)
--     s = array.size
--     a = array.sort
--     if s.odd?
--         a[s/2-1]
--     else
--         (a[s/2-1] + a[s/2]) / 2
--     end
-- end
-- median = med(is)
-- is.sort.map { |i| (i - median).abs }.sum

part1 :: IO ()
part1 = do
    putStrLn "Day 7 part 1"
    contents <- readFile "day7.txt"
    let vals = map read $ splitOn "," contents
    let med = median vals
    let result = foldl (\s v -> s + (abs (med - v))) 0 vals
    putStrLn $ show result

median :: [Int] -> Int
median vals
    | (odd count) = sorted !! middle
    | otherwise = avgMiddle
    where
        count = length vals
        sorted = sort vals
        middle = count `div` 2 - 1
        avgMiddle = ((sorted !! middle) + (sorted !! (middle + 1))) `div` 2

part2 :: IO ()
part2 = do
    putStrLn "Day 7 part 2"
    contents <- readFile "day7.txt"
    let vals = map read $ splitOn "," contents
    let avg = (sum vals) `div` (length vals)
    let result = foldl (\s v -> s + (cost (avg - v))) 0 vals
    putStrLn $ show result

cost :: Int -> Int
cost v = 
    n * (n + 1) `div` 2
    where
        n = abs v