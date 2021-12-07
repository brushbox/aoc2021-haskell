module Day6
    ( part1
    , part2
    ) where

import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as Map

type Day = Int
type FishCount = Int
type FishMap = Map.Map Day FishCount

part1 :: IO ()
part1 = do
    putStrLn "Day 6 part 1"
    -- let contents = "3,4,3,1,2"
    contents <- readFile "day6.txt"
    let fish = map read (splitOn "," contents)
    putStrLn $ show $ goFish 80 fish

part2 :: IO ()
part2 = do
    putStrLn "Day 6 part 2"
    -- let contents = "3,4,3,1,2"
    contents <- readFile "day6.txt"
    let fish = map read (splitOn "," contents)
    putStrLn $ show $ goFish 256 fish

goFish :: Int -> [Day] -> Int
goFish days fish =
    countFish mp' days
    where
        mp = buildMap fish
        mp' = foldl spawn mp [0..(days - 1)]

-- we have a list where each entry is a fish represented by what day in the 
-- spawning cycle it is up to
buildMap :: [Day] -> FishMap
buildMap fish = foldl (\mp day -> Map.insertWith (+) day 1 mp) Map.empty fish

spawn :: FishMap -> Day -> FishMap
spawn mp day =
    mp'' 
    where
        fishCount = Map.findWithDefault 0 day mp
        mp' = Map.insertWith (+) (day + 7) fishCount mp
        mp'' = Map.insertWith (+) (day + 9) fishCount mp'

-- countFish :: FishMap -> Day -> FishCount
-- countFish mp day =
--     Map.foldlWithKey sum 0 mp
--     where
--         sum s k v
--             | (k - day) < 0 = s
--             | otherwise = s + v

countFish :: FishMap -> Day -> FishCount
countFish mp day =
    foldl (\s ix -> (lookup ix) + s) 0 daysLeft
    where
        lookup ix = Map.findWithDefault 0 ix mp
        daysLeft = dropWhile (< day) (Map.keys mp)

showMap :: FishMap -> Day -> String
showMap mp day =
    Map.foldlWithKey present "" mp
    where
        present str k v 
            | (k - day) < 0 = str
            | v == 0 = str
            | otherwise = str ++ (dayInfo (k - day) v)
        dayInfo k v = (show k) ++ ":" ++ (show v) ++ ", "
