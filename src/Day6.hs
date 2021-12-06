module Day6
    ( part1
    , part2
    ) where

-- import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map.Strict as Map

type FishMap = Map.Map Int Int

part1 :: IO ()
part1 = do
    putStrLn "Day 6 part 1"
    -- let fish = map read $ splitOn "," "3,4,3,1,2"
    contents <- readFile "day6.txt"
    let fish = map read (splitOn "," contents)
    goFish 80 fish

part2 :: IO ()
part2 = do
    putStrLn "Day 6 part 2"
    -- let fish = map read $ splitOn "," "3,4,3,1,2"
    contents <- readFile "day6.txt"
    let fish = map read (splitOn "," contents)
    goFish 256 fish

goFish :: Int -> [Int] -> IO ()
goFish gens fish = do
    let mp = buildMap fish
    let mp' = foldl spawn mp [0..(gens - 1)]
    -- putStrLn $ showMap mp' gens
    putStrLn $ show $ countFish mp' gens

buildMap :: [Int] -> FishMap
buildMap fish =
    foldl (\mp day -> Map.insertWith (+) day 1 mp) Map.empty fish

spawn :: FishMap -> Int -> FishMap
spawn mp day =
    mp'' 
    where
        dayCount = fromMaybe 0 $ Map.lookup day mp
        mp' = Map.insertWith (+) (day + 7) dayCount mp
        mp'' = Map.insertWith (+) (day + 9) dayCount mp'

countFish :: FishMap -> Int -> Int
countFish mp day =
    Map.foldlWithKey sum 0 mp
    where
        sum s k v
            | (k - day) < 0 = s
            | otherwise = s + v

showMap :: FishMap -> Int -> String
showMap mp day =
    -- Map.foldlWithKey (\str k v -> str ++ (dayInfo (k - day) v)) "" mp
    Map.foldlWithKey present "" mp
    where
        present str k v 
            | (k - day) < 0 = str
            | v == 0 = str
            | otherwise = str ++ (dayInfo (k - day) v)
        dayInfo k v = (show k) ++ ":" ++ (show v) ++ ", "
