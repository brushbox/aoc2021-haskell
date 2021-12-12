module Day11
    ( part1
    , part2
    ) where

import Debug.Trace
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char

type Point = (Int, Int)
type OctoMap = Map Point Int

sample = "11111\n19991\n19191\n19991\n11111"

part1 :: IO ()
part1 = do
    putStrLn "Day 11 part 1"
    -- ls <- lines <$> readFile "day11.txt"
    ls <- lines <$> readFile "day11_example.txt"
    -- let ls = lines sample
    -- let (m, (w, h)) = buildOctoMap ls
    let (m, size) = buildOctoMap ls
    let (_, flashCount) = applyStep m size 100
    -- let (m', flashes) = incrementMap m (w, h)
    -- let m'' = foldFlashes m' flashes
    -- putStrLn $ toString m (w, h)
    -- putStrLn $ show flashes
    -- putStrLn $ toString m'' (w, h)
    putStrLn $ show flashCount
    
part2 :: IO ()
part2 = do
    putStrLn "Day 11 part 2"
    -- ls <- lines <$> readFile "day11.txt"

charToInt :: Char -> Int
charToInt c = read (c:"")

buildOctoMap :: [String] -> (OctoMap, Point)
buildOctoMap ls =
    (octoMap, (width, height))
    where
        width = length $ head ls
        height = length ls
        octoMap = foldl (\m (y, l) -> buildOctoMapLine m y l) Map.empty (zip [0..] ls)
        buildOctoMapLine :: OctoMap -> Int -> String -> OctoMap
        buildOctoMapLine m y l =
            foldl (\m (x, c) -> Map.insert (x, y) (charToInt c) m) m (zip [0..] l)

applyStep :: OctoMap -> Point -> Int -> (OctoMap, Int)
applyStep m pt count =
    foldl (\(m', sum) _ -> stepAndSum m' pt sum) (m, 0) [1..count]

stepAndSum :: OctoMap -> Point -> Int -> (OctoMap, Int)
stepAndSum m pt sum =
    (trace (mapAndFlashString (m', sum + flashes) pt) (m', sum + flashes))
    where
        (m', flashes) = step m pt

step :: OctoMap -> Point -> (OctoMap, Int)
step m size =
    countFlashesAndReset flashedMap size
    where
        flashedMap = foldFlashes incdMap flashes
        (incdMap, flashes) = incrementMap m size

incrementMap :: OctoMap -> Point -> (OctoMap, [Point])
incrementMap m (width, height) = 
    foldl (\(m', flashes) pt -> incAndCollectFlashes flashes $ incrementAndFindFlash m' pt) (m, []) points
    where
        points = allPoints width height
        incAndCollectFlashes flashes (m', Nothing) = (m', flashes)
        incAndCollectFlashes flashes (m', Just flash) = (m', flash:flashes)

increment :: OctoMap -> Point -> (OctoMap, Int)
increment m pt =
    (Map.insert pt newVal m, newVal)
    where
        newVal :: Int
        newVal = (1 + (Map.findWithDefault (-1) pt m))

incrementAndFindFlash :: OctoMap -> Point -> (OctoMap, Maybe Point)
incrementAndFindFlash m pt =
    if val == 10 then
        (m', Just pt)
    else
        (m', Nothing)
    where
        (m', val) = increment m pt

incrementAndFlash :: OctoMap -> Point -> OctoMap
incrementAndFlash m pt =
    if val == 10 then
        flash m' pt
    else
        m'
    where
        (m', val) = increment m pt

foldFlashes :: OctoMap -> [Point] -> OctoMap
foldFlashes m flashes =
    foldl (\m pt -> flash m pt) m flashes

flash :: OctoMap -> Point -> OctoMap
flash m (x, y) =
    foldl (\m pt' -> incrementAndFlash m pt') m adjacents
    where
        adjacents = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx /= 0 || dy /= 0)]

mapAndFlashString :: (OctoMap, Int) -> Point -> String
mapAndFlashString (m, flashes) size = 
    (toString m size) ++ "\n" ++ (show flashes) ++ "\n"

toString :: OctoMap -> Point -> String
toString m (w, h) =
    unlines ls
    where
        ls = map (\y -> toLine y w m) [0..(h-1)]
        toLine :: Int -> Int -> OctoMap -> String
        toLine y w m = foldr (\x l -> (show $ Map.findWithDefault (-1) (x, y) m) ++ l) "" [0..(w-1)]

countFlashesAndReset :: OctoMap -> Point -> (OctoMap, Int)
countFlashesAndReset m (w, h) =
    foldl (\(m', count) pt -> (countAndReset m' pt count)) (m, 0) points
    where
        points = allPoints w h
        countAndReset :: OctoMap -> Point -> Int -> (OctoMap, Int)
        countAndReset m pt count =
            if (Map.findWithDefault 0 pt m) > 9 then
                (Map.insert pt 0 m, count + 1)
            else
                (m, count)

allPoints :: Int -> Int -> [Point]
allPoints width height = 
    [(x, y) | x <- [0..(width-1)], y <- [0..(height-1)]]
