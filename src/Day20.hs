module Day20 (
  part1,
  part2
)
where

import Data.Bits
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)
type Transform = String
type Image = Map Point Char
type InfiniteImage = (Char, Image) -- the Char is for the state of points outside the mapped Image - either '.' or '#'

sample = readFile "day20_example.txt"

part1 :: IO ()
part1 = do
    print "Day 20 part 1"
    input <- readFile "day20.txt"
    let (tx, img) = parseInput input
    -- let inf' = enhanceImage tx ('.', img)
    -- let (_, img') = enhanceImage tx inf'
    let (_, img') = enhanceNTimes 2 tx ('.', img)
    let answer = sum $ bitwise img'
    print answer

part2 :: IO ()
part2 = do
    print "Day 20 part 2"
    input <- readFile "day20.txt"
    let (tx, img) = parseInput input
    let (_, img') = enhanceNTimes 50 tx ('.', img)
    let answer = sum $ bitwise img'
    print answer

enhanceNTimes :: Int -> Transform -> InfiniteImage -> InfiniteImage
enhanceNTimes 0 _ iimg = iimg
enhanceNTimes n tx iimg = enhanceNTimes (n - 1) tx (enhanceImage tx iimg)

parseInput :: String -> (Transform, Image)
parseInput input = (transform, image)
  where
    transform = head ls
    image = parseImage $ drop 2 ls
    ls = lines input

parseImage :: [String] -> Image
parseImage ls = Map.fromList $ concatMap parseRow (zip ls [0..])
  where
    parseRow (row, y) = [((x, y), c) | (c, x) <- zip row [0..]]

enhancementMatrix :: Point -> InfiniteImage -> [[Char]]
enhancementMatrix (x,y) (inf, image) = map row [-1,0,1]
  where
    row dy = map (lookup dy) [-1,0,1]
    lookup dy dx =  fromMaybe inf $ image Map.!? (x+dx, y+dy)

enhancementIndex :: [[Char]] -> Int
enhancementIndex matrix = fromBinary $ concat matrix

fromBinary :: [Char] -> Int
fromBinary = fromBinary' '.' '#' 0

fromBinary' :: Char -> Char -> Int -> [Char] -> Int
fromBinary' _ _ n [] = n
fromBinary' zero one n (c:cs) | c == zero = fromBinary' zero one (n `shiftL` 1) cs
                              | otherwise = fromBinary' zero one (n `shiftL` 1 .|. 1) cs

enhanceImage :: Transform -> InfiniteImage -> InfiniteImage
enhanceImage tx infImage@(inf, image) = (inf', image')
  where
    inf' = tx !! fromBinary (replicate 9 inf)
    image' = Map.fromList $ concatMap txRow [(minY-1)..(maxY+1)]
    txRow y = map (enhance y) [(minX-1)..(maxX+1)]
    enhance :: Int -> Int -> (Point, Char)
    enhance y x = ((x,y), enhancePixel (x, y))
    enhancePixel pt = tx !! enhancementIndex (enhancementMatrix pt infImage)
    ((minX, maxX), (minY, maxY)) = imageBounds image

imageBounds :: Image -> ((Int, Int), (Int, Int))
imageBounds image = foldl (\((miX, maX), (miY, maY)) (x, y) -> ((min x miX, max x maX), (min y miY, max y maY))) ((maxBound, minBound), (maxBound, minBound)) (Map.keys image)


imgS :: Image -> String
imgS image = unlines $ map renderRow [minY..maxY]
  where
    ((minX, maxX), (minY, maxY)) = imageBounds image
    renderRow y = map (\x -> image Map.! (x, y)) [minX..maxX]

bitwise :: Image -> [Int]
bitwise img = map toBit $ Map.elems img
  where
    toBit '.' = 0
    toBit '#' = 1
    toBit _ = error "No such bit"
