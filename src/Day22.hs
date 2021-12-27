module Day22 (
  part1,
  part2
)
where

import Data.List.Split
  
-- on x=-3..43,y=-38..14,z=-10..36
-- off x=-13..1,y=9..26,z=18..33
-- on x=-5..43,y=-2..49,z=-41..9

data Action = On | Off
  deriving Show

type Range = (Int, Int)
data Prism = Prism Range Range Range
  deriving Show
data Command = Command Action Prism
  deriving Show
type Reactor = [Prism]

part1 :: IO ()
part1 = do
  print "Day22 part1"
  input <- readFile "day22_example.txt"
  -- input <- readFile "day22.txt"
  let commands = parseCommands input
  print commands
  
parseCommands :: String -> [Command]
parseCommands input = map parseCommand $ lines input

parseCommand :: String -> Command
parseCommand cmdString = Command action prism
  where
    action = parseAction actionString
    prism = parsePrism prismString
    [actionString, prismString] = splitOn " " cmdString

parseAction :: String -> Action
parseAction "on" = On
parseAction "off" = Off
parseAction _ = error "Unexpected action"

-- on x=-5..43,y=-2..49,z=-41..9
parsePrism :: String -> Prism
parsePrism prismString = Prism xRange yRange zRange
  where
    [xRange, yRange,zRange] = map parseRange $ splitOn "," prismString

parseRange :: String -> Range
parseRange rangeString = (minValue, maxValue)
  where
    [minValue, maxValue] = map read $ splitOn ".." rangePart
    [_, rangePart] = splitOn "=" rangeString

prismsIntersect :: Prism -> Prism -> Bool 
prismsIntersect (Prism (x1s, x1e) (y1s, y1e) (z1s, z1e)) (Prism (x2s, x2e) (y2s, y2e) (z2s, z2e)) =
  x1s <= x2e && x1e >= x2s &&
  y1s <= y2e && y1e >= y2s &&
  z1s <= z2e && z1e >= z2s

intersection :: Prism -> Prism -> Maybe Prism
intersection p1@(Prism (x1s, x1e) (y1s, y1e) (z1s, z1e)) p2@(Prism (x2s, x2e) (y2s, y2e) (z2s, z2e))
  | prismsIntersect p1 p2 = Just $ Prism (max x1s x2s, min x1e x2e) (max y1s y2s, min y1e y2e) (max z1s z2s, min z1e z2e)
  | otherwise = Nothing

-- small prism must be a complete subset of the big one
splitPrism :: Prism -> Prism -> [Prism]
splitPrism big@(Prism (bxs, bxe) (bys, bye) (bzs, bze)) small@(Prism (sxs, sxe) (sys, sye) (szs, sze)) = 
  splits
  -- filter validPrism splits
  where
    splits = 
      [
        Prism (bxs,     sxs - 1) (bys,     bye)     (bzs,     bze),
        Prism (sxe + 1, bxe)     (bys,     bye)     (bzs,     bze),
        Prism (sxs,     sxe)     (bys,     sys + 1) (bzs,     bze),
        Prism (sxs,     sxe)     (sys,     sye)     (sze + 1, bze),
        Prism (sxs,     sxe)     (sye - 1, bye)     (bzs,     bze),
        Prism (sxs,     sxe)     (sys,     sye)     (bzs,     szs - 1)
      ]

validPrism :: Prism -> Bool
validPrism (Prism (x1, x2) (y1, y2) (z1, z2)) = x1 <= x2 && y1 <= y2 && z1 <= z2

cubes :: Prism -> Int
cubes (Prism (x1, x2) (y1, y2) (z1, z2)) = (z2 - z1 + 1) * (y2 - y1 + 1) * (x2 - x1 + 1)
    
part2 :: IO ()
part2 = do
  print "Day22 part2"
