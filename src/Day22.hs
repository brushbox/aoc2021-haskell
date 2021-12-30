module Day22 (
  part1,
  part2
)
where

import Data.List
import Data.List.Split

-- on x=-3..43,y=-38..14,z=-10..36
-- off x=-13..1,y=9..26,z=18..33
-- on x=-5..43,y=-2..49,z=-41..9

data Action = On | Off
  deriving Show

type Range = (Int, Int)
data Prism = Prism String Range Range Range
  deriving Show
data Command = Command Action Prism
  deriving Show
type Reactor = [Prism]

example = "on x=10..12,y=10..12,z=10..12\non x=11..13,y=11..13,z=11..13\noff x=9..11,y=9..11,z=9..11\non x=10..10,y=10..10,z=10..10"

part1 :: IO ()
part1 = do
  print "Day22 part1"
  -- let input = example
  -- input <- readFile "day22_example.txt"
  input <- readFile "day22.txt"
  let cubes = part1' input
  print cubes

reactorRegion :: Prism
reactorRegion = Prism "Reactor" (-50, 50) (-50, 50) (-50, 50)

part1' input = sum $ map cubes $ rebootReactor validCommands
  where
    commands = parseCommands input
    validCommands = filter (prismsIntersect reactorRegion . commandPrism) commands
    commandPrism (Command _ p) = p

{-
  Any leading offs can be be dropped (there aren't any anyway)
  The first on can be added to the set as-is.
  Any subsequent "on" needs to:
  * be put into a list of prisms `list = [prism]`
  * check for intersections and split if there are any
    * the splits replace the prism we tested.
    * the resulting list needs to be re-checked until no prims are split
  * the resulting list/set of prisms can be added to the set
  Any "off" needs to:
  * be put into a list of prisms `list = [prism]`
  * check for intersections and if there are any:
    * we need to split the prism that is already in the set
      * the splits will be added back into the set - except for the one that intersects (not returned by the split fn anyway)
    * the off prism needs to be checked against all prisms in the set
-}
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

parsePrism :: String -> Prism
parsePrism prismString = Prism "" xRange yRange zRange
  where
    [xRange, yRange,zRange] = map parseRange $ splitOn "," prismString

parseRange :: String -> Range
parseRange rangeString = (minValue, maxValue)
  where
    [minValue, maxValue] = map read $ splitOn ".." rangePart
    [_, rangePart] = splitOn "=" rangeString

rebootReactor :: [Command] -> Reactor
rebootReactor = rebootReactor' []

rebootReactor' :: Reactor -> [Command] -> Reactor
rebootReactor' = foldl runCommand

runCommand :: Reactor -> Command -> Reactor
runCommand r (Command On p) = addOnPrism r p
runCommand r (Command Off p) = subtractOffPrism r p

{-
Given a Reactor and a Prism to add.
- if the Reactor is empty - then Reactor' is [Prism]
- if there is no intersection - then Reactor' is prism:reactor
- if there is an intersection then we split the incoming block and add all the splits recursively.
-}
addOnPrism :: Reactor -> Prism -> Reactor
addOnPrism [] p = [p]
addOnPrism reactor prism = case find (prismsIntersect prism) reactor of
  Nothing -> prism:reactor
  Just reactorPrism -> foldl addOnPrism reactor (splitPrism prism (intersection reactorPrism prism))

{-
If the prism intersects with a prism in the reactor:
- split the prism in the reactor - add the splits to Reactor'
If the prism does not intersect with a particular prism in the Reactor
- add the reactor prism to the Reactor'
-}
subtractOffPrism :: Reactor -> Prism -> Reactor
subtractOffPrism reactor prism =
  foldl (\r rp -> r ++ turnOff rp prism) [] reactor
  where
    turnOff rp p | prismsIntersect rp p  = splitPrism rp (intersection rp p)
                 | otherwise = [rp]

prismsIntersect :: Prism -> Prism -> Bool
prismsIntersect (Prism _ (x1s, x1e) (y1s, y1e) (z1s, z1e)) (Prism _ (x2s, x2e) (y2s, y2e) (z2s, z2e)) =
  x1s <= x2e && x1e >= x2s &&
  y1s <= y2e && y1e >= y2s &&
  z1s <= z2e && z1e >= z2s

intersection :: Prism -> Prism -> Prism
intersection p1@(Prism _ (x1s, x1e) (y1s, y1e) (z1s, z1e)) p2@(Prism _ (x2s, x2e) (y2s, y2e) (z2s, z2e))
  | prismsIntersect p1 p2 = Prism "" (max x1s x2s, min x1e x2e) (max y1s y2s, min y1e y2e) (max z1s z2s, min z1e z2e)
  | otherwise = error "Prisms do not intersect!"

-- small prism must be a complete subset of the big one
splitPrism :: Prism -> Prism -> [Prism]
splitPrism big@(Prism _ (bsx, bex) (bsy, bey) (bsz, bez)) small@(Prism _ (ssx, sex) (ssy, sey) (ssz, sez)) =
  filter validPrism splits
  where
    splits =
      [
        Prism "A" (bsx,     ssx - 1) (bsy,     bey)     (bsz,     bez),
        Prism "B" (sex + 1, bex)     (bsy,     bey)     (bsz,     bez),
        Prism "C" (ssx,     sex)     (sey + 1, bey)     (bsz,     bez),
        Prism "D" (ssx,     sex)     (bsy,     ssy - 1) (bsz,     bez),
        Prism "E" (ssx,     sex)     (ssy,     sey)     (bsz,     ssz - 1),
        Prism "F" (ssx,     sex)     (ssy,     sey)     (sez + 1, bez)
      ]

validPrism :: Prism -> Bool
validPrism (Prism _ (x1, x2) (y1, y2) (z1, z2)) = x1 <= x2 && y1 <= y2 && z1 <= z2

cubes :: Prism -> Int
cubes (Prism _ (x1, x2) (y1, y2) (z1, z2)) = (z2 - z1 + 1) * (y2 - y1 + 1) * (x2 - x1 + 1)

part2 :: IO ()
part2 = do
  print "Day22 part2"
  input <- readFile "day22.txt"
  print $ part2' input

part2' input = sum $ map cubes $ rebootReactor commands
  where
    commands = parseCommands input