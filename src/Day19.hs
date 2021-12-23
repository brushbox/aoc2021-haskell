module Day19 (
  part1,
  part2
)
where

import Data.List
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

type Point3 = (Int, Int, Int)
type Beacons = Set Point3
type Scanner = (Int, Beacons)

{-
We have sets of beacons - relative to a scanner.
The scanners can be in one of 24 rotations around the xyz axes.
Let's find the distances between each point in each Scanner's set.

When we compare one set with another we are looking for non-null intersection on the sets of distances.
-}
exScanner0 = lines "--- scanner 0 ---\n404,-588,-901\n528,-643,409\n-838,591,734\n390,-675,-793\n-537,-823,-458\n-485,-357,347\n-345,-311,381\n-661,-816,-575\n-876,649,763\n-618,-824,-621\n553,345,-567\n474,580,667\n-447,-329,318\n-584,868,-557\n544,-627,-890\n564,392,-477\n455,729,728\n-892,524,684\n-689,845,-530\n423,-701,434\n7,-33,-71\n630,319,-379\n443,580,662\n-789,900,-551\n459,-707,401"
exScanner1 = lines "--- scanner 1 ---\n686,422,578\n605,423,415\n515,917,-361\n-336,658,858\n95,138,22\n-476,619,847\n-340,-569,-846\n567,-361,727\n-460,603,-452\n669,-402,600\n729,430,532\n-500,-761,534\n-322,571,750\n-466,-666,-811\n-429,-592,574\n-355,545,-477\n703,-491,-529\n-328,-685,520\n413,935,-424\n-391,539,-444\n586,-435,557\n-364,-763,-893\n807,-499,-711\n755,-354,-619\n553,889,-390"

s0 = parseScanner exScanner0
s1 = parseScanner exScanner1

part1 :: IO ()
part1 = do
    print "Day 19 part 1"
    input <- readFile "day19.txt"
    let scanners = parseInput input
    print scanners

part2 :: IO ()
part2 = do
    print "Day 19 part 2"
    input <- readFile "day19.txt"
    print "yay"


parseInput :: String -> [Scanner]
parseInput input = parseScanners (lines input)


parseScanners :: [String] -> [Scanner]
parseScanners [] = []
parseScanners ls = parseScanner scannerLines:parseScanners rest'
  where
    rest' = if null rest then rest else tail rest
    (scannerLines, rest) = break null ls

parseScanner :: [String] -> Scanner
parseScanner [] = error "Cannot parse an empty scanner input"
parseScanner (idLine:points) = (scannerID idLine, parsePoints points)
  where
    scannerID idLine = read $ head $ splitOn " " $ drop 12 idLine
    parsePoints points = Set.fromList $ map parsePoint points
    parsePoint pointStr = (x, y, z)
      where
        [x, y, z] = map read $ splitOn "," pointStr

distances :: [Point3] -> [Int]
distances [] = []
distances (pt:pts) = distancesBetween pt pts ++ distances pts

distancesBetween :: Point3 -> [Point3] -> [Int]
distancesBetween pt = map (pointDistance pt)

-- manhattan or cartesian?
-- or magSquared even?
pointDistance :: Point3 -> Point3 -> Int
--pointDistance (x1,y1,z1) (x2,y2,z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)
pointDistance (x1,y1,z1) (x2,y2,z2) = dx*dx + dy*dy + dz*dz
  where
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2
