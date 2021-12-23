{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day18
  (part1,
  part2)
  where

import Data.Char
import Data.Maybe

data SNumber = Number Int | Pair Int SNumber SNumber
  deriving Eq

instance Show SNumber where
  show (Number n) = show n
  -- show (Pair d l r) = "[d:" ++ show d ++ "," ++ show l ++ "," ++ show r ++ "]"
  show (Pair d l r) = "[" ++ show l ++ "," ++ show r ++ "]"

data Crumb = LeftCrumb Int SNumber | RightCrumb Int SNumber deriving (Show)
type Breadcrumbs = [Crumb]
type Zipper = (SNumber, Breadcrumbs)

example = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]\n[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]\n[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]\n[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]\n[7,[5,[[3,8],[1,4]]]]\n[[2,[2,2]],[8,[8,1]]]\n[2,9]\n[1,[[[9,3],9],[[9,0],[0,7]]]]\n[[[5,[7,4]],7],1]\n[[[[4,2],2],6],[8,7]]"
homework = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n[[[5,[2,8]],4],[5,[[9,9],0]]]\n[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n[[[[5,4],[7,7]],8],[[8,3],8]]\n[[9,3],[[9,9],[6,[4,9]]]]\n[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

part1 :: IO ()
part1 = do
    print "Day 18 part 1"
    input <- readFile "day18.txt"
    print $ magnitude $ doHomework input

part2 :: IO ()
part2 = do
    print "Day 18 part 2"
    input <- readFile "day18.txt"
    let numbers = parseInput input
    let result = maximum (addAllPairs numbers)
    print result

parseInput :: String -> [SNumber]
parseInput input = map (fst . parseSNumber 1) $ lines input

parseSNumber :: Int -> String -> (SNumber, String)
parseSNumber depth ('[':numString) = (Pair depth left right, drop 1 remainder') -- take the trailing ] 
  where
    (left, remainder) = parseSNumber (depth + 1) numString
    (right, remainder') = parseSNumber (depth + 1) $ drop 1 remainder -- take the comma
parseSNumber _ (d:numString) = (Number $ digitToInt d, numString)
parseSNumber _ x = error $ "What do I do with" ++ x

addAllPairs :: [SNumber] -> [Int]
addAllPairs ns =
  concatMap (\(a, b) -> [magnitude $ snailAdd a b, magnitude $ snailAdd b a]) [(a, b) | a <- ns, b <- ns, a /= b]

addStrings :: String -> String -> String
addStrings sn1 sn2 = show (snailAdd n1 n2)
  where
    (n1, _) = parseSNumber 1 sn1
    (n2, _) = parseSNumber 1 sn2

reduceString :: String -> String
reduceString sn = show (snailReduce n)
  where
    (n, _) = parseSNumber 1 sn

doHomework :: String -> SNumber
doHomework input = foldl snailAdd first rest
  where
    (first:rest) = parseInput input

magnitude :: SNumber -> Int
magnitude (Number n) = n
magnitude (Pair _ l r) = 3 * magnitude l + 2 * magnitude r

snailAdd :: SNumber -> SNumber -> SNumber
snailAdd left right = snailReduce $ Pair 1 (deepen left) (deepen right)

snailReduce :: SNumber -> SNumber
snailReduce sNumber = snailReduce' (zipper sNumber)
  where
    snailReduce' :: Zipper -> SNumber
    snailReduce' z = case explode z of
      Nothing ->  case split z of
        Nothing -> rootNumber z
        Just z' -> snailReduce' z'
      Just z' -> snailReduce' z'

rootNumber :: Zipper -> SNumber
rootNumber z = fst . fromJust $ goRoot z

deepen :: SNumber -> SNumber
deepen n@(Number _) = n
deepen (Pair d l r) = Pair (d + 1) (deepen l) (deepen r)

zipper :: SNumber -> Zipper
zipper pair = (pair, [])

goLeft :: Zipper -> Maybe Zipper
goLeft (Number _, _) = Nothing
goLeft (Pair d l r, crumbs) = Just (l, LeftCrumb d r:crumbs)

goRight :: Zipper -> Maybe Zipper
goRight (Number _, _) = Nothing
goRight (Pair d l r, crumbs) = Just (r, RightCrumb d l:crumbs)

goUp :: Zipper -> Maybe Zipper
goUp (_, []) = Nothing
goUp (l, LeftCrumb d r:crumbs) = Just (Pair d l r, crumbs)
goUp (r, RightCrumb d l:crumbs) = Just (Pair d l r, crumbs)

modify :: (SNumber -> SNumber) -> Zipper -> Maybe Zipper
modify f (snum, bs) = Just (f snum, bs)

goRoot :: Zipper -> Maybe Zipper
goRoot z@(_, []) = Just z
goRoot z = goUp z >>= goRoot

numberToLeftOf :: Zipper -> Maybe Zipper
numberToLeftOf zipper = Just zipper >>= findLeftFork >>= goUp >>= goLeft >>= rightmostNumber

numberToRightOf :: Zipper -> Maybe Zipper
numberToRightOf zipper = Just zipper >>= findRightFork >>= goUp >>= goRight >>= leftmostNumber

findLeftFork :: Zipper -> Maybe Zipper
findLeftFork (_, []) = Nothing
findLeftFork z@(_, (LeftCrumb _ _):_) = findLeftFork parent
  where
    Just parent = goUp z
findLeftFork z = Just z

findRightFork :: Zipper -> Maybe Zipper
findRightFork (_, []) = Nothing
findRightFork z@(_, (RightCrumb _ _):_) = findRightFork parent
  where
    Just parent = goUp z
findRightFork z = Just z

rightmostNumber :: Zipper -> Maybe Zipper
rightmostNumber z@(Number _, _) = Just z
rightmostNumber z = goRight z >>= rightmostNumber

leftmostNumber :: Zipper -> Maybe Zipper
leftmostNumber z@(Number _, _) = Just z
leftmostNumber z = goLeft z >>= leftmostNumber

findSplitter :: Zipper -> Maybe Zipper
findSplitter z@(Number x, _) | x >= 10 = Just z
                             | otherwise = Nothing
findSplitter z = maybeGoRight (goLeft z >>= findSplitter)
  where
    maybeGoRight Nothing = goRight z >>= findSplitter
    maybeGoRight mz = mz

findExploder :: Zipper -> Maybe Zipper
findExploder (Number _, _) = Nothing
findExploder z@(Pair 5 _ _, _) = Just z
findExploder z@(Pair {}, _) = maybeGoRight (goLeft z >>= findExploder)
  where
    maybeGoRight Nothing = goRight z >>= findExploder
    maybeGoRight mz = mz

explode :: Zipper -> Maybe Zipper
explode z =
  case zexploder of
    (Just z) -> addLeft zexploder >>= addRight >>= explodePair >>= goRoot
    _ -> Nothing
  where
    zexploder = goRoot z >>= findExploder
    (Just (Pair _ (Number leftVal) (Number rightVal), _)) = zexploder

    addLeft :: Maybe Zipper -> Maybe Zipper
    addLeft Nothing = Just z
    addLeft (Just zipper) = (Just . fromMaybe zipper) $ numberToLeftOf zipper >>= modify (\(Number v) -> Number (v + leftVal))

    addRight :: Zipper -> Maybe Zipper
    addRight zipper = (Just . fromMaybe zipper) $ goRoot zipper >>= findExploder >>= numberToRightOf >>= modify (\(Number v) -> Number (v + rightVal))

    explodePair :: Zipper -> Maybe Zipper
    explodePair z = goRoot z >>= findExploder >>= modify (\_ -> Number 0)

split :: Zipper -> Maybe Zipper
split zipper = splitterZipper >>= modify splitNumber >>= goRoot
  where
    splitterZipper = goRoot zipper >>= findSplitter
    splitNumber (Number n) = Pair (parentDepth + 1) (Number (floor $ fromIntegral n / 2)) (Number (ceiling $ fromIntegral n / 2))
    parentDepth = case splitterZipper >>= goUp of
      (Just (Pair d _ _, _)) -> d
      _ -> 0

-- debugMsg :: String -> Zipper -> String
-- debugMsg s z = s ++ show (fst z) ++ " -- " ++ show (rootNumber z)
