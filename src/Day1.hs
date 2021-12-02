module Day1 
    ( part1
    , part2
    )
    where

-- Ruby: input.split("\n").map(&:to_i).each_cons(2).select { |(a, b)| a < b }.count
part1 :: IO ()
part1 = do
    numbers <- ints
    putStrLn . show . length . increasing $ pairs numbers

-- Ruby: input.split("\n").map(&:to_i).each_cons(3).map(&:sum).each_cons(2).select { |(a, b)| a < b }.count
part2 :: IO ()
part2 = do
    numbers <- ints
    putStrLn . show . length . increasing . pairs $ sumTriples numbers

pairs :: [Int] -> [(Int, Int)]
pairs ns = zip ns (drop 1 ns)

increasing :: [(Int, Int)] -> [(Int, Int)]
-- increasing = filter $ (\(a, b) -> a < b)
increasing = filter $ uncurry (<)


sumTriples :: [Int] -> [Int]
sumTriples nums = zipWith3 (\a b c -> a + b + c) nums (drop 1 nums) (drop 2 nums)

ints :: IO [Int]
ints = do
    input <- lines <$> readFile "day1.txt"
    return $ numbers input

numbers :: [String] -> [Int]
numbers = map read
