    {-# LANGUAGE BinaryLiterals #-}
    
    module Day3 
        ( part1
        , part2
    )
    where

import Data.Bits

{-
111110110111 

gamma rate - most common bits
epsilon rate - least common bits 
if we sum up the 1s and 0s for each bit we can use the count to know whether 1s or 0s are more common.
if sum > count / 2 -> 1s are more common, otherwise 0s.

We have 12 bits, so we will have 12 sums and 1 count to use for each.
Working from left to right our offsets will be the inverse of the powers.
But I don't think we need the powers.
When we finish we can fold over the sums, shifting the accumulator left each time.
-}
part1 :: IO ()
part1 = do
    lines <- lines <$> readFile "day3.txt"
    let count = length lines
    let sums = foldl sumBits zeroSum lines
    let gamma = calcGamma sums count
    let epsilon = gamma `xor` mask
    putStrLn (show $ gamma * epsilon)

zeroSum :: [Int]
zeroSum = take 12 [0, 0..]

mask = 0b111111111111
startBit = 0b100000000000

sumBits :: [Int] -> String -> [Int]
sumBits sums bitString =
    map sumBits $ zip sums (map charToBit bitString)
    where
        sumBits (a, b) = a + b

calcGamma :: [Int] -> Int -> Int
calcGamma sums count =
    foldl mostCommonShift 0 sums
    where
        mostCommonShift num bitSum =
            (num `shiftL` 1) .|. (mostCommon bitSum)
        mostCommon bitSum = 
            if bitSum > (count `div` 2)
            then
                1
            else
                0

charToBit :: Char -> Int
charToBit '1' = 1
charToBit '0' = 0

data Bit = On | Off
    deriving Show

part2 :: IO ()
part2 = do
    putStrLn "Part2"
    lines <- lines <$> readFile "day3.txt"
    -- lines <- lines <$> readFile "day3_example.txt"
    let nums = map fromBinary lines
    let oxygen = oxygenRating startBit nums
    let co2 = co2Rating startBit nums
    -- let oxygen = oxygenRating 16 nums
    -- let co2 = co2Rating 16 nums
    putStrLn $ show oxygen
    putStrLn $ show co2
    putStrLn $ show (oxygen * co2)
    -- let mcv = mostCommonValue 16 nums
    -- let nums' = filterOnBit mcv 16 nums
    -- putStrLn $ show mcv
    -- putStrLn $ show nums'

    -- let mcv' = mostCommonValue 8 nums'
    -- let nums'' = filterOnBit mcv' 8 nums'
    -- putStrLn $ show mcv'
    -- putStrLn $ show nums''

    -- let mcv'' = mostCommonValue 4 nums''
    -- let nums''' = filterOnBit mcv'' 4 nums''
    -- putStrLn $ show mcv''
    -- putStrLn $ show nums'''

    -- let mcv3 = mostCommonValue 2 nums'''
    -- let nums4 = filterOnBit mcv3 2 nums'''
    -- putStrLn $ show mcv3
    -- putStrLn $ show nums4

    -- let mcv4 = mostCommonValue 2 nums4
    -- let nums5 = filterOnBit mcv4 2 nums4
    -- putStrLn $ show mcv4
    -- putStrLn $ show nums5

    -- let mcv5 = mostCommonValue 1 nums5
    -- let nums6 = filterOnBit mcv5 1 nums5
    -- putStrLn $ show mcv5
    -- putStrLn $ show nums6

    -- putStrLn $ show $ oxygenRating 16 nums


    -- putStrLn "\n\n########################\n"
    -- let lcv = leastCommonValue 16 nums
    -- let nums' = filterOnBit lcv 16 nums
    -- putStrLn $ show lcv
    -- putStrLn $ show nums'

    -- let lcv' = leastCommonValue 8 nums'
    -- let nums'' = filterOnBit lcv' 8 nums'
    -- putStrLn $ show lcv'
    -- putStrLn $ show nums''

    -- let lcv'' = leastCommonValue 4 nums''
    -- let nums''' = filterOnBit lcv'' 4 nums''
    -- putStrLn $ show lcv''
    -- putStrLn $ show nums'''

    -- let lcv3 = leastCommonValue 2 nums'''
    -- let nums4 = filterOnBit lcv3 2 nums'''
    -- putStrLn $ show lcv3
    -- putStrLn $ show nums4

    -- let lcv4 = leastCommonValue 2 nums4
    -- let nums5 = filterOnBit lcv4 2 nums4
    -- putStrLn $ show lcv4
    -- putStrLn $ show nums5

    -- let lcv5 = leastCommonValue 1 nums5
    -- let nums6 = filterOnBit lcv5 1 nums5
    -- putStrLn $ show lcv5
    -- putStrLn $ show nums6

    -- putStrLn $ show $ co2Rating 16 nums
    
    -- putStrLn $ show (23 * 15)

oxygenRating :: Int -> [Int] -> Int
oxygenRating _ [rating] = rating
oxygenRating bitMask nums =
    oxygenRating (bitMask `shiftR` 1) filteredNums
    where
        filteredNums = filterOnBit mcvBit bitMask nums
        mcvBit = mostCommonValue bitMask nums

co2Rating :: Int -> [Int] -> Int
co2Rating _ [rating] = rating
co2Rating bitMask nums =
    co2Rating (bitMask `shiftR` 1) filteredNums
    where
        filteredNums = filterOnBit lcvBit bitMask nums
        lcvBit = leastCommonValue bitMask nums

filterOnBit :: Bit -> Int -> [Int] -> [Int]
filterOnBit On bitMask = filter (bitMatches bitMask)
filterOnBit Off bitMask = filter (bitDiffers bitMask)

bitMatches :: Int -> Int -> Bool
bitMatches bit num = num .&. bit /= 0

bitDiffers :: Int -> Int -> Bool
bitDiffers bit num = not (bitMatches bit num)

fromBinary :: String -> Int
fromBinary = foldl nextBit 0 
    where
        nextBit val char = (val `shiftL` 1) .|. charToBit char

mostCommonValue :: Int -> [Int] -> Bit
mostCommonValue bit nums =
    if matches * 2 >= count
        then On
        else Off
    where
        count = length nums
        matches = length $ filter (bitMatches bit) nums

leastCommonValue :: Int -> [Int] -> Bit
leastCommonValue bit nums =
    if matches * 2 < count
        then On
        else Off
    where
        count = length nums
        matches = length $ filter (bitMatches bit) nums