module Day16
    ( part1
    , part2
    ) where

import Data.Bits

input = "60556F980272DCE609BC01300042622C428BC200DC128C50FCC0159E9DB9AEA86003430BE5EFA8DB0AC401A4CA4E8A3400E6CFF7518F51A554100180956198529B6A700965634F96C0B99DCF4A13DF6D200DCE801A497FF5BE5FFD6B99DE2B11250034C00F5003900B1270024009D610031400E70020C0093002980652700298051310030C00F50028802B2200809C00F999EF39C79C8800849D398CE4027CCECBDA25A00D4040198D31920C8002170DA37C660009B26EFCA204FDF10E7A85E402304E0E60066A200F4638311C440198A11B635180233023A0094C6186630C44017E500345310FF0A65B0273982C929EEC0000264180390661FC403006E2EC1D86A600F43285504CC02A9D64931293779335983D300568035200042A29C55886200FC6A8B31CE647880323E0068E6E175E9B85D72525B743005646DA57C007CE6634C354CC698689BDBF1005F7231A0FE002F91067EF2E40167B17B503E666693FD9848803106252DFAD40E63D42020041648F24460400D8ECE007CBF26F92B0949B275C9402794338B329F88DC97D608028D9982BF802327D4A9FC10B803F33BD804E7B5DDAA4356014A646D1079E8467EF702A573FAF335EB74906CF5F2ACA00B43E8A460086002A3277BA74911C9531F613009A5CCE7D8248065000402B92D47F14B97C723B953C7B22392788A7CD62C1EC00D14CC23F1D94A3D100A1C200F42A8C51A00010A847176380002110EA31C713004A366006A0200C47483109C0010F8C10AE13C9CA9BDE59080325A0068A6B4CF333949EE635B495003273F76E000BCA47E2331A9DE5D698272F722200DDE801F098EDAC7131DB58E24F5C5D300627122456E58D4C01091C7A283E00ACD34CB20426500BA7F1EBDBBD209FAC75F579ACEB3E5D8FD2DD4E300565EBEDD32AD6008CCE3A492F98E15CC013C0086A5A12E7C46761DBB8CDDBD8BE656780"
example = "38006F45291200"

data Packet =  Literal Int Int Int
  | Operator Int Int [Packet]
  deriving (Show)

part1 :: IO ()
part1 = do
    putStrLn "Day 16 part 1"
    let binString = hexToBin input
    let (packet, _, _) = parsePacket binString
    putStrLn $ show (sumPacketVersion packet)

part2 :: IO ()
part2 = do
    putStrLn "Day 16 part 2"
    let binString = hexToBin input
    let (packet, _, _) = parsePacket binString
    putStrLn $ show (evalPacket packet)

hexToBin :: String -> String
hexToBin s = foldr (\hexit bits -> prepend (hexitToBits hexit) bits) "" s
  where
    prepend [a,b,c,d] bits = a:b:c:d:bits

hexitToBits :: Char -> String
hexitToBits '0' = "0000"
hexitToBits '1' = "0001"
hexitToBits '2' = "0010"
hexitToBits '3' = "0011"
hexitToBits '4' = "0100"
hexitToBits '5' = "0101"
hexitToBits '6' = "0110"
hexitToBits '7' = "0111"
hexitToBits '8' = "1000"
hexitToBits '9' = "1001"
hexitToBits 'A' = "1010"
hexitToBits 'B' = "1011"
hexitToBits 'C' = "1100"
hexitToBits 'D' = "1101"
hexitToBits 'E' = "1110"
hexitToBits 'F' = "1111"

bitsToInt :: Int -> String -> Int
bitsToInt = foldl (\i b -> (shift i 1) .|. (bitValue b))
  where
    bitValue '0' = 0
    bitValue '1' = 1

version :: String -> Int
version bitString = bitsToInt 0 $ take 3 bitString

-- packetTypeID 4 = Literal
-- 
parsePacket :: String -> (Packet, Int, String)
parsePacket bitString = 
  case packetTypeID of
    4 -> parseLiteral packetVersion packetTypeID (drop 6 bitString)
    otherwise -> parseOperator packetVersion packetTypeID (drop 6 bitString)
  where
    packetVersion = bitsToInt 0 $ take 3 bitString
    packetTypeID = bitsToInt 0 $ packetTypeBits
    packetTypeBits = take 3 $ drop 3 bitString

parseLiteral :: Int -> Int -> String -> (Packet, Int, String)
parseLiteral version packetType bitString =
  (Literal version packetType value, 6 + bitsUsed, remainder)
  where
    (value, bitsUsed, remainder) = parseVarLengthInt bitString

parseOperator :: Int -> Int -> String -> (Packet, Int, String)
parseOperator version packetType bitString
  | lengthTypeID == '0' = parseOperatorByBitLength version packetType subpacketBits (drop 16 bitString)
  | otherwise = parseOperatorByPacketCount version packetType subpacketCount (drop 12 bitString)
  where
    lengthTypeID = head bitString
    subpacketBits = bitsToInt 0 $ take 15 (tail bitString)
    subpacketCount = bitsToInt 0 $ take 11( tail bitString)

parseOperatorByBitLength :: Int -> Int -> Int -> String -> (Packet, Int, String)
parseOperatorByBitLength version packetType bitCount bitString = (Operator version packetType subpackets, 22 + bitCount, rest)
  where
    (subpackets, rest) = parseSubpacketsByBits bitString bitCount

parseSubpacketsByBits :: String -> Int -> ([Packet], String)
parseSubpacketsByBits bitString 0 = ([], bitString)
parseSubpacketsByBits bitString bitCount = (packet:subpackets, rest')
  where
    (packet, bits, rest) = parsePacket bitString
    (subpackets, rest') = parseSubpacketsByBits rest (bitCount - bits)

parseOperatorByPacketCount :: Int -> Int -> Int -> String -> (Packet, Int, String)
parseOperatorByPacketCount version packetType packetCount bitString = (Operator version packetType subpackets, 18 + bitCount, rest)
  where
    (subpackets, bitCount, rest) = parseSubpacketsByCount bitString packetCount

parseSubpacketsByCount :: String -> Int -> ([Packet], Int, String)
parseSubpacketsByCount bitString 0 = ([], 0, bitString)
parseSubpacketsByCount bitString packetCount = (packet:subpackets, bits + bits', rest')
  where
    (packet, bits, rest) = parsePacket bitString
    (subpackets, bits', rest') = parseSubpacketsByCount rest (packetCount - 1)

parseVarLengthInt :: String -> (Int, Int, String)
parseVarLengthInt bitString = chunkValue 0 chunk rest
  where
    chunk = take 5 bitString
    rest = drop 5 bitString

chunkValue :: Int -> String -> String -> (Int, Int, String)
chunkValue valSoFar (c:bits) rest | c == '1' = (val, 5 + bitCount, remainder) -- chunkValue (bitsToInt valSoFar bits) nextChunk nextRest
                                  | c == '0' = (bitsToInt valSoFar bits, 5, rest)
  where
    (val, bitCount, remainder) = chunkValue (bitsToInt valSoFar bits) nextChunk nextRest
    nextChunk = take 5 rest
    nextRest = drop 5 rest

sumPacketVersion :: Packet -> Int
sumPacketVersion (Literal v _ _) = v
sumPacketVersion (Operator v _ subpackets) = v + sumSubpackets
  where sumSubpackets = foldl (\s p -> s + (sumPacketVersion p)) 0 subpackets

evalPacket :: Packet -> Int
evalPacket (Literal _ _ value) = value
evalPacket (Operator _ typeID subpackets) 
  | typeID == 0 = sum evaluated
  | typeID == 1 = product evaluated
  | typeID == 2 = minimum evaluated
  | typeID == 3 = maximum evaluated
  | typeID == 5 = greaterThan
  | typeID == 6 = lessThan 
  | typeID == 7 = equalTo
  | otherwise = error "Unexpected operator packet type"
  where
    greaterThan | first > second = 1
                | otherwise = 0
    lessThan | first < second = 1
             | otherwise = 0
    equalTo | first == second = 1
            | otherwise = 0
    evaluated = map evalPacket subpackets
    [first, second] = evaluated
