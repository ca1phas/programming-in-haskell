import Data.Char (chr, ord)

type Bit = Int

bit2int :: [Bit] -> Int
bit2int = foldr (\x y -> x + 2 * y) 0

int2bit :: Int -> [Bit]
int2bit n = n `mod` 2 : int2bit (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bs = take 8 (bs ++ repeat 0)

encode :: String -> [Bit]
encode = concatMap (make8 . int2bit . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bs = take 8 bs : chop8 (drop 8 bs)

decode :: [Bit] -> String
decode = map (chr . bit2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-- CHAPTER 7
-- Q7
oddOnes :: [Bit] -> Bool
oddOnes = odd . sum

parityEncode :: String -> [Bit]
parityEncode cs
  | oddOnes bs = 1 : bs
  | otherwise = 0 : bs
  where
    bs = encode cs

parityDecode :: [Bit] -> String
parityDecode (b : bs)
  | b == 0 && not (oddOnes bs) = decode bs
  | b == 1 && oddOnes bs = decode bs
  | otherwise = error "Parity error"

-- Q8
faultyChannel :: [Bit] -> [Bit]
faultyChannel = tail

faultyTransmit :: String -> String
faultyTransmit = parityDecode . faultyChannel . parityEncode