import Data.Char

-- Cipher
char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2char ((char2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> String -> String
encode n = map (shift n)

-- Frequency table
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers [] = 0
lowers (c : cs) = lowers cs + (if isLower c then 1 else 0)

count :: Char -> String -> Int
count _ [] = 0
count x (c : cs) = count x cs + (if x == c then 1 else 0)

freqs :: String -> [Float]
freqs cs = [percent (count x cs) m | x <- ['a' .. 'z']] where m = lowers cs

-- Cracking the cipher
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: (Eq a) => a -> [a] -> [Int]
positions y xs = [i | (x, i) <- zip xs [0 ..], x == y]

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs
