-- CHAPTER 4
-- Q8: Luhn algorithm
luhnSubtract :: Int -> Int
luhnSubtract n
  | n > 9 = n - 9
  | otherwise = n

luhnDouble :: Int -> Int
luhnDouble n = luhnSubtract (n * 2)

luhn4 :: Int -> Int -> Int -> Int -> Bool
luhn4 a b c d = (a' + b' + c' + d') `mod` 10 == 0
  where
    a' = luhnDouble a
    b' = luhnSubtract b
    c' = luhnDouble c
    d' = luhnSubtract d

-- CHAPTER 7
-- Q10
altMap' :: (a -> b) -> (a -> b) -> Bool -> [a] -> [b]
altMap' _ _ _ [] = []
altMap' f g True (a : as) = f a : altMap' f g False as
altMap' f g False (a : as) = g a : altMap' f g True as

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = altMap' f g True

intToRList :: Int -> [Int]
intToRList n
  | n < 10 = [n]
  | otherwise = n `mod` 10 : intToRList (n `div` 10)

intToList :: Int -> [Int]
intToList n = reverse $ intToRList n

luhn :: Int -> Bool
luhn xs = sum (altMap luhnDouble luhnSubtract $ intToList xs) `mod` 10 == 0