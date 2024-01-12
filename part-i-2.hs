-- CHAPTER 6
-- Q1
factorial :: Int -> Int
factorial 0 = 1
factorial n | n >= 0 = n * factorial (n - 1)

-- Q2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- Q3
(^) :: Int -> Int -> Int
_ ^ 0 = 1
a ^ b = a * (a Main.^ (b - 1))

-- Q4
euclid :: Int -> Int -> Int
euclid a 0 = a
euclid a b = euclid b (a `mod` b)

-- Q6
and :: [Bool] -> Bool
and = foldr (&&) True

concat :: [[a]] -> [a]
concat = foldr (++) []

replicate :: Int -> a -> [a]
replicate n a
  | n == 0 = []
  | n > 0 = a : Main.replicate (n - 1) a

(!!) :: [a] -> Int -> a
(x : _) !! 0 = x
(_ : xs) !! n = xs Main.!! (n - 1)

elem :: (Eq a) => a -> [a] -> Bool
elem _ [] = False
elem a (x : xs) = a == x || Main.elem a xs

-- Q7
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- Q8
msort' :: (Ord a) => Int -> [a] -> [a]
msort' 0 _ = []
msort' 1 xs = xs
msort' n xs = merge (msort' m as) (msort' (n - m) bs)
  where
    m = n `div` 2
    (as, bs) = (take m xs, drop (n - m) xs)

msort :: (Ord a) => [a] -> [a]
msort xs = msort' (length xs) xs

-- Q9
sum' :: (Num a) => [a] -> a
sum' = foldr (+) 0

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

last' :: [a] -> a
last' [x] = x
last' (_ : xs) = last' xs