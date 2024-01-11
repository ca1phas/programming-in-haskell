-- CHAPTER 1
-- Q4:
-- How should the definition of the function qsort be modified
-- so that it produces a reverse sorted version of a list?
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    larger = [a | a <- xs, a > x]
    smaller = [a | a <- xs, a <= x]

rqsort :: (Ord a) => [a] -> [a]
rqsort [] = []
rqsort (x : xs) = rqsort larger ++ [x] ++ rqsort smaller
  where
    larger = [a | a <- xs, a > x]
    smaller = [a | a <- xs, a <= x]

-- Efficiency improvement using constructive induction
eqsort' :: (Ord a) => [a] -> [a] -> [a]
eqsort' [] ys = ys
eqsort' (x : xs) ys = eqsort' smaller $ x : eqsort' larger ys
  where
    larger = [a | a <- xs, a > x]
    smaller = [a | a <- xs, a <= x]

eqsort :: (Ord a) => [a] -> [a]
eqsort xs = eqsort' xs []

erqsort' :: (Ord a) => [a] -> [a] -> [a]
erqsort' [] ys = ys
erqsort' (x : xs) ys = erqsort' larger $ x : erqsort' smaller ys
  where
    larger = [a | a <- xs, a > x]
    smaller = [a | a <- xs, a <= x]

erqsort :: (Ord a) => [a] -> [a]
erqsort xs = erqsort' xs []

-- CHAPTER 2
-- Q4
last' :: [a] -> a
last' [x] = x
last' (x : xs) = last' xs

last'' :: [a] -> a
last'' = head . reverse

-- Q5
init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' (x : xs) = x : init' xs

init'' :: [a] -> [a]
init'' = reverse . tail . reverse

-- CHAPTER 4
-- Q1
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- Q2
-- a: head + tail
third :: [a] -> a
third = head . tail . tail

-- b: !!
third' :: [a] -> a
third' xs = xs !! 2

-- c: pattern matching
third'' :: [a] -> a
third'' (_ : _ : x : _) = x

-- Q3
-- a: conditional expression
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- b: guarded equations
safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

-- c: pattern matching
safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

-- Q4
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

-- Q5
(&&&) :: Bool -> Bool -> Bool
a &&& b =
  if a == True
    then if b == True then True else False
    else False

-- Q6
(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a == True then b else False

-- Q7
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

mult' :: Int -> Int -> Int -> Int
mult' = \x -> \y -> \z -> x * y * z
