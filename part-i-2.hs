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
and' :: [Bool] -> Bool
and' = foldr (&&) True

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

-- CHAPTER 7
-- Q1
-- map f (filter p x)

-- Q2
all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f

any' :: (a -> Bool) -> [a] -> Bool
any' f = or . map f

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x : xs)
  | f x = x : takeWhile' f xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x : xs)
  | f x = dropWhile' f xs
  | otherwise = xs

-- Q3
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x xs -> if f x then x : xs else xs) []

-- Q4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> 10 * x + y) 0

-- Q5
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-- Q6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

int2bit :: Int -> [Int]
int2bit = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Int] -> [[Int]]
chop8 = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' = unfold (const False) id

-- Q9
altMap' :: (a -> b) -> (a -> b) -> Bool -> [a] -> [b]
altMap' _ _ _ [] = []
altMap' f g True (a : as) = f a : altMap' f g False as
altMap' f g False (a : as) = g a : altMap' f g True as

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = altMap' f g True

-- CHAPTER 10
-- Q1
data Nat = Zero | Succ Nat
  deriving (Show)

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = add n (Succ m)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ Zero) m = m
mult (Succ n) m = add m (mult n m)

-- Q2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: (Ord a) => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r)
  | x < y = occurs x l
  | x > y = occurs x r
  | otherwise = x == y

occurs' :: (Ord a) => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node l y r)
  | ordering == LT = occurs' x l
  | ordering == GT = occurs' x r
  | ordering == EQ = True
  | otherwise = False
  where
    ordering = compare x y

-- Q3
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

leaves :: Tree' a -> Int
leaves (Leaf' _) = 1
leaves (Node' l r) = leaves l + leaves r

balanced :: Tree' a -> Bool
balanced (Leaf' _) = True
balanced (Node' l r) =
  abs (leaves l - leaves r) <= 1
    && balanced l
    && balanced r

-- Q4
halves :: [a] -> ([a], [a])
halves xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance as) (balance bs)
  where
    (as, bs) = halves xs

-- Q5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- Q6
data Maybe' a = Nothing' | Just' a

instance (Eq a) => Eq (Maybe' a) where
  (==) :: Maybe' a -> Maybe' a -> Bool
  Just' a == Just' b = a == b
  Nothing' == Nothing' = True
  _ == _ = False

-- instance (Eq a) => Eq [a] where
--   (==) :: [a] -> [a] -> Bool
--   [] == [] = True
--   (x : xs) == (y : ys) = x == y && xs == ys
--   _ == _ = False
