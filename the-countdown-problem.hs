import Data.List (sortBy)

data Op = Add | Sub | Mul | Div

ops :: [Op]
ops = [Add, Sub, Mul, Div]

instance Show Op where
  show :: Op -> String
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Sub x y = x > y
valid Div x y = x `mod` y == 0
valid _ _ _ = True

valid' :: Op -> Int -> Int -> Bool
valid' Sub x y = x > y
valid' Add x y = x <= y
valid' Mul x y = x <= y && x /= 1 && y /= 1
valid' Div x y = x `mod` y == 0 && y /= 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show :: Expr -> String
  show (Val n) = show n
  show (App o l r) = show' l ++ show o ++ show' r
    where
      show' (Val n) = show n
      show' e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) =
  [ apply o x y
    | x <- eval l,
      y <- eval r,
      valid o x y
  ]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x :) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concatMap (interleave x) (perms xs)

choices :: [a] -> [[a]]
choices = concatMap perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = values e `elem` choices ns && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [ e
    | (ls, rs) <- split ns,
      l <- exprs ls,
      r <- exprs rs,
      e <- combine l r
  ]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns =
  [ res
    | (ls, rs) <- split ns,
      lx <- results ls,
      ry <- results rs,
      res <- combine' lx ry
  ]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid' o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]

-- Chapter 9
-- Q1
choices' :: [a] -> [[a]]
choices' xs = [ps | ss <- subs xs, ps <- perms ss]

-- Q2
removeone :: (Eq a) => a -> [a] -> [a]
removeone x [] = []
removeone x (y : ys)
  | x == y = ys
  | otherwise = y : removeone x ys

isChoice :: (Eq a) => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x : xs) ys = x `elem` ys && isChoice xs (removeone x ys)

-- Q4
-- total = 33,665,406
total :: Int
total = length [e | cs <- choices [1, 3, 7, 10, 25, 50], e <- exprs cs]

-- stotal = 245,644
stotal :: Int
stotal = length [i | cs <- choices [1, 3, 7, 10, 25, 50], e <- exprs cs, i <- eval e]

-- Q6
-- b
solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n
  | null ss = solutions'' ns (n - 1) ++ solutions'' ns (n + 1)
  | otherwise = ss
  where
    ss = solutions' ns n

-- c
eops :: Expr -> [Op]
eops (Val n) = []
eops (App o l r) = o : eops l ++ eops r

sortGT :: (Ord a1, Ord a2) => (a1, a2, c1) -> (a1, a2, c2) -> Ordering
sortGT (a, b, _) (c, d, _)
  | a < c = LT
  | a > c = GT
  | otherwise = compare b d

osolutions :: [Int] -> Int -> [Expr]
osolutions ns n = [s | (_, _, s) <- sortBy sortGT ss]
  where
    ss =
      [ (length $ values e, length $ eops e, e)
        | ns' <- choices ns,
          (e, m) <- results ns',
          m == n
      ]