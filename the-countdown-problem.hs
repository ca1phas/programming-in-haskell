import Control.Monad (guard)
import Data.List (permutations)

data Expr
  = Val Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr

instance Show Expr where
  show :: Expr -> String
  show (Val n) = show n
  show (Add l r) = show l ++ "+" ++ show r
  show (Sub l r) = show l ++ "-" ++ show r
  show (Mul (Val n) (Val m)) = show n ++ "*" ++ show m
  show (Mul l r) = "(" ++ show l ++ ")*(" ++ show r ++ ")"
  show (Div (Val n) (Val m)) = show n ++ "/" ++ show m
  show (Div l r) = "(" ++ show l ++ ")/(" ++ show r ++ ")"

ops :: [Expr -> Expr -> Expr]
ops = [Add, Sub, Mul, Div]

comb :: [Int] -> [[Int]]
comb [] = [[]]
comb (x : xs) = cs ++ [x : c | c <- cs] where cs = comb xs

perm :: [Int] -> [[Int]]
perm xs = [p | c <- comb xs, p <- permutations c]

split :: [Int] -> [([Int], [Int])]
split xs = [splitAt n xs | n <- [1 .. length xs - 1]]

generate' :: [Int] -> [Expr]
generate' [] = []
generate' [x] = [Val x]
generate' xs = [op el er | p <- perm xs, (ls, rs) <- split p, el <- generate' ls, er <- generate' rs, op <- ops]

generate :: [Int] -> [Expr]
generate xs = [Val x | x <- xs] ++ generate' xs

eval :: Expr -> Maybe Int
eval (Val n) = if n > 0 then Just n else Nothing
eval (Add l r) = (+) <$> eval l <*> eval r
eval (Mul l r) = (*) <$> eval l <*> eval r
eval (Sub l r) = do
  x <- eval l
  y <- eval r
  guard (x > y)
  Just (x - y)
eval (Div l r) = do
  x <- eval l
  y <- eval r
  guard (x `mod` y == 0)
  Just (x `div` y)

solutions :: Int -> [Int] -> [Expr]
solutions t xs = [e | e <- generate xs, eval e == Just t]