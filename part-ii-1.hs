import Data.Char (isDigit, ord, toLower)
import System.IO (hSetEcho, stdin)

-- Chapter 10
-- Q1
putStr' :: String -> IO ()
putStr' cs = sequence_ [putChar c | c <- cs]

-- Q4
validInt :: String -> Bool
validInt "" = True
validInt (c : cs) = isDigit c && validInt cs

getSum' :: Int -> Int -> IO Int
getSum' s 0 = return s
getSum' s n = do
  x <- getLine
  if validInt x
    then do
      getSum' (s + (read x :: Int)) (n - 1)
    else getSum' s n

getSum :: Int -> IO Int
getSum = getSum' 0

adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- getLine
  if validInt n
    then do
      s <- getSum (read n :: Int)
      putStrLn ("The total is " ++ show s)
    else adder

-- Q5
sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (a : as) = do
  x <- a
  xs <- sequence' as
  return (x : xs)

adder' :: IO ()
adder' = do
  putStr "How many numbers? "
  n <- getLine
  if validInt n
    then do
      xs <-
        sequence'
          [ do
              x <- getLine
              return (read x :: Int)
            | _ <- [1 .. (read n :: Int)]
          ]
      putStrLn ("The total is " ++ show (sum xs))
    else adder

-- Q6
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  return c

readLine' :: String -> IO String
readLine' cs = do
  c <- getCh
  case c of
    '\n' -> return []
    '\DEL' ->
      do
        putStr "\DEL\b"
        readLine' cs
    _ ->
      do
        cs' <- readLine' cs
        return (c : cs')

readLine :: IO String
readLine = readLine' ""

-- Chapter 12
-- Q1
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

-- Q2
-- instance Functor ((->) a) where
--   fmap :: (b -> c) -> (a -> b) -> (a -> c)
--   fmap = (.)

-- Q3
-- instance Applicative ((->) a) where
--   pure :: b -> (a -> b)
--   pure = const

--   (<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
--   g <*> h = \x -> g x . h x

-- Q4
newtype ZipList a = Z [a] deriving (Show)

instance Functor ZipList where
  fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap f (Z as) = Z [f a | a <- as]

instance Applicative ZipList where
  pure :: a -> ZipList a
  pure x = Z $ repeat x

  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  Z fs <*> Z as = Z [f a | (f, a) <- zip fs as]

-- Q5
-- 1st law: pure id <*> v = v
-- pure id :: f (a -> a)
-- v :: f a

-- 2nd law: pure (g x) = pure g <*> pure x
-- g :: a -> b
-- x :: a
-- g x :: b
-- pure (g x) :: f b
-- pure g :: f (a -> b)
-- pure x :: f a
-- pure g <*> pure x :: f b

-- 3rd law: u <*> pure y = pure (\g -> g y) <*> u
-- y :: b
-- pure y :: f b
-- u :: f (b -> a)
-- u <*> pure y :: f a
-- \g -> g y :: (b -> a) -> a
-- pure (\g -> g y) :: f ((b -> a) -> a)
-- pure (\g -> g y) <*> u :: f a

-- 4th law: u <*> v <*> w = (pure (.) <*> u <*> v) <*> w
-- u :: f (b -> c)
-- v :: f (a -> b)
-- w :: f a
-- u <*> v :: f (a -> c)
-- u <*> v <*> w :: f c
-- pure (.) -> f ((b -> c) -> (a -> b) -> a -> c)
-- pure (.) <*> u :: f ((a -> b) -> a -> c)
-- pure (.) <*> u <*> v :: f (a -> c)
-- (pure (.) <*> u <*> v) <*> w :: f c

-- Q6
-- instance Monad ((->) a) where
--   return :: b -> (a -> b)
--   return = pure

--   (>>=) :: (a -> b) -> (b -> (a -> c)) -> (a -> c)
--   g >>= h = \x -> h (g x) x

-- Q7
data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap _ (Val n) = Val n
  fmap f (Var a) = Var (f a)
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
  pure :: a -> Expr a
  pure = Var

  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  Val n <*> _ = Val n
  Var f <*> e = fmap f e
  Add g h <*> e = Add (g <*> e) (h <*> e)

instance Monad Expr where
  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Val n >>= _ = Val n
  Var a >>= f = f a
  Add l r >>= f = Add (l >>= f) (r >>= f)

expr :: Expr Char
expr = Add (Var 'x') (Val 5)

sub :: Char -> Expr Char
sub var = Val (ord var)

result :: Expr Char
result = do
  e <- expr
  sub e

-- Q8
type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do
    a <- st
    return $ g a

instance Applicative ST where
  pure :: a -> ST a
  pure x = S (x,)

  (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> sta = do
    f <- stf
    a <- sta
    return $ f a

instance Monad ST where
  (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')
