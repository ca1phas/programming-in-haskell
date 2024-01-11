type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  fmap :: (a -> b) -> ST a -> ST b
  fmap f sta =
    S
      ( \s ->
          let (a, s') = app sta s
           in (f a, s')
      )

instance Applicative ST where
  pure :: a -> ST a
  pure a = S (a,)

  (<*>) :: ST (a -> b) -> ST a -> ST b
  stf <*> sta =
    S
      ( \s ->
          let (a, s') = app sta s
              (f, s'') = app stf s'
           in (f a, s'')
      )

instance Monad ST where
  -- Laws
  -- (1) mx >>= return == mx
  -- (2) return x >>= f == f x
  -- (3) (mx >>= f) >>= g == mx >>= (\x -> (f x >== g))

  return :: a -> ST a
  return = pure

  (>>=) :: ST a -> (a -> ST b) -> ST b
  sta >>= f =
    S
      ( \s ->
          let (a, s') = app sta s
           in app (f a) s'
      )

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance (Show a) => Show (Tree a) where
  show (Leaf a) = "Leaf " ++ show a
  show (Node l r) = "Node (" ++ show l ++ ") (" ++ show r ++ ")"

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf a) = Leaf <$> fresh
mlabel (Node l r) =
  do
    l' <- mlabel l
    r' <- mlabel r
    return (Node l' r')

label :: Tree a -> Tree Int
label t = fst (app (mlabel t) 1)

t1 :: Tree Char
t1 = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x : xs) =
  do
    y <- f x
    ys <- mapM' f xs
    return (y : ys)

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

join' :: (Monad m) => m (m a) -> m a
join' mmx =
  do
    mx <- mmx
    x <- mx
    return x

data Expr a
  = Var a
  | Val Int
  | Add (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  fmap :: (a -> b) -> Expr a -> Expr b
  fmap _ (Val n) = Val n
  fmap f (Var a) = Var (f a)
  fmap f (Add l r) = Add (fmap f l) (fmap f r)

instance Applicative Expr where
  pure :: a -> Expr a
  pure = Var

  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _ <*> Val n = Val n
  Var f <*> e = fmap f e
  Add eF eG <*> e = Add (eF <*> e) (eG <*> e)

instance Monad Expr where
  return :: a -> Expr a
  return = pure

  (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var a >>= f = f a
  Val n >>= _ = Val n
  Add l r >>= f = Add (l >>= f) (r >>= f)
