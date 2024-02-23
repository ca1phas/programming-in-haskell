import Data.Foldable
import Data.Monoid

-- Chapter 14
-- Q1
-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty :: (a, b)
--   mempty = (mempty, mempty)

--   mappend :: (a, b) -> (a, b) -> (a, b)
--   (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- Q2
-- instance (Monoid b) => Monoid (a -> b) where
--   mempty :: a -> b
--   mempty _ = mempty

--   mappend :: (a -> b) -> (a -> b) -> (a -> b)
--   f `mappend` y = \x -> f x `mappend` g x

-- Q3
-- instance Foldable Maybe where
--   foldMap :: (Monoid b) => (a -> b) -> Maybe a -> b
--   foldMap _ Nothing = mempty
--   foldMap f (Just a) = f a

-- instance Traversable Maybe where
--   traverse :: (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
--   traverse _ Nothing = pure Nothing
--   traverse f (Just a) = Just <$> f a

-- Q4
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap :: (Monoid b) => (a -> b) -> Tree a -> b
  foldMap f Leaf = mempty
  foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r

-- Q5
filterF :: (Foldable t) => (a -> Bool) -> t a -> [a]
filterF f = foldMap (\a -> [a | f a])

-- Chapter 15
-- Q4
fibs' :: Integer -> Integer -> [Integer]
fibs' z y = z' : fibs' z' z where z' = z + y

fibs :: [Integer]
fibs = 0 : 1 : fibs' 1 0

-- Q5
repeatT :: a -> Tree a
repeatT a = Node (repeatT a) a (repeatT a)

takeT :: Int -> Tree a -> Tree a
takeT 0 _ = Leaf
takeT _ Leaf = Leaf
takeT n (Node l a r) = Node (takeT n' l) a (takeT n' r) where n' = n - 1

replicateT :: Int -> a -> Tree a
replicateT n = takeT n . repeatT

-- Q6
as :: Double -> Double -> [Double]
as n a = a' : (if d < 0.000000001 then [] else as n a')
  where
    a' = (a + n / a) / 2
    d = a - a'

sqroot :: Double -> Double
sqroot n = last (as n $ (1 + n) / 2)