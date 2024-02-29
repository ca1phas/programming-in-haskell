import Data.Set (Set, fromList)

unfoldm :: (Monoid m) => (t -> Maybe (m, t)) -> t -> m
unfoldm f x =
  case f x of
    Just (m, t) -> m `mappend` unfoldm f t
    Nothing -> mempty

setExample :: (Ord a, Num a) => a -> Set a
setExample = unfoldm (\x -> if x > 100 then Nothing else Just (fromList [x], x + 1))
