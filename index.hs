-- C1 Q4:
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