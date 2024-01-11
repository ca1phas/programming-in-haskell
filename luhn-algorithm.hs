-- CHAPTER 4
-- Q8: Luhn algorithm
luhnSubtract :: Int -> Int
luhnSubtract n
  | n > 9 = n - 9
  | otherwise = n

luhnDouble :: Int -> Int
luhnDouble n = luhnSubtract (n * 2)

luhn4 :: Int -> Int -> Int -> Int -> Bool
luhn4 a b c d = (a' + b' + c' + d') `mod` 10 == 0
  where
    a' = luhnDouble a
    b' = luhnSubtract b
    c' = luhnDouble c
    d' = luhnSubtract d