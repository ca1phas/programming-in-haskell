import Data.List (sort)

-- First past the post
votes :: [Int]
votes = [0, 2, 1, 2, 2, 0]

countVotes :: (Eq a) => a -> [a] -> Int
countVotes x = length . filter (== x)

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

result :: (Ord a) => [a] -> [(Int, a)]
result vs = sort [(countVotes v vs, v) | v <- removeDuplicates vs]

winner :: (Ord a) => [a] -> a
winner = snd . last . result

-- Alternative vote system
ballots :: [[Int]]
ballots =
  [ [0, 1],
    [2],
    [2, 0, 1],
    [2, 1, 0],
    [1]
  ]

rmempty :: (Eq a) => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: (Eq a) => a -> [[a]] -> [[a]]
elim a = map (filter (/= a))

rank :: (Ord a) => [[a]] -> [a]
rank = map snd . result . map head

winner' :: (Ord a) => [[a]] -> a
winner' votes =
  case rank (rmempty votes) of
    [c] -> c
    (c : cs) -> winner' (elim c votes)