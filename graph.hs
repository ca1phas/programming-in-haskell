-- Chinese postman problem, brute force search
type Pt = Char

type Edge = (String, Int)

type PtEdge = (Pt, [Edge])

es0 :: [Edge]
es0 =
  [ ("ac", 1),
    ("ae", 1),
    ("bd", 1),
    ("de", 1),
    ("ad", 3),
    ("ab", 4),
    ("cd", 4),
    ("bc", 5)
  ]

ptes :: [PtEdge]
ptes = [(p, match p) | p <- "abcde"]
  where
    match p = [e | e <- es0, p `elem` fst e]

next :: Pt -> [Edge]
next p = head [es' | (p', es') <- ptes, p == p']

allused :: [Edge] -> Bool
allused es = all (`elem` es) es0

cpp' :: Pt -> Pt -> [Edge] -> [([Pt], Int)]
cpp' a p used
  | allused used && a == p = [("", 0)]
  | otherwise =
      [ (p' : ps', c + c')
        | (ps, c) <- nxtes,
          let p' = head $ filter (/= p) ps,
          (ps', c') <- cpp' a p' ((ps, c) : used)
      ]
  where
    es = next p
    avlbes = filter (`notElem` used) es
    nxtes =
      if null avlbes
        then
          let nps = [head $ filter (/= p) ps | (ps, c) <- es]
              nps' = filter (any (`notElem` used) . next) nps
           in [(ps, cs) | (ps, cs) <- es, head (filter (/= p) ps) `elem` nps']
        else avlbes

cpp :: Pt -> [([Pt], Int)]
cpp p = cpp' p p []

best' :: ([Pt], Int) -> [([Pt], Int)] -> ([Pt], Int)
best' b [] = b
best' (pts, c) ((pts', c') : xs) = best' (if c < c' then (pts, c) else (pts', c')) xs

best :: ([Pt], Int)
best = best' ("", 10000) [s | p <- "abcde", s <- cpp p]
