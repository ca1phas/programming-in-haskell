-- Chapter 11 Q3&4
import Data.Char (isDigit, toUpper)
import Data.List (transpose)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

-- Definitions
data Tree a = Node a [Tree a] deriving (Show)

data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

size :: Int
size = 3

winlen :: Int
winlen = 3

depth :: Int
depth = 9

next :: Player -> Player
next O = X
next B = B
next X = O

-- Grid utilities
empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (notElem B)

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any (line p) (rows ++ cols ++ dias)
  where
    rows = g
    cols = transpose g
    dias = [diag g, diag (map reverse g)]

-- Q4b
line :: Player -> [Player] -> Bool
line p ps = line' p ps winlen

line' :: Player -> [Player] -> Int -> Bool
line' p' _ 0 = True
line' p' [] _ = False
line' p' (p : ps) n
  | p == p' = line' p' ps (n - 1)
  | otherwise = line' p' ps winlen

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size - 1]]

won :: Grid -> Bool
won g = wins O g || wins X g

-- IO (Grid)
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave _ [] = []
interleave _ [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

-- Game utilities
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p
  | not $ valid g i = []
  | otherwise = [chop size (xs ++ [p] ++ ys)]
  where
    (xs, B : ys) = splitAt i $ concat g

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= [] && all isDigit xs
    then return $ read xs
    else do
      putStrLn "Invalid number."
      getNat prompt

-- Human vs human
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw \n"
  | otherwise = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "Invalid move"
          run' g p
        [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Game Tree
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. (size ^ 2 - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

-- Q4c
mastertree :: Tree Grid
mastertree = mastertree' empty O

mastertree' :: Grid -> Player -> Tree Grid
mastertree' g p = Node g [mastertree' g' (next p) | g' <- moves g p]

getTree :: Grid -> Tree Grid
getTree = getTree' mastertree

getTree' :: Tree Grid -> Grid -> Tree Grid
getTree' t g
  | g == tg = t
  | otherwise = head [getTree' (Node tg' ts') g | Node tg' ts' <- ts, identical tg' g]
  where
    Node tg ts = t

identical :: Grid -> Grid -> Bool
identical tg g = and [p == p' | (p, p') <- zip (concat tg) (concat g), p /= B]

-- Minimax
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree

-- Q4d
pminimax :: Tree Grid -> Tree (Grid, Player)
pminimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
pminimax (Node g ts)
  | turn g == O =
      Node
        ( g,
          if null os
            then if null bs then X else B
            else O
        )
        ts'
  | turn g == X =
      Node
        ( g,
          if null xs
            then if null bs then O else B
            else X
        )
        ts'
  where
    ts' = [pminimax t | t <- ts]
    xs = [p | Node (_, p) _ <- ts', p == X]
    bs = [p | Node (_, p) _ <- ts', p == B]
    os = [p | Node (_, p) _ <- ts', p == O]

bestmove4 :: Grid -> Grid
bestmove4 g = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (getTree g)
    Node (_, best) ts = pminimax tree

-- Q3
bestmove3 :: Grid -> Grid
bestmove3 g = mintree [(g', maxdepth t) | t <- ts, let Node (g', p') sts = t, p' == best]
  where
    tree = prune depth (getTree g)
    Node (_, best) ts = minimax tree

maxdepth :: Tree a -> Int
maxdepth (Node _ []) = 0
maxdepth (Node _ ts) = 1 + maximum [maxdepth t | t <- ts]

mintree :: [(Grid, Int)] -> Grid
mintree [(x, a)] = x
mintree ((x, a) : (y, b) : ls) = mintree (if a < b then (x, a) : ls else (y, b) : ls)

-- Human vs AI
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  -- Q4a
  putStr "Select Player (O/X): "
  p <- getLine

  case toUpper $ head p of
    'O' -> play empty O X
    'X' -> play empty O O
    '_' -> do
      putStrLn "Invalid player."
      main

play :: Grid -> Player -> Player -> IO ()
play g p ai = do
  cls
  goto (1, 1)
  putGrid g
  play' g p ai

play' :: Grid -> Player -> Player -> IO ()
play' g p ai
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw \n"
  | p /= ai = do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "Invalid move"
          play' g p ai
        [g'] -> play g' (next p) ai
  | p == ai = do
      putStrLn $ "Player " ++ show ai ++ " is thinking..."

      (play $! bestmove4 g) (next p) ai

-- Time taken for 1st move
-- bestmove -> 70s
-- bestmove3 -> 70s
-- bestmove4 -> 12s

-- Time taken for 2nd move
-- bestmove -> 8.5s
-- bestmove3 -> 8.5s
-- bestmove4 -> 0.8s
