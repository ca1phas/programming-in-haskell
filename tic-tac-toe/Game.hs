module Game where

import Data.Char (isDigit)

-- Defintiions

data Player = X | O deriving (Show, Eq)

type Move = (Int, Int)

type Board = ([Move], [Move])

allMoves :: [Move]
allMoves = [(1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (2, 3), (3, 1), (3, 2), (3, 3)]

-- Game logic
winRow :: [Move] -> Bool
winRow mvs =
  and [s `elem` mvs | s <- r1]
    || and [s `elem` mvs | s <- r2]
    || and [s `elem` mvs | s <- r3]
  where
    r1 = [(1, 1), (1, 2), (1, 3)]
    r2 = [(2, 1), (2, 2), (2, 3)]
    r3 = [(3, 1), (3, 2), (3, 3)]

winCol :: [Move] -> Bool
winCol mvs =
  and [s `elem` mvs | s <- c1]
    || and [s `elem` mvs | s <- c2]
    || and [s `elem` mvs | s <- c3]
  where
    c1 = [(1, 1), (2, 1), (3, 1)]
    c2 = [(1, 2), (2, 2), (3, 2)]
    c3 = [(1, 3), (2, 3), (3, 3)]

winDia :: [Move] -> Bool
winDia mvs =
  (2, 2) `elem` mvs
    && (and [s `elem` mvs | s <- d1] || and [s `elem` mvs | s <- d2])
  where
    d1 = [(1, 1), (3, 3)]
    d2 = [(1, 3), (3, 1)]

winner :: [Move] -> Bool
winner mvs = winRow mvs || winCol mvs || winDia mvs

blank :: Board -> Bool
blank (xs, os) = null xs && null os

complete :: Board -> Bool
complete (xs, os) = (length xs + length os) == 9

switch :: Player -> Player
switch X = O
switch O = X

makeMove :: Board -> Player -> Move -> Board
makeMove (xs, os) X move = (move : xs, os)
makeMove (xs, os) O move = (xs, move : os)

-- IO Utilities
printBoard' :: Board -> [String]
printBoard' (xs, os) =
  [ [if (r, c) `elem` xs then 'X' else if (r, c) `elem` os then 'O' else ' ' | c <- [1 .. 3]]
    | r <- [1 .. 3]
  ]

printBoard :: Board -> IO ()
printBoard board = sequence_ [putStrLn r | r <- printBoard' board]

validMoves :: Board -> [Move]
validMoves (xs, os) = filter (\p -> not (p `elem` xs || p `elem` os)) allMoves

validInt :: String -> Bool
validInt "" = True
validInt (c : cs) = isDigit c && validInt cs

getRow :: Board -> IO Int
getRow board = do
  putStr "Select row: "
  r <- getLine
  if validInt r && read r < 4 && read r > 0
    then return $ read r
    else do
      putStrLn "Invalid row."
      getRow board

getCol :: Board -> IO Int
getCol board = do
  putStr "Select column: "
  c <- getLine
  if validInt c && read c < 4 && read c > 0
    then return $ read c
    else do
      putStrLn "Invalid column."
      getCol board

getMove :: Board -> IO Move
getMove board = do
  r <- getRow board
  c <- getCol board

  if (r, c) `elem` validMoves board
    then return (r, c)
    else do
      putStrLn "Invalid move."
      getMove board

play' :: Board -> Player -> IO ()
play' board player = do
  printBoard board

  let (xs, os) = board

  if winner xs
    then putStrLn "The winner is Player X!"
    else
      if winner os
        then putStrLn "The winner is Player O!"
        else
          if complete board
            then putStrLn "It's a draw."
            else do
              putStrLn ("It's Player " ++ show player ++ "'s turn.")

              move <- getMove board

              play' (makeMove board player move) (switch player)

play :: IO ()
play = play' ([], []) X
