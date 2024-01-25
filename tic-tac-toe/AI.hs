module AI where

import Game
  ( Board,
    Move,
    Player (O, X),
    allMoves,
    blank,
    complete,
    getMove,
    makeMove,
    play,
    printBoard,
    switch,
    validMoves,
    winner,
  )

snds :: [(a, b)] -> [b]
snds [] = []
snds ((_, b) : xs) = b : snds xs

boardScore :: Board -> Int
boardScore (xs, os)
  | winner xs = 1
  | winner os = -1
  | otherwise = 0

gameEnd :: Board -> Bool
gameEnd (xs, os) = winner xs || winner os || complete (xs, os)

maxBoardScore :: Board -> Int
maxBoardScore board
  | gameEnd board = boardScore board
  | otherwise = maximum [minBoardScore (mv : xs, os) | mv <- validMoves board]
  where
    (xs, os) = board

minBoardScore :: Board -> Int
minBoardScore board
  | gameEnd board = boardScore board
  | otherwise = minimum [maxBoardScore (xs, mv : os) | mv <- validMoves board]
  where
    (xs, os) = board

maxMoves :: Board -> [Move]
maxMoves (xs, os) = [mv | (mv, s) <- moveScores, s == maxScore]
  where
    moveScores = [(mv, minBoardScore (mv : xs, os)) | mv <- validMoves (xs, os)]
    maxScore = maximum $ snds moveScores

minMoves :: Board -> [Move]
minMoves (xs, os) = [mv | (mv, s) <- moveScores, s == minScore]
  where
    moveScores = [(mv, maxBoardScore (xs, mv : os)) | mv <- validMoves (xs, os)]
    minScore = minimum $ snds moveScores

bestMoves :: Board -> Player -> [Move]
bestMoves board player
  | blank board = [(1, 1)]
  | player == X = maxMoves board
  | otherwise = minMoves board

b :: Board
b =
  ( [(2, 3), (3, 1), (1, 1)],
    [(1, 2), (2, 1), (2, 2)]
  )

playAI' :: Player -> Board -> Player -> IO ()
playAI' ai board player = do
  printBoard board

  let (xs, os) = board

  if winner xs
    then putStrLn (if ai == X then "You lost." else "You won!")
    else
      if winner os
        then putStrLn (if ai == O then "You lost." else "You won!")
        else
          if complete board
            then putStrLn "It's a draw."
            else
              if ai == player
                then do
                  putStrLn "It's the AI's turn."

                  let moves = bestMoves board ai

                  playAI' ai (makeMove board player $ head moves) (switch player)
                else do
                  putStrLn ("It's Player " ++ show player ++ "'s turn.")

                  move <- getMove board

                  playAI' ai (makeMove board player move) (switch player)

playAI :: Player -> IO ()
playAI ai = playAI' ai ([], []) X
