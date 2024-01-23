import Data.Char (isDigit)

-- Defintions
type Row = Int

type Count = Int

type Player = Int

type Board = [Row]

-- Game Settings
noRows :: Int
noRows = 10

startingRow :: Row
startingRow = 1

startingPlayer :: Player
startingPlayer = 1

rowIndex :: Row -> Int
rowIndex row = row - startingRow

-- Logic (Board)
newBoard :: Int -> Board
newBoard 0 = []
newBoard n = n : newBoard (n - 1)

updateBoard' :: Board -> Row -> Count -> Board
updateBoard' [] _ _ = []
updateBoard' (_ : rs) 0 newCount = newCount : rs
updateBoard' (r : rs) n newCount = r : updateBoard' rs (n - 1) newCount

updateBoard :: Board -> Row -> Count -> Board
updateBoard board row = updateBoard' board (rowIndex row)

removeStars :: Board -> Row -> Count -> Board
removeStars board row count = updateBoard board row (board !! rowIndex row - count)

emptyBoard :: Board -> Bool
emptyBoard = all (== 0)

-- Logic (Input)
isInt :: String -> Bool
isInt "" = True
isInt (c : cs) = isDigit c && isInt cs

validRowStr :: Board -> String -> Bool
validRowStr board rowStr =
  isInt rowStr
    && let i = rowIndex (read rowStr :: Int)
        in i >= 0 && i < length board && board !! i /= 0

validCountStr :: Board -> Row -> String -> Bool
validCountStr board row countStr =
  isInt countStr
    && ( let count = read countStr :: Int
          in count > 0 && count <= board !! rowIndex row
       )

-- Logic (Game)
changePlayer :: Player -> Player
changePlayer 1 = 2
changePlayer 2 = 1

-- IO
-- Chapter 10 Q2&3
printBoard :: Board -> IO ()
printBoard board =
  sequence_
    [ putStrLn (show i ++ " : " ++ replicate r '*')
      | (i, r) <- zip [1 ..] board
    ]

getRow :: Board -> IO Int
getRow board = do
  putStr "Select row: "
  rowStr <- getLine
  if validRowStr board rowStr
    then do
      putStr "\n"
      return (read rowStr :: Int)
    else do
      putStrLn "Invalid row"
      getRow board

getCount :: Board -> Row -> IO Int
getCount board row = do
  putStr "Enter count: "
  countStr <- getLine
  if validCountStr board row countStr
    then do
      putStr "\n"
      return (read countStr :: Int)
    else do
      putStrLn "Invalid count"
      getCount board row

play :: Board -> Player -> IO ()
play board player = do
  putStrLn $ "It's Player " ++ show player ++ "'s turn."
  printBoard board
  row <- getRow board
  count <- getCount board row
  let board' = removeStars board row count
   in if emptyBoard board'
        then do putStrLn $ "The winner is Player " ++ show player ++ "!"
        else do play board' $ changePlayer player

main :: IO ()
main = play (newBoard noRows) startingPlayer
