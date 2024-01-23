data Status = Empty | Cell deriving (Eq, Show)

type Row = [Status]

type Board = [Row]

type Matrix = (Int, Int)

-- Utilities
neighbours :: Board -> Matrix -> [Matrix]
neighbours board (m, n) =
  [ (m', n')
    | m' <- [m - 1, m, m + 1],
      m' >= 0 && m' < size,
      n' <- [n - 1, n, n + 1],
      n' >= 0 && n' < size,
      m' /= m || n' /= n
  ]
  where
    size = length board

countCell' :: Board -> [Matrix] -> Int -> Int
countCell' _ [] i = i
countCell' board ((m, n) : ns) i
  | board !! m !! n == Cell = countCell' board ns $ i + 1
  | otherwise = countCell' board ns i

countCell :: Board -> Matrix -> Int
countCell board cell = countCell' board (neighbours board cell) 0

-- Rules
survive :: Board -> Matrix -> Bool
survive board (m, n) = board !! m !! n == Cell && (noLivingCells == 2 || noLivingCells == 3)
  where
    noLivingCells = countCell board (m, n)

birth :: Board -> Matrix -> Bool
birth board (m, n) = board !! m !! n == Empty && countCell board (m, n) == 3

-- Main
nextGen :: Board -> Board
nextGen board =
  [ [ if survive board (m, n) || birth board (m, n)
        then Cell
        else Empty
      | n <- ls
    ]
    | m <- ls
  ]
  where
    ls = [0 .. length board - 1]
    size = length board

initial :: Board
initial =
  [ [Empty, Empty, Empty, Empty, Empty],
    [Empty, Empty, Empty, Cell, Empty],
    [Empty, Cell, Empty, Cell, Empty],
    [Empty, Empty, Cell, Cell, Empty],
    [Empty, Empty, Empty, Empty, Empty]
  ]
