-- Definitions
type Pos = (Int, Int)

type Board = [Pos]

-- Screen utilities
cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO ()
writeat p xs = do
  goto p
  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

-- Game of life
width :: Int
width = 10

height :: Int
height = 10

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) =
  map
    wrap
    [ (x - 1, y - 1),
      (x, y - 1),
      (x + 1, y - 1),
      (x - 1, y),
      (x + 1, y),
      (x - 1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    ]

wrap :: Pos -> Pos
wrap (x, y) = ((x - 1) `mod` width + 1, (y - 1) `mod` height + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2, 3]]

births :: Board -> [Pos]
births b =
  [ p
    | p <- rmdups $ concatMap neighbs b,
      isEmpty b p,
      liveneighbs b p == 3
  ]

rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do
  cls
  showcells b
  wait 10000
  life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]

-- Example
glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]
