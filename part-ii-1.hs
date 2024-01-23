import Data.Char (isDigit)
import System.IO (hSetEcho, stdin)

-- Chapter 10
-- Q1
putStr' :: String -> IO ()
putStr' cs = sequence_ [putChar c | c <- cs]

-- Q4
validInt :: String -> Bool
validInt "" = True
validInt (c : cs) = isDigit c && validInt cs

getSum' :: Int -> Int -> IO Int
getSum' s 0 = return s
getSum' s n = do
  x <- getLine
  if validInt x
    then do
      getSum' (s + (read x :: Int)) (n - 1)
    else getSum' s n

getSum :: Int -> IO Int
getSum = getSum' 0

adder :: IO ()
adder = do
  putStr "How many numbers? "
  n <- getLine
  if validInt n
    then do
      s <- getSum (read n :: Int)
      putStrLn ("The total is " ++ show s)
    else adder

-- Q5
sequence' :: [IO a] -> IO [a]
sequence' [] = return []
sequence' (a : as) = do
  x <- a
  xs <- sequence' as
  return (x : xs)

adder' :: IO ()
adder' = do
  putStr "How many numbers? "
  n <- getLine
  if validInt n
    then do
      xs <-
        sequence'
          [ do
              x <- getLine
              return (read x :: Int)
            | _ <- [1 .. (read n :: Int)]
          ]
      putStrLn ("The total is " ++ show (sum xs))
    else adder

-- Q6
getCh :: IO Char
getCh = do
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  return c

readLine' :: String -> IO String
readLine' cs = do
  c <- getCh
  case c of
    '\n' -> return []
    '\DEL' ->
      do
        putStr "\DEL\b"
        readLine' cs
    _ ->
      do
        cs' <- readLine' cs
        return (c : cs')

readLine :: IO String
readLine = readLine' ""